
module type Letter = sig

  type t

  val equiv : t -> t -> bool
  val compare : t -> t -> int
  val epsilon : t
  val is_epsilon : t -> bool
end

module type S = sig

  type lt

  type t_simp =
    | S_Empty
    | S_Letter of lt
    | S_Concat of t_simp * t_simp
    | S_Union of t_simp * t_simp
    | S_Star of t_simp

  type t_ext =
    | Letter of lt
    | Concat of t_ext list
    | Union of t_ext list
    | Star of t_ext
    | Plus of t_ext
    | Option of t_ext

  val empty : t_simp
  val is_empty : t_simp -> bool

  val letter : lt -> t_simp
  val concat : t_simp -> t_simp -> t_simp
  val union : t_simp -> t_simp -> t_simp
  val star : t_simp -> t_simp

  val simp_to_ext : t_simp -> t_ext
  val simplify : t_ext -> t_ext

end

module Make (Lt : Letter) : S with type lt = Lt.t = struct

  type lt = Lt.t

  type t_simp =
    | S_Empty
    | S_Letter of lt
    | S_Concat of t_simp * t_simp
    | S_Union of t_simp * t_simp
    | S_Star of t_simp

  type t_ext =
    | Letter of lt
    | Concat of t_ext list
    | Union of t_ext list
    | Star of t_ext
    | Plus of t_ext
    | Option of t_ext

  let empty = S_Empty

  let is_empty (r : t_simp) : bool =
    r = empty

  let letter (l : lt) : t_simp =
    S_Letter l

  let concat (r1 : t_simp)
      (r2 : t_simp) : t_simp =
    S_Concat (r1, r2)

  let union (r1 : t_simp)
      (r2 : t_simp) : t_simp =
    S_Union (r1, r2)

  let star (r : t_simp) : t_simp =
    S_Star r

  let rec simp_to_ext (r : t_simp) : t_ext =
    match r with
    | S_Empty -> failwith "Can't convert the empty regex"
    | S_Letter l -> Letter l
    | S_Concat (r1, r2) -> Concat [simp_to_ext r1; simp_to_ext r2]
    | S_Union (r1, r2) -> Union [simp_to_ext r1; simp_to_ext r2]
    | S_Star r -> Star (simp_to_ext r)

  let rec flatten_aux (f : t_ext -> t_ext list option)
      (l : t_ext list) : t_ext list =
    let flatten_aux' = flatten_aux f in
    match l with
    | [] -> []
    | r :: l -> (
        match f r with
        | Some ll -> List.append ll @@ flatten_aux' l
        | None -> r :: flatten_aux' l
      )
  and flatten (r : t_ext) : t_ext =
    match r with
    | Letter _ -> r
    | Concat l ->
      let f =
        fun (r : t_ext) : t_ext list option ->
          match r with
          | Concat l -> Some l
          | _ -> None
      in
      Concat (flatten_aux f @@ List.map flatten l)
    | Union l ->
      let f =
        fun (r : t_ext) : t_ext list option ->
          match r with
          | Union l -> Some l
          | _ -> None
      in
      Union (flatten_aux f @@ List.map flatten l)
    | Star r -> Star (flatten r)
    | Plus r -> Plus (flatten r)
    | Option r -> Option (flatten r)

  let get_rid_of_duplicate (comp : 'a -> 'a -> bool)
      (l : 'a list) : 'a list =
    List.rev @@ List.fold_left (
      fun (acc : 'a list)
        (elt : 'a) : 'a list ->
        if List.exists (comp elt) acc then
          acc
        else
          elt :: acc
    )
      [] l

  let simplify (r : t_ext) : t_ext =
    let rec compare (r1 : t_ext)
        (r2 : t_ext) : bool =
      match r1, r2 with
      | Letter lt1, Letter lt2 -> Lt.equiv lt1 lt2
      | Concat l1, Concat l2
      | Union l1, Union l2 -> (try List.for_all2 compare l1 l2 with _ -> false)
      | Star r1, Star r2
      | Plus r1, Plus r2
      | Option r1, Option r2 -> compare r1 r2
      | _, _ -> false
    and simp (r : t_ext) : t_ext =
      match r with
      | Letter _ -> r
      | Concat l ->
        let l = List.map simp l in
        let rec loop (l : t_ext list) : t_ext list =
          match l with
          | []
          | _ :: [] -> l
          | r1 :: r2 :: l ->
            match r1, r2 with
            | Star rr1, Star rr2 ->
              if compare rr1 rr2 then
                loop @@ (Star rr1) :: l
              else
                List.cons r1 @@ loop @@ r2 :: l
            | Star rr1, Option rr2
            | Option rr1, Star rr2 ->
              if compare rr1 rr2 then
                loop @@ (simp @@ Star rr1) :: l
              else
                List.cons r1 @@ loop @@ r2 :: l
            | Star rr1, rr2
            | rr1, Star rr2 ->
              if compare rr1 rr2 then
                loop @@ (simp @@ Plus rr1) :: l
              else
                List.cons r1 @@ loop @@ r2 :: l
            | Letter lt1, Letter lt2 ->
              if Lt.is_epsilon lt1 then
                loop @@ r2 :: l
              else if Lt.is_epsilon lt2 then
                loop @@ r1 :: l
              else
                List.cons r1 @@ loop @@ r2 :: l
            | Letter lt, _ ->
              if Lt.is_epsilon lt then
                loop @@ r2 :: l
              else
                List.cons r1 @@ loop @@ r2 :: l
            | _, Letter lt ->
              if Lt.is_epsilon lt then
                loop @@ r1 :: l
              else
                List.cons r1 @@ loop @@ r2 :: l
            | _ ->
              List.cons r1 @@ loop @@ r2 :: l
        in
        (
          match loop l with
          | [] -> Letter Lt.epsilon
          | r :: [] -> r
          | l -> Concat l
        )
      | Union l ->
        let unique_l = get_rid_of_duplicate compare @@ List.map simp l
        in
        (
          match unique_l with
          | [] -> assert false
          | r :: [] -> r
          | _ -> (
              let (all_eps, without_eps) = List.partition (
                  fun (r : t_ext) : bool ->
                    match r with
                    | Letter lt -> Lt.is_epsilon lt
                    | _ -> false
                )
                  unique_l
              in

              (* Returns the prefix and what it comes next *)
              let calc_prefix (r : t_ext) : t_ext option * t_ext option =
                match r with
                | Concat l -> (
                    match l with
                    | [] -> None, None
                    | r' :: next -> Some r', Some (Concat next)
                  )
                | Plus r -> Some r, Some (Star r)
                | _ -> Some r, None
              in
              (* Returns the suffix and what it comes before *)
              let rec calc_suffix (r : t_ext) : t_ext option * t_ext option =
                match r with
                | Concat l -> (
                    match l with
                    | [] -> None, None
                    | r :: [] -> Some r, None
                    | r' :: next ->
                      let (suff, before) = calc_suffix @@ Concat next in
                      match before with
                      | None -> suff, Some r'
                      | Some before -> suff, Some (flatten @@ Concat [r'; before])
                  )
                | Plus r -> Some (Star r), Some r
                | _ -> Some r, None
              in
              (* Factorize

                  The boolean tells us if we can simplify the result, i.e
                    it has been factorized, thus changed (it prevents
                    looping infinitely when simplifying the new expression)
              *)
              let factorize (r : t_ext) : t_ext * bool =
                let aux (r : t_ext)
                    (calc : t_ext -> t_ext option * t_ext option)
                    (concat_factors : t_ext -> t_ext -> t_ext)
                    (factorize : t_ext -> t_ext list -> t_ext) : t_ext * bool =
                  match r with
                  | Union l ->
                    (* list of all regexp without the common factor *)
                    let max_left = ref l in
                    (* greatest common factor found *)
                    let max_factor = ref None in
                    let factor_not_found = ref true in
                    let has_been_factorized = ref false in
                    let () =
                      while !factor_not_found do
                        let all_factors, all_left = List.split
                          @@ List.map calc !max_left
                        in
                        let compare_opt (o1 : t_ext option)
                            (o2 : t_ext option) : bool =
                          match o1, o2 with
                          | None, None -> true
                          | Some r1, Some r2 -> compare r1 r2
                          | _ -> false
                        in
                        match get_rid_of_duplicate compare_opt all_factors with
                        | [] -> assert false (* No factor : issue *)
                        | Some factor :: [] -> (
                            has_been_factorized := true ;
                            max_left := List.map (
                                fun (left : t_ext option) : t_ext ->
                                  match left with
                                  | None -> Letter Lt.epsilon
                                  | Some r -> r
                              )
                                all_left ;
                            match !max_factor with
                            | None -> max_factor := Some factor
                            | Some old_factor ->
                              max_factor := Some (concat_factors old_factor factor)
                          )
                        | None :: [] (* No factor found *)
                        | _ -> factor_not_found := false (* More than one factor *)
                      done
                    in
                    (
                      match !max_factor with
                      | None -> r, false
                      | Some factor -> factorize factor !max_left, !has_been_factorized
                    )
                  | _ -> r, false
                in
                let r, is_factorized = aux r calc_prefix
                    (
                      fun (old_factor : t_ext)
                        (factor : t_ext) : t_ext ->
                        Concat [ old_factor ; factor ]
                    )
                    (
                      fun (prefix : t_ext)
                        (left : t_ext list) : t_ext ->
                        Concat [ prefix; Union left ]
                    )
                in
                let r, is_factorized' =  aux r calc_suffix
                    (
                      fun (old_factor : t_ext)
                        (factor : t_ext) : t_ext ->
                        Concat [ factor ; old_factor ]
                    )
                    (
                      fun (suffix : t_ext)
                        (left : t_ext list) : t_ext ->
                        Concat [ Union left; suffix ]
                    )
                in
                r, is_factorized || is_factorized'
              in
              match all_eps with
              | [] -> (* no epsilon *)
                let res, is_factorized = factorize @@ Union without_eps in
                if is_factorized then (* else it will loop without an end *)
                  flatten @@ simp res
                else
                  res
              | _ -> (* at least one (singleton because we got rid of duplicates) *)
                match without_eps with
                | [] -> Letter Lt.epsilon (* it was an union of espilon (why not) *)
                | r :: [] -> simp @@ Option r
                | _ ->
                  let res, is_factorized = factorize @@ Union without_eps in
                  if is_factorized then
                    simp @@ Option (flatten @@ res)
                  else
                    simp @@ Option res

            )
        )
      | Star r ->
        begin
          match simp r with
          | Letter l ->
            if Lt.is_epsilon l then
              Letter l
            else
              Star (Letter l)
          | Star r
          | Plus r
          | Option r -> Star r
          | r -> Star r
        end
      | Plus r ->
        begin
          match simp r with
          | Letter l ->
            if Lt.is_epsilon l then
              Letter l
            else
              Plus (Letter l)
          | Star r
          | Option r -> Star r
          | Plus r -> Plus r
          | r -> Plus r
        end
      | Option r ->
        begin
          match simp r with
          | Letter l ->
            if Lt.is_epsilon l then
              Letter l
            else
              Option (Letter l)
          | Star r
          | Plus r -> Star r
          | Option r -> Option r
          | r -> Option r
        end
    in
    simp @@ flatten r

end