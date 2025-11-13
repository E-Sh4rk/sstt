module V = struct
  type t = {
    id : int;
    name : string;
    kind : [ `user | `generated | `weak ];
  }

  let equal a b = a == b || a.id == b.id
  let hash a = a.id
  let compare a b = compare a.id b.id
  let check x = assert (x.id >= 0)

  let dump ppf x =
    Format.fprintf ppf "VAR(%d,%s,%s)" x.id x.name
      (match x.kind with
      | `user -> "`user"
      | `generated -> "`generated"
      | `weak -> "`weak")
end

include V

let print ppf v = Format.fprintf ppf "@{<prettify>%s@}" ("'" ^ v.name)

module Set = struct
  include SortedList.Make (V)

  let print ppf (s : t) =
    let open Format in
    fprintf ppf "%a"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ",@ ") print)
      (s :> V.t list)
end

module Map = Set.Map

let mk =
  let id = ref ~-1 in
  fun ?(kind = `user) name ->
    incr id;
    { id = !id; name; kind }

let next_name buff =
  let a = Bytes.make 1 'a' in
  let rec loop i =
    if i < 0 then Bytes.cat a buff
    else
      let c = Bytes.get buff i in
      let c = Char.code c in
      let c = c + 1 in
      Bytes.set buff i (Char.chr (((c - 97) mod 26) + 97));
      if c > 122 then loop (i - 1) else buff
  in
  loop (Bytes.length buff - 1)

let full_renaming vars =
  let name = ref (Bytes.make 1 'a') in
  Map.map_from_slist
    (fun v ->
      let n = Bytes.to_string !name in
      let () = name := next_name !name in
      mk ~kind:v.kind n)
    vars

let renaming vars =
  let tbl = Hashtbl.create 17 in
  Set.iter
    (fun v ->
      let vv =
        try Hashtbl.find tbl v.name with
        | Not_found -> []
      in
      Hashtbl.replace tbl v.name (v :: vv))
    vars;
  Map.from_list (fun _ _ -> assert false)
  @@ Hashtbl.fold
       (fun _ vv acc ->
         let vv =
           List.sort
             (fun v1 v2 ->
               let c =
                 match (v1.kind, v2.kind) with
                 | `user, `user
                 | `generated, `generated
                 | `weak, `weak ->
                     0
                 | `weak, _ -> -1
                 | _, `weak -> 1
                 | `user, _ -> -1
                 | _, `user -> 1
               in
               if c == 0 then compare v1 v2 else c)
             vv
         in
         match vv with
         | [] -> assert false
         | x :: rest ->
             (x, x)
             :: (snd
                @@ List.fold_left
                     (fun (i, acc) x ->
                       ( i + 1,
                         (x, mk ~kind:x.kind (x.name ^ string_of_int i)) :: acc
                       ))
                     (1, acc) rest))
       tbl []

let name v = v.name
let kind v = v.kind
let id v = v.id

let merge pl nl =
  let ps = Set.from_list pl in
  let ns = Set.from_list nl in
  if Set.disjoint ps ns then Some (ps, ns) else None
