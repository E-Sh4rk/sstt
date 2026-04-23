module type HashedPair = sig
  type t1
  type t2
  val hash : t1 -> t2 -> int
  val equal : t1 -> t2 -> t1 -> t2 -> bool
end

module Make2 (H : HashedPair) = struct
  type 'a entry = int * H.t1 * H.t2 * 'a

  type 'a t = {
    mutable data : 'a entry Weak.t;
    mutable size : int;
    mutable mask : int;
    initial : int;
  }

  let initial_capacity n =
    let c = ref 1 in
    while !c < n do c := !c lsl 1 done;
    !c

  let create n =
    let cap = initial_capacity (max n 64) in
    { data = Weak.create cap; size = 0; mask = cap - 1; initial = cap; }

  let clear t =
    Weak.fill t.data 0 (Weak.length t.data) None;
    t.size <- 0
  let reset t =
    t.data <- Weak.create t.initial;
    t.mask <- t.initial - 1;
    t.size <- 0

  let[@inline always] find_slot init_h data mask key1 key2 =
    let rec loop init_h h =
      match Weak.get data h with
      | None -> - h - 2, None
      | Some (cur_h, k1, k2, o) ->
        if cur_h = init_h && H.equal k1 k2 key1 key2 then h, Some o
        else loop init_h ((h + 1) land mask)
    in
    loop init_h (init_h land mask)

  let insert_no_check init_h data mask o =
    let h = ref (init_h land mask) in
    while Weak.check data !h do
      h := (!h + 1) land mask
    done;
    Weak.set data !h o

  let resize t =
    let old_data = t.data in
    let new_cap = Weak.length old_data lsl 1 in
    let new_mask = new_cap - 1 in
    let new_data = Weak.create new_cap in
    for i = 0 to Weak.length old_data - 1 do
      match Weak.get old_data i with
      | None -> ()
      | Some (init_h, _, _, _) as o -> insert_no_check init_h new_data new_mask o
    done;
    t.data <- new_data;
    t.mask <- new_mask

  let find_opt { data; mask; _ } key1 key2 =
    let _,o = find_slot (H.hash key1 key2) data mask key1 key2 in o

  let find t key1 key2 =
    match find_opt t key1 key2 with
    | Some v -> v
    | None -> raise Not_found

  let calc_size n t = (n lsl 2) > (t lsl 1) + t (* n > 0.75 t *)
  let add t key1 key2 v =
    let init_h = H.hash key1 key2 in
    let i,_ = find_slot init_h t.data t.mask key1 key2 in
    let o = Some (init_h, key1, key2, v) in
    if i >= 0 then
      Weak.set t.data (i) o
    else begin
      if calc_size t.size (Weak.length t.data) then begin
        resize t;
        insert_no_check init_h t.data t.mask  o
      end else
        Weak.set t.data (- i - 2) o;
      t.size <- t.size + 1
    end

  let length t = t.size
end

module Make (H : Hashtbl.HashedType) = struct
  module H2 =
  struct
    type t1 = H.t
    type t2 = unit
    let hash t1 () = H.hash t1
    let equal t1 () t2 () = H.equal t1 t2
  end
  include Make2(H2)
  let add t k v = add t k () v
  let find t k = find t k ()
  let find_opt t k = find_opt t k ()

end