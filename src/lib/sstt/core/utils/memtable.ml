module type HashedPair = sig
  type t1
  type t2
  val hash : t1 -> t2 -> int
  val equal : t1 -> t2 -> t1 -> t2 -> bool
end

module Make2 (H : HashedPair) = struct
  type 'a entry =
    | Empty
    | Slot of H.t1 * H.t2 * 'a option

  type 'a t = {
    mutable data : 'a entry array;
    initial : int;
    mutable size : int;
    mutable mask : int;
  }

  let initial_capacity n =
    let c = ref 1 in
    while !c < n do c := !c lsl 1 done;
    !c

  let create n =
    let cap = initial_capacity (max n 4) in
    let table =
      { data = Array.make cap Empty; size = 0; initial = cap; mask = cap - 1 }
    in
    table

  let clear t =
    Array.fill t.data 0 (Array.length t.data) Empty;
    t.size <- 0
  let reset t =
    t.data <- Array.make t.initial Empty;
    t.mask <- t.initial - 1;
    t.size <- 0

  let[@inline always] find_slot data mask key1 key2 =
    let rec loop h =
      match Array.unsafe_get data h with
      | Empty -> - h - 2
      | Slot (k1, k2, _) ->
        if H.equal k1 k2 key1 key2 then h
        else loop ((h+1) land mask)
    in
    loop ((H.hash key1 key2) land mask)

  let insert_no_check data mask key1 key2 o =
    let h = ref ((H.hash key1 key2) land mask) in
    while Array.get data !h <> Empty do
      h := (!h + 1) land mask
    done;
    data.(!h) <- Slot (key1, key2, o)

  let resize t =
    let old_data = t.data in
    let new_cap = Array.length old_data lsl 1 in
    let new_mask = new_cap - 1 in
    let new_data = Array.make new_cap Empty in
    Array.iter (function
        | Empty -> ()
        | Slot (k1, k2, o) -> insert_no_check new_data new_mask k1 k2 o
      ) old_data;
    t.data <- new_data;
    t.mask <- new_mask

  let find_opt { data; mask; _ }  key1 key2 =
    let i = find_slot data mask key1 key2 in
    if i >= 0 then match Array.unsafe_get data i with
      | Slot (_,_, o) -> o
      | Empty -> assert false
    else None

  let find t key1 key2 =
    match find_opt t key1 key2 with
    | Some v -> v
    | None -> raise Not_found

  let calc_size n t = (n lsl 2) > (t lsl 1) + t (* n > 3t/4*)
  let add t key1 key2 value =
    let i = find_slot t.data t.mask key1 key2 in
    let o = Some value in
    if i >= 0 then
      t.data.(i) <- Slot (key1, key2, o)
    else begin
      if calc_size t.size (Array.length t.data) then begin
        resize t;
        insert_no_check t.data t.mask key1 key2 o
      end else
        t.data.(- i - 2) <- Slot (key1, key2, o);
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