exception InvalidAccess
(** Raised if a entry is used more than once. *)

module MakeOpt(V : Hashtbl.HashedType)(R : sig type t val equal : t -> t-> bool end): sig
  (**
     Hash table specialized for computations over co-inductive structures.

      This table can be used for computations over co-inductive structures whose
      results depend on an initial guess. When exploring a co-inductive value
      [v : V.t], we first fix its result to an initial guess before exploring it.
      If we find it again, return the initial guess. When coming back after exploration,
      if the result is consistent with the initial guess we can simply return it.
      If not, we need to invalidate all results stored in the table that depended
      (directly or indirectly) on the initial guess.
      This fits nicely with a look-up table pattern :

     - first, one looks for [v] in the table, using [find ~default:r table v]
     - if [v] is not in the table, it associates an initial result [r : R.t],
          The exploration of [v] can continue.
     - if [v] is in the table, it means it is encountered again. The
          value stored is returned as [Some r] and all values that are curently being
          explored become [v]'s direct dependencies, which we record.

     - when returning from the initial exploration of [v] with a computed
       result [r'], one needs to update the result [update table v r']:
     - if [R.equal r r'] then the initial guess was correct the table is in a consistent state.
     - otherwise the transitive dependencies of [v] are removed from the table: they
          were computed while making the (wrong) hypothesis that the result for
          [v] was [r], while it is [r']. Later calls to [find ~default:r table v]
       will return [r'] unless it is itself invalidated.

      {@ocaml[ let rec explore table v =

        match find ~default:r table v with (* if [v] is not [Active] yet it
        binds it to [d] in the table *)
        | Some r -> r                     (* [v] was bound to some value *)
        | None ->
          let r' = (* COMPUTATION, may call explore recursively *) in

          (* this will invalidate the dependencies if [not (R.equal r r')] *)
          update table v r'

      ]}
  *)

  type t
  (** The type of the table.*)

  val create : unit -> t
  (** Creates an empty table *)

  val clear : t -> unit
  (** Clears the table. *)

  val find : default:R.t -> t -> V.t -> R.t option
  (** Retrieves the result associated with a value.
      If the value is not in the table, the supplied [default] result
      is added and a entry is returned.
  *)

  val update : ?naive:bool -> t -> V.t -> R.t -> unit
  (** Updates the value associated with the value that created the entry.
        If the supplied value is not equal to the original one, all values in
        the table whose result dependend on the original result are removed from
        the table.

      @raise InvalidAccess if the value is not already in the table.
  *)

end = struct
  module H = Hashtbl.Make(V)

  type stack =
      Cons of { key : V.t; mutable marked : bool ; next : stack }
    | Nil
  type entry = {
    mutable dependencies :stack list;  (* the top of the stack at the time the entry was accessed *)
    mutable result : R.t option;       (* the result stored in this entry *)
  }
  and t = {
    table :  entry H.t;                 (* The table of all entries *)
    mutable stack : stack;              (* The stack of entries. *)
  }
  let create () = { table = H.create 0; stack = Nil}
  let clear t = H.clear t.table; t.stack <- Nil

  let find ~default t key =
    match H.find_opt t.table key with
    | None ->
      (* The key is not in the table start from scratch *)
      let entry = { dependencies = []; result = Some default } in
      t.stack <- Cons { key; marked = false; next = t.stack };
      H.add t.table key entry;
      None

    | Some entry ->
      entry.dependencies <- t.stack::entry.dependencies;
      entry.result

  (* remove from the list until we find ourselves, this is when we where put
     on the stack *)
  let rec invalidate tbl stop deps =
    match deps with
    | Cons ({ key ; next ; marked }  as r) when deps != stop ->
      if not marked then begin
        r.marked <- true;
        match H.find_opt tbl key with
          None -> ()
        | Some cp ->
          H.remove tbl key; List.iter (invalidate tbl stop) cp.dependencies
      end;
      invalidate  tbl stop next
    | _ -> ()
  let[@warning "-27"] update ?(naive=false) t key r =
    match H.find_opt t.table key, t.stack  with
    | Some ({ result = Some old_r; _ } as cp), Cons s ->
      if not (R.equal r old_r) then begin
        cp.result <- Some r;
        List.iter (invalidate t.table t.stack) cp.dependencies;
      end;
      t.stack <- s.next
    | _ -> raise InvalidAccess
end
module MakeSimple(V : Set.OrderedType)(R : sig type t val equal : t -> t-> bool end): sig
  type t
  val create : unit -> t
  val clear : t -> unit
  val find : default:R.t -> t -> V.t -> R.t option
  val update : ?naive:bool -> t -> V.t -> R.t -> unit

end = struct

  module M = Map.Make(V)

  type t = (R.t M.t list) ref

  let create () = ref ([M.empty])
  let clear r = r := [ M.empty ]
  let find ~default t key =
    let cache = match !t with [] -> assert false | c :: _ -> c in
    match M.find_opt key cache with
      Some _ as v -> v
    | None ->
      let new_cache = M.add key default cache in
      t := new_cache :: !t;
      None

  let update ?(naive=false) t key r =
    match !t with
    | [] | [ _ ] -> raise InvalidAccess
    | cache :: old_cache :: prev_stack ->
      match M.find_opt key cache with
      | None -> raise InvalidAccess
      | Some old_r ->
        let new_cache =
          if R.equal r old_r && not naive then cache else M.add key r old_cache
        in
        t := (new_cache :: prev_stack)
end
