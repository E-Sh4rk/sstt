module Make (V : Hashtbl.HashedType) (R : sig type t val equal : t -> t-> bool end): sig

  (** 
     Hash table specialized for computations over co-inductive structures.

      This table can be used for computations over co-inductive structures whose
      results depend on an initial guess. When exploring a co-inductive value
       [v : V.t], we say that [v] is [Active], if it is being explored and the
      exploration is not finished. The API is as follows:
     - first, one looks for [v] in the table, using [find ~default:r table v]

     - if [v] is not in the table, it associates an initial result [r : R.t],
            returns [None] and [v] becomes active. The exploration of
            [v] can continue.
     - if [v] is in the table, it means it is encountered again. The initial
            value stored is returned as [Some r] and all values that became
            active after [v] and are still active are recorded. 
            These are the dependencies of [v].

     - when returning from the initial exploration of [v] with a computed
        result [r'], one needs to update the result [update table v r']:
     - if [R.equal r r'] then the initial guess was correct, and all
              dependencies are left as-is
     - otherwise, the dependencies of [v] are removed from the table: they
              were computed while making the (wrong) hypothesis that the result for
              [v] was [r], while it is [r']. Later calls to [find ~default:r table
              v] will return [r'] unless it is itself invalidated.


      {ocaml[ let rec explore table v =

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

  exception InvalidAccess
  (** Raised if a entry is used more than once. *)

  val create : unit -> t
  (** Creates an empty table *)

  val clear : t -> unit
  (** Clears the table. *)

  val find : default:R.t -> t -> V.t -> R.t option
  (** Retrieves the result associated with a value.
      If the value is not in the table, the supplied [default] result
      is added and a entry is returned.
  *)

  val update : t -> V.t -> R.t -> unit
  (** Updates the value associated with the value that created the entry.
        If the supplied value is not equal to the original one, all values in
        the table whose result dependend on the original result are removed from
        the table.

      @raise InvalidAccess if the value is not already in the table.
  *)

end = struct
  module H = Hashtbl.Make (V)

  exception InvalidAccess
  type stack = 
      Cons of { key : V.t; mutable marked : bool ; next : stack }
    | Nil
  type entry = {
    mutable active : bool;            (* status of the entry *)
    mutable dependencies :stack list;  (* the top of the stack at the time the entry was accessed *)
    mutable result : R.t option;      (* the result stored in this entry *)
  }
  and t = {
    table :  entry H.t;                 (* The table of all entrys *)
    mutable stack : stack;           (* The stack of entrys. *)
  }
  let create () = { table = H.create 0; stack = Nil}
  let clear t = H.clear t.table; t.stack <- Nil

  let find ~default t key = 
    match H.find_opt t.table key with
    | None -> 
      (* The key is not in the table start from scratch *)
      let entry = { active = true; dependencies = []; result = Some default } in
      t.stack <- Cons { key; marked = false; next = t.stack };
      H.add t.table key entry;
      None

    | Some entry -> 
      (* We find an entry, if it is active, record the dependencies, that is
         the current stack. *)
      if entry.active then entry.dependencies <- t.stack::entry.dependencies;
      entry.result

  (* remove from the list until we find ourselves, this is when we where put
     on the stack *)
  let rec invalidate tbl stop deps = 
    match deps with
    | Cons ({ key ; next ; marked }  as r) when not marked && not (V.equal key stop) ->
      H.remove tbl key;
      r.marked <- true;
      invalidate tbl stop next
    | _ -> ()

  let update t key r =
    match H.find_opt t.table key, t.stack  with
    | Some ({ active = true; result = Some old_r; _ } as cp), Cons s ->
      if not (R.equal r old_r) then begin
        List.iter (invalidate t.table key) cp.dependencies;
        cp.result <- Some r;
      end;
      t.stack <- s.next;
      cp.active <- false
    | _ -> raise InvalidAccess
end