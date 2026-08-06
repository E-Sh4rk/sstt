exception InvalidAccess
(** Raised if a entry is used more than once. *)

module type S = sig
  (**
     Table specialized for computations over co-inductive structures.

      This table can be used for boolean computations over co-inductive
      structures whose results depend on an initial guess, the initial guess
      being [true]. When exploring a co-inductive value [v : key], we first fix
      its result to [true] before exploring it. If we find it again, return
      [true]. When coming back after exploration, if the result is [true] the
      guess was correct and we can simply return it. If it is [false], the
      results that depended (directly or indirectly) on the initial guess
      cannot be trusted anymore.
      This fits nicely with a look-up table pattern :

     - first, one looks for [v] in the table, using [find table v]
     - if [v] is not in the table, it associates the result [true] to it,
          The exploration of [v] can continue.
     - if [v] is in the table, it means it is encountered again. The
          value stored is returned as [Some r].

     - when returning from the initial exploration of [v] with a computed
       result [r'], one needs to update the result [update table v r']:
     - if [r'] is [true] then the initial guess was correct the table is in a consistent state.
     - otherwise the results that depended on the initial guess are removed from
          the table: they were computed while making the (wrong) hypothesis
          that the result for [v] was [true], while it is [false]. Later calls
          to [find table v] will return [false].

      {@ocaml[ let rec explore table v =

        match find table v with (* if [v] is not [Active] yet it
        binds it to [true] in the table *)
        | Some r -> r                     (* [v] was bound to some value *)
        | None ->
          let r' = (* COMPUTATION, may call explore recursively *) in

          (* this will discard the results depending on the guess if [r'] is [false] *)
          update table v r'

      ]}

      {2 Monotonicity}

      The computation is assumed to be {e monotonic} w.r.t. the guesses stored
      in the table: adding the hypothesis that the result for some value is
      [true] can only make more results [true]. Consequently, a computed
      [false] does not rely on any hypothesis (it would still be [false] with
      fewer hypotheses), and thus never has to be discarded: results equal to
      [false] are definitive. Only results equal to [true] (be they guesses of
      ongoing explorations or computed results) may be discarded.
  *)

  type key
  (** The type of the values explored. *)

  type t
  (** The type of the table.*)

  val create : unit -> t
  (** Creates an empty table *)

  val clear : t -> unit
  (** Clears the table. *)

  val find : t -> key -> bool option
  (** Retrieves the result associated with a value.
      If the value is not in the table, the initial guess [true]
      is added and a entry is returned.
  *)

  val update : t -> key -> bool -> unit
  (** Updates the value associated with the value that created the entry.
        If the supplied value is [false], all values in
        the table whose result dependend on the initial guess are removed from
        the table.

      @raise InvalidAccess if the value is not already in the table.
  *)
end

(** Hash table implementation of {!S}. The results that depend on a guess are
    tracked precisely, so that only them are discarded when the guess turns out
    to be wrong. *)
module MakeOpt(V : Hashtbl.HashedType): S with type key = V.t = struct
  module H = Hashtbl.Make(V)

  type key = V.t

  type stack = entry list
  and entry = {
    mutable dependencies : stack list;  (* the top of the stack at the time the entry was accessed.
                                           Only recorded for provisional entries, as definitive
                                           ones are never invalidated. *)
    mutable status : status;            (* the result stored in this entry. *)
  }
  and status =
    | Stale                             (* This entry has been invalidated. We do not remove
                                           entries but just overwrite stale entries when they
                                           are added again. *)
    | Provisional                       (* Result [true]: either the guess of an entry currently
                                           being explored, or a result that may depend on such a
                                           guess. It may thus be invalidated. *)
    | Definitive                        (* Result [false]: by monotonicity of the computation,
                                           it does not depend on any guess and is never
                                           invalidated. *)
  and t = {
    table :  entry H.t;                 (* The table of all entries *)
    mutable stack : stack;              (* The stack of entries. *)
  }
  let create () = { table = H.create 0; stack = []}
  let clear t = H.clear t.table; t.stack <- []

  let find t key =
    match H.find_opt t.table key with
    | None | Some { status = Stale; _ } ->
      (* The key is not in the table or has a stale entry, overwrite it *)
      let entry = { dependencies = []; status = Provisional } in
      t.stack <- entry :: t.stack;
      H.replace t.table key entry;
      None
    | Some { status = Definitive; _ } ->
      (* No dependency to record: this result will never be invalidated *)
      Some false
    | Some entry ->
      entry.dependencies <- t.stack::entry.dependencies;
      Some true

  (* Invalidate the stack until we reach the stop level. We recursively
     invalidate the dependencies of each entry. We set the status of an
     entry to Stale. This allows us to not remove
     the entry from the table (avoid a table look-up using the key).
     This also ensures that an entry is not invalidated more than once.

     When we reach a definitive entry, we can stop walking this stack: the
     entries below it only depend on the invalidated result through it, and
     its own result will not change.
  *)
  let rec invalidate_stack stop stack todo =
    if stack == stop then invalidate stop todo
    else
      match stack with
      |  entry :: next ->
        begin match entry.status with
          | Definitive -> invalidate stop todo
          | Stale -> invalidate_stack stop next todo
          | Provisional ->
            entry.status <- Stale;
            invalidate stop entry.dependencies;
            invalidate_stack stop next todo
        end
      | [] -> invalidate stop todo
  and[@inline always] invalidate stop = function
      [] -> ()
    | dep :: todo -> invalidate_stack stop dep todo

  let update t _key r =
    match t.stack with
    | ({status = Provisional; _ } as entry) ::next ->
      if not r then begin
        entry.status <- Definitive;
        invalidate t.stack entry.dependencies;
        entry.dependencies <- [] (* This entry will never be invalidated *)
      end;
      t.stack <- next
    | _ -> raise InvalidAccess
end

(** Straightforward implementation of {!S}, using persistent sets: when a guess
    turns out to be wrong, all the results equal to [true] that were computed
    in the meantime are discarded, even those that did not depend on it. *)
module MakeSimple(V : Set.OrderedType): S with type key = V.t = struct

  module S = Set.Make(V)

  type key = V.t

  (* [t] contains the keys whose result is [true] (a guess, that may be
     invalidated), [f] those whose result is [false] (definitive). *)
  type cache = { t : S.t ; f : S.t }

  type t = (cache list) ref

  let empty_cache = { t = S.empty ; f = S.empty }

  let create () = ref ([empty_cache])
  let clear r = r := [ empty_cache ]
  let find t key =
    let cache = match !t with [] -> assert false | c :: _ -> c in
    if S.mem key cache.t then Some true
    else if S.mem key cache.f then Some false
    else begin
      let new_cache = { cache with t = S.add key cache.t } in
      t := new_cache :: !t;
      None
    end

  let update t key r =
    match !t with
    | [] | [ _ ] -> raise InvalidAccess
    | cache :: old_cache :: prev_stack ->
      if S.mem key cache.t |> not then raise InvalidAccess ;
      let new_cache =
        if r then cache
        else { old_cache with f = S.add key cache.f }
      in
      t := (new_cache :: prev_stack)
end

(** Implementation of {!S} without any caching: only the keys currently being
    explored are remembered, so that a key is never explored twice in the same
    call stack (which is enough to guarantee termination in theory).
    Every other result is recomputed from scratch. Useful to
    measure what the caching strategies of {!MakeOpt} and {!MakeSimple} actually buy. *)
module MakeNaive(V : Set.OrderedType): S with type key = V.t = struct

  module S = Set.Make(V)

  type key = V.t

  (* The keys currently being explored, all of them guessed to be [true]. *)
  type t = S.t ref

  let create () = ref S.empty
  let clear t = t := S.empty

  let find t key =
    if S.mem key !t then Some true
    else begin t := S.add key !t ; None end

  let update t key _r =
    if S.mem key !t |> not then raise InvalidAccess ;
    t := S.remove key !t
end
