module Make :
  (H : Hashtbl.HashedType) ->
    sig
      type 'a t
      val create : int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val find_opt : 'a t -> H.t -> 'a option
      val find : 'a t -> H.t -> 'a
      val add : 'a t -> H.t -> 'a -> unit
      val length : 'a t -> int
    end

module type HashedPair = sig
       type t1
       type t2
       val hash : t1 -> t2 -> int
       val equal : t1 -> t2 -> t1 -> t2 -> bool
end

module Make2 :
  (H : HashedPair) ->
    sig
      type 'a t
      val create : int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val find_opt : 'a t -> H.t1 -> H.t2 -> 'a option
      val find : 'a t -> H.t1 -> H.t2 -> 'a
      val add : 'a t -> H.t1 -> H.t2 -> 'a -> unit
      val length : 'a t -> int
    end
