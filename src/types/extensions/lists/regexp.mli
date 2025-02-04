
module type Letter = sig
  type t
  val equal : t -> t -> bool
end

type 'l t =
| Empty | Epsilon | Letter of 'l
| Union of 'l t * 'l t
| Concat of 'l t * 'l t
| Star of 'l t

module Make(L:Letter) : sig
  type nonrec t = L.t t
  val simple_re : t -> t
  val brzozowski : t array array -> t array -> t
end
