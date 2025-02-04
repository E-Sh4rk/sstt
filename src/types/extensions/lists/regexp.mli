
module type Letter = sig
  type t
  val equal : t -> t -> bool
end

type 'l t =
| Empty | Epsilon | Letter of 'l
| Union of 'l t * 'l t
| Concat of 'l t * 'l t
| Star of 'l t

type 'l t_ext =
| EEpsilon | ELetter of 'l
| EUnion of 'l t_ext list
| EConcat of 'l t_ext list
| EStar of 'l t_ext
| EOption of 'l t_ext
| EPlus of 'l t_ext

module Make(L:Letter) : sig
  type nonrec t = L.t t
  type nonrec t_ext = L.t t_ext
  val brzozowski : t array array -> t array -> t
  val simple_re : t -> t
  val to_ext : t -> t_ext
end
