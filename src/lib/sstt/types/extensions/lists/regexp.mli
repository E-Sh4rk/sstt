
module type Letter = sig
  type t
  val equal : t -> t -> bool
end

module type Regexp = sig
  type lt

  type t =
  | Empty | Epsilon | Letter of lt
  | Union of t * t
  | Concat of t * t
  | Star of t
  
  type t_ext =
  | EEpsilon | ELetter of lt
  | EUnion of t_ext list
  | EConcat of t_ext list
  | EStar of t_ext
  | EOption of t_ext
  | EPlus of t_ext

  val brzozowski : t array array -> t array -> t
  val simple_re : (t -> t) -> t -> t
  val to_ext : t -> t_ext
end

module Make(L:Letter) : Regexp with type lt=L.t
