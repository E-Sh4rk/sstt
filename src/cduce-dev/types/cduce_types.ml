module Tset = Tset
module Bool = Bool
module AtomSet = AtomSet
module Intervals = Intervals
module CharSet = CharSet
module AbstractSet = AbstractSet
module Ident = Ident
module Var = Var

(* This whole module is ungodly black magic to make dune and odoc generate
   sensible files for end-users.*)

(** ℂDuce type algebra *)
module Types = struct
  include Cduce_types__Types

  (** Positive systems and least solutions. *)
  module Positive = struct
    include Positive
  end

  (** Pretty-printing of types. *)
  module Print = struct
    include Print
  end

  (** Generate a sample from a type. *)
  module Sample = struct
    include Sample
  end

  (** Convenience module to build regular expression types. *)
  module Sequence = struct
    include Sequence
  end

  (** Convenience module to fold or iterate over the components of a type. *)
  module Iter = struct
    include Iter
  end

  (** Type substitutions. *)
  module Subst = struct
    include Subst
  end

  (** Type tallying. *)
  module Tallying = struct
    include Tallying
  end
end

module Builtin_defs = Builtin_defs
module Compunit = Compunit

(**/**)
