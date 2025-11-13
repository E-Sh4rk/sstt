(** Generic iteration over the components of a type *)

type pack =
  | Int : (module Types.Kind) -> pack
  | Char : (module Types.Kind) -> pack
  | Atom : (module Types.Kind) -> pack
  | Times :
      (module Types.Kind
         with type Dnf.atom = Types.Node.t * Types.Node.t
          and type Dnf.line = (Types.Node.t * Types.Node.t) Bdd.line
          and type Dnf.dnf = (Types.Node.t * Types.Node.t) Bdd.dnf)
      -> pack
  | Xml :
      (module Types.Kind
         with type Dnf.atom = Types.Node.t * Types.Node.t
          and type Dnf.line = (Types.Node.t * Types.Node.t) Bdd.line
          and type Dnf.dnf = (Types.Node.t * Types.Node.t) Bdd.dnf)
      -> pack
  | Function :
      (module Types.Kind
         with type Dnf.atom = Types.Node.t * Types.Node.t
          and type Dnf.line = (Types.Node.t * Types.Node.t) Bdd.line
          and type Dnf.dnf = (Types.Node.t * Types.Node.t) Bdd.dnf)
      -> pack
  | Record :
      (module Types.Kind
         with type Dnf.atom = bool * Types.Node.t Ident.label_map
          and type Dnf.line = (bool * Types.Node.t Ident.label_map) Bdd.line
          and type Dnf.dnf = (bool * Types.Node.t Ident.label_map) Bdd.dnf)
      -> pack
  | Abstract : (module Types.Kind) -> pack
  | Absent : pack
      (** Type [pack] represent the kind of a type component together with the
  corresponding [Kind] module from [Types].
    The type of the module is constrained just enough to allow to process
    similar kinds in a uniform way :
    - for basic types, the type of the underlying representation is left
      abstract, only exposing the polymorphic variable part of the dnf.
    - for products, xml products and arrows, the type of the dnf is exposed and
      the same 
    - for records the type of the dnf is exposed
    - an [Absent] constructor gives functions using packs a chance to check if
      the type has the absent flag and act accordingly. 
  *)

val fold : ('a -> pack -> Types.t -> 'a) -> 'a -> Types.t -> 'a
(** [fold f acc t] calls 
    [(f acc Absent (.... f (f acc (Int _) t) (Chars _) t) ...))], that is 
    folds the function [t] over all the type components of [t].
      The [pack] argument passed to [f] allows one to discriminate on a
      particular kind of [t].
*)

val iter : (pack -> Types.t -> unit) -> Types.t -> unit
(** [iter f t] calls [f] for each type component of [t]. 
*)
