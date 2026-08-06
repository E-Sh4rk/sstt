(** Abstract type constructors with variance-annotated parameters.

    An abstract type is opaque: nothing is known about its content, only about
    how its parameters may vary. It is encoded as a tag (whose properties
    reflect the variance of the parameters) applied to a record encoding the
    parameters, so that, for instance, a covariant parameter makes
    [t(int) <= t(int|bool)] hold, while an invariant one does not.
*)

open Core

type variance = Cov | Cav | Inv
(** The variance of a parameter: covariant, contravariant, or invariant. *)

type 't params = 't list
(** The parameters of an abstract type, in the order of its declaration. *)

type 't t = ('t params list * 't params list) list
(** An abstract type, as a union of clauses made of positive and negative
    instances of the same constructor. *)

val define : string -> variance list -> Tag.t
(** [define name vs] declares a new abstract type constructor of name [name],
    with one parameter per element of [vs], of the given variance. As
    {!Sstt.Tag.mk}, it returns a fresh tag even if another constructor has the
    same name. *)

val is_abstract : Tag.t -> bool
(** Tests whether a tag has been declared by {!define}. *)

val arity : Tag.t -> int
(** The number of parameters of an abstract type constructor.
    @raise Invalid_argument if the tag is not abstract. *)

val parameters : Tag.t -> variance list
(** The variance of each parameter of an abstract type constructor.
    @raise Invalid_argument if the tag is not abstract. *)

val mk : Tag.t -> Ty.t list -> Ty.t
(** [mk tag tys] is the abstract type [tag] instantiated with the parameters
    [tys].
    @raise Invalid_argument if the tag is not abstract, or if the number of
    parameters is wrong. *)

val mk_any : Tag.t -> Ty.t
(** [mk_any tag] is the union of all the instances of [tag].
    @raise Invalid_argument if the tag is not abstract. *)

val destruct : Tag.t -> Ty.t -> Ty.t t
(** [destruct tag ty] returns the parameters of the instances of [tag]
    occurring in [ty].
    @raise Invalid_argument if [ty] is not a valid encoding of instances of
    [tag]. *)

val to_t : Printer.build_ctx -> TagComp.t -> Printer.descr t option
(** Recognizes an abstract type (see {!Sstt.Extensions}). *)

val map : Printer.descr t Printer.map
(** Applies the given function to every parameter. *)

val print : Tag.t -> int -> Prec.assoc -> Format.formatter -> Printer.descr t -> unit
(** Prints an abstract type, using the name given to [tag] at declaration
    time. *)

val printer_builder : Tag.t -> Printer.extension_builder
(** The printer extension for the given abstract type constructor. *)

val printer_params : Tag.t -> Printer.params
(** Printing parameters recognizing the given abstract type constructor. *)
