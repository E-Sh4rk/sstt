(** {1 Operations on types }*)

module Row = Row

module Subst = Subst

module Op = Op

module Transform = Transform

module Tallying = Tallying

module Dependencies = Dependencies

module FieldCtx = FieldCtx

(** {1 Pretty-printing of types} *)

module Prec = Prec
module Printer = Printer

(** {1 Extensions }

These modules provide several common data-types, encoded as tagged type with a
particular tag. They all follow the same pattern:

- a tag identifying the encoding (or, when the extension is parameterized, a
  function creating one), together with constructors and destructors for the
  types of the extension;
- a type [t] representing such a type in a form that is convenient to print,
  and the three functions [to_t], [map] and [print] that {!Printer.builder}
  expects. [to_t] returns [None] when the tag component it is given is not a
  valid encoding for this extension;
- [printer_builder], the result of applying {!Printer.builder} to the previous
  three, and [printer_params], ready-to-use {!Printer.params} recognizing the
  extension. The parameters of several extensions can be combined with
  {!Printer.merge_params}.
*)


module Extensions = struct
  module Lists = Lists
  module Strings = Strings
  module Floats = Floats
  module Bools = Bools
  module Chars = Chars
  module Abstracts = Abstracts
  module Maps = Maps
  module Hierarchy = Hierarchy
end
