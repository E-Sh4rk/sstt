(** {1 Operations on types }*)

module Row = Row

module Subst = Subst

module Op = Op

module Transform = Transform

module Tallying = Tallying

(** {1 Pretty-printing of types} *)

module Prec = Prec
module Printer = Printer

(** {1 Extensions } 

This modules provides several common data-types, encoded as tagged type with a
particular tag.
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
