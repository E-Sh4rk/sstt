val print_xml : utf8:bool -> Ns.table -> Value.t -> Value.t
val dump_xml : utf8:bool -> Ns.table -> Value.t -> Value.t

val print_xml_subst :
  utf8:bool -> Ns.table -> Value.t -> (Ns.Uri.t * Ns.Uri.t) list -> Value.t
