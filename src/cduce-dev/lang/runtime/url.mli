val is_url : string -> bool
val local : string -> string -> string
val url_loader : (string -> string) ref
val load_url : string -> string
