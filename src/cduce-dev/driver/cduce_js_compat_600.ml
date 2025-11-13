open Js_of_ocaml
let set_scroll_top (e : Dom_html.element Js.t) i = e ##.scrollTop := Js.float (float i)