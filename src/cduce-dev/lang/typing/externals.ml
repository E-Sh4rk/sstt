type ml_type

type ext_info = (string * string * int * ml_type) list

let has_ext = ref false

let register =
  ref (fun _ _ _ ->
      Cduce_error.(raise_err Generic "No built-in support for ocaml externals"))

let ext_info = ref (fun () -> assert false)

let resolve s args =
  has_ext := true;
  !register true s args

let typ s args = snd (!register false s args)
let get () = if !has_ext then Some (!ext_info ()) else None
