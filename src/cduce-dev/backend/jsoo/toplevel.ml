open Cduce_core

let init_top ppf =
  let () = Cduce_config.init_all () in
  Format.fprintf ppf "        CDuce version %s\n@." Version.cduce_version

let eval_top ppf ppf_err input =
  try ignore @@ Cduce_driver.topinput ~source:Cduce_loc.jsoo_source ppf ppf_err (String.to_seq input) with
  |  Cduce_error.Error (_, (Driver_Escape, _)) -> ()
