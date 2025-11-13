open Cduce_error

let start_with s p =
  let l = String.length p in
  let n = String.length s in
  if n >= l && String.sub s 0 l = p then Some (String.sub s l (n - l)) else None

let is_scheme_char = function
  | 'A' .. 'Z'
  | 'a' .. 'z'
  | '0' .. '9'
  | '+'
  | '-'
  | '.' ->
      true
  | _ -> false

let extract_url_scheme s =
  let rec loop s len i =
    if i < len then
      match s.[i] with
      | ':' -> i
      | c when is_scheme_char c -> loop s len (i + 1)
      | _ -> Cduce_error.raise_err Url_Malformed_URL s
    else raise_err Url_Malformed_URL s
  in
  let len = String.length s in
  if len == 0 || not (is_scheme_char s.[0]) then raise_err Url_Malformed_URL s;
  let i = loop s len 0 in
  (String.sub s 0 i, String.sub s (i + 1) (len - i - 1))

let is_windows =
  match Sys.os_type with
  | "Cygwin"
  | "Win32" ->
      true
  | _ -> false

let is_url s =
  try
    let uscheme, _ = extract_url_scheme s in
    if is_windows then String.length uscheme > 1
      (* Windows drive letter in a path *)
    else true
  with
  | Error (_, (Url_Malformed_URL, _)) -> false

let no_load_url s =
  let msg =
    Printf.sprintf
      "Error \"%s\": \n cduce compiled without support for external URL loading"
      s
  in
  Value.failwith' msg

let url_loader = ref no_load_url

type kind =
  | File of string
  | Uri of string
  | String of string

let kind s =
  match start_with s "string:" with
  | None -> if is_url s then Uri s else File s
  | Some s -> String s

let remove_last_char s c =
  let last = String.length s - 1 in
  if s.[last] == c then String.sub s 0 last else s

let remove_first_char s c =
  let len = String.length s in
  if len > 0 && s.[0] == c then String.sub s 1 (len - 1) else s

let remove_last_component s c =
  try
    let i = String.rindex s c in
    String.sub s 0 i
  with
  | _ -> s

let local base rel =
  match (kind base, kind rel) with
  | File _, File _ ->
      let base = remove_last_char base Filename.dir_sep.[0] in
      let base = remove_last_component base Filename.dir_sep.[0] in
      let rel = remove_first_char rel Filename.dir_sep.[0] in
      Filename.concat base rel
  | _, (String _ | Uri _)
  | String _, File _ ->
      rel
  | Uri _, File _ ->
      let base = remove_last_char base '/' in
      let base = remove_last_component base '/' in
      let rel = remove_first_char rel '/' in
      base ^ "/" ^ rel

let load_file fn =
  try
    let ic = open_in fn in
    let len = in_channel_length ic in
    let s = Bytes.create len in
    really_input ic s 0 len;
    close_in ic;
    Bytes.to_string s
  with
  | exn ->
      Value.failwith' (Printf.sprintf "load_file: %s" (Printexc.to_string exn))

let load_url s =
  match start_with s "string:" with
  | None -> if is_url s then !url_loader s else load_file s
  | Some s -> s
