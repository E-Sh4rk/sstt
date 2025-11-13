open Cduce_core

let load_url s =
  try
    let buff = Buffer.create 4096 in
    let conn = Curl.init () in
    let () =
      Curl.(setopt conn (CURLOPT_SSL_OPTIONS [ CURLSSLOPT_NATIVE_CA ]))
    in
    Curl.set_url conn s;
    Curl.set_writefunction conn (fun str ->
        try
          Buffer.add_string buff str;
          String.length str
        with
        | Failure _ -> 0);
    Curl.perform conn;
    Buffer.contents buff
  with
  | Curl.CurlException (_code, n, msg) ->
      Value.failwith' (Printf.sprintf "Curl error for url `%s' %i: %s" s n msg)

let use () = Url.url_loader := load_url
let () = Cduce_config.register "curl" "Load external URLs with curl" use
