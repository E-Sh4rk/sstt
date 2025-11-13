open Cduce_core

let error msg = Value.failwith' (Printf.sprintf "Netclient error. %s" msg)

let load_url s =
  match Neturl.extract_url_scheme s with
  | "http" -> (
      try Nethttp_client.Convenience.http_get s with
      | Nethttp_client.Bad_message s ->
          let msg = Printf.sprintf "Bad HTTP answer: %s" s in
          error msg
      | Nethttp_client.Http_error (n, s) ->
          let msg = Printf.sprintf "HTTP error %i: %s" n s in
          error msg
      | Nethttp_client.No_reply -> error "No reply"
      | Nethttp_client.Http_protocol exn ->
          let msg = Printexc.to_string exn in
          error msg)
  | "file" ->
      error
        "FIXME: write in url.ml the code so that netclient handle file:// \
         protocol"
  | sc ->
      let msg = Printf.sprintf "Netclient does not handle the %s protocol" sc in
      error msg

let use () = Url.url_loader := load_url

let () =
  Cduce_config.register ~priority:~-1 "netclient"
    "Load external URLs with netclient" use
