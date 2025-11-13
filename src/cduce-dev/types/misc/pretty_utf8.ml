type t = Uchar.t * int

let append_uchar buffer u = Buffer.add_utf_8_uchar buffer u

let utf_8_byte_length u =
  match Uchar.to_int u with
  | u when u < 0 -> assert false
  | u when u <= 0x007F -> 1
  | u when u <= 0x07FF -> 2
  | u when u <= 0xFFFF -> 3
  | u when u <= 0x10FFFF -> 4
  | _ -> assert false

let create i =
  let c = Uchar.of_int i in
  (c, utf_8_byte_length c)

let add_to_buffer buffer (c, _) = append_uchar buffer c
let length (_, l) = l
let utf8_symbols = Hashtbl.create 16
let fill_table (utf8_sym, _) str = Hashtbl.add utf8_symbols utf8_sym str

module Ptree = struct
  module M = Map.Make (Char)

  type u = t

  type t = {
    pretty : u option;
    branches : t M.t;
  }

  let empty = { pretty = None; branches = M.empty }

  let add str utf8_sym t =
    fill_table utf8_sym str;
    let length = String.length str in
    let rec aux i t =
      if i = length then
        match t.pretty with
        | None -> { t with pretty = Some utf8_sym }
        | Some _ -> t
      else
        let c = String.unsafe_get str i in
        let branches =
          try M.find c t.branches with
          | Not_found -> empty
        in
        { t with branches = M.add c (aux (i + 1) branches) t.branches }
    in
    aux 0 t

  (* Keeping this unused function since we may want to use it *)
  let _mem_sub string index t =
    let length = String.length string in
    let rec aux i t =
      if i = length then None
      else
        match M.find (String.unsafe_get string i) t.branches with
        | { pretty = Some s; _ } -> Some (s, i + 1 - index)
        | t -> aux (i + 1) t
        | exception Not_found -> None
        | exception Invalid_argument _ ->
            Format.eprintf "@.%s %d %d@." string i length;
            raise Exit
    in
    aux index t

  let mem_exact string index t =
    let length = String.length string in
    let rec aux i t =
      if i = length then
        match t.pretty with
        | Some p -> Some (p, i - index)
        | None -> None
      else
        match M.find (String.unsafe_get string i) t.branches with
        | t -> aux (i + 1) t
        | exception Not_found -> None
        | exception Invalid_argument _ ->
            Format.eprintf "@.Trying to access %d in %S of length %d@." i string
              length;
            raise Exit
    in
    aux index t
end

let prettify_ptree = ref Ptree.empty

let register_utf8_binding s ((u, _) as sym) =
  if Uchar.is_char u then
    raise
      (Invalid_argument
         (Printf.sprintf "%c can not be used as a prettyfying symbol"
            (Uchar.to_char u)))
  else prettify_ptree := Ptree.add s sym !prettify_ptree

let get_utf8_binding u = Hashtbl.find utf8_symbols u

let prettify string pos nb_chars =
  let buffer = Buffer.create (nb_chars - pos) in
  let rec aux pos new_nb_chars =
    if pos = nb_chars then (Buffer.contents buffer, new_nb_chars)
      (* Check if substring of length 2 corresponds
         to a symbol that we want to prettify *)
    else
      match Ptree.mem_exact string pos !prettify_ptree with
      | None ->
          Buffer.add_char buffer (String.unsafe_get string pos);
          aux (pos + 1) new_nb_chars
      | Some (uchar, nb_char_read) ->
          add_to_buffer buffer uchar;
          aux (pos + nb_char_read) (new_nb_chars + length uchar - nb_char_read)
  in
  aux pos nb_chars
