open Encodings
open Utf8

(* Replace old pcre_replace : all spaces are changed to " " *)
let replace_space s =
  let open Utf8 in
  let buffstr = Buffer.create 16 in
  let is = start_index s in
  let ie = end_index s in
  let rec replace s i =
    if i = ie then mk (Buffer.contents buffstr)
    else
      let ci, ic = next s i in
      match ci with
      | 0x9
      | 0xD
      | 0xA ->
          store buffstr 0x20;
          replace s ic
      | c ->
          store buffstr c;
          replace s ic
  in
  replace s is

(* Replace old pcre_replace : all multiple spaces are replaced
   by only one *)
let replace_spaces s =
  let open Utf8 in
  let buffstr = Buffer.create 16 in
  let i_start = start_index s in
  let i_end = end_index s in
  let rec replace s i =
    if i >= i_end then mk (Buffer.contents buffstr)
    else
      let char_i, i' = next s i in
      match char_i with
      | 0x20 ->
          store buffstr 0x20;
          begin
            let rec replace_loop s i =
              if i >= i_end then replace s i
              else
                let char_i, i' = next s i in
                match char_i with
                | 0x20 -> replace_loop s i'
                | c ->
                    store buffstr c;
                    replace s i'
            in
            replace_loop s i'
          end
      | c ->
          store buffstr c;
          replace s i'
  in
  replace s i_start

(* Replace pcre_replace : remove first and last space from the string *)
let replace_margins s =
  let open Utf8 in
  let buffstr = Buffer.create 16 in
  let i_start = start_index s in
  let i_end = end_index s in
  let i = ref i_start in
  while !i < i_end do
    begin
      let char_i, i' = next s !i in
      if !i == i_start && char_i == 0x20 then i := i'
      else begin
        if i' == i_end && char_i == 0x20 then i := i'
        else begin
          store buffstr char_i;
          i := i'
        end
      end
    end
  done;
  mk (Buffer.contents buffstr)

(* Replace pcre_split : spliting with white_spaces_XML *)
let split_spaces s =
  let open Utf8 in
  let buffstr = Buffer.create 16 in
  let is = start_index s in
  let ie = end_index s in
  let rec split s i l =
    if i = ie then begin
      if Buffer.length buffstr > 0 then
        let str = mk (Buffer.contents buffstr) in
        let l = str :: l in
        List.rev l
      else List.rev l
    end
    else
      let ci, ic = next s i in
      match ci with
      | 0x20
      | 0x9
      | 0xD
      | 0xA ->
          if Buffer.length buffstr > 0 then
            let str = mk (Buffer.contents buffstr) in
            let l = str :: l in
            begin
              Buffer.clear buffstr;
              split s ic l
            end
          else split s ic l
      | c ->
          store buffstr c;
          split s ic l
  in
  split s is []

(* Return all the characters before a chosen one until string finishes
   and the index where the function stopped *)
let next_token s index char =
  let open Utf8 in
  let buffstr = Buffer.create 16 in
  let i_end = end_index s in
  if index == i_end then (mk "", index)
  else
    let x, y = next s index in
    let char_curr, i' = (ref x, ref y) in
    begin
      while !char_curr != char && !i' < i_end do
        begin
          store buffstr !char_curr;
          let x, y = next s !i' in
          char_curr := x;
          i' := y
        end
      done;
      if !char_curr != char && !i' == i_end then store buffstr !char_curr;
      (mk (Buffer.contents buffstr), !i')
    end

(* Return the substring asked and the index where the function stopped *)
let sub_token s index num =
  let open Utf8 in
  let buffstr = Buffer.create num in
  let i = ref 0 in
  let index = ref index in
  let s_end = end_index s in
  begin
    while !i < num && !index != s_end do
      begin
        let char_curr, i' = next s !index in
        store buffstr char_curr;
        index := i';
        i := !i + 1
      end
    done;
    (mk (Buffer.contents buffstr), !index)
  end

(* Applies a function to all the chars in the string *)
let str_for_all f s =
  let n = String.length s in
  let rec loop i = if i < n then f s.[i] && loop (i + 1) else true in
  loop 0

(* Checks if the string contains the right number of int *)
let validate_int s min max =
  let open Utf8 in
  let s = get_str s in
  let len = String.length s in
  len >= min && len <= max && str_for_all (fun c -> c >= '0' && c <= '9') s
