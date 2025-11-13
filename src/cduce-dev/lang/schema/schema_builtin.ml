open Printf
open Encodings
open Schema_utils
open Schema_common
open Schema_types

(* TODO dates: boundary checks (e.g. 95/26/2003) *)
(* TODO a lot of almost cut-and-paste code, expecially in gFoo types validation
*)

(* TODO: distinguish primitive and derived types in the interface *)

(** {2 Aux/Misc stuff} *)

let zero = Intervals.V.mk "0"

(*
  Special character used as end of string marker.
  0x00 is a good choice since it cannot appear in a
  well formed XML document without being escaped.
*)

let eos_uchar = 0
let eos_char = Char.chr eos_uchar
let eos_str = String.make 1 eos_char
let eos_utf8 = Utf8.mk eos_str
let xsd = Schema_xml.xsd
let add_xsd_prefix s = (xsd, Utf8.mk s)
let unsupported = [ "NOTATION"; "QName" ]
let is_empty s = Utf8.equal s (Utf8.mk "")

(* split a string at XML recommendation "S" production boundaries *)
let split_xml_S s = split_spaces s

let char_of_hex =
  let int_of_hex_char = function
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | 'a'
    | 'A' ->
      10
    | 'b'
    | 'B' ->
      11
    | 'c'
    | 'C' ->
      12
    | 'd'
    | 'D' ->
      13
    | 'e'
    | 'E' ->
      14
    | 'f'
    | 'F' ->
      15
    | _ -> assert false
  in
  (* most significative, least significative *)
  fun ms ls -> Char.unsafe_chr ((int_of_hex_char ms * 16) + int_of_hex_char ls)

(* add special char to mark the end of a string *)
let add_end s = Utf8.concat s eos_utf8

let remove_end s =
  let last = Utf8.rewind s (Utf8.end_index s) in
  Utf8.mk (Utf8.get_substr s (Utf8.start_index s) last)

let simple_type_error name = Cduce_error.(raise_err Schema_builtin_Error name)
let qualify = Ns.Label.mk_ascii

(** {2 CDuce types} *)

let positive_field = (false, qualify "positive", Builtin_defs.bool)
let year_field = (false, qualify "year", Builtin_defs.int)
let month_field = (false, qualify "month", Builtin_defs.int)
let day_field = (false, qualify "day", Builtin_defs.int)
let hour_field = (false, qualify "hour", Builtin_defs.int)
let minute_field = (false, qualify "minute", Builtin_defs.int)
let second_field = (false, qualify "second", Builtin_defs.int)

(* TODO this should be a decimal *)
let time_type_fields = [ hour_field; minute_field; second_field ]
let date_type_fields = [ year_field; month_field; day_field ]
let time_kind_field = (false, qualify "time_kind", Builtin_defs.time_kind)
let time_kind kind = (qualify "time_kind", Value.Atom (AtomSet.V.mk_ascii kind))

(* TODO the constraint that at least one part should be present isn't easily
   expressible with CDuce types *)
let duration_type =
  Types.rec_of_list false
    [
      time_kind_field;
      positive_field;
      (true, qualify "year", Builtin_defs.int);
      (true, qualify "month", Builtin_defs.int);
      (true, qualify "day", Builtin_defs.int);
      (true, qualify "hour", Builtin_defs.int);
      (true, qualify "minute", Builtin_defs.int);
      (true, qualify "second", Builtin_defs.int);
      (* TODO this should be a decimal *)
    ]

let timezone_type =
  Types.rec_of_list false [ positive_field; hour_field; minute_field ]

let timezone_type_fields = [ (true, qualify "timezone", timezone_type) ]

let time_type =
  Types.rec_of_list false
    ((time_kind_field :: time_type_fields) @ timezone_type_fields)

let date_type =
  Types.rec_of_list false (time_kind_field :: positive_field :: date_type_fields)

let dateTime_type =
  Types.rec_of_list false
    (time_kind_field :: positive_field
     :: (date_type_fields @ time_type_fields @ timezone_type_fields))

let gYearMonth_type =
  Types.rec_of_list false
    [ positive_field; time_kind_field; year_field; month_field ]

let gYear_type =
  Types.rec_of_list false [ time_kind_field; positive_field; year_field ]

let gMonthDay_type =
  Types.rec_of_list false [ time_kind_field; month_field; day_field ]

let gDay_type = Types.rec_of_list false [ time_kind_field; day_field ]
let gMonth_type = Types.rec_of_list false [ time_kind_field; month_field ]
let nonPositiveInteger_type = Builtin_defs.non_pos_int
let negativeInteger_type = Builtin_defs.neg_int
let nonNegativeInteger_type = Builtin_defs.non_neg_int
let positiveInteger_type = Builtin_defs.pos_int
let long_type = Builtin_defs.long_int
let int_type = Builtin_defs.int_int
let short_type = Builtin_defs.short_int
let byte_type = Builtin_defs.unsigned_byte_int
let string_list_type = Types.Sequence.star Builtin_defs.string

(** {2 Validation functions (string -> Value.t)} *)

let make_sign s =
  if Utf8.equal s (Utf8.mk "+") || is_empty s then Value.vtrue
  else if Utf8.equal s (Utf8.mk "-") then Value.vfalse
  else failwith "error spotted"

let make_integer s =
  let s = Utf8.get_str s in
  try Value.Integer (Intervals.V.mk s) with
  | Failure _ -> simple_type_error "integer"

let validate_decimal s =
  let s = Utf8.get_str s in
  try Value.float (float_of_string s) with
  | Failure _ -> simple_type_error "decimal"

(*
let is_digit c =
  c >= 48 (* '0' *) && c <= 57 (* '9' *)

let parse_pos_int s i =
  let buff = Buffer.create 10 in
  let end_ = Utf8.end_index s in
  let rec loop j =
    if j < end_ then
      let c, j = Utf8.next s j in
      if is_digit c then
        begin
          Utf8.store buff c;
          loop j
        end
      else j
    else j
  in
  let j = loop i in
  Utf8.mk (Buffer.contents buff), j

let between_bi bi i j =
  (Big_int.le_big_int bi (Big_int.big_int_of_int j)) &&
  (Big_int.le_big_int (Big_int.big_int_of_int i) bi)

let parse_year s i =
  let end_ = Utf8.end_index s in
  if i >= end_ then simple_type_error "gYear : ''" else
    let c = Utf8.get s i in
    let sign, i =
      if c == 45 then Utf8.mk "-", Utf8.advance s i else Utf8.empty, i
    in
    let num, j = parse_pos_int s i in
    let snum = Utf8.get_str (Utf8.concat sign num) in
    let bi = Big_int.big_int_of_string snum in
    let len = String.length snum in
    if (snum <> "0000") &&
       (snum <> "-0000") &&
       (((between_bi bi 0 9999) && len == 4) ||
        ((between_bi bi -9999 0) && len == 5) ||
        (((Big_int.gt_big_int bi (Big_int.big_int_of_int 9999)) && len >= 5)||
         ((Big_int.lt_big_int bi (Big_int.big_int_of_int ~-9999)) && len >= 6)))
    then
      make_integer snum, j
    else
      simple_type_error ("gYear : '" ^ snum ^ "'")

let parse_month s i =
  let end_ = Utf8.end_index s in
  if i < end_ then
    let m, j = parse_pos_int s i in
    let snum = Utf8.get_str m in
    let len = String.length snum in
    let mi = int_of_string snum in
    if len == 2 && mi >= 1 && mi <= 12 then
      make_integer snum, j
    else simple_type_error ("gMonth : '" ^ snum ^ "'")
  else simple_type_error ("gMonth : ''")

let parse_day s i =
  let end_ = Utf8.end_index s in
  if i < end_ then
    let m, j = parse_pos_int s i in
    let snum = Utf8.get_str m in
    let len = String.length snum in
    let mi = int_of_string snum in
    if len == 2 && mi >= 1 && mi <= 31 then
      make_integer snum, j
    else simple_type_error ("gDay : '" ^ snum ^ "'")
  else simple_type_error ("gDay : ''")


let parse_char s i m =
  let error () =
    simple_type_error ("'" ^ (Utf8.(get_str (mk_char m))) ^"' expected")
  let end_ = Utf8.end_index s in
  if i < end_ then
    let c, i = Utf8.next s i in
    if c == m then i
    else
      error ()
  else
    error ()
*)
let parse_date =
  (* "(\\d{4,})-(\\d{2})-(\\d{2})" *)
  let open Utf8 in
  fun s ->
    let s = add_end s in
    let year, y_end = next_token s (start_index s) 0x2D (* '-' *) in
    let month, m_end = next_token s y_end 0x2D (* '-' *) in
    let day, d_end = next_token s m_end eos_uchar in
    if
      validate_int year 4 max_int
      && validate_int month 2 2 && validate_int day 2 2
      && d_end == end_index s
      (* delete ? *)
    then
      [
        (qualify "year", make_integer year);
        (qualify "month", make_integer month);
        (qualify "day", make_integer day);
      ]
    else simple_type_error "date"

let parse_time =
  (* "(\\d{2}):(\\d{2}):(\\d{2})" *)
  let open Utf8 in
  fun s ->
    let s = add_end s in
    let hour, h_end = next_token s (start_index s) 0x3A (* ':' *) in
    let minute, m_end = next_token s h_end 0x3A (* ':' *) in
    let second, s_end = next_token s m_end eos_uchar in
    if
      validate_int hour 2 2 && validate_int minute 2 2
      && validate_int second 2 2
      && s_end == end_index s
    then
      [
        (qualify "hour", make_integer hour);
        (qualify "minute", make_integer minute);
        (qualify "second", make_integer second);
      ]
    else simple_type_error "time"

let parse_timezone =
  (* "(Z)|(([+-])?(\\d{2}):(\\d{2}))" *)
  let open Utf8 in
  fun s ->
    let s = add_end s in
    let hour, h_end = next_token s (start_index s) eos_uchar in
    if equal hour (mk "Z") && h_end == end_index s then
      [
        (qualify "positive", Value.vtrue);
        (qualify "hour", make_integer (mk "0"));
        (qualify "minute", make_integer (mk "0"));
      ]
    else
      let hour, h_end = next_token s (start_index s) 0x3A (* ':' *) in
      let minute, m_end = next_token s h_end eos_uchar in
      if
        validate_int hour 2 2 && validate_int minute 2 2 && m_end == end_index s
      then
        [
          (qualify "positive", Value.vtrue);
          (qualify "hour", make_integer hour);
          (qualify "minute", make_integer minute);
        ]
      else
        let hour' = get_str hour in
        let sign = mk (String.sub hour' 0 1) in
        let hour = mk (String.sub hour' 1 (String.length hour' - 1)) in
        if
          validate_int hour 2 2 && validate_int minute 2 2
          && m_end == end_index s
        then
          [
            (qualify "positive", make_sign sign);
            (qualify "hour", make_integer hour);
            (qualify "minute", make_integer minute);
          ]
        else simple_type_error "timezone"

(* parse a timezone from a string, if it's empty return the empty list,
   otherwise return a list containing a pair <"timezone", timezone value> *)
let parse_timezone' s =
  if is_empty s then []
  else [ (qualify "timezone", Value.vrecord (parse_timezone s)) ]

let parse_timezone_ddash s =
  let open Utf8 in
  if is_empty s then []
  else
    let prefix, start_tz = sub_token s (start_index s) 2 in
    let s =
      if Utf8.equal prefix (Utf8.mk "--") then
        let s = add_end s in
        let s, _ = sub_token s start_tz eos_uchar in
        s
      else s
    in
    if is_empty s then []
    else [ (qualify "timezone", Value.vrecord (parse_timezone s)) ]

let validate_string s = Value.string_utf8 s

let validate_normalizedString s =
  validate_string (normalize_white_space `Replace s)

let validate_token s = validate_string (normalize_white_space `Collapse s)

let validate_token_list s =
  Value.sequence (List.map validate_token (split_xml_S s))

let validate_interval interval type_name s =
  let integer =
    let s = Utf8.get_str s in
    if String.length s = 0 then simple_type_error "integer"
    else
      try Intervals.V.mk s with
      | Failure _ -> simple_type_error "integer"
  in
  if Intervals.contains integer interval then Value.Integer integer
  else simple_type_error type_name

let validate_bool s =
  if Utf8.equal s (Utf8.mk "true") || Utf8.equal s (Utf8.mk "1") then
    Value.vtrue
  else if Utf8.equal s (Utf8.mk "false") || Utf8.equal s (Utf8.mk "0") then
    Value.vfalse
  else simple_type_error "boolean"

let validate_duration =
  (* "^([+-])?P((\\d+)Y)?((\\d+)M)?((\\d+)D)?(T((\\d+)H)?((\\d+)M)?((\\d+)S)?)?$" *)
  let open Utf8 in
  (* invalid special cases of duration that are easier to test alone *)
  let p = mk "P" in
  let np = mk "-P" in
  fun s ->
    let abort () = simple_type_error "duration" in
    if equal s p || equal s np then abort ()
    else
      let s = add_end s in
      let sign, sign_end = next_token s (start_index s) 0x50 (* 'P' *) in
      let year', y_end' = next_token s sign_end 0x59 (* 'Y' *) in
      let year, y_end =
        if not (validate_int year' 1 max_int) then (mk "", sign_end)
        else (year', y_end')
      in
      let month', mo_end' = next_token s y_end 0x4D (* 'M' *) in
      let month, mo_end =
        if not (validate_int month' 1 max_int) then (mk "", y_end)
        else (month', mo_end')
      in
      let day', d_end' = next_token s mo_end 0x44 (* 'D' *) in
      let day, d_end =
        if not (validate_int day' 1 max_int) then (mk "", mo_end)
        else (day', d_end')
      in
      let t, t_end = next_token s d_end 0x54 (* 'T' *) in
      if not (is_empty t) then abort ()
      else
        let hour', h_end' = next_token s t_end 0x48 (* 'H' *) in
        let hour, h_end =
          if not (validate_int hour' 1 max_int) then (mk "", t_end)
          else (hour', h_end')
        in
        let minute', m_end' = next_token s h_end 0x4D (* 'M' *) in
        let minute, m_end =
          if not (validate_int minute' 1 max_int) then (mk "", h_end)
          else (minute', m_end')
        in
        let second', s_end' = next_token s m_end 0x53 (* 'S' *) in
        let second, s_end =
          if not (validate_int second' 1 max_int) then (mk "", m_end)
          else (second', s_end')
        in
        let end_str, e_end = next_token s s_end eos_uchar in
        if e_end != end_index s || not (is_empty end_str) then abort ()
        else
          try
            let fields =
              (time_kind "duration" :: [ (qualify "positive", make_sign sign) ])
              @ (if is_empty year then []
                 else [ (qualify "year", make_integer year) ])
              @ (if is_empty month then []
                 else [ (qualify "month", make_integer month) ])
              @ (if is_empty day then []
                 else [ (qualify "day", make_integer day) ])
              @ (if is_empty hour then []
                 else [ (qualify "hour", make_integer hour) ])
              @ (if is_empty minute then []
                 else [ (qualify "minute", make_integer minute) ])
              @
              if is_empty second then []
              else [ (qualify "second", make_integer second) ]
            in
            Value.vrecord fields
          with
          | Cduce_error.Error (_, (Schema_builtin_Error, _)) -> abort ()

let validate_dateTime =
  (* "^([+-])?(%s)T(%s)(%s)?$" : "(\\d{4,})-(\\d{2})-(\\d{2})" ,
     "(\\d{2}):(\\d{2}):(\\d{2})" , "(Z)|(([+-])?(\\d{2}):(\\d{2}))" *)
  let open Utf8 in
  fun s ->
    let s = add_end s in
    let sign_uchar, sign_end = next s (start_index s) in
    let sign, sign_end =
      if sign_uchar == 0x2B (* '+' *) || sign_uchar == 0x2D (* '-' *) then
        (mk (String.make 1 (Char.chr sign_uchar)), sign_end)
      else (mk "", start_index s)
    in
    let date, d_end = next_token s sign_end 0x54 (* 'T' *) in
    let time, t_end = sub_token s d_end 8 in
    let tzone, tz_end = next_token s t_end eos_uchar in
    try
      let fields =
        (time_kind "dateTime" :: [ (qualify "positive", make_sign sign) ])
        @ parse_date date @ parse_time time @ parse_timezone' tzone
      in
      Value.vrecord fields
    with
    | Cduce_error.Error (_, (Schema_builtin_Error,_)) -> simple_type_error "dateTime"

let validate_gYearMonth =
  (* "(-)?(\\d{4,})-(\\d{2})(%s)?" : "(Z)|(([+-])?(\\d{2}):(\\d{2}))" *)
  let open Utf8 in
  fun s ->
    let abort () = simple_type_error "gYearMonth" in
    let s = add_end s in
    let sign_uchar, sign_end = next s (start_index s) in
    let sign, sign_end =
      if sign_uchar == 0x2D (* '-' *) then
        (mk (String.make 1 (Char.chr sign_uchar)), sign_end)
      else (mk "", start_index s)
    in
    let year, y_end = next_token s sign_end 0x2D (* '-' *) in
    let month, m_end = sub_token s y_end 2 in
    let tzone, tz_end = next_token s m_end eos_uchar in
    if (not (validate_int year 4 max_int)) || not (validate_int month 2 2) then
      abort ()
    else
      try
        let fields =
          [
            time_kind "gYearMonth";
            (qualify "positive", make_sign sign);
            (qualify "year", make_integer year);
            (qualify "month", make_integer month);
          ]
          @ parse_timezone' tzone
        in
        Value.vrecord fields
      with
      | Cduce_error.Error (_, (Schema_builtin_Error, _)) -> abort ()

let validate_gYear =
  let open Utf8 in
  fun s ->
    let abort () = simple_type_error "gYear" in
    let end_ = end_index s in
    let i = start_index s in
    if i == end_ then abort ()
    else
      let sign, i =
        let c = get s i in
        if c == 0x2D then begin
          (mk "-", advance s i)
        end
        else (empty, i)
      in
      (* returns the first char that is not a year *)
      let rec loop i =
        if i == end_ then i
        else
          match get s i with
          | 0x2B
          | 0x2D
          | 0x5A (* +, -, Z *) ->
            i
          | 0x3A (* : *) -> rewind s (rewind s i)
          | 0x30
          | 0x31
          | 0x32
          | 0x33
          | 0x34
          | 0x35
          | 0x36
          | 0x37
          | 0x38
          | 0x39 ->
            loop (advance s i)
          | _ -> abort ()
      in
      let limit = loop i in
      let year = mk (get_substr s i limit) in
      let tz = mk (get_substr s limit end_) in
      let fields =
        [
          time_kind "gYear";
          (qualify "positive", make_sign sign);
          (qualify "year", make_integer year);
        ]
        @ parse_timezone' tz
      in
      Value.vrecord fields

let validate_gYear_ =
  (* "(-)?(\\d{4,})(%s)?" : "(Z)|(([+-])?(\\d{2}):(\\d{2}))" *)
  let open Utf8 in
  fun s ->
    let abort u = simple_type_error ("gYear" ^ " " ^ u) in
    let sign_uchar, sign_end = next s (start_index s) in
    let sign, sign_end =
      if sign_uchar == 0x2D (* '-' *) then
        (mk (String.make 1 (Char.chr sign_uchar)), sign_end)
      else (mk "", start_index s)
    in
    let year, y_end = next_token s sign_end 0x5A (* 'Z' *) in
    let year_ = remove_end year in
    if validate_int year_ 4 max_int then
      let tzone, tz_end = next_token s (rewind s y_end) eos_uchar in
      try
        let fields =
          [
            time_kind "gYear";
            (qualify "positive", make_sign sign);
            (qualify "year", make_integer year_);
          ]
          @ parse_timezone' tzone
        in
        Value.vrecord fields
      with
      | Cduce_error.Error (_, (Schema_builtin_Error, u)) -> abort u
    else
      let year, y_end = next_token s sign_end 0x2D (* '-' *) in
      let year_ = remove_end year in
      if validate_int year_ 4 max_int then
        let tzone, tz_end = next_token s (rewind s y_end) eos_uchar in
        try
          let fields =
            [
              time_kind "gYear";
              (qualify "positive", make_sign sign);
              (qualify "year", make_integer year_);
            ]
            @ parse_timezone' tzone
          in
          Value.vrecord fields
        with
        | Cduce_error.Error (_, (Schema_builtin_Error, u)) -> abort u
      else
        let year, y_end = next_token s sign_end 0x2B (* '+' *) in
        let year_ = remove_end year in
        if validate_int year_ 4 max_int then
          let tzone, tz_end = next_token s (rewind s y_end) eos_uchar in
          try
            let fields =
              [
                time_kind "gYear";
                (qualify "positive", make_sign sign);
                (qualify "year", make_integer year_);
              ]
              @ parse_timezone' tzone
            in
            Value.vrecord fields
          with
          | Cduce_error.Error (_, (Schema_builtin_Error,u)) -> abort u
        else
          let year, y_end = next_token s sign_end 0x3A (* ':' *) in
          let year_ = remove_end year in
          if validate_int year_ 6 max_int then
            let tzone, tz_end =
              next_token s (rewind s (rewind s y_end)) eos_uchar
            in
            try
              let fields =
                [
                  time_kind "gYear";
                  (qualify "positive", make_sign sign);
                  (qualify "year", make_integer year_);
                ]
                @ parse_timezone' tzone
              in
              Value.vrecord fields
            with
            | Cduce_error.Error (_, (Schema_builtin_Error, u)) -> abort u
          else
            let year, y_end = next_token s sign_end 0x3A (* ':' *) in
            let year_ = remove_end year_ in
            if not (validate_int year_ 4 max_int) then
              abort (Utf8.get_str year_)
            else
              let tzone, tz_end =
                next_token s (rewind s (rewind s y_end)) eos_uchar
              in
              try
                let fields =
                  [
                    time_kind "gYear";
                    (qualify "positive", make_sign sign);
                    (qualify "year", make_integer year_);
                  ]
                  @ parse_timezone' tzone
                in
                Value.vrecord fields
              with
              | Cduce_error.Error (_, (Schema_builtin_Error, u)) -> abort u

let validate_gMonthDay =
  (* "--(\\d{2})-(\\d{2})(%s)?" : "(Z)|(([+-])?(\\d{2}):(\\d{2}))" *)
  let open Utf8 in
  fun s ->
    let abort () = simple_type_error "gMonthDay" in
    let s = add_end s in
    let start, s_end = sub_token s (start_index s) 2 in
    let month, m_end = next_token s s_end 0x2D (* '-' *) in
    let day, d_end = sub_token s m_end 2 in
    let tzone, tz_end = next_token s d_end eos_uchar in
    if
      (not (equal start (mk "--")))
      || (not (validate_int month 2 2))
      || not (validate_int day 2 2)
    then abort ()
    else
      try
        let fields =
          [
            time_kind "gMonthDay";
            (qualify "month", make_integer month);
            (qualify "day", make_integer day);
          ]
          @ parse_timezone' tzone
        in
        Value.vrecord fields
      with
      | Cduce_error.Error (_, (Schema_builtin_Error,_)) -> abort ()

let validate_gDay =
  (* "---(\\d{2})(%s)?" : "(Z)|(([+-])?(\\d{2}):(\\d{2}))" *)
  let open Utf8 in
  fun s ->
    let abort () = simple_type_error "gDay" in
    let s = add_end s in
    let start, s_end = sub_token s (start_index s) 3 in
    let day, d_end = sub_token s s_end 2 in
    let tzone, tz_end = next_token s d_end eos_uchar in
    if (not (equal start (mk "---"))) || not (validate_int day 2 2) then
      abort ()
    else
      try
        let fields =
          time_kind "gDay"
          :: (qualify "day", make_integer day)
          :: parse_timezone' tzone
        in
        Value.vrecord fields
      with
      | Cduce_error.Error (_, (Schema_builtin_Error, _)) -> abort ()

let validate_gMonth =
  (* "--(\\d{2})--(%s)?" : "(Z)|(([+-])?(\\d{2}):(\\d{2}))" *)
  let open Utf8 in
  fun s ->
    let abort () = simple_type_error "gMonth " in
    let s = add_end s in
    let start, s_end = sub_token s (start_index s) 2 in
    let month, m_end = sub_token s s_end 2 in
    let tzone, tz_end = next_token s m_end eos_uchar in
    if (not (equal start (mk "--"))) || not (validate_int month 2 2) then
      failwith (get_str start ^ " " ^ get_str month ^ " " ^ get_str tzone)
    else
      try
        let fields =
          time_kind "gMonth"
          :: (qualify "month", make_integer month)
          :: parse_timezone_ddash tzone
        in
        Value.vrecord fields
      with
      | Cduce_error.Error (_, (Schema_builtin_Error, _)) -> abort ()

let validate_time =
  (* "^(%s)(%s)?$" : "(\\d{2}):(\\d{2}):(\\d{2})" , "(Z)|(([+-])?(\\d{2}):(\\d{2}))" *)
  let open Utf8 in
  fun s ->
    let s = add_end s in
    let time, t_end = sub_token s (start_index s) 8 in
    let tzone, tz_end = next_token s t_end eos_uchar in
    try
      let fields =
        (time_kind "time" :: parse_time time) @ parse_timezone' tzone
      in
      Value.vrecord fields
    with
    | Cduce_error.Error (_, (Schema_builtin_Error, _)) -> simple_type_error "time"

let validate_date =
  (* "^(-)?(%s)(%s)?$" : "(\\d{4,})-(\\d{2})-(\\d{2})" , "(Z)|(([+-])?(\\d{2}):(\\d{2}))" *)
  let open Utf8 in
  fun s ->
    let s = add_end s in
    let sign_uchar, sign_end = next s (start_index s) in
    let sign, sign_end =
      if sign_uchar == 0x2D (* '-' *) then
        (mk (String.make 1 (Char.chr sign_uchar)), sign_end)
      else (mk "", sign_end)
    in
    let date_y, d_end =
      if sign_uchar == 0x2D (* '-' *) then next_token s sign_end 0x2D (* '-' *)
      else next_token s (start_index s) 0x2D (* '-' *)
    in
    let date_t, d_end = sub_token s d_end 5 in
    let date = concat date_y (concat (mk "-") date_t) in
    let tzone, tz_end = next_token s d_end eos_uchar in
    if not (tz_end == end_index s) then simple_type_error "date"
    else
      try
        let fields =
          (time_kind "date" :: [ (qualify "positive", make_sign sign) ])
          @ parse_date date @ parse_timezone' tzone
        in
        Value.vrecord fields
      with
        Cduce_error.Error (_, (Schema_builtin_Error, _)) -> simple_type_error "date"

let validate_hexBinary s =
  let s = Utf8.get_str s in
  let len = String.length s in
  if len mod 2 <> 0 then simple_type_error "hexBinary";
  let res = Bytes.create (len / 2) in
  let rec aux idx =
    if idx < len then begin
      Bytes.unsafe_set res (idx / 2)
        (char_of_hex (String.unsafe_get s idx) (String.unsafe_get s (idx + 1)));
      aux (idx + 2)
    end
  in
  aux 0;
  validate_string (Utf8.mk (Bytes.to_string res))

let base64_chars =
  Bytes.of_string
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let base64_codes =
  let s = Bytes.make 256 '\000' in
  for i = 0 to Bytes.length base64_chars - 1 do
    Bytes.set s (Char.code (Bytes.get base64_chars i)) (Char.chr i)
  done;
  s

let base64_decode s =
  let len = Bytes.length s in
  if len mod 4 != 0 then simple_type_error "base64"
  else
    let padding =
      if len > 0 then
        if Bytes.get s (len - 2) == '=' then 2
        else if Bytes.get s (len - 1) == '=' then 1
        else 0
      else 0
    in

    let rec loop out i j =
      if i < len then (
        let c0 =
          Char.code (Bytes.get base64_codes (Char.code (Bytes.get s i)))
        in
        let c1 =
          Char.code (Bytes.get base64_codes (Char.code (Bytes.get s (i + 1))))
        in
        let c2 =
          Char.code (Bytes.get base64_codes (Char.code (Bytes.get s (i + 2))))
        in
        let c3 =
          Char.code (Bytes.get base64_codes (Char.code (Bytes.get s (i + 3))))
        in
        Bytes.set out j (Char.unsafe_chr ((c0 lsl 2) lor (c1 lsr 4)));
        Bytes.set out (j + 1) (Char.unsafe_chr ((c1 lsl 4) lor (c2 lsr 2)));
        Bytes.set out (j + 2) (Char.unsafe_chr ((c2 lsl 6) lor c3));
        loop out (i + 4) (j + 3))
      else out
    in
    let out = loop (Bytes.make (len * 3 / 4) '\000') 0 0 in
    if padding > 0 then Bytes.sub out 0 (len - padding) else out

let validate_base64Binary s =
  let s = Utf8.get_str s in
  validate_string
    (Utf8.mk (Bytes.to_string (base64_decode (Bytes.of_string s))))

let is_hex = function
  | 'A' .. 'F'
  | 'a' .. 'f'
  | '0' .. '9' ->
    true
  | _ -> false

(* see http://www.datypic.com/sc/xsd/t-xsd_anyURI.html *)
let validate_anyURI s =
  let rec loop s len found_sharp i =
    if i < len then
      match Bytes.get s i with
      | '#' ->
        if found_sharp then Cduce_error.(raise_err Schema_builtin_Malformed_URL s)
        else loop s len true (i + 1)
      | '%' ->
        if
          i < len - 2
          && is_hex (Bytes.get s (i + 1))
          && is_hex (Bytes.get s (i + 2))
        then loop s len found_sharp (i + 3)
        else Cduce_error.(raise_err Schema_builtin_Malformed_URL s)
      | _ -> loop s len found_sharp (i + 1)
    else s
  in
  let s = Bytes.of_string (Utf8.get_str s) in
  try
    validate_string
      (Utf8.mk (Bytes.to_string (loop s (Bytes.length s) false 0)))
  with
  | Cduce_error.Error (_, (Schema_builtin_Malformed_URL, _)) -> simple_type_error "anyURI"

(** {2 API backend} *)

type t = simple_type_definition * Types.t * (Utf8.t -> Value.t)

module QTable = Hashtbl.Make (Ns.QName)

let builtins : t QTable.t = QTable.create 50
let reg = QTable.add builtins

let restrict name (base, _, _) facets cd v =
  let name = add_xsd_prefix name in
  let t = simple_restrict (Some name) base facets in
  let b = (t, cd, v) in
  reg name b;
  b

let list name (item, _, _) cd v =
  let name = add_xsd_prefix name in
  let t = simple_list (Some name) item in
  let b = (t, cd, v) in
  reg name b;
  b

let primitive name cd v =
  let name = add_xsd_prefix name in
  let rec t =
    {
      st_name = Some name;
      st_variety = Atomic t;
      st_facets = no_facets;
      st_base = None;
    }
  in
  let b = (t, cd, v) in
  reg name b;
  b

let alias name b =
  let name = add_xsd_prefix name in
  reg name b

let any_simple_type =
  primitive "anySimpleType" Builtin_defs.string validate_string

let string = primitive "string" Builtin_defs.string validate_string
let _ = primitive "boolean" Builtin_defs.bool validate_bool
let _ = primitive "hexBinary" Builtin_defs.string validate_hexBinary
let _ = primitive "base64Binary" Builtin_defs.string validate_base64Binary
let _ = primitive "anyURI" Builtin_defs.string validate_anyURI
let _ = primitive "duration" duration_type validate_duration
let _ = primitive "dateTime" dateTime_type validate_dateTime
let _ = primitive "time" time_type validate_time
let _ = primitive "date" date_type validate_date
let _ = primitive "gYearMonth" gYearMonth_type validate_gYearMonth
let _ = primitive "gYear" gYear_type validate_gYear
let _ = primitive "gMonthDay" gMonthDay_type validate_gMonthDay
let _ = primitive "gDay" gDay_type validate_gDay
let _ = primitive "gMonth" gMonth_type validate_gMonth
let decimal = primitive "decimal" Builtin_defs.float validate_decimal

let _ =
  alias "float" decimal;
  alias "double" decimal

let _ = List.iter (fun n -> alias n string) unsupported

let int_type (name, min, max) =
  let ival =
    match (min, max) with
    | Some min, Some max ->
      let min = Intervals.V.mk min
      and max = Intervals.V.mk max in
      Intervals.bounded min max
    | None, Some max ->
      let max = Intervals.V.mk max in
      Intervals.left max
    | Some min, None ->
      let min = Intervals.V.mk min in
      Intervals.right min
    | None, None -> Intervals.any
  in
  ignore (primitive name (Types.interval ival) (validate_interval ival name))

let () =
  List.iter int_type
    [
      ("integer", None, None);
      ("nonPositiveInteger", None, Some "0");
      ("negativeInteger", None, Some "-1");
      ("long", Some "-9223372036854775808", Some "9223372036854775807");
      ("int", Some "-2147483648", Some "2147483647");
      ("short", Some "-32768", Some "32767");
      ("byte", Some "-128", Some "127");
      ("nonNegativeInteger", Some "0", None);
      ("unsignedLong", Some "0", Some "18446744073709551615");
      ("unsignedInt", Some "0", Some "4294967295");
      ("unsignedShort", Some "0", Some "65535");
      ("unsignedByte", Some "0", Some "255");
      ("positiveInteger", Some "1", None);
    ]

let normalized_string =
  restrict "normalizedString" string
    { no_facets with whiteSpace = (`Replace, false) }
    Builtin_defs.string validate_normalizedString

let token =
  restrict "token" normalized_string
    { no_facets with whiteSpace = (`Collapse, false) }
    Builtin_defs.string validate_token

let _ =
  alias "language" token;
  alias "Name" token;
  alias "NMTOKEN" token;
  alias "NCName" token;
  alias "ID" token;
  alias "IDREF" token;
  alias "ENTITY" token

let nmtokens = list "NMTOKENS" token string_list_type validate_token_list

let _ =
  alias "IDREFS" nmtokens;
  alias "ENTITIES" nmtokens

(** {2 Printing} *)

type kind =
  | Duration
  | DateTime
  | Time
  | Date
  | GYearMonth
  | GYear
  | GMonthDay
  | GDay
  | GMonth

type timezone = bool * Intervals.V.t * Intervals.V.t

(* positive, hour, minute *)
type time_value = {
  kind : kind option;
  positive : bool option;
  year : Intervals.V.t option;
  month : Intervals.V.t option;
  day : Intervals.V.t option;
  hour : Intervals.V.t option;
  minute : Intervals.V.t option;
  second : Intervals.V.t option;
  timezone : timezone option;
}

let null_value =
  {
    kind = None;
    positive = None;
    year = None;
    month = None;
    day = None;
    hour = None;
    minute = None;
    second = None;
    timezone = None;
  }

let string_of_time_type fields =
  let fail () = Cduce_error.(raise_err Schema_builtin_Error "") in
  let parse_int = function
    | Value.Integer i -> i
    | _ -> fail ()
  in
  let parse_timezone v =
    let fields =
      try Value.get_fields v with
      | Invalid_argument _ -> fail ()
    in
    let positive, hour, minute = (ref true, ref zero, ref zero) in
    List.iter
      (fun (lab, value) ->
         let ns, name = Ns.Label.value lab in
         if ns != Ns.empty then fail ();
         match Utf8.get_str name with
         | "positive" -> positive := Value.equal value Value.vtrue
         | "hour" -> hour := parse_int value
         | "minute" -> minute := parse_int value
         | _ -> fail ())
      fields;
    (!positive, !hour, !minute)
  in
  let parse_time_kind = function
    | Value.Atom q -> (
        let _, s = AtomSet.V.value q in
        match Utf8.get_str s with
        | "duration" -> Duration
        | "dateTime" -> DateTime
        | "time" -> Time
        | "date" -> Date
        | "gYearMonth" -> GYearMonth
        | "gYear" -> GYear
        | "gMonthDay" -> GMonthDay
        | "gDay" -> GDay
        | "gMonth" -> GMonth
        | _ -> fail ())
    | _ -> fail ()
  in
  let parse_positive = function
    | v when Value.equal v Value.vfalse -> false
    | _ -> true
  in
  let string_of_positive v =
    match v.positive with
    | Some false -> "-"
    | _ -> ""
  in
  let string_of_year v =
    match v.year with
    | None -> fail ()
    | Some i -> Intervals.V.to_string i
  in
  let string_of_month v =
    match v.month with
    | None -> fail ()
    | Some i -> Intervals.V.to_string i
  in
  let string_of_day v =
    match v.day with
    | None -> fail ()
    | Some i -> Intervals.V.to_string i
  in
  let string_of_hour v =
    match v.hour with
    | None -> fail ()
    | Some i -> Intervals.V.to_string i
  in
  let string_of_minute v =
    match v.minute with
    | None -> fail ()
    | Some i -> Intervals.V.to_string i
  in
  let string_of_second v =
    match v.second with
    | None -> fail ()
    | Some i -> Intervals.V.to_string i
  in
  let string_of_date v =
    sprintf "%s-%s-%s" (string_of_year v) (string_of_month v) (string_of_day v)
  in
  let string_of_timezone v =
    match v.timezone with
    | Some (positive, hour, minute) ->
      sprintf "Z%s%s:%s"
        (if not positive then "-" else "")
        (Intervals.V.to_string hour)
        (Intervals.V.to_string minute)
    | None -> ""
  in
  let string_of_time v =
    sprintf "%s:%s:%s" (string_of_hour v) (string_of_minute v)
      (string_of_second v)
  in
  let v =
    List.fold_left
      (fun acc (lab, value) ->
         let ns, local = Ns.Label.value lab in
         if ns != Ns.empty then fail ();
         match Utf8.get_str local with
         | "year" -> { acc with year = Some (parse_int value) }
         | "month" -> { acc with month = Some (parse_int value) }
         | "day" -> { acc with day = Some (parse_int value) }
         | "hour" -> { acc with hour = Some (parse_int value) }
         | "minute" -> { acc with minute = Some (parse_int value) }
         | "second" -> { acc with second = Some (parse_int value) }
         | "timezone" -> { acc with timezone = Some (parse_timezone value) }
         | "time_kind" -> { acc with kind = Some (parse_time_kind value) }
         | "positive" -> { acc with positive = Some (parse_positive value) }
         | _ -> fail ())
      null_value fields
  in
  let s =
    match v.kind with
    | None -> fail ()
    | Some Duration ->
      sprintf "%sP%s%s%s%s" (string_of_positive v)
        (match v.year with
         | Some v -> Intervals.V.to_string v ^ "Y"
         | _ -> "")
        (match v.month with
         | Some v -> Intervals.V.to_string v ^ "M"
         | _ -> "")
        (match v.day with
         | Some v -> Intervals.V.to_string v ^ "D"
         | _ -> "")
        (if v.hour = None && v.minute = None && v.second = None then ""
         else
           "T"
           ^ (match v.hour with
               | Some v -> Intervals.V.to_string v ^ "H"
               | _ -> "")
           ^ (match v.minute with
               | Some v -> Intervals.V.to_string v ^ "M"
               | _ -> "")
           ^
           match v.second with
           | Some v -> Intervals.V.to_string v ^ "S"
           | _ -> "")
    | Some DateTime ->
      sprintf "%s%sT%s%s" (string_of_positive v) (string_of_date v)
        (string_of_time v) (string_of_timezone v)
    | Some Time ->
      sprintf "%s%s%s" (string_of_positive v) (string_of_time v)
        (string_of_timezone v)
    | Some Date ->
      sprintf "%s%s%s" (string_of_positive v) (string_of_date v)
        (string_of_timezone v)
    | Some GYearMonth ->
      sprintf "%s%s-%s%s" (string_of_positive v) (string_of_year v)
        (string_of_month v) (string_of_timezone v)
    | Some GYear ->
      sprintf "%s%s%s" (string_of_positive v) (string_of_year v)
        (string_of_timezone v)
    | Some GMonthDay ->
      sprintf "--%s%s%s" (string_of_month v) (string_of_day v)
        (string_of_timezone v)
    | Some GDay -> sprintf "---%s%s" (string_of_day v) (string_of_timezone v)
    | Some GMonth ->
      sprintf "--%s--%s" (string_of_month v) (string_of_timezone v)
  in
  Utf8.mk s

(** {2 API} *)

let xsd_any = add_xsd_prefix "anyType"
let is s = QTable.mem builtins s || Ns.QName.equal s xsd_any
let iter f = QTable.iter f builtins
let get name = QTable.find builtins name
let simple_type (st, _, _) = st
let cd_type (_, t, _) = t
let validate (_, _, v) = v

let of_st = function
  | { st_name = Some n } -> get n
  | _ -> assert false
