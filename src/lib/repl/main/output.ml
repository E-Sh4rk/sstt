open Effect.Deep
open Effect

type msg_kind =
  Error | Warning | Info | Msg | Log of int

type _ Effect.t += Print: (msg_kind * ('a, Format.formatter, unit) format) -> 'a t

let print k fmt = perform (Print (k,fmt))

let with_basic_output fmtout f arg =
  match f arg with
  | x -> x
  | effect Print (kind, fmt), k ->
    begin match kind with
    | Error ->
      let fmt = "[Error] "^^fmt^^"@." in
      continue k (Format.fprintf fmtout fmt)
    | Warning ->
      let fmt = "[Warning] "^^fmt^^"@." in
      continue k (Format.fprintf fmtout fmt)
    | Info ->
      let fmt = "[Info] "^^fmt^^"@." in
      continue k (Format.fprintf fmtout fmt)
    | Msg ->
      let fmt = fmt^^"@." in
      continue k (Format.fprintf fmtout fmt)
    | Log _ ->
      continue k (Format.ifprintf fmtout fmt)
    end

let with_rich_output fmtout f arg =
  match f arg with
  | x -> x
  | effect Print (kind, fmt), k ->
    begin match kind with
    | Error ->
      let fmt = "@{<red;bold>[Error] "^^fmt^^"@}@." in
      continue k (Format.fprintf fmtout fmt)
    | Warning ->
      let fmt = "@{<yellow;bold>[Warning] "^^fmt^^"@}@." in
      continue k (Format.fprintf fmtout fmt)
    | Info ->
      let fmt = "@{<blue;bold>[Info] "^^fmt^^"@}@." in
      continue k (Format.fprintf fmtout fmt)
    | Msg ->
      let fmt = fmt^^"@." in
      continue k (Format.fprintf fmtout fmt)
    | Log _ ->
      continue k (Format.ifprintf fmtout fmt)
    end
