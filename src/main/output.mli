
type msg_kind =
  Error | Warning | Info | Msg | Log of int

val print : msg_kind -> ('a, Format.formatter, unit) format -> 'a
val with_basic_output : Format.formatter -> ('a -> 'b) -> 'a -> 'b
val with_rich_output : Format.formatter -> ('a -> 'b) -> 'a -> 'b
