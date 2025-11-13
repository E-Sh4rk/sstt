(* The automata for pattern matching *)
open Ident

type source =
  | Catch
  | Const of Types.const
  | Stack of int
  | Left
  | Right
  | Nil
  | Recompose of int * int

type result = int * source array * int
(* Return code, result values, number of values to pop *)

type actions =
  | AIgnore of result
  | AKind of actions_kind

and actions_kind = {
  basic : (Types.t * result) list;
  atoms : result AtomSet.map;
  chars : result CharSet.map;
  prod : result dispatch dispatch;
  xml : result dispatch dispatch;
  record : record option;
}

and record =
  | RecLabel of label * result dispatch dispatch
  | RecNolabel of result option * result option

and 'a dispatch =
  | Dispatch of state * 'a array
  | TailCall of state
  | Ignore of 'a
  | Impossible

and state = {
  uid : int;
  arity : int array;
  mutable actions : actions;
  mutable fail_code : int;
  mutable expected_type : string;
}

type 'a rhs =
  | Match of int * 'a
  | Fail
