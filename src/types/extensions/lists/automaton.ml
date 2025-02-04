open Regexp
open Sstt_utils

module Make (L : Letter) = struct
  module State = Int
  type state = State.t
  module States = Set.Make(Int)
  module Transitions = struct
    module SPMap = Map.Make(struct
      type t = State.t * State.t
      let compare (s1,s2) (s1',s2') =
        compare s1 s1' |> ccmp
        compare s2 s2'
    end)
    type t = L.t list SPMap.t
    let empty = SPMap.empty
    let find s1 s2 t =
      match SPMap.find_opt (s1,s2) t with
      | None -> []
      | Some lst -> lst
    let add s1 l s2 t =
      let nl = l::(find s1 s2 t) in
      SPMap.add (s1,s2) nl t
  end

  type t = {
    mutable st_counter : State.t ;
    mutable trans : Transitions.t ;
    mutable finals : States.t ;
  }
  let create (_ : unit) : t = {
    st_counter = 0 ;
    trans = Transitions.empty ;
    finals = States.empty ;
  }

  let is_initial (_ : t) (s : state) = s = 0

  (** First state created is initial *)
  let mk_state (auto : t) : state =
    let st = auto.st_counter in
    auto.st_counter <- auto.st_counter + 1 ;
    st

  let add_trans (auto : t)
      (state1 : state)
      (letter : L.t)
      (state2 : state) : unit =
    auto.trans <- Transitions.add state1 letter state2 auto.trans

  let set_final (auto : t)
      (state : state) : unit =
    auto.finals <- States.add state auto.finals

  module R = Regexp(L)
  let to_regexp (auto : t) =
    let lst_to_reg lst =
      match lst with
      | [] -> Empty
      | hd::lst ->
        List.fold_left (fun acc e -> Union (acc, Car e)) (Car hd) lst
    in
    let n = auto.st_counter in
    let m = Array.make_matrix n n Empty in
    for i = 0 to n-1 do
      for j = 0 to n-1 do
        m.(i).(j) <- lst_to_reg (Transitions.find i j auto.trans)
      done
    done ;
    let v = Array.make n Empty in
    auto.finals |> States.elements |> List.iter (fun i -> v.(i) <- Epsilon) ;
    R.brzozowski m v |> R.simple_re

end