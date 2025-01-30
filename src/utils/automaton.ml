module type S = sig

  type lt
  type state = private int
  type t

  module R : Regexp.S
  type regexp

  val create : unit -> t

  val mk_state : t -> state

  val add_trans : t -> state -> lt-> state -> unit
  val add_transitions : t -> (state * lt * state) list -> unit
  val set_start : t -> state -> unit
  val set_starts : t -> state list -> unit
  val set_final : t -> state -> unit
  val set_finals : t -> state list -> unit

  val check_word : t -> lt list -> bool

  val to_regex_my : t -> regexp

end

module Make (Lt : Regexp.Letter) : S with type lt = Lt.t
                                      and module R = Regexp.Make(Lt)
                                      and type regexp = Regexp.Make(Lt).t_simp = struct

  type lt = Lt.t

  type state = int
  module StateSet = Set.Make(Int)
  type states = StateSet.t

  type trans = state * lt * state
  module Trans = struct

    type t = trans

    let compare (state1, letter, state2 : t)
        (state1', letter', state2' : t) : state =
      let c1 = compare state1 state1' in
      match c1 with
      | 0 ->
        begin
          let c2 = Lt.compare letter letter' in
          match c2 with
          | 0 -> compare state2 state2'
          | _ -> c2
        end
      | _ -> c1

  end
  module TransSet = Set.Make(Trans)
  type transitions = TransSet.t

  type t = {
    mutable st_counter : state ;
    mutable states : states ;
    mutable starts : states ;
    mutable trans : transitions ;
    mutable finals : states ;
  }



  module R = Regexp.Make(Lt)
  type regexp = R.t_simp

  (* ================================================================= *)
  (* ================================================================= *)
  (* ================================================================= *)

  let create (_ : unit) : t = {
    st_counter = -1 ;
    states = StateSet.empty ;
    starts = StateSet.empty ;
    trans = TransSet.empty ;
    finals = StateSet.empty ;
  }



  let mk_state (auto : t) : state =
    auto.st_counter <- auto.st_counter + 1 ;
    auto.states <- StateSet.add auto.st_counter auto.states ;
    auto.st_counter

  let add_trans (auto : t)
      (state1 : state)
      (letter : lt)
      (state2 : state) : unit =
    if StateSet.mem state1 auto.states && StateSet.mem state2 auto.states then
      auto.trans <- TransSet.add (state1, letter, state2) auto.trans
    else
      failwith "[add_trans] both given states must be auto's states"

  let add_transitions (auto : t)
      (transitions : trans list) : unit =
    List.iter (
      fun (state1, letter, state2 : trans) : unit ->
        add_trans auto state1 letter state2
    )
      transitions

  let set_start (auto : t)
      (state : state) : unit =
    if StateSet.mem state auto.states then
      auto.starts <- StateSet.add state auto.starts
    else
      failwith "[set_start] given state must be an auto's state"

  let set_starts (auto : t)
      (states : state list) : unit =
    List.iter (set_start auto) states

  let set_final (auto : t)
      (state : state) : unit =
    if StateSet.mem state auto.states then
      auto.finals <- StateSet.add state auto.finals
    else
      failwith "[set_final] given state must be an auto's state"

  let set_finals (auto : t)
      (states : state list) : unit =
    List.iter (set_final auto) states



  let check_word (auto : t)
      (word : lt list) : bool =
    let end_states =
      List.fold_left (
        fun (current_states : states)
          (letter : lt) : states ->
          StateSet.fold (
            fun (s : state)
              (next_states : states) : states ->
              let next_states_for_s = TransSet.fold (
                  fun (s1, l, s2 : trans)
                    (next_states_labeled_letter : states) : states ->
                    if s = s1 && Lt.compare letter l = 0 then
                      StateSet.add s2 next_states_labeled_letter
                    else
                      next_states_labeled_letter
                )
                  auto.trans StateSet.empty
              in
              StateSet.union next_states next_states_for_s
          )
            current_states StateSet.empty
      )
        auto.starts word
    in
    StateSet.exists (Fun.flip StateSet.mem @@ auto.finals) end_states

  let to_regex_my (auto: t) : regexp =
    (* States renaming

       States are all from 0 to n
       And we can't remove a state, so there is no "hole"

       Therefore, we can just add 1 for all states
    *)
    let () = auto.states <- StateSet.map ((+) 1) auto.states in
    let () = auto.starts <- StateSet.map ((+) 1) auto.starts in
    let () = auto.finals <- StateSet.map ((+) 1) auto.finals in
    let () = auto.trans <- TransSet.map (
        fun (s1, l, s2 : trans) : trans -> s1 + 1, l, s2 + 1
      )
        auto.trans
    in
    (* McNaughton-Yamada algorithm *)
    let n = StateSet.cardinal auto.states in
    let get_transition_between (trans : transitions)
        (state1 : state)
        (state2 : state) : transitions =
      TransSet.filter (
        fun (state1', _, state2') ->
          state1 = state1'
          && state2 = state2'
      )
        trans
    in
    let mat1 = Array.init n (
        fun (i : state) : regexp array ->
          Array.init n (
            fun (j : state) : regexp ->
              let transitions = get_transition_between auto.trans (i+1) (j+1) in
              let regex = TransSet.fold (
                  fun (_, l, _ : trans)
                    (acc : regexp) : regexp ->
                    R.(
                      if is_empty acc then
                        letter l
                      else
                        union acc @@ letter l
                    )
                )
                  transitions R.empty
              in
              if TransSet.is_empty transitions then
                if i == j then
                  R.letter Lt.epsilon
                else
                  regex
              else
              if i == j && not @@ TransSet.mem (i, Lt.epsilon, j) transitions then
                R.(union regex @@ letter Lt.epsilon)
              else
                regex
          )
      )
    in
    let mat2 = Array.make_matrix n n R.empty in
    let choose_mat = ref true in
    let algo (k : int)
        (p : int)
        (q : int)
        (mat : regexp array array)
        (mat' : regexp array array) : unit =
      mat'.(p).(q) <-
        if R.is_empty mat.(p).(k) || R.is_empty mat.(k).(q) then
          mat.(p).(q)
        else if R.is_empty mat.(p).(q) then
          if R.is_empty mat.(k).(k) then
            R.letter Lt.epsilon
          else
            R.(
              concat mat.(p).(k)
              @@ concat (star mat.(k).(k)) mat.(k).(q)
            )
        else if k = p then
          R.(concat (star mat.(p).(p)) mat.(p).(q))
        else if k = q then
          R.(
            concat mat.(p).(q)
            @@ star mat.(q).(q)
          )
        else
          R.(
            union mat.(p).(q)
            @@ concat mat.(p).(k)
            @@ concat (star mat.(k).(k)) mat.(k).(q)
          )
    in
    let () =
      for k = 0 to n-1 do
        for p = 0 to n-1 do
          for q = 0 to n-1 do
            let algo = algo k p q in
            (* Instead of doing copies, we read and write alternately in two matrixes *)
            if !choose_mat then
              algo mat1 mat2
            else
              algo mat2 mat1
          done
        done ;
        choose_mat := not !choose_mat
      done
    in
    StateSet.fold (
      fun (start_state : state)
        (acc : regexp) : regexp ->
        StateSet.fold (
          fun (end_state : state)
            (acc' : regexp) : regexp ->
            if R.is_empty acc' then
              if start_state = end_state then
                R.(
                  union (letter Lt.epsilon) (
                    if !choose_mat then
                      mat1.(start_state-1).(end_state-1)
                    else
                      mat2.(start_state-1).(end_state-1)
                  )
                )
              else
              if !choose_mat then
                mat1.(start_state-1).(end_state-1)
              else
                mat2.(start_state-1).(end_state-1)
            else
            if start_state = end_state then
              R.(
                union acc'
                @@ union (letter Lt.epsilon) (
                  if !choose_mat then
                    mat1.(start_state-1).(end_state-1)
                  else
                    mat2.(start_state-1).(end_state-1)
                )
              )
            else
              R.(
                union acc' (
                  if !choose_mat then
                    mat1.(start_state-1).(end_state-1)
                  else
                    mat2.(start_state-1).(end_state-1)
                )
              )
        )
          auto.finals acc
    )
      auto.starts R.empty

end