let features = ref []

let get_features () =
  List.stable_sort (fun (p1, _, _, _) (p2, _, _, _) -> compare p2 p1) !features

let initialized = ref false
let init_all () =
  if not !initialized then begin
    initialized := true;
    List.iter (fun (_, _, _, f) -> f ()) (List.rev (get_features ()))
  end

let register ?(priority = 0) n d f =
  features := (priority, n, d, f) :: !features

let descrs () = List.rev_map (fun (_, n, d, _) -> (n, d)) (get_features ())
let inhibit n = features := List.filter (fun (_, n', _, _) -> n <> n') !features
