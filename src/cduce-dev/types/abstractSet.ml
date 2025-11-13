module V = struct
  type t = string * Obj.t
end

module T = Custom.String
include SortedList.FiniteCofinite (T)

let test l =
  match l with
  | Finite [] -> Tset.Empty
  | Cofinite [] -> Tset.Full
  | _ -> Tset.Unknown

let print = function
  | Finite l -> List.map (fun x ppf -> Format.fprintf ppf "!%s" x) l
  | Cofinite l ->
      [
        (fun ppf ->
          Format.fprintf ppf "@[Abstract";
          List.iter (fun x -> Format.fprintf ppf " \\@ !%s" x) l;
          Format.fprintf ppf "@]");
      ]

let contains_sample s t =
  match (s, t) with
  | None, Cofinite _ -> true
  | None, Finite _ -> false
  | Some s, t -> contains s t
