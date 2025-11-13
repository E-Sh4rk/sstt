open Cduce_loc

type type_fun = loc -> Types.t -> bool -> Types.t

let register op arity typ eval =
  Typer.register_op op arity typ;
  Eval.register_op op eval

let register_unary op typ eval =
  register op 1
    (function
      | [ tf ] -> typ tf
      | _ ->
        Cduce_error.(raise_err Typer_Error
                       ("Built-in operator " ^ op ^ " needs exactly one argument")))
    (function
      | [ v ] -> eval v
      | _ -> assert false)

let register_binary op typ eval =
  register op 2
    (function
      | [ tf1; tf2 ] -> typ tf1 tf2
      | _ ->
        Cduce_error.(raise_err Typer_Error
                       ("Built-in operator " ^ op ^ " needs exactly two arguments")))
    (function
      | [ v1; v2 ] -> eval v1 v2
      | _ -> assert false)

let register_cst op t v =
  register op 0
    (function
      | [] -> fun _ _ _ -> t
      | _ -> assert false)
    (function
      | [] -> v
      | _ -> assert false)

let register_fun op dom codom eval =
  let t = Types.arrow (Types.cons dom) (Types.cons codom) in
  register_cst op t
    (Value.Abstraction
       (Some [ (dom, codom) ], eval, not (Var.Set.is_empty (Types.Subst.vars t))))

let register_fun2 op dom1 dom2 codom eval =
  let t2 = Types.arrow (Types.cons dom2) (Types.cons codom) in
  let iface2 = Some [ (dom2, codom) ] in
  let t = Types.arrow (Types.cons dom1) (Types.cons t2) in
  let poly = not (Var.Set.is_empty (Types.Subst.vars t)) in
  let poly2 = not (Var.Set.is_empty (Types.Subst.vars t2)) in
  register_cst op t
    (Value.Abstraction
       ( Some [ (dom1, t2) ],
         (fun v1 -> Value.Abstraction (iface2, eval v1, poly)),
         poly2 ))

let register_fun3 op dom1 dom2 dom3 codom eval =
  let t3 = Types.arrow (Types.cons dom3) (Types.cons codom) in
  let t2 = Types.arrow (Types.cons dom2) (Types.cons t3) in
  let t1 = Types.arrow (Types.cons dom1) (Types.cons t2) in
  let iface3 = Some [ (dom3, codom) ] in
  let iface2 = Some [ (dom2, t3) ] in
  let iface1 = Some [ (dom1, t2) ] in
  let poly1 = not (Var.Set.is_empty (Types.Subst.vars t1)) in
  let poly2 = not (Var.Set.is_empty (Types.Subst.vars t2)) in
  let poly3 = not (Var.Set.is_empty (Types.Subst.vars t3)) in
  register_cst op t1
    (Value.Abstraction
       ( iface1,
         (fun x1 ->
            Value.Abstraction
              ( iface2,
                (fun x2 -> Value.Abstraction (iface3, eval x1 x2, poly3)),
                poly2 )),
         poly1 ))

let register_op op ?(expect = Types.any) typ eval =
  let f : type_fun -> type_fun = 
    fun tf loc t0 prec ->
      let t = tf loc expect true in
      typ loc t
  in
  register_unary op f eval

let register_op2 op t1 t2 s eval =
  register_binary op
    (fun tf1 tf2 loc _ _ ->
       ignore (tf1 loc t1 false);
       ignore (tf2 loc t2 false);
       s)
    eval
