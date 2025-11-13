type pack =
  | Int : (module Types.Kind) -> pack
  | Char : (module Types.Kind) -> pack
  | Atom : (module Types.Kind) -> pack
  | Times :
      (module Types.Kind
         with type Dnf.atom = Types.Node.t * Types.Node.t
          and type Dnf.line = (Types.Node.t * Types.Node.t) Bdd.line
          and type Dnf.dnf = (Types.Node.t * Types.Node.t) Bdd.dnf)
      -> pack
  | Xml :
      (module Types.Kind
         with type Dnf.atom = Types.Node.t * Types.Node.t
          and type Dnf.line = (Types.Node.t * Types.Node.t) Bdd.line
          and type Dnf.dnf = (Types.Node.t * Types.Node.t) Bdd.dnf)
      -> pack
  | Function :
      (module Types.Kind
         with type Dnf.atom = Types.Node.t * Types.Node.t
          and type Dnf.line = (Types.Node.t * Types.Node.t) Bdd.line
          and type Dnf.dnf = (Types.Node.t * Types.Node.t) Bdd.dnf)
      -> pack
  | Record :
      (module Types.Kind
         with type Dnf.atom = bool * Types.Node.t Ident.label_map
          and type Dnf.line = (bool * Types.Node.t Ident.label_map) Bdd.line
          and type Dnf.dnf = (bool * Types.Node.t Ident.label_map) Bdd.dnf)
      -> pack
  | Abstract : (module Types.Kind) -> pack
  | Absent : pack

let int_pack = Int (module Types.Int)
let char_pack = Char (module Types.Char)
let atom_pack = Atom (module Types.Atom)
let times_pack = Times (module Types.Times)
let xml_pack = Xml (module Types.Xml)
let rec_pack = Record (module Types.Rec)
let fun_pack = Function (module Types.Function)
let abstract_pack = Abstract (module Types.Abstract)

let fold f acc t =
  let acc = f acc int_pack t in
  let acc = f acc char_pack t in
  let acc = f acc atom_pack t in
  let acc = f acc times_pack t in
  let acc = f acc xml_pack t in
  let acc = f acc rec_pack t in
  let acc = f acc fun_pack t in
  let acc = f acc abstract_pack t in
  let acc = if Types.Record.has_absent t then f acc Absent t else acc in
  acc

let iter f (t : Types.t) = fold (fun () pack t -> f pack t) () t
