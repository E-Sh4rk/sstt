# Row Polymorphism

SSTT implements row-polymorphism as formalized in the article
[*Revisiting Row Polymorphism for Set-Theoretic Types*](https://mlaurent.ovh/publications/row_poly.pdf).

## Record types: bindings and tail

A **record** in SSTT is a set-theoretic type for labeled collections of values.
Internally, a record is described by a list of **explicit bindings**: a finite mapping from field labels to field types, plus a **tail**.

The **tail** is a field type that applies to every label *not* explicitly listed.
It captures what happens for unlisted fields — in particular, it determines whether the record is closed (no extra fields) or open (extra fields allowed).

The full syntax for a record with an explicit tail is:

```
{ label1 : f1 ; label2 : f2 ; ... ;; tail }
```

where each `fi` and `tail` are **field types**.

### Field types

A field type `f` is either:
- An ordinary type `t` — the field is present and has type `t`
- An optional type `t?` — the field is either present with type `t`, or absent (i.e. `t?` = `t | absent`, where `absent` is a special value that does *not* inhabit `any`)
- A row variable `` `x `` or a type variable `'x`
- A Boolean combination of the above elements

Note that the value `absent` cannot be directly mentioned,
instead an absent field can be given the type `empty?`.


### Closed and open records

Two common shorthands use the tail to express closedness:

- **Closed record** `{ a:bool ; b:int }` is shorthand for `{ a:bool ; b:int ;; empty? }`.
  The tail `empty?` means every unlisted field must be absent, so no extra fields are allowed.

- **Open record** `{ a:bool ; b:int ..}` is shorthand for `{ a:bool ; b:int ;; any? }`.
  The tail `any?` places no constraint on unlisted fields, so any extra fields are allowed.

## Row variables vs type variables

SSTT supports two kinds of variables for polymorphism.

### Row variables `` `x ``

A **row variable** `` `x `` (lowercase) or `` `X `` (uppercase, polymorphic) ranges over *rows* — that is, mappings from labels to field types. *Rows* are noted using the same syntax as *records types* (e.g. ``{ a:bool ..}`` for the row assigning `bool` to the label `a` and `any?` to any other label), even though rows are *not* types (they are assignments).

When a row variable appears as the tail, it captures the entire set of unlisted fields. A row variable cannot appear outside of a record constructor.

Example — tallying ``[ { a:bool ; b:int ..} <= { a:bool ;; `A } ]`` forces `` `A `` to be a supertype of the row `{ b:int ..}`:
```
> [ { a:bool ; b:int ..} <= { a:bool ;; `A } ] ;;
[
  `A: { a : `A ; b : int | `A ..}
]
```

The solution encodes the following constraints:
- `` `A.b `` (the assignment for label `b` in `A`) is larger than `int`
- `` `A.a `` can be anything
- for any other label `l`, `` `A.l `` must be greater than `any?`
(this is captured by the tail `..` of the row)

### Type variables `'x`

A **type variable** `'x` (lowercase) or `` 'X `` (uppercase, polymorphic) ranges over *types*, not rows.
When used as a tail, it applies the *same* type uniformly to every unlisted field.
A type variable *cannot* capture `absent`: as a consequence, when used as a tail, the optional variant `'x?` is needed if the record may lack some fields.

Example — tallying `` [ { a:bool ; b:int } <= { ;; 'A? } ] `` forces `'A?` to capture the possible values of all fields (listed or not), i.e. `bool|int|absent`, and thus forces `'A` to capture `bool|int`:
```
> [ { a:bool ; b:int } <= { ;; 'A? } ] ;;
[
  'A: bool | int | 'A
]
```

Example — tallying `` [ { a:bool ; b:int } <= { ;; 'A } ] `` has *no solution*, because a plain type variable `'A` cannot capture the absent field type.

## Row-polymorphic function types

Row variables and type variables in record tails enable expressive polymorphic function types.

**Row variable tail — `mix`:**
```
mix: { ;; `A } -> { ;; `B } -> { ;; `A | `B }
```
This function takes two records and returns their per-field union. The row variables `` `A `` and `` `B `` each capture an arbitrary set of fields, and `` `A | `B `` denotes the pointwise union of the two rows.

**Optional type variable tail — `get_any_field`:**
```
get_any_field: { ;; 'a? } -> 'a
```
This function accepts any record (open or closed, with any fields) and returns the value of one of its fields. The tail `'a?` constrains all fields to have type `'a | absent`, so the present fields all share type `'a`.
