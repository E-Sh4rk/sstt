# REPL

## Syntax

### Types

Types are built with the following constructors:
- `empty`, `any`, `tuple`, `arrow`, `record`, `int`, `enum`
- Monomorphic type variable `'x` (lowercase)
- Polymorphic type variables `'X` (uppercase)
- Enum `id`, where `id` is the identifier of an atom (enums that are not declared are created on-the-fly)
- Tag `id(t)`, where `id` is the identifier of a tag (tags that are not declared are created on-the-fly with the `identity` property, or the `monotonic` property if their name
starts with `_`, or `no property` if their name
starts with `__`)
- Arrow `t -> t`
- Tuple `(t, ..., t)`
- Closed record `{ label : t ; ... }`
- Opened record `{ label : t ; ... ..}`
- Interval `(i..i)`, `(i..)` or `(..i)`, where `i` is an integer
- Singleton interval `i`, where `i` is an integer
- Recursive type `t where id=t and ...`

The usual set-theoretic operations can be used:
union `|`, intersection `&`, difference `\`, and negation `~`.

The operator precedence is the usual one: `~`, `\`, `&`, `|`, `->`.

### Type computations

A type computation is either:
- A type `t`
- A subtyping test (either `t <= t`, `t = t`, or `t >= t`)
- A type substitution, noted inside brackets: `[ 'x : t ; ... ]`
- A set of constraints, corresponding to a tallying instance, also noted inside brackets: `[ t <= t ; ... ]`
- The application of a type substitution `s` to a type `t`, noted `t s`
- The composition of two type substitutions `s1` and `s2`, noted `s1 s2`
- The application of a parameter type `t2` to an arrow type `t1`, noted `t1 t2`

Note that if a polymorphic type variable appears in a subtyping test, it will return true if and only if there exists an instantiation that makes the subtyping instance hold. For instance, `'X <= int` is true, while `'x <= int` is false.

### Commands

A command in the REPL ends with `;;`. It can be either:
- A type alias definition `type id = t ;;`, or
- An atom definition `define id ;;`, or
- A tag definition `define id(p) ;;` where `p` is either ` ` (no property), `<=` (monotonic),
`&` (cap-preserving), `&&` (cap-empty-preserving), `|` (cup-preserving), `||` (cup-any-preserving), `=` (identity), or
- A type computation

## Examples

```
> type bool = true | false ;;

> (false,true) <= (bool,bool) ;;
true

> tag(true,false) | tag(false,true) ;;
tag((false, true) | (true, false))

> tag1(true,false) | tag2(false,true) ;;
tag1(true, false) | tag2(false, true)

> { l1 : any } & { l1 : any ; l2 : any ..} ;;
empty

> (true -> true) & (false -> false) <= bool -> bool ;;
true

> (true, (false, nil)) <= X where X = (bool, X) | nil ;;
true

> 'x & ~'x ;;
empty

> (int -> bool -> int) 42 bool ;;
int

> ('x, 'y) [ 'x: bool ; 'y: int ] ;;
bool, int

> [ ('X, 'Y) <= ('x, 'y) ] ;;
[
  'X: 'x & 'X ;
  'Y: 'y & 'Y
]
[
  'X: empty
]
[
  'Y: empty
]
```
