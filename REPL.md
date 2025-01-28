# REPL

## Syntax

### Types

Types are built with the following constructors:
- `empty`, `any`, `tuple`, `arrow`, `record`, `int`
- Monomorphic type variable `'x` (lowercase)
- Polymorphic type variables `'X` (uppercase)
- Atom `id`, where `id` is the identifier of an atom (atoms are created on-the-fly and do not need to be declared)
- Tag `id(t)`, where `id` is the identifier of a tag (tags are created on-the-fly and do not need to be declared)
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

### Commands

A command in the REPL ends with `;;`. It can be either:
- A type alias definition `type id = t ;;`, or
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
