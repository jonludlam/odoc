
# Module `Recent`

```
module type S = sig ... end
```
```
module type S1 = functor (_ : Recent.S) -> Recent.S
```
```
type variant =
| A
| B of int
| C (** foo *)
| D (** bar *)
| E of {
a : int;
}

```
```
type _ gadt =
| A : int Recent.gadt
| B : int -> string Recent.gadt (** foo *)
| C : {
a : int;
} -> unit Recent.gadt

```
```
type polymorphic_variant = [
| `A
| `B of int
| `C (** foo *)
| `D (** bar *)
]
```
```
type empty_variant = |
```
```
type nonrec nonrec_ = int
```
```
type empty_conj =
| X : [< `X of & 'a & int * float ] -> Recent.empty_conj

```
```
type conj =
| X : [< `X of int & [< `B of int & float ] ] -> Recent.conj

```
```
val empty_conj : [< `X of & 'a & int * float ]
```
```
val conj : [< `X of int & [< `B of int & float ] ]
```
```
module Z : sig ... end
```
```
module X : sig ... end
```
```
module type PolyS = sig ... end
```