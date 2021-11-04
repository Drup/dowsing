# ᚛ Dowsing

Dowsing is a type of divination employed in attempts to locate functions by giving a type.

```
$ search "'a list * 'a -> bool"
...
List.mem : 'a -> 'a list -> bool
...

$ search "'a list -> ('a * 'b -> 'b) -> 'b -> 'b"
...
List.fold_left :
  ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
List.fold_right :
  ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
...
```

## Install and Use

```
opam pin git@github.com:Drup/dowsing.git
dowsing help
```
