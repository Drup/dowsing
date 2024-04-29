Some initial basic tests.

  $ dowsing search --index fmt.db "int -> int -> int"
  

  $ dowsing search --index fmt.db "int -> int -> int -> int"
  

  $ dowsing search --index fmt.db "int -> int list"
  

  $ dowsing search --index fmt.db ". int -> int -> 'a"
  :Fmt.id : 'a -> 'a
  :Fmt.to_to_string : 'a Fmt.t -> 'a -> string
  :Fmt.const : 'a Fmt.t -> 'a -> 'b Fmt.t
  :Fmt.epr : ('a, Format.formatter, unit) format -> 'a
  :Fmt.pr : ('a, Format.formatter, unit) format -> 'a
  :Fmt.str : ('a, Format.formatter, unit, string) format4 -> 'a
  :Fmt.strf : ('a, Format.formatter, unit, string) format4 -> 'a
  :Fmt.failwith : ('a, Format.formatter, unit, 'b) format4 -> 'a
  :Fmt.failwith_notrace : ('a, Format.formatter, unit, 'b) format4 -> 'a
  :Fmt.invalid_arg : ('a, Format.formatter, unit, 'b) format4 -> 'a
  :Fmt.fmt : ('a, Format.formatter, unit) format -> Format.formatter -> 'a
  :Fmt.pf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
  :Fmt.str_like :
  Format.formatter -> ('a, Format.formatter, unit, string) format4 -> 'a
  :Fmt.strf_like :
  Format.formatter -> ('a, Format.formatter, unit, string) format4 -> 'a
  :Fmt.error : ('b, Format.formatter, unit, ('a, string) result) format4 -> 'b
  :Fmt.error_msg :
  ('b, Format.formatter, unit, ('a, [> `Msg of string ]) result) format4 -> 'b
  :Fmt.kstr : (string -> 'a) -> ('b, Format.formatter, unit, 'a) format4 -> 'b
  :Fmt.kstrf : (string -> 'a) -> ('b, Format.formatter, unit, 'a) format4 -> 'b
  :Fmt.kpf :
  (Format.formatter -> 'a) ->
  Format.formatter -> ('b, Format.formatter, unit, 'a) format4 -> 'b

