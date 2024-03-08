Some initial basic tests.

  $ dowsindex search --index fmt.db fmt "int -> int -> int"
  dowsindex: [INFO] [0-1]
  
  

  $ dowsindex search --index fmt.db fmt "int -> int -> int -> int"
  dowsindex: [INFO] [0-1]
  
  

  $ dowsindex search --index fmt.db fmt "int -> int list"
  dowsindex: [INFO] [0-1]
  
  

  $ dowsindex search --index fmt.db fmt ". int -> int -> 'a"
  dowsindex: [INFO] [0-12][73-74]
  
  fmt:Fmt.id : 'a -> 'a
  fmt:Fmt.to_to_string : 'a Fmt.t -> 'a -> string
  fmt:Fmt.const : 'a Fmt.t -> 'a -> 'b Fmt.t
  fmt:Fmt.epr : ('a, Format.formatter, unit) format -> 'a
  fmt:Fmt.pr : ('a, Format.formatter, unit) format -> 'a
  fmt:Fmt.str : ('a, Format.formatter, unit, string) format4 -> 'a
  fmt:Fmt.strf : ('a, Format.formatter, unit, string) format4 -> 'a
  fmt:Fmt.failwith : ('a, Format.formatter, unit, 'b) format4 -> 'a
  fmt:Fmt.failwith_notrace : ('a, Format.formatter, unit, 'b) format4 -> 'a
  fmt:Fmt.invalid_arg : ('a, Format.formatter, unit, 'b) format4 -> 'a
  fmt:Fmt.fmt : ('a, Format.formatter, unit) format -> Format.formatter -> 'a
  fmt:Fmt.pf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
  fmt:Fmt.error_msg :
  ('b, Format.formatter, unit, ('a, [> `Msg of string ]) result) format4 -> 'b
  fmt:Fmt.error :
  ('b, Format.formatter, unit, ('a, string) result) format4 -> 'b
  fmt:Fmt.str_like :
  Format.formatter -> ('a, Format.formatter, unit, string) format4 -> 'a
  fmt:Fmt.strf_like :
  Format.formatter -> ('a, Format.formatter, unit, string) format4 -> 'a
  fmt:Fmt.kstr :
  (string -> 'a) -> ('b, Format.formatter, unit, 'a) format4 -> 'b
  fmt:Fmt.kstrf :
  (string -> 'a) -> ('b, Format.formatter, unit, 'a) format4 -> 'b
  fmt:Fmt.kpf :
  (Format.formatter -> 'a) ->
  Format.formatter -> ('b, Format.formatter, unit, 'a) format4 -> 'b

