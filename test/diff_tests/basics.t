Some initial basic tests.

  $ dowsindex search --index fmt.db fmt "int -> int -> int"
  dowsindex: [INFO] [0-1]
  
  

  $ dowsindex search --index fmt.db fmt "int -> int -> int -> int"
  dowsindex: [INFO] [0-1]
  
  

  $ dowsindex search --index fmt.db fmt "int -> int list"
  dowsindex: [INFO] [0-1]
  
  

  $ dowsindex search --index fmt.db fmt ". int -> int -> 'a"
  dowsindex: [INFO] [0-12][73-74]
  
  Fmt.id : 'a -> 'a
  Fmt.to_to_string : 'a Fmt.t -> 'a -> string
  Fmt.const : 'a Fmt.t -> 'a -> 'b Fmt.t

