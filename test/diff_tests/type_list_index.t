Some basic tests on index made from type list

  $ dowsindex search --index idx_from_list.db "int -> int"
  dowsindex: [INFO] [0-3][8-10]
  
  2 : int -> int
  3 : int -> 'a
  9 : 'c -> int
  4 : 'a -> 'b

  $ dowsindex search --index idx_from_list.db "float -> int"
  dowsindex: [INFO] [0-2][7-10]
  
  7 : float -> int
  9 : 'c -> int
  10 : float -> 'a
  4 : 'a -> 'b


  $ dowsindex search --index idx_from_list.db "int list"
  dowsindex: [INFO] [0-1][12-13]
  
  

