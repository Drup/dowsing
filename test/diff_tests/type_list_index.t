Some basic tests on index made from type list

  $ dowsindex search --index idx_from_list.db "int -> int"
  dowsindex: [INFO] [0-3][7-9]
  
  2 : int -> int
  3 : int -> 'a
  6 : 'c -> int
  4 : 'a -> 'b

  $ dowsindex search --index idx_from_list.db "float -> int"
  dowsindex: [INFO] [0-2][8-9]
  
  6 : 'c -> int
  7 : float -> 'a
  4 : 'a -> 'b


  $ dowsindex search --index idx_from_list.db "int list"
  dowsindex: [INFO] [0-1][9-10]
  
  

