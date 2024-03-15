Some basic tests on index made from type list

  $ dowsing search --index idx_from_list.db "int -> int"
  dowsing: [INFO] [0-3][8-10]
  
  test:2 : int -> int
  test:3 : int -> 'a
  test:9 : 'c -> int
  test:4 : 'a -> 'b

  $ dowsing search --index idx_from_list.db "float -> int"
  dowsing: [INFO] [0-2][7-10]
  
  test:7 : float -> int
  test:9 : 'c -> int
  test:10 : float -> 'a
  test:4 : 'a -> 'b


  $ dowsing search --index idx_from_list.db "int list"
  dowsing: [INFO] [0-1][12-13]
  
  

