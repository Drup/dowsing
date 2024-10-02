open CCOption.Infix

type state = Done | Seen | Fresh

exception Cycle of Variable.t list

let find_cycle graph =
  let states = Variable.HMap.create 10 in
  Variable.Map.iter (fun v _ -> Variable.HMap.add states v Fresh) graph;
  let rec dfs v =
    match Variable.HMap.get_or ~default:Fresh states v with
    | Fresh ->
        Variable.HMap.add states v Seen;
        let res =
          let t = Variable.Map.get_or ~default:Type.dummy v graph in
          Iter.find_map dfs (Type.iter_vars t)
        in
        Variable.HMap.add states v Done;
        let+ u, path = res in
        if Variable.equal u v then raise (Cycle (u :: path)) else (u, v :: path)
    | Seen -> Some (v, [v])
    | Done -> None
  in
  try
    Variable.Map.iter (fun v _ -> assert (CCOption.is_none (dfs v))) graph;
    None
  with Cycle l -> Some l

let occur_check env = find_cycle (Env.vars env)
