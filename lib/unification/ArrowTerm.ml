type t = {
  args: Type.Tuple.t;
  ret: Type.t;
}

type problem = {
  left: t;
  right: t;
}

let make args ret : t = {args; ret}

let pp ppf self =
  Fmt.pf ppf "%a -> %a"
    (Type.Tuple.pp Type.pp)
    self.args Type.pp self.ret

let make_problem left right = {left; right}

let pp_problem ppf self =
  Fmt.pf ppf "%a = %a" pp self.left pp self.right
