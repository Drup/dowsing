type t = {
  args: ACTerm.t;
  ret: Type.t;
}

type problem = {
  left: t;
  right: t;
}

let make args ret : t = {args; ret}
let make_problem left right = {left; right}

let pp_problem ppf self =
  Fmt.pf ppf "%a -> %a = %a -> %a"
    Fmt.(array ~sep:(any " * ") Pure.pp) self.left.args
    Type.pp self.left.ret
    Fmt.(array ~sep:(any " * ") Pure.pp) self.right.args
    Type.pp self.right.ret
