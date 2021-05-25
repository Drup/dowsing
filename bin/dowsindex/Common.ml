let exit ?(code = 0) ?(out = stdout) ?msg () =
  CCOpt.iter (Printf.fprintf out "%s\n") msg ;
  exit code

let error ?msg () =
  let msg = CCOpt.map (Fmt.str "error: %s") msg in
  exit () ~code:1 ~out:stderr ?msg

let type_of_string env str =
  try Type.of_string env str
  with Syntaxerr.Error _ ->
    error () ~msg:"syntax error in type argument."
