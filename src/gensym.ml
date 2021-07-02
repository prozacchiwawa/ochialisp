let argname_ctr = ref 0

let gensym name =
  let _ = argname_ctr := !argname_ctr + 1 in
  Printf.sprintf "%s_$_%d" name !argname_ctr
