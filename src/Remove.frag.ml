let remove (k : key) (t : tree) : tree =
  let l, _, r = split k t in
  join2 l r
