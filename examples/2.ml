let rec power a b =
  if b = 0
  then 1
  else
    let half = power a (b / 2) in
    let full = half * half in
    if b mod 2 = 1 then
      full * a
    else
      full
in
let double' = fun x -> 2 * x in
power 10 (double' 5)
