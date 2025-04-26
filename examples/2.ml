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
let rec fib n =
  if n = 0 then
    0
  else if n = 1 then
    1
  else
    fib (n - 1) + fib (n - 2)
in
power 3 ((fib 8) - 1)

