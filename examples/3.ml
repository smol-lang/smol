let rec fact n =
  if n < 0 then
    -1
  else if n <= 1 then
    1
  else
    n * fact (n - 1)
in
print_int (fact 10)
