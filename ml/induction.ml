let rec gcd x y =
  let y' = x mod y in
  if y' = 0 then y else gcd y y'

