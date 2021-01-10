let
  fun divides(m : Integer, n : Integer) : Boolean = n / m * m == n;
  fun trialDivision(m : Integer, n : Integer) : Boolean = n < m * m ? true : divides(m, n) ? false : trialDivision(m + 2, n);
  fun isPrime(n : Integer) : Boolean = n < 2 ? false : n == 2 ? true : divides(2, n) ? false : trialDivision(3, n);
  var n : Integer := 1;
  var s : Integer := 4;
  var k : Integer := 3
in
while k <= 5000 do
begin
  while !isPrime(k) do k := k + 2;
  s := s + k * k;
  n := n + 1;
  if divides(n, s)
    then begin printint(n) ; printint(s) end
    else n := n;
  k := k + 2
end
