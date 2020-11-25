let
  fun max(m : Integer, n : Integer) : Integer = m > n ? m : n;
  fun divides(m : Integer, n : Integer) : Boolean = n / m * m == n;
  fun trialDivision(m : Integer, n : Integer) : Boolean = n < m*m ? true : !divides(m, n) && trialDivision(m + 2, n);
  fun isPrime(n : Integer) : Boolean = n < 2 ? false : n == 2 || !divides(2, n) && trialDivision(3, n);
  var lower    : Integer;
  var upper    : Integer
in
begin
  getint(lower);
  getint(upper);
  lower := max(lower, 2);
  if lower <= 2 && 2 <= upper
    then begin printint(2); lower := 3 end
    else lower := lower;
  lower := 2 * (lower / 2) + 1;
  while lower <= upper do
    begin
      if isPrime(lower)
        then printint(lower)
        else lower := lower;
      lower := lower + 2
    end
end
