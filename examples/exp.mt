let
  fun divides(m : Integer, n : Integer) : Boolean = n / m * m == n;
  fun exp(m : Integer, n : Integer) : Integer = n <= 0 ? 1 : n == 1 ? m : divides(2, n) ? exp(m*m, n/2) : m * exp(m*m, (n-1) / 2);
  var i : Integer;
  var j : Integer
in
begin
  i := 1;
  while i <= 10 do
    begin
      j := 1;
      while j <= 10 do
        begin
          printint(exp(i, j));
          j := j + 1
        end;
      i := i + 1
    end
end
