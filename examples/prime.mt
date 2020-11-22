let
  var lower    : Integer;
  var upper    : Integer;
  var i        : Integer;
  var continue : Boolean;
  fun max(m : Integer, n : Integer) : Integer = m > n ? m : n;
  fun divides(m : Integer, n : Integer) : Boolean = n/m * m == n
in
begin
  getint(lower);
  getint(upper);
  lower := max(lower, 2);
  if lower <= 2 && 2 <= upper
    then begin printint(2); lower := 3 end
    else lower := 2 * (lower / 2) + 1;
  while lower <= upper do
    begin
      i        := 3;
      continue := true;
      while continue && i*i <= lower do
        begin
          continue := !divides(i, lower);
          i := i + 2
        end;
      if continue
        then printint(lower)
        else lower := lower;
      lower := lower + 2
    end
end
