let
  var lower;
  var upper;
  var i;
  var continue
in
begin
  getint(lower);
  getint(upper);
  if lower <= 2 && 2 <= upper
    then begin printint(2); lower := 3 end
    else lower := 2 * (lower / 2) + 1;
  while lower <= upper do
    begin
      i        := 3;
      continue := 1;
      while continue && i*i <= lower do
        begin
          continue := i * (lower / i) != lower;
          i        := i + 2
        end;
      if continue
        then printint(lower)
        else lower := lower;
      lower := lower + 2
    end
end
