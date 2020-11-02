let
  var n;
  var i        := 2;
  var continue := 1
in
begin
  getint(n);
  while continue && i*i <= n do
    begin
      continue := i * (n / i) != n;
      if i == 2
        then i := 3
        else i := i + 2
    end;
  printint(n < 2 ? 0 : continue)
end
