let
  var n;
  var i        := 2;
  var continue := 1;
  var mod
in
begin
  getint(n);
  while continue && i*i <= n do
    begin
      mod := n;
      while mod > 0 do mod := mod - i;
      continue := mod != 0;
      i := i + 1
    end;
  printint(n < 2 ? 0 : continue)
end
