let
  var n : Integer;
  var x : Integer;
  var i : Integer
in
begin
  getint (n);
  if n < 0 then x := 0 else x := 1;
  i := 2;
  while i <= n do
    begin
      x := x * i;
      i := i + 1
    end;
  printint (x)
end
