let
  var n    : Integer;
  var half : Integer
in
begin
  getint (n);
  while n > 1 do
  begin
    printint(n);
    half := n / 2;
    if n == 2 * half
      then n := half
      else n := 3*n + 1
  end;
  printint(1)
end
