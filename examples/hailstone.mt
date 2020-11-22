let
  var n      : Integer;
  fun even(n : Integer) : Boolean = n / 2 * 2 == n;
  fun next(n : Integer) : Integer = even(n) ? n / 2 : n * 3 + 1
in
begin
  getint(n);
  while n > 1 do
  begin
    printint(n);
    n := next(n)
  end;
  printint(1)
end
