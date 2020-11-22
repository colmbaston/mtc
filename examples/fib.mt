let
  var n : Integer;
  var i : Integer := 0;
  fun fib(n : Integer) : Integer = n <= 0 ? 0 : (n == 1 ? 1 : fib(n-1) + fib(n-2))
in
begin
  getint(n);
  while i <= n do
    begin
      printint(fib(i));
      i := i + 1
    end
end
