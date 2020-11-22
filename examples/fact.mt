let
  var i : Integer := 0;
  fun fact(n : Integer) : Integer = n <= 1 ? 1 : n * fact(n-1)
in
begin
  while i <= 20 do
  begin
    printint(fact(i));
    i := i + 1
  end
end
