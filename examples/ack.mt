let
  var i : Integer;
  var j : Integer;
  fun ack(m : Integer, n : Integer) : Integer = m <= 0 ? n + 1 : (n <= 0 ? ack(m-1, 1) : ack(m-1, ack(m, n-1)))
in
begin
  i := 0;
  while i <= 3 do
    begin
      j := 0;
      while j <= 10 do
        begin
          printint(ack(i, j));
          j := j + 1
        end;
      i := i + 1
    end
end
