let
  var a;
  var b;
  var c;
  var n
in
begin
  getint(n);
  c := 1;
  while c <= n do
    begin
      b := 1;
      while b <= c do
        begin
          a := 1;
          while a <= b do
            begin
              if a*a + b*b == c*c
                then
                  begin
                    printint(a);
                    printint(b);
                    printint(c)
                  end
                else n := n;
              a := a + 1
            end;
          b := b + 1
        end;
      c := c + 1
    end
end
