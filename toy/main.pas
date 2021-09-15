{
Un pas de danse est soit :
- s N, avec N le nombre d'éléments à la fin à remettre au début
  s 3 sur ABCDE donne CDEAB
- x I J, avec I et J les positions des éléments (à partir de 0) à échanger
  x 4 2 sur ABCDE donne ABEDC
- p AB, avec A et B les éléments à échanger (où qu'ils soient)
  p A C sur ABCDE donne CBADE

Pour simplifier les choses, le code d'exemple ne gère pas les erreurs.
}

procedure spin;
var
  n: 1..15;
begin
  readln(n);
  writeln('spin ', n);
end;

procedure exchange;
var
  p1, p2: 0..15;
begin
  readln(p1, p2);
  writeln('exchange ', p1, ', ', p2);
end;

procedure partner;
var
  c1, c2: 'A'..'P';
begin
  readln(c1, c2);
  writeln('partner ', c1, ', ', c2);
end;

procedure step;
var
  o, space: char;
begin
  read(o, space);
  case o of
    's': spin;
    'x': exchange;
    'p': partner;
  end;
end;


begin
  while not eof do step
end.
