unit u_utils;

{$mode fpc}{$H+}

interface

uses
  u_types; { Biblioteca local para tipagem }

function IntToStrPure(n: LongInt): AnsiString; { Int -> String }
function TrimSpaces(const S: AnsiString): AnsiString; { Corte de espaços em branco }
function ReadAllText(const FileName: AnsiString): AnsiString;
function FindKeyPos(const JSON, Key: AnsiString): LongInt; { Localiza posição do "Key" em JSON (retorna 0 se não achar) }
{ Encontra índices do bloco a partir de startPos; retorna iStart e iEnd e ok=true se achou }
procedure ExtractBracketIndices(const JSON: AnsiString; startPos: LongInt; var iStart, iEnd: LongInt; var ok: Boolean);
{ Extrai todas as literais de string dentro de uma seção (assumindo JSON simples) }
procedure ExtractStringsFromArray(const Section: AnsiString; var outArr: TStrArray);
{ Extrai tripletas de strings (src,dst,sym) repetidas e adiciona em outTrans }
procedure ExtractTransitions(const Section: AnsiString; var outTrans: TTransArray);

implementation

function IntToStrPure(n: LongInt): AnsiString;
var
  s: AnsiString;
  neg: Boolean;
  d: LongInt;
begin
  s := '';
  if n = 0 then
  begin
    IntToStrPure := '0';
    Exit;
  end;
  neg := n < 0; { sinal negativo }
  if neg then n := -n;
  while n > 0 do
  begin
    d := n mod 10;
    s := AnsiString(Chr(Ord('0') + d)) + s; 
    n := n div 10;
  end;
  if neg then s := '-' + s;
  IntToStrPure := s;
end;

function TrimSpaces(const S: AnsiString): AnsiString;
var
  l, r: LongInt;
  res: AnsiString;
begin
  res := '';
  l := 1; r := Length(S);
  while (l <= r) and (S[l] in [#9, #10, #13, ' ']) do Inc(l); { Enquanto houver espaço no início, avança }
  while (r >= l) and (S[r] in [#9, #10, #13, ' ']) do Dec(r); { Enquanto houver espaço no fim, recua }
  if l > r then res := '' else res := Copy(S, l, r - l + 1);
  TrimSpaces := res;
end;

function ReadAllText(const FileName: AnsiString): AnsiString;
var
  f: Text;
  line: AnsiString;
  okCode: LongInt;
  res: AnsiString;
begin
  res := '';
  Assign(f, FileName);
  {$I-} Reset(f); {$I+}
  okCode := IOResult;
  if okCode <> 0 then
  begin
    ReadAllText := '';
    Exit;
  end;
  while not Eof(f) do
  begin
    ReadLn(f, line);
    res := res + line + #10;
  end;
  Close(f);
  ReadAllText := res;
end;

function FindKeyPos(const JSON, Key: AnsiString): LongInt;
var
  pattern: AnsiString;
  p: LongInt;
begin
  pattern := '"' + Key + '"';
  p := Pos(pattern, JSON);
  FindKeyPos := p;
end;

procedure ExtractBracketIndices(const JSON: AnsiString; startPos: LongInt; var iStart, iEnd: LongInt; var ok: Boolean);
var
  i, n, depth: LongInt;
  inStr: Boolean;
  ch: AnsiChar;
begin
  ok := False;
  iStart := -1;
  iEnd := -1;
  n := Length(JSON);
  i := startPos;
  if i < 1 then i := 1;
  while (i <= n) and (JSON[i] <> '[') do Inc(i);
  if (i > n) then Exit;
  iStart := i;
  depth := 0;
  inStr := False;
  while i <= n do
  begin
    ch := JSON[i];
    if inStr then
    begin
      if ch = '"' then inStr := False
      else if ch = '\' then if i < n then Inc(i);
    end
    else
    begin
      if ch = '"' then inStr := True
      else if ch = '[' then Inc(depth)
      else if ch = ']' then
      begin
        Dec(depth);
        if depth = 0 then
        begin
          iEnd := i;
          ok := True;
          Exit;
        end;
      end;
    end;
    Inc(i);
  end;
end;

procedure ExtractStringsFromArray(const Section: AnsiString; var outArr: TStrArray);
var
  i, n: LongInt;
  inStr: Boolean;
  ch: AnsiChar;
  buf: AnsiString;
  res: TStrArray;
begin
  SetLength(res, 0);
  n := Length(Section);
  inStr := False;
  buf := '';
  i := 1;
  while i <= n do
  begin
    ch := Section[i];
    if not inStr then
    begin
      if ch = '"' then
      begin
        inStr := True;
        buf := '';
      end;
    end
    else
    begin
      if ch = '"' then
      begin
        inStr := False;
        SetLength(res, Length(res) + 1);
        res[High(res)] := buf;
      end
      else if ch = '\' then
      begin
        if i < n then
        begin
          Inc(i);
          buf := buf + Section[i];
        end;
      end
      else buf := buf + ch;
    end;
    Inc(i);
  end;
  outArr := res;
end;

procedure ExtractTransitions(const Section: AnsiString; var outTrans: TTransArray);
var
  i, n, countInTriple: LongInt;
  inStr: Boolean;
  ch: AnsiChar;
  buf, s1, s2, s3: AnsiString;
  res: TTransArray;
begin
  SetLength(res, 0);
  n := Length(Section);
  inStr := False;
  buf := '';
  s1 := ''; s2 := ''; s3 := ''; countInTriple := 0;
  i := 1;
  while i <= n do
  begin
    ch := Section[i];
    if not inStr then
    begin
      if ch = '"' then
      begin
        inStr := True;
        buf := '';
      end;
    end
    else
    begin
      if ch = '"' then
      begin
        inStr := False;
        Inc(countInTriple);
        case countInTriple of
          1: s1 := buf;
          2: s2 := buf;
          3: begin
               s3 := buf;
               SetLength(res, Length(res) + 1);
               res[High(res)].src := s1;
               res[High(res)].dst := s2;
               res[High(res)].sym := s3;
               s1 := ''; s2 := ''; s3 := ''; countInTriple := 0;
             end;
        end;
      end
      else if ch = '\' then
      begin
        if i < n then
        begin
          Inc(i);
          buf := buf + Section[i];
        end;
      end
      else buf := buf + ch;
    end;
    Inc(i);
  end;
  outTrans := res;
end;

end.
