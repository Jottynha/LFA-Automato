program teste;

{$mode fpc}

type
  TStrArray = array of ansistring;
  TTransition = record
    src, dst, sym: ansistring;
  end;
  TTransArray = array of TTransition;

// ----------------- Utilidades de string/arrays -----------------
procedure AddStr(var A: TStrArray; const S: ansistring);
var n: longint;
begin
  n := Length(A);
  SetLength(A, n+1);
  A[n] := S;
end;

procedure AddTrans(var A: TTransArray; const S1, S2, S3: ansistring);
var n: longint;
begin
  n := Length(A);
  SetLength(A, n+1);
  A[n].src := S1; A[n].dst := S2; A[n].sym := S3;
end;

function TrimSpaces(const S: ansistring): ansistring;
var i, l, r: longint;
begin
  l := 1; r := Length(S);
  while (l <= r) and (S[l] in [#9,#10,#13,' ']) do Inc(l);
  while (r >= l) and (S[r] in [#9,#10,#13,' ']) do Dec(r);
  if l > r then TrimSpaces := '' else TrimSpaces := Copy(S, l, r-l+1);
end;

function ReadAllText(const FileName: ansistring): ansistring;
var f: Text; line: ansistring;
begin
  ReadAllText := '';
  Assign(f, FileName);
  {$I-}
  Reset(f);
  {$I+}
  if IOResult <> 0 then exit;
  while not Eof(f) do
  begin
    ReadLn(f, line);
    ReadAllText := ReadAllText + line + #10;
  end;
  Close(f);
end;

function FindKeyPos(const JSON, Key: ansistring): longint;
var pattern: ansistring; p: longint;
begin
  pattern := '"' + Key + '"';
  // busca simples
  p := Pos(pattern, JSON);
  FindKeyPos := p;
end;

// Extrai a seção entre colchetes que segue a chave (ex: "alfabeto": [ ... ])
procedure ExtractBracketSection(const JSON: ansistring; startPos: longint; var content: ansistring; var ok: boolean);
var i, n, depth: longint; inStr: boolean; ch: char;
begin
  content := '';
  ok := false;
  n := Length(JSON);
  // encontrar primeiro '[' após startPos
  i := startPos;
  while (i <= n) and (JSON[i] <> '[') do Inc(i);
  if (i > n) or (JSON[i] <> '[') then exit;
  // agora varrer até colchete correspondente, respeitando strings
  depth := 0; inStr := false;
  while i <= n do
  begin
    ch := JSON[i];
    if inStr then
    begin
      if ch = '"' then inStr := false
      else if ch = '\\' then // pular escape
      begin
        if i < n then Inc(i);
      end;
    end
    else
    begin
      if ch = '"' then inStr := true
      else if ch = '[' then Inc(depth)
      else if ch = ']' then
      begin
        Dec(depth);
        if depth = 0 then
        begin
          // incluir conteúdo sem os colchetes externos
          // conteúdo começa no primeiro char após '[' original
          // precisamos achar o início novamente
          // retroceder para achar o '[' inicial
          // melhor: refazer: encontrar posInicial '[' em j0
        end;
      end;
    end;
    Inc(i);
    if (depth = 0) and (content <> '') then ;
  end;
end;

// Versão com retorno de índices: encontra [ ... ] e retorna início e fim dos colchetes (inclusive)
procedure ExtractBracketIndices(const JSON: ansistring; startPos: longint; var iStart, iEnd: longint; var ok: boolean);
var i, n, depth: longint; inStr: boolean; ch: char;
begin
  ok := false; iStart := -1; iEnd := -1;
  n := Length(JSON);
  i := startPos;
  while (i <= n) and (JSON[i] <> '[') do Inc(i);
  if (i > n) then exit;
  iStart := i; depth := 0; inStr := false;
  while i <= n do
  begin
    ch := JSON[i];
    if inStr then
    begin
      if ch = '"' then inStr := false
      else if ch = '\\' then if i < n then Inc(i);
    end
    else
    begin
      if ch = '"' then inStr := true
      else if ch = '[' then Inc(depth)
      else if ch = ']' then
      begin
        Dec(depth);
        if depth = 0 then begin iEnd := i; ok := true; exit; end;
      end;
    end;
    Inc(i);
  end;
end;

// Extrai todas as strings literais dentro de um conteúdo (assumindo JSON simples)
procedure ExtractStringsFromArray(const Section: ansistring; var outArr: TStrArray);
var i, n: longint; inStr: boolean; ch: char; buf: ansistring;
begin
  SetLength(outArr, 0);
  n := Length(Section); inStr := false; buf := '';
  i := 1;
  while i <= n do
  begin
    ch := Section[i];
    if not inStr then
    begin
      if ch = '"' then begin inStr := true; buf := ''; end;
    end
    else
    begin
      if ch = '"' then
      begin
        inStr := false;
        AddStr(outArr, buf);
      end
      else if ch = '\\' then
      begin
        if i < n then begin Inc(i); buf := buf + Section[i]; end;
      end
      else buf := buf + ch;
    end;
    Inc(i);
  end;
end;

// Para transições: extrai triplas de strings na ordem [src,dst,sym] repetidamente
procedure ExtractTransitions(const Section: ansistring; var outTrans: TTransArray);
var i, n, countInTriple: longint; inStr: boolean; ch: char; buf, s1, s2, s3: ansistring;
begin
  SetLength(outTrans, 0);
  n := Length(Section); inStr := false; buf := '';
  s1 := ''; s2 := ''; s3 := ''; countInTriple := 0;
  i := 1;
  while i <= n do
  begin
    ch := Section[i];
    if not inStr then
    begin
      if ch = '"' then begin inStr := true; buf := ''; end;
    end
    else
    begin
      if ch = '"' then
      begin
        inStr := false;
        Inc(countInTriple);
        case countInTriple of
          1: s1 := buf;
          2: s2 := buf;
          3: begin s3 := buf; AddTrans(outTrans, s1, s2, s3); s1 := ''; s2 := ''; s3 := ''; countInTriple := 0; end;
        end;
      end
      else if ch = '\\' then
      begin
        if i < n then begin Inc(i); buf := buf + Section[i]; end;
      end
      else buf := buf + ch;
    end;
    Inc(i);
  end;
end;

// ----------------- Programa principal -----------------
var
  path, json: ansistring;
  pKey, iStart, iEnd: longint; ok: boolean;
  section: ansistring;
  alphabet, states, initials, finals: TStrArray;
  transicoes: TTransArray;
  i: longint;
begin
  if ParamCount >= 1 then path := ParamStr(1) else path := 'data/automato.json';
  json := ReadAllText(path);
  if json = '' then
  begin
    WriteLn('Erro ao ler arquivo: ', path);
    Halt(1);
  end;

  // alfabeto
  pKey := FindKeyPos(json, 'alfabeto');
  ExtractBracketIndices(json, pKey, iStart, iEnd, ok);
  if ok then
  begin
    section := Copy(json, iStart+1, iEnd-iStart-1);
    ExtractStringsFromArray(section, alphabet);
  end;

  // estados
  pKey := FindKeyPos(json, 'estados');
  ExtractBracketIndices(json, pKey, iStart, iEnd, ok);
  if ok then
  begin
    section := Copy(json, iStart+1, iEnd-iStart-1);
    ExtractStringsFromArray(section, states);
  end;

  // iniciais
  pKey := FindKeyPos(json, 'estadosI');
  ExtractBracketIndices(json, pKey, iStart, iEnd, ok);
  if ok then
  begin
    section := Copy(json, iStart+1, iEnd-iStart-1);
    ExtractStringsFromArray(section, initials);
  end;

  // finais
  pKey := FindKeyPos(json, 'estadoF');
  ExtractBracketIndices(json, pKey, iStart, iEnd, ok);
  if ok then
  begin
    section := Copy(json, iStart+1, iEnd-iStart-1);
    ExtractStringsFromArray(section, finals);
  end;

  // transições
  pKey := FindKeyPos(json, 'transicoes');
  ExtractBracketIndices(json, pKey, iStart, iEnd, ok);
  if ok then
  begin
    section := Copy(json, iStart+1, iEnd-iStart-1);
    ExtractTransitions(section, transicoes);
  end;

  // Exibir
  WriteLn('Alfabeto:');
  for i := 0 to High(alphabet) do WriteLn('  ', alphabet[i]);

  WriteLn('Estados:');
  for i := 0 to High(states) do WriteLn('  ', states[i]);

  WriteLn('Iniciais:');
  for i := 0 to High(initials) do WriteLn('  ', initials[i]);

  WriteLn('Finais:');
  for i := 0 to High(finals) do WriteLn('  ', finals[i]);

  WriteLn('Transicoes:');
  for i := 0 to High(transicoes) do
    WriteLn('  ', transicoes[i].src, ' --', transicoes[i].sym, '--> ', transicoes[i].dst);
end.
