program teste;

{$mode fpc}

// IntToStr em Pascal puro (evita depender de SysUtils)
function IntToStr(n: longint): ansistring;
var s: ansistring; neg: boolean; d: longint;
begin
  if n = 0 then begin IntToStr := '0'; exit; end;
  neg := n < 0; if neg then n := -n;
  s := '';
  while n > 0 do
  begin
    d := n mod 10;
    s := Chr(Ord('0') + d) + s;
    n := n div 10;
  end;
  if neg then s := '-' + s;
  IntToStr := s;
end;

type
  TStrArray = array of ansistring;
  TTransition = record
    src, dst, sym: ansistring;
  end;
  TTransArray = array of TTransition;

const
  EPS: ansistring = '&';

// ----------------- Utilidades de string/arrays -----------------
procedure AddStr(var A: TStrArray; const S: ansistring);
var n: longint;
begin
  n := Length(A);
  SetLength(A, n+1);
  A[n] := S;
end;

procedure AddStrUnique(var A: TStrArray; const S: ansistring);
var i: longint;
begin
  for i := 0 to High(A) do if A[i] = S then exit;
  AddStr(A, S);
end;

function ContainsStr(const A: TStrArray; const S: ansistring): boolean;
var i: longint;
begin
  for i := 0 to High(A) do if A[i] = S then begin ContainsStr := true; exit; end;
  ContainsStr := false;
end;

procedure AddTrans(var A: TTransArray; const S1, S2, S3: ansistring);
var n: longint;
begin
  n := Length(A);
  SetLength(A, n+1);
  A[n].src := S1; A[n].dst := S2; A[n].sym := S3;
end;

procedure AddTransUnique(var A: TTransArray; const S1, S2, S3: ansistring);
var i: longint;
begin
  for i := 0 to High(A) do
    if (A[i].src=S1) and (A[i].dst=S2) and (A[i].sym=S3) then exit;
  AddTrans(A, S1, S2, S3);
end;

function TrimSpaces(const S: ansistring): ansistring;
var l, r: longint;
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

// --------- Operações de autômato (puro) ---------

function GetTargets(const Trans: TTransArray; const Src, Sym: ansistring): TStrArray;
var
  i: longint;
  res: TStrArray;
begin
  SetLength(res, 0);
  for i := 0 to High(Trans) do
    if (Trans[i].src = Src) and (Trans[i].sym = Sym) then
      AddStrUnique(res, Trans[i].dst);
  GetTargets := res;
end;

function UnionStr(const A, B: TStrArray): TStrArray;
var i: longint; tmp: TStrArray;
begin
  tmp := A;
  for i := 0 to High(B) do AddStrUnique(tmp, B[i]);
  UnionStr := tmp;
end;

function IntersectsStr(const A, B: TStrArray): boolean;
var i: longint;
begin
  for i := 0 to High(A) do if ContainsStr(B, A[i]) then begin IntersectsStr := true; exit; end;
  IntersectsStr := false;
end;

function EpsClosure(const Trans: TTransArray; const StartStates: TStrArray): TStrArray;
var stack, res: TStrArray; i: longint; s: ansistring; tgts: TStrArray;
begin
  res := StartStates;
  stack := StartStates;
  while Length(stack) > 0 do
  begin
    s := stack[High(stack)];
    SetLength(stack, Length(stack)-1);
    tgts := GetTargets(Trans, s, EPS);
    for i := 0 to High(tgts) do
      if not ContainsStr(res, tgts[i]) then begin AddStr(res, tgts[i]); AddStr(stack, tgts[i]); end;
  end;
  EpsClosure := res;
end;

procedure ConvertMultipleInitialsToAFNEps(var States, Initials: TStrArray; var Trans: TTransArray);
var i: longint; base, novo: ansistring; idx: longint;
begin
  if Length(Initials) <= 1 then exit;
  base := 'Qi'; novo := base; idx := 0;
  while ContainsStr(States, novo) do begin Inc(idx); novo := base + IntToStr(idx); end;
  AddStrUnique(States, novo);
  for i := 0 to High(Initials) do AddTransUnique(Trans, novo, Initials[i], EPS);
  SetLength(Initials, 1); Initials[0] := novo;
  WriteLn('Novo inicial criado: ', novo, ' com transições & para antigos iniciais.');
end;

procedure RemoveEpsilon(var Alphabet, States, Initials, Finals: TStrArray; var Trans: TTransArray);
var i, j, k: longint; p, q, r, sym: ansistring; Ep, temp, U: TStrArray; newTrans: TTransArray; newFinals: TStrArray; tg: TStrArray;
begin
  SetLength(newTrans, 0);
  SetLength(newFinals, 0);
  // finais: recalculados via fecho-ε
  for i := 0 to High(States) do
  begin
    p := States[i];
    Ep := EpsClosure(Trans, TStrArray([p]));
    if IntersectsStr(Ep, Finals) then AddStrUnique(newFinals, p);
  end;
  // transições por símbolo (exceto EPS)
  for i := 0 to High(States) do
  begin
    p := States[i];
    Ep := EpsClosure(Trans, TStrArray([p]));
    for j := 0 to High(Alphabet) do
    begin
      sym := Alphabet[j];
      if sym = EPS then continue;
      SetLength(temp, 0);
      for k := 0 to High(Ep) do
      begin
        q := Ep[k];
        tg := GetTargets(Trans, q, sym);
        temp := UnionStr(temp, tg);
      end;
      U := EpsClosure(Trans, temp);
      for k := 0 to High(U) do
      begin
        r := U[k];
        AddTransUnique(newTrans, p, r, sym);
      end;
    end;
  end;
  Trans := newTrans;
  Finals := newFinals;
  WriteLn('Removidas transições & (epsilon).');
end;

// ----------------- Exibição e Menu -----------------
procedure PrintAutomaton(const Alphabet, States, Initials, Finals: TStrArray; const Trans: TTransArray);
var i: longint;
begin
  WriteLn('Alfabeto:');
  for i := 0 to High(Alphabet) do WriteLn('  ', Alphabet[i]);
  WriteLn('Estados:');
  for i := 0 to High(States) do WriteLn('  ', States[i]);
  WriteLn('Iniciais:');
  for i := 0 to High(Initials) do WriteLn('  ', Initials[i]);
  WriteLn('Finais:');
  for i := 0 to High(Finals) do WriteLn('  ', Finals[i]);
  WriteLn('Transicoes:');
  for i := 0 to High(Trans) do
    WriteLn('  ', Trans[i].src, ' --', Trans[i].sym, '--> ', Trans[i].dst);
end;

// ----------------- Programa principal -----------------
var
  path, json: ansistring;
  pKey, iStart, iEnd: longint; ok: boolean;
  section: ansistring;
  alphabet, states, initials, finals: TStrArray;
  transicoes: TTransArray;
  op: ansistring;
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

  // Menu
  while true do
  begin
    WriteLn; WriteLn('--- MENU ---');
    WriteLn('1) Converter múltiplos estados iniciais -> AFN-& (novo inicial com & para cada antigo)');
    WriteLn('2) Converter AFN-& para AFN (remover &)');
    WriteLn('3) Mostrar autômato');
    WriteLn('0) Sair');
    Write('Escolha: '); ReadLn(op);
    case op of
      '1': begin ConvertMultipleInitialsToAFNEps(states, initials, transicoes); PrintAutomaton(alphabet, states, initials, finals, transicoes); end;
      '2': begin RemoveEpsilon(alphabet, states, initials, finals, transicoes); PrintAutomaton(alphabet, states, initials, finals, transicoes); end;
      '3': PrintAutomaton(alphabet, states, initials, finals, transicoes);
      '0': break;
    else
      WriteLn('Opção inválida.');
    end;
  end;
end.
