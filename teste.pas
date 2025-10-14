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

// ordenação simples para normalizar chaves de conjuntos
procedure SortStrArray(var A: TStrArray);
var i, j, min: longint; tmp: ansistring;
begin
  for i := 0 to High(A) do
  begin
    min := i;
    for j := i+1 to High(A) do
      if A[j] < A[min] then min := j;
    if min <> i then begin tmp := A[i]; A[i] := A[min]; A[min] := tmp; end;
  end;
end;

function KeyFromSet(const S: TStrArray): ansistring;
var tmp: TStrArray; i: longint; key: ansistring;
begin
  tmp := S;
  SortStrArray(tmp);
  key := '{';
  for i := 0 to High(tmp) do
  begin
    if i > 0 then key := key + ',';
    key := key + tmp[i];
  end;
  key := key + '}';
  KeyFromSet := key;
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

// NFA -> DFA (construção por subconjuntos; considera fecho-ε)
procedure NFAToDFA(var Alphabet, States, Initials, Finals: TStrArray; var Trans: TTransArray);
var
  startClosure, Tset, moveSet, Uset: TStrArray;
  keys, names, queue, seen: TStrArray;
  newStates, newFinals: TStrArray;
  newTrans: TTransArray;
  curKey, Ukey, nameT, nameU, sym: ansistring;
  i, j: longint;

  function FindNameByKey(const Key: ansistring): ansistring;
  var p: longint; nm: ansistring;
  begin
    nm := '';
    for p := 0 to High(keys) do if keys[p] = Key then begin nm := names[p]; break; end;
    FindNameByKey := nm;
  end;

  procedure EnsureMapping(const Key: ansistring; var Name: ansistring);
  var nm: ansistring;
  begin
    nm := FindNameByKey(Key);
    if nm <> '' then begin Name := nm; exit; end;
    nm := 'S' + IntToStr(Length(keys));
    AddStr(keys, Key);
    AddStr(names, nm);
    AddStr(newStates, nm);
    Name := nm;
  end;

  function Dequeue(var Q: TStrArray; var Key: ansistring): boolean;
  var t: longint;
  begin
    if Length(Q) = 0 then begin Dequeue := false; exit; end;
    Key := Q[0];
    for t := 1 to High(Q) do Q[t-1] := Q[t];
    SetLength(Q, Length(Q)-1);
    Dequeue := true;
  end;

  function SeenKey(const Key: ansistring): boolean;
  begin
    SeenKey := ContainsStr(seen, Key);
  end;

  procedure MarkSeen(const Key: ansistring);
  begin
    AddStrUnique(seen, Key);
  end;

  function KeyToStates(const Key: ansistring): TStrArray;
  var s: ansistring; i: longint; part: ansistring; ret: TStrArray;
  begin
    s := Key;
    if (Length(s) >= 2) and (s[1] = '{') and (s[Length(s)] = '}') then s := Copy(s, 2, Length(s)-2);
    SetLength(ret, 0); part := '';
    for i := 1 to Length(s) do
    begin
      if s[i] = ',' then begin AddStrUnique(ret, part); part := ''; end
      else part := part + s[i];
    end;
    if part <> '' then AddStrUnique(ret, part);
    KeyToStates := ret;
  end;

begin
  // fecho-ε dos iniciais
  startClosure := EpsClosure(Trans, Initials);

  SetLength(keys, 0); SetLength(names, 0);
  SetLength(queue, 0); SetLength(seen, 0);
  SetLength(newStates, 0); SetLength(newFinals, 0);
  SetLength(newTrans, 0);

  curKey := KeyFromSet(startClosure);
  EnsureMapping(curKey, nameT);
  AddStr(queue, curKey);

  // BFS em conjuntos
  while Dequeue(queue, curKey) do
  begin
    if SeenKey(curKey) then continue;
    MarkSeen(curKey);
    Tset := KeyToStates(curKey);
    nameT := FindNameByKey(curKey);

    if IntersectsStr(Tset, Finals) then AddStrUnique(newFinals, nameT);

    for j := 0 to High(Alphabet) do
    begin
      sym := Alphabet[j];
      if sym = EPS then continue;
      SetLength(moveSet, 0);
      for i := 0 to High(Tset) do
        moveSet := UnionStr(moveSet, GetTargets(Trans, Tset[i], sym));
      Uset := EpsClosure(Trans, moveSet);
      Ukey := KeyFromSet(Uset);
      EnsureMapping(Ukey, nameU);
      AddTransUnique(newTrans, nameT, nameU, sym);
      if (Length(Uset) > 0) and (not SeenKey(Ukey)) then AddStr(queue, Ukey);
    end;
  end;

  // aplicar novo automato
  States := newStates;
  SetLength(Initials, 1); Initials[0] := FindNameByKey(KeyFromSet(startClosure));
  Finals := newFinals;
  Trans := newTrans;
  WriteLn('Construído AFD por subconjuntos.');
end;

// ----------------- Minimização de AFD (Hopcroft) -----------------
procedure MinimizeDFAHopcroft(var Alphabet, States, Initials, Finals: TStrArray; var Trans: TTransArray);
var
  allStates, finalsCopy, alphabetCopy: TStrArray;
  i, j, k: longint;
  needDead: boolean;
  deadName: ansistring;
  sym, dst: ansistring;
  // Partições
  P, W, newP: array of TStrArray;
  Cblock, X, inter, diff: TStrArray;
  blockF, blockNF: TStrArray;
  blockMapFrom, blockMapTo: TStrArray;
  newStates, newInitials, newFinals: TStrArray;
  newTrans: TTransArray;

  function IsDFA: boolean;
  var a, b: longint;
  begin
    for a := 0 to High(Trans) do
      for b := a+1 to High(Trans) do
        if (Trans[a].src = Trans[b].src) and (Trans[a].sym = Trans[b].sym) then
        begin IsDFA := false; exit; end;
    IsDFA := true;
  end;

  function UniqueName(const Base: ansistring): ansistring;
  var idx: longint; cand: ansistring;
  begin
    idx := 0; cand := Base;
    while ContainsStr(States, cand) do begin cand := Base + IntToStr(idx); Inc(idx); end;
    UniqueName := cand;
  end;

  function DetDest(const S0, Sym0: ansistring): ansistring;
  var tarr: TStrArray;
  begin
    tarr := GetTargets(Trans, S0, Sym0);
    if Length(tarr) > 0 then DetDest := tarr[0] else DetDest := '';
  end;

  procedure IntersectStr(const A, B: TStrArray; var OutArr: TStrArray);
  var u: longint;
  begin
    SetLength(OutArr, 0);
    for u := 0 to High(A) do if ContainsStr(B, A[u]) then AddStr(OutArr, A[u]);
  end;

  procedure DiffStr(const A, B: TStrArray; var OutArr: TStrArray);
  var u: longint;
  begin
    SetLength(OutArr, 0);
    for u := 0 to High(A) do if not ContainsStr(B, A[u]) then AddStr(OutArr, A[u]);
  end;

  function BlocksEqualSet(const X0, Y0: TStrArray): boolean;
  var a1, a2: TStrArray; u: longint;
  begin
    a1 := X0; a2 := Y0; SortStrArray(a1); SortStrArray(a2);
    if Length(a1) <> Length(a2) then begin BlocksEqualSet := false; exit; end;
    for u := 0 to High(a1) do if a1[u] <> a2[u] then begin BlocksEqualSet := false; exit; end;
    BlocksEqualSet := true;
  end;

  function IsBlockInQueue(const Block: TStrArray; var idx: longint): boolean;
  var t: longint;
  begin
    for t := 0 to High(W) do
      if BlocksEqualSet(W[t], Block) then begin idx := t; IsBlockInQueue := true; exit; end;
    idx := -1; IsBlockInQueue := false;
  end;

  procedure EnqueueBlock(const Block: TStrArray);
  var wlen: longint;
  begin
    wlen := Length(W);
    SetLength(W, wlen+1);
    W[wlen] := Block;
  end;

  function DequeueBlock(var Block: TStrArray): boolean;
  var wlen, t: longint;
  begin
    wlen := Length(W);
    if wlen = 0 then begin DequeueBlock := false; exit; end;
    Block := W[0];
    for t := 1 to wlen-1 do W[t-1] := W[t];
    SetLength(W, wlen-1);
    DequeueBlock := true;
  end;

  function MapState(const S0: ansistring): ansistring;
  var q: longint;
  begin
    for q := 0 to High(blockMapFrom) do if blockMapFrom[q] = S0 then begin MapState := blockMapTo[q]; exit; end;
    MapState := S0;
  end;

var idxW, tlen, t: longint; name: ansistring;
begin
  if not IsDFA then begin WriteLn('Aviso: autômato não determinístico. Converta para AFD antes de minimizar.'); exit; end;

  // Copiar bases
  allStates := States; alphabetCopy := Alphabet;

  // Completar AFD com DEAD se necessário
  needDead := false;
  for i := 0 to High(allStates) do
    for j := 0 to High(alphabetCopy) do
      if alphabetCopy[j] <> EPS then
        if Length(GetTargets(Trans, allStates[i], alphabetCopy[j])) = 0 then needDead := true;

  if needDead then
  begin
    deadName := UniqueName('DEAD');
    AddStrUnique(States, deadName);
    // loops do DEAD
    for j := 0 to High(alphabetCopy) do if alphabetCopy[j] <> EPS then AddTransUnique(Trans, deadName, deadName, alphabetCopy[j]);
    // completar faltantes
    for i := 0 to High(allStates) do
      for j := 0 to High(alphabetCopy) do if alphabetCopy[j] <> EPS then
        if Length(GetTargets(Trans, allStates[i], alphabetCopy[j])) = 0 then
          AddTransUnique(Trans, allStates[i], deadName, alphabetCopy[j]);
    // atualizar lista de estados
    allStates := States;
  end;

  // Partições iniciais
  SetLength(blockF, 0); SetLength(blockNF, 0);
  for i := 0 to High(allStates) do
    if ContainsStr(Finals, allStates[i]) then AddStr(blockF, allStates[i])
    else AddStr(blockNF, allStates[i]);

  SetLength(P, 0);
  if Length(blockF) > 0 then begin SetLength(P, Length(P)+1); P[High(P)] := blockF; end;
  if Length(blockNF) > 0 then begin SetLength(P, Length(P)+1); P[High(P)] := blockNF; end;

  // W := P
  SetLength(W, Length(P));
  for i := 0 to High(P) do W[i] := P[i];

  // Refinamento
  while DequeueBlock(Cblock) do
  begin
    for j := 0 to High(alphabetCopy) do
    begin
      sym := alphabetCopy[j]; if sym = EPS then continue;
      // X = { q | delta(q, sym) em Cblock }
      SetLength(X, 0);
      for i := 0 to High(allStates) do
      begin
        dst := DetDest(allStates[i], sym);
        if ContainsStr(Cblock, dst) then AddStr(X, allStates[i]);
      end;

      // newP = P refinado por X
      SetLength(newP, 0);
      for i := 0 to High(P) do
      begin
        IntersectStr(P[i], X, inter);
        DiffStr(P[i], X, diff);
        if (Length(inter) > 0) and (Length(diff) > 0) then
        begin
          // substitui Y por inter e diff
          SetLength(newP, Length(newP)+1); newP[High(newP)] := inter;
          SetLength(newP, Length(newP)+1); newP[High(newP)] := diff;
          // Atualiza W
          if IsBlockInQueue(P[i], idxW) then
          begin
            // remove P[i] de W, adiciona ambos
            tlen := Length(W);
            for t := idxW+1 to tlen-1 do W[t-1] := W[t];
            SetLength(W, tlen-1);
            EnqueueBlock(inter); EnqueueBlock(diff);
          end
          else
          begin
            if Length(inter) <= Length(diff) then EnqueueBlock(inter) else EnqueueBlock(diff);
          end;
        end
        else
        begin
          SetLength(newP, Length(newP)+1); newP[High(newP)] := P[i];
        end;
      end;
      P := newP;
    end;
  end;

  // Construir mapeamento estado -> bloco
  SetLength(blockMapFrom, 0); SetLength(blockMapTo, 0);
  for i := 0 to High(P) do
  begin
    name := 'M' + IntToStr(i);
    for k := 0 to High(P[i]) do
    begin
      AddStr(blockMapFrom, P[i][k]);
      AddStr(blockMapTo, name);
    end;
  end;

  // Novos estados
  SetLength(newStates, 0);
  for i := 0 to High(P) do
  begin
    name := 'M' + IntToStr(i);
    AddStrUnique(newStates, name);
  end;

  // Novo(s) inicial(is)
  SetLength(newInitials, 0);
  for i := 0 to High(Initials) do AddStrUnique(newInitials, MapState(Initials[i]));

  // Novos finais
  SetLength(newFinals, 0);
  for i := 0 to High(Finals) do AddStrUnique(newFinals, MapState(Finals[i]));

  // Novas transições
  SetLength(newTrans, 0);
  for i := 0 to High(allStates) do
    for j := 0 to High(alphabetCopy) do if alphabetCopy[j] <> EPS then
    begin
      sym := alphabetCopy[j];
      dst := DetDest(allStates[i], sym);
      AddTransUnique(newTrans, MapState(allStates[i]), MapState(dst), sym);
    end;

  // Atribuir
  States := newStates;
  Initials := newInitials;
  Finals := newFinals;
  Trans := newTrans;
  WriteLn('AFD minimizado (Hopcroft).');
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
    WriteLn('4) Converter AFN para AFD (subconjuntos)');
    WriteLn('5) Minimizar AFD (Hopcroft)');
    WriteLn('0) Sair');
    Write('Escolha: '); ReadLn(op);
    case op of
      '1': begin ConvertMultipleInitialsToAFNEps(states, initials, transicoes); PrintAutomaton(alphabet, states, initials, finals, transicoes); end;
      '2': begin RemoveEpsilon(alphabet, states, initials, finals, transicoes); PrintAutomaton(alphabet, states, initials, finals, transicoes); end;
      '3': PrintAutomaton(alphabet, states, initials, finals, transicoes);
      '4': begin NFAToDFA(alphabet, states, initials, finals, transicoes); PrintAutomaton(alphabet, states, initials, finals, transicoes); end;
      '5': begin MinimizeDFAHopcroft(alphabet, states, initials, finals, transicoes); PrintAutomaton(alphabet, states, initials, finals, transicoes); end;
      '0': break;
    else
      WriteLn('Opção inválida.');
    end;
  end;
end.
