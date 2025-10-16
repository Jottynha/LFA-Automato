unit u_automaton;

{$mode fpc}{$H+}

interface

uses
  u_types, u_utils;

const
  EPS: AnsiString = '&'; { símbolo interno para épsilon }

{ Operações básicas }
function GetTargets(const Trans: TTransArray; const Src, Sym: AnsiString): TStrArray;
function UnionStr(const A, B: TStrArray): TStrArray;
function IntersectsStr(const A, B: TStrArray): Boolean;
procedure SortStrArray(var A: TStrArray);
function KeyFromSet(const S: TStrArray): AnsiString;
function EpsClosure(const Trans: TTransArray; const StartStates: TStrArray): TStrArray;

{ Aceitação (AFN com & ou AFD) }
function Accepts(const Alphabet: TStrArray; const Trans: TTransArray; const Initials, Finals: TStrArray; const Word: AnsiString): Boolean;

{ Transformações }
procedure ConvertMultipleInitialsToAFNEps(var States, Initials: TStrArray; var Trans: TTransArray);
procedure RemoveEpsilon(var Alphabet, States, Initials, Finals: TStrArray; var Trans: TTransArray);
procedure NFAToDFA(var Alphabet, States, Initials, Finals: TStrArray; var Trans: TTransArray);
procedure MinimizeDFAHopcroft(var Alphabet, States, Initials, Finals: TStrArray; var Trans: TTransArray);

{ I/O e utilitários }
procedure TestarPalavras(var Alphabet, States, Initials, Finals: TStrArray; var Trans: TTransArray);
procedure PrintAutomaton(const Alphabet, States, Initials, Finals: TStrArray; const Trans: TTransArray);

implementation

{ === AUXILIARES === }

function GetTargets(const Trans: TTransArray; const Src, Sym: AnsiString): TStrArray;
var
  i: LongInt;
  resArr: TStrArray;
begin
  SetLength(resArr, 0);
  if Length(Trans) = 0 then
  begin
    GetTargets := resArr;
    Exit;
  end;
  for i := 0 to High(Trans) do
    if (Trans[i].src = Src) and (Trans[i].sym = Sym) then
    begin
      SetLength(resArr, Length(resArr) + 1);
      resArr[High(resArr)] := Trans[i].dst;
    end;
  GetTargets := resArr;
end;

function UnionStr(const A, B: TStrArray): TStrArray;
var
  i: LongInt;
  resArr: TStrArray;
begin
  resArr := A;
  if Length(B) = 0 then
  begin
    UnionStr := resArr;
    Exit;
  end;
  for i := 0 to High(B) do
  begin
    if IndexOfStr(resArr, B[i]) = -1 then
    begin
      SetLength(resArr, Length(resArr) + 1);
      resArr[High(resArr)] := B[i];
    end;
  end;
  UnionStr := resArr;
end;

function IntersectsStr(const A, B: TStrArray): Boolean;
var
  i: LongInt;
  ans: Boolean;
begin
  if (Length(A) = 0) or (Length(B) = 0) then
  begin
    IntersectsStr := False;
    Exit;
  end;
  ans := False;
  for i := 0 to High(A) do
    if IndexOfStr(B, A[i]) <> -1 then
    begin
      ans := True;
      Break;
    end;
  IntersectsStr := ans;
end;

procedure SortStrArray(var A: TStrArray);
var
  i, j, min: LongInt;
  tmp: AnsiString;
begin
  if Length(A) <= 1 then Exit;
  for i := 0 to High(A) - 1 do
  begin
    min := i;
    for j := i + 1 to High(A) do
      if A[j] < A[min] then min := j;
    if min <> i then
    begin
      tmp := A[i];
      A[i] := A[min];
      A[min] := tmp;
    end;
  end;
end;

function KeyFromSet(const S: TStrArray): AnsiString;
var
  tmp: TStrArray;
  i: LongInt;
  key: AnsiString;
begin
  tmp := S;
  SortStrArray(tmp);
  key := '{';
  if Length(tmp) > 0 then
    for i := 0 to High(tmp) do
    begin
      if i > 0 then key := key + ',';
      key := key + tmp[i];
    end;
  key := key + '}';
  KeyFromSet := key;
end;

function EpsClosure(const Trans: TTransArray; const StartStates: TStrArray): TStrArray;
var
  stack, resArr: TStrArray;
  i: LongInt;
  s: AnsiString;
  tgts: TStrArray;
begin
  resArr := StartStates;
  stack := StartStates;
  while Length(stack) > 0 do
  begin
    s := stack[High(stack)];
    SetLength(stack, Length(stack) - 1);
    tgts := GetTargets(Trans, s, EPS);
    if Length(tgts) = 0 then Continue;
    for i := 0 to High(tgts) do
      if IndexOfStr(resArr, tgts[i]) = -1 then
      begin
        SetLength(resArr, Length(resArr) + 1);
        resArr[High(resArr)] := tgts[i];
        SetLength(stack, Length(stack) + 1);
        stack[High(stack)] := tgts[i];
      end;
  end;
  EpsClosure := resArr;
end;

{ === ACEITAÇÃO === }

function Accepts(const Alphabet: TStrArray; const Trans: TTransArray; const Initials, Finals: TStrArray; const Word: AnsiString): Boolean;
var
  current, nextSet, tg: TStrArray;
  i, j: LongInt;
  ch: AnsiString;
  ans: Boolean;
begin
  current := EpsClosure(Trans, Initials);
  if Length(Word) = 0 then
  begin
    ans := IntersectsStr(current, Finals);
    Accepts := ans;
    Exit;
  end;
  for i := 1 to Length(Word) do
  begin
    ch := Word[i];
    if IndexOfStr(Alphabet, ch) = -1 then
    begin
      Accepts := False;
      Exit;
    end;
    SetLength(nextSet, 0);
    if Length(current) > 0 then
      for j := 0 to High(current) do
      begin
        tg := GetTargets(Trans, current[j], ch);
        nextSet := UnionStr(nextSet, tg);
      end;
    current := EpsClosure(Trans, nextSet);
  end;
  ans := IntersectsStr(current, Finals);
  Accepts := ans;
end;

{ === TRANSFORMAÇÕES === }

procedure ConvertMultipleInitialsToAFNEps(var States, Initials: TStrArray; var Trans: TTransArray);
var
  i: LongInt;
  base, novo: AnsiString;
  idx: LongInt;
begin
  if Length(Initials) <= 1 then Exit;
  base := 'Qi';
  idx := 0;
  novo := base + IntToStrPure(idx);
  while IndexOfStr(States, novo) <> -1 do
  begin
    Inc(idx);
    novo := base + IntToStrPure(idx);
  end;
  if IndexOfStr(States, novo) = -1 then
  begin
    SetLength(States, Length(States) + 1);
    States[High(States)] := novo;
  end;
  for i := 0 to High(Initials) do
  begin
    SetLength(Trans, Length(Trans) + 1);
    Trans[High(Trans)].src := novo;
    Trans[High(Trans)].dst := Initials[i];
    Trans[High(Trans)].sym := EPS;
  end;
  SetLength(Initials, 1);
  Initials[0] := novo;
  WriteLn('Novo inicial criado: ', novo);
end;

procedure RemoveEpsilon(var Alphabet, States, Initials, Finals: TStrArray; var Trans: TTransArray);
var
  i, j, k, t: LongInt;
  p, q, r, sym: AnsiString;
  Ep, temp, U: TStrArray;
  newTrans: TTransArray;
  newFinals: TStrArray;
  tg: TStrArray;
  dup: Boolean;
begin
  SetLength(newTrans, 0);
  SetLength(newFinals, 0);

  { recalcular finais via fecho-ε }
  if Length(States) > 0 then
  begin
    for i := 0 to High(States) do
    begin
      p := States[i];
      Ep := EpsClosure(Trans, MakeArray1(p));
      if IntersectsStr(Ep, Finals) then
      begin
        if IndexOfStr(newFinals, p) = -1 then
        begin
          SetLength(newFinals, Length(newFinals) + 1);
          newFinals[High(newFinals)] := p;
        end;
      end;
    end;
  end;

  { construir novas transições (sem &) }
  if Length(States) > 0 then
  begin
    for i := 0 to High(States) do
    begin
      p := States[i];
      Ep := EpsClosure(Trans, MakeArray1(p));
      if Length(Alphabet) = 0 then Continue;
      for j := 0 to High(Alphabet) do
      begin
        sym := Alphabet[j];
        if sym = EPS then Continue;
        SetLength(temp, 0);
        if Length(Ep) > 0 then
          for k := 0 to High(Ep) do
          begin
            q := Ep[k];
            tg := GetTargets(Trans, q, sym);
            temp := UnionStr(temp, tg);
          end;
        U := EpsClosure(Trans, temp);
        if Length(U) > 0 then
        begin
          for k := 0 to High(U) do
          begin
            r := U[k];
            { AddUnique to newTrans }
            if Length(newTrans) = 0 then
            begin
              SetLength(newTrans, 1);
              newTrans[0].src := p;
              newTrans[0].dst := r;
              newTrans[0].sym := sym;
            end
            else
            begin
              dup := False;
              for t := 0 to High(newTrans) do
                if (newTrans[t].src = p) and (newTrans[t].dst = r) and (newTrans[t].sym = sym) then
                begin
                  dup := True;
                  Break;
                end;
              if not dup then
              begin
                SetLength(newTrans, Length(newTrans) + 1);
                newTrans[High(newTrans)].src := p;
                newTrans[High(newTrans)].dst := r;
                newTrans[High(newTrans)].sym := sym;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  Trans := newTrans;
  Finals := newFinals;
  WriteLn('Removidas transições eps (&).');
end;

procedure NFAToDFA(var Alphabet, States, Initials, Finals: TStrArray; var Trans: TTransArray);
var
  startClosure, Tset, moveSet, Uset: TStrArray;
  keys, names, queue, seen: TStrArray;
  newStates, newFinals: TStrArray;
  newTrans: TTransArray;
  curKey, Ukey, nameT, nameU, sym: AnsiString;
  i, j, p, qq, tidx: LongInt;
  dup: Boolean;

  function FindNameByKey(const Key: AnsiString): AnsiString;
  var
    idx: LongInt;
  begin
    FindNameByKey := '';
    if Length(keys) = 0 then Exit;
    for idx := 0 to High(keys) do
      if keys[idx] = Key then
      begin
        FindNameByKey := names[idx];
        Exit;
      end;
  end;

  procedure EnsureMapping(const Key: AnsiString; var Name: AnsiString);
  var
    nm: AnsiString;
  begin
    nm := FindNameByKey(Key);
    if nm <> '' then
    begin
      Name := nm;
      Exit;
    end;
    nm := 'S' + IntToStrPure(Length(keys));
    SetLength(keys, Length(keys) + 1);
    keys[High(keys)] := Key;
    SetLength(names, Length(names) + 1);
    names[High(names)] := nm;
    SetLength(newStates, Length(newStates) + 1);
    newStates[High(newStates)] := nm;
    Name := nm;
  end;

  function Dequeue(var Q: TStrArray; var Key: AnsiString): Boolean;
  var
    qqi: LongInt;
  begin
    if Length(Q) = 0 then
    begin
      Dequeue := False;
      Exit;
    end;
    Key := Q[0];
    for qqi := 1 to High(Q) do Q[qqi - 1] := Q[qqi];
    SetLength(Q, Length(Q) - 1);
    Dequeue := True;
  end;

  function SeenKey(const Key: AnsiString): Boolean;
  begin
    SeenKey := IndexOfStr(seen, Key) <> -1;
  end;

  procedure MarkSeen(const Key: AnsiString);
  begin
    if IndexOfStr(seen, Key) = -1 then
    begin
      SetLength(seen, Length(seen) + 1);
      seen[High(seen)] := Key;
    end;
  end;

  function KeyToStates(const Key: AnsiString): TStrArray; { Recebe uma chave e retorna o conjunto de estados correspondente }
  var
    s: AnsiString;
    idxc: LongInt;
    part: AnsiString;
    ret: TStrArray;
  begin
    s := Key;
    if (Length(s) >= 2) and (s[1] = '{') and (s[Length(s)] = '}') then
      s := Copy(s, 2, Length(s) - 2);
    SetLength(ret, 0);
    part := '';
    for idxc := 1 to Length(s) do
    begin
      if s[idxc] = ',' then
      begin
        if part <> '' then
        begin
          SetLength(ret, Length(ret) + 1);
          ret[High(ret)] := part;
        end;
        part := '';
      end
      else
        part := part + s[idxc];
    end;
    if part <> '' then
    begin
      SetLength(ret, Length(ret) + 1);
      ret[High(ret)] := part;
    end;
    KeyToStates := ret;
  end;

begin
  startClosure := EpsClosure(Trans, Initials);

  SetLength(keys, 0);
  SetLength(names, 0);
  SetLength(queue, 0);
  SetLength(seen, 0);
  SetLength(newStates, 0);
  SetLength(newFinals, 0);
  SetLength(newTrans, 0);

  curKey := KeyFromSet(startClosure);
  EnsureMapping(curKey, nameT);
  SetLength(queue, Length(queue) + 1);
  queue[High(queue)] := curKey;

  while Dequeue(queue, curKey) do
  begin
    if SeenKey(curKey) then Continue;
    MarkSeen(curKey);
    Tset := KeyToStates(curKey);
    nameT := FindNameByKey(curKey);

    if IntersectsStr(Tset, Finals) then
    begin
      if IndexOfStr(newFinals, nameT) = -1 then
      begin
        SetLength(newFinals, Length(newFinals) + 1);
        newFinals[High(newFinals)] := nameT;
      end;
    end;

    if Length(Alphabet) > 0 then
    begin
      for j := 0 to High(Alphabet) do
      begin
        sym := Alphabet[j];
        if sym = EPS then Continue;
        SetLength(moveSet, 0);
        if Length(Tset) > 0 then
          for i := 0 to High(Tset) do
            moveSet := UnionStr(moveSet, GetTargets(Trans, Tset[i], sym));
        Uset := EpsClosure(Trans, moveSet);
        Ukey := KeyFromSet(Uset);
        EnsureMapping(Ukey, nameU);

        dup := False;
        if Length(newTrans) > 0 then
        begin
          for p := 0 to High(newTrans) do
            if (newTrans[p].src = nameT) and (newTrans[p].dst = nameU) and (newTrans[p].sym = sym) then
            begin
              dup := True;
              Break;
            end;
        end;
        if not dup then
        begin
          SetLength(newTrans, Length(newTrans) + 1);
          newTrans[High(newTrans)].src := nameT;
          newTrans[High(newTrans)].dst := nameU;
          newTrans[High(newTrans)].sym := sym;
        end;

        if (Length(Uset) > 0) and (not SeenKey(Ukey)) then
        begin
          SetLength(queue, Length(queue) + 1);
          queue[High(queue)] := Ukey;
        end;
      end;
    end;
  end;

  States := newStates;
  SetLength(Initials, 1);
  Initials[0] := FindNameByKey(KeyFromSet(startClosure));
  Finals := newFinals;
  Trans := newTrans;
  WriteLn('Construído AFD por subconjuntos.');
end;

procedure MinimizeDFAHopcroft(var Alphabet, States, Initials, Finals: TStrArray; var Trans: TTransArray);
var
  allStates, alphabetCopy: TStrArray;
  i, j, k, t, t2, idxW, tlen: LongInt;
  needDead: Boolean;
  deadName: AnsiString;
  sym, dst: AnsiString;
  P, W, newP: array of TStrArray;
  Cblock, X, inter, diff: TStrArray;
  blockF, blockNF: TStrArray;
  blockMapFrom, blockMapTo: TStrArray;
  newStates, newInitials, newFinals: TStrArray;
  newTrans: TTransArray;
  name: AnsiString;
  dup: Boolean;

  function IsDFA: Boolean;
  var
    a, b: LongInt;
  begin
    if Length(Trans) = 0 then
    begin
      IsDFA := True;
      Exit;
    end;
    for a := 0 to High(Trans) do
      for b := a + 1 to High(Trans) do
        if (Trans[a].src = Trans[b].src) and (Trans[a].sym = Trans[b].sym) then
        begin
          IsDFA := False;
          Exit;
        end;
    IsDFA := True;
  end;

  function DetDest(const S0, Sym0: AnsiString): AnsiString;
  var
    tarr: TStrArray;
  begin
    tarr := GetTargets(Trans, S0, Sym0);
    if Length(tarr) > 0 then DetDest := tarr[0] else DetDest := '';
  end;

  procedure IntersectStrProc(const A, B: TStrArray; var OutArr: TStrArray);
  var
    u: LongInt;
  begin
    SetLength(OutArr, 0);
    if (Length(A) = 0) or (Length(B) = 0) then Exit;
    for u := 0 to High(A) do
      if IndexOfStr(B, A[u]) <> -1 then
      begin
        SetLength(OutArr, Length(OutArr) + 1);
        OutArr[High(OutArr)] := A[u];
      end;
  end;

  procedure DiffStrProc(const A, B: TStrArray; var OutArr: TStrArray);
  var
    u: LongInt;
  begin
    SetLength(OutArr, 0);
    if Length(A) = 0 then Exit;
    for u := 0 to High(A) do
      if IndexOfStr(B, A[u]) = -1 then
      begin
        SetLength(OutArr, Length(OutArr) + 1);
        OutArr[High(OutArr)] := A[u];
      end;
  end;

  function BlocksEqualSet(const X0, Y0: TStrArray): Boolean;
  var
    a1, a2: TStrArray;
    u: LongInt;
  begin
    a1 := X0;
    a2 := Y0;
    SortStrArray(a1);
    SortStrArray(a2);
    if Length(a1) <> Length(a2) then
    begin
      BlocksEqualSet := False;
      Exit;
    end;
    for u := 0 to High(a1) do
      if a1[u] <> a2[u] then
      begin
        BlocksEqualSet := False;
        Exit;
      end;
    BlocksEqualSet := True;
  end;

  function IsBlockInQueue(const Block: TStrArray; var idx: LongInt): Boolean;
  var
    tt: LongInt;
  begin
    if Length(W) = 0 then
    begin
      IsBlockInQueue := False;
      idx := -1;
      Exit;
    end;
    for tt := 0 to High(W) do
      if BlocksEqualSet(W[tt], Block) then
      begin
        idx := tt;
        IsBlockInQueue := True;
        Exit;
      end;
    idx := -1;
    IsBlockInQueue := False;
  end;

  procedure EnqueueBlock(const Block: TStrArray);
  var
    wlen: LongInt;
  begin
    wlen := Length(W);
    SetLength(W, wlen + 1);
    W[wlen] := Block;
  end;

  function DequeueBlock(var Block: TStrArray): Boolean;
  var
    wlen, tt: LongInt;
  begin
    wlen := Length(W);
    if wlen = 0 then
    begin
      DequeueBlock := False;
      Exit;
    end;
    Block := W[0];
    for tt := 1 to wlen - 1 do W[tt - 1] := W[tt];
    SetLength(W, wlen - 1);
    DequeueBlock := True;
  end;

  function MapState(const S0: AnsiString): AnsiString;
  var
    q: LongInt;
  begin
    if Length(blockMapFrom) = 0 then
    begin
      MapState := S0;
      Exit;
    end;
    for q := 0 to High(blockMapFrom) do
      if blockMapFrom[q] = S0 then
      begin
        MapState := blockMapTo[q];
        Exit;
      end;
    MapState := S0;
  end;

begin
  if Length(Trans) = 0 then
  begin
    WriteLn('Sem transições. Nada a minimizar.');
    Exit;
  end;
  if not IsDFA then
  begin
    WriteLn('Aviso: autômato não determinístico. Converta para AFD antes de minimizar.');
    Exit;
  end;

  allStates := States;
  alphabetCopy := Alphabet;

  { completar com DEAD se necessário }
  needDead := False;
  if (Length(allStates) > 0) and (Length(alphabetCopy) > 0) then
  begin
    for i := 0 to High(allStates) do
      for j := 0 to High(alphabetCopy) do
        if alphabetCopy[j] <> EPS then
          if Length(GetTargets(Trans, allStates[i], alphabetCopy[j])) = 0 then needDead := True;
  end;

  if needDead then
  begin
    deadName := 'DEAD';
    if IndexOfStr(States, deadName) <> -1 then deadName := deadName + '_X';
    SetLength(States, Length(States) + 1);
    States[High(States)] := deadName;
    if Length(alphabetCopy) > 0 then
    begin
      for j := 0 to High(alphabetCopy) do
        if alphabetCopy[j] <> EPS then
        begin
          SetLength(Trans, Length(Trans) + 1);
          Trans[High(Trans)].src := deadName;
          Trans[High(Trans)].dst := deadName;
          Trans[High(Trans)].sym := alphabetCopy[j];
        end;
      for i := 0 to High(allStates) do
        for j := 0 to High(alphabetCopy) do
          if alphabetCopy[j] <> EPS then
            if Length(GetTargets(Trans, allStates[i], alphabetCopy[j])) = 0 then
            begin
              SetLength(Trans, Length(Trans) + 1);
              Trans[High(Trans)].src := allStates[i];
              Trans[High(Trans)].dst := deadName;
              Trans[High(Trans)].sym := alphabetCopy[j];
            end;
    end;
    allStates := States;
  end;

  { particionar em finais / não-finais }
  SetLength(blockF, 0);
  SetLength(blockNF, 0);
  if Length(allStates) > 0 then
  begin
    for i := 0 to High(allStates) do
    begin
      if IndexOfStr(Finals, allStates[i]) <> -1 then
      begin
        SetLength(blockF, Length(blockF) + 1);
        blockF[High(blockF)] := allStates[i];
      end
      else
      begin
        SetLength(blockNF, Length(blockNF) + 1);
        blockNF[High(blockNF)] := allStates[i];
      end;
    end;
  end;

  SetLength(P, 0);
  if Length(blockF) > 0 then
  begin
    SetLength(P, Length(P) + 1);
    P[High(P)] := blockF;
  end;
  if Length(blockNF) > 0 then
  begin
    SetLength(P, Length(P) + 1);
    P[High(P)] := blockNF;
  end;

  SetLength(W, Length(P));
  if Length(P) > 0 then
    for i := 0 to High(P) do W[i] := P[i];

  { refinamento }
  while DequeueBlock(Cblock) do
  begin
    if Length(alphabetCopy) = 0 then Continue;
    for j := 0 to High(alphabetCopy) do
    begin
      sym := alphabetCopy[j];
      if sym = EPS then Continue;
      SetLength(X, 0);
      if Length(allStates) > 0 then
      begin
        for i := 0 to High(allStates) do
        begin
          dst := DetDest(allStates[i], sym);
          if IndexOfStr(Cblock, dst) <> -1 then
          begin
            SetLength(X, Length(X) + 1);
            X[High(X)] := allStates[i];
          end;
        end;
      end;

      SetLength(newP, 0);
      if Length(P) > 0 then
      begin
        for i := 0 to High(P) do
        begin
          IntersectStrProc(P[i], X, inter);
          DiffStrProc(P[i], X, diff);
          if (Length(inter) > 0) and (Length(diff) > 0) then
          begin
            SetLength(newP, Length(newP) + 1);
            newP[High(newP)] := inter;
            SetLength(newP, Length(newP) + 1);
            newP[High(newP)] := diff;

            if IsBlockInQueue(P[i], idxW) then
            begin
              tlen := Length(W);
              if tlen > 0 then
                for t := idxW + 1 to tlen - 1 do W[t - 1] := W[t];
              SetLength(W, tlen - 1);
              EnqueueBlock(inter);
              EnqueueBlock(diff);
            end
            else
            begin
              if Length(inter) <= Length(diff) then EnqueueBlock(inter) else EnqueueBlock(diff);
            end;
          end
          else
          begin
            SetLength(newP, Length(newP) + 1);
            newP[High(newP)] := P[i];
          end;
        end;
      end;
      P := newP;
    end;
  end;

  { construir mapeamento }
  SetLength(blockMapFrom, 0);
  SetLength(blockMapTo, 0);
  if Length(P) > 0 then
  begin
    for i := 0 to High(P) do
    begin
      name := 'M' + IntToStrPure(i);
      if Length(P[i]) > 0 then
        for k := 0 to High(P[i]) do
        begin
          SetLength(blockMapFrom, Length(blockMapFrom) + 1);
          blockMapFrom[High(blockMapFrom)] := P[i][k];
          SetLength(blockMapTo, Length(blockMapTo) + 1);
          blockMapTo[High(blockMapTo)] := name;
        end;
    end;
  end;

  SetLength(newStates, 0);
  if Length(P) > 0 then
  begin
    for i := 0 to High(P) do
    begin
      name := 'M' + IntToStrPure(i);
      SetLength(newStates, Length(newStates) + 1);
      newStates[High(newStates)] := name;
    end;
  end;

  SetLength(newInitials, 0);
  if Length(Initials) > 0 then
  begin
    for i := 0 to High(Initials) do
    begin
      SetLength(newInitials, Length(newInitials) + 1);
      newInitials[High(newInitials)] := MapState(Initials[i]);
    end;
  end;

  SetLength(newFinals, 0);
  if Length(Finals) > 0 then
  begin
    for i := 0 to High(Finals) do
    begin
      SetLength(newFinals, Length(newFinals) + 1);
      newFinals[High(newFinals)] := MapState(Finals[i]);
    end;
  end;

  SetLength(newTrans, 0);
  if Length(allStates) > 0 then
  begin
    for i := 0 to High(allStates) do
      if Length(alphabetCopy) > 0 then
        for j := 0 to High(alphabetCopy) do
          if alphabetCopy[j] <> EPS then
          begin
            sym := alphabetCopy[j];
            dst := DetDest(allStates[i], sym);
            if dst = '' then Continue;
            dup := False;
            if Length(newTrans) > 0 then
            begin
              for t2 := 0 to High(newTrans) do
                if (newTrans[t2].src = MapState(allStates[i])) and (newTrans[t2].dst = MapState(dst)) and (newTrans[t2].sym = sym) then
                begin
                  dup := True;
                  Break;
                end;
            end;
            if not dup then
            begin
              SetLength(newTrans, Length(newTrans) + 1);
              newTrans[High(newTrans)].src := MapState(allStates[i]);
              newTrans[High(newTrans)].dst := MapState(dst);
              newTrans[High(newTrans)].sym := sym;
            end;
          end;
  end;

  States := newStates;
  Initials := newInitials;
  Finals := newFinals;
  Trans := newTrans;
  WriteLn('AFD minimizado (Hopcroft).');
end;

{ === IMPRESSÃO === }

procedure PrintAutomaton(const Alphabet, States, Initials, Finals: TStrArray; const Trans: TTransArray);
var
  i: LongInt;
begin
  WriteLn('--- Automato ---');
  WriteLn('Alfabeto:');
  if Length(Alphabet) > 0 then
    for i := 0 to High(Alphabet) do WriteLn('  ', Alphabet[i]);
  WriteLn('Estados:');
  if Length(States) > 0 then
    for i := 0 to High(States) do WriteLn('  ', States[i]);
  WriteLn('Iniciais:');
  if Length(Initials) > 0 then
    for i := 0 to High(Initials) do WriteLn('  ', Initials[i]);
  WriteLn('Finais:');
  if Length(Finals) > 0 then
    for i := 0 to High(Finals) do WriteLn('  ', Finals[i]);
  WriteLn('Transicoes:');
  if Length(Trans) > 0 then
    for i := 0 to High(Trans) do
      WriteLn('  ', Trans[i].src, ' --', Trans[i].sym, '--> ', Trans[i].dst);
end;

procedure TestarPalavras(var Alphabet, States, Initials, Finals: TStrArray; var Trans: TTransArray);
var
  modo, caminho, linha: AnsiString;
  f: Text;
  ok: Boolean;
begin
  Write('Testar palavras de (f)ile ou (t)erminal? [f/t]: ');
  ReadLn(modo);
  modo := TrimSpaces(modo);
  if (modo <> 'f') and (modo <> 't') then
  begin
    WriteLn('Opção inválida.');
    Exit;
  end;
  if modo = 'f' then
  begin
    Write('Caminho do arquivo: ');
    ReadLn(caminho);
    caminho := TrimSpaces(caminho);
    Assign(f, caminho);
    {$I-}
    Reset(f);
    {$I+}
    if IOResult <> 0 then
    begin
      WriteLn('Não foi possível abrir arquivo.');
      Exit;
    end;
    while not Eof(f) do
    begin
      ReadLn(f, linha);
      ok := Accepts(Alphabet, Trans, Initials, Finals, linha);
      if linha = '' then Write('(vazia)') else Write(linha);
      WriteLn(' -> ', ok);
    end;
    Close(f);
  end
  else
  begin
    WriteLn('Digite palavras ("sair" para terminar). Linha vazia testa palavra vazia.');
    while True do
    begin
      Write('> ');
      ReadLn(linha);
      if TrimSpaces(linha) = 'sair' then Break;
      ok := Accepts(Alphabet, Trans, Initials, Finals, linha);
      if linha = '' then Write('(vazia)') else Write(linha);
      WriteLn(' -> ', ok);
    end;
  end;
end;

end.
