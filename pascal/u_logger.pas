unit u_logger;

{$mode fpc}{$H+}

interface

uses
  u_types, SysUtils;

var
  LogFile: Text;
  LogEnabled: Boolean;

procedure InitLogger(const FileName: AnsiString);
procedure CloseLogger;
procedure LogBox(const Title, Content: AnsiString);
procedure LogLine(const Msg: AnsiString);
procedure LogSeparator;
procedure LogHeader(const Title: AnsiString);
procedure LogArray(const Name: AnsiString; const Arr: TStrArray);
procedure LogTransitions(const Trans: TTransArray);
procedure LogEmptySet(const Name: AnsiString);

implementation

function RepeatChar(c: Char; count: LongInt): AnsiString;
var
  i: LongInt;
  res: AnsiString;
begin
  res := '';
  for i := 1 to count do
    res := res + c;
  RepeatChar := res;
end;

procedure InitLogger(const FileName: AnsiString);
begin
  LogEnabled := True;
  Assign(LogFile, FileName);
  {$I-}
  Rewrite(LogFile);
  {$I+}
  if IOResult <> 0 then
  begin
    LogEnabled := False;
    WriteLn('AVISO: Não foi possível criar arquivo de log: ', FileName);
  end
  else
  begin
    WriteLn(LogFile, '+================================================================+');
    WriteLn(LogFile, '|  SIMULADOR DE AUTÔMATOS FINITOS - LOG DE EXECUÇÃO             |');
    WriteLn(LogFile, '|  Data: ', FormatDateTime('dd/mm/yyyy hh:nn:ss', Now), '                               |');
    WriteLn(LogFile, '+================================================================+');
    WriteLn(LogFile);
    Flush(LogFile);
  end;
end;

procedure CloseLogger;
begin
  if LogEnabled then
  begin
    WriteLn(LogFile);
    WriteLn(LogFile, '+================================================================+');
    WriteLn(LogFile, '|  FIM DO LOG                                                    |');
    WriteLn(LogFile, '+================================================================+');
    Close(LogFile);
  end;
end;

procedure LogBox(const Title, Content: AnsiString);
var
  lines: array of AnsiString;
  i, count, maxLen, titleLen: LongInt;
  line, paddedTitle: AnsiString;
  start, p: LongInt;
begin
  if not LogEnabled then Exit;
  
  { Dividir conteúdo em linhas }
  SetLength(lines, 0);
  start := 1;
  for p := 1 to Length(Content) do
  begin
    if Content[p] = #10 then
    begin
      SetLength(lines, Length(lines) + 1);
      lines[High(lines)] := Copy(Content, start, p - start);
      start := p + 1;
    end;
  end;
  if start <= Length(Content) then
  begin
    SetLength(lines, Length(lines) + 1);
    lines[High(lines)] := Copy(Content, start, Length(Content) - start + 1);
  end;
  
  { Calcular largura máxima }
  maxLen := 50;
  titleLen := Length(Title);
  if titleLen + 4 > maxLen then maxLen := titleLen + 4;
  for i := 0 to High(lines) do
    if Length(lines[i]) + 4 > maxLen then maxLen := Length(lines[i]) + 4;
  
  { Cabeçalho }
  WriteLn(LogFile, '+', RepeatChar('-', maxLen - 2), '+');
  
  { Título centralizado }
  paddedTitle := Title;
  while Length(paddedTitle) < maxLen - 4 do
    paddedTitle := ' ' + paddedTitle + ' ';
  if Length(paddedTitle) < maxLen - 4 then
    paddedTitle := paddedTitle + ' ';
  WriteLn(LogFile, '| ', paddedTitle, ' |');
  
  { Separador }
  WriteLn(LogFile, '+', RepeatChar('-', maxLen - 2), '+');
  
  { Conteúdo }
  for i := 0 to High(lines) do
  begin
    line := lines[i];
    while Length(line) < maxLen - 4 do
      line := line + ' ';
    WriteLn(LogFile, '| ', line, ' |');
  end;
  
  { Rodapé }
  WriteLn(LogFile, '+', RepeatChar('-', maxLen - 2), '+');
  WriteLn(LogFile);
  Flush(LogFile);
end;

procedure LogLine(const Msg: AnsiString);
begin
  if not LogEnabled then Exit;
  WriteLn(LogFile, Msg);
  Flush(LogFile);
end;

procedure LogSeparator;
begin
  if not LogEnabled then Exit;
  WriteLn(LogFile, '===============================================================');
  WriteLn(LogFile);
  Flush(LogFile);
end;

procedure LogHeader(const Title: AnsiString);
var
  paddedTitle: AnsiString;
begin
  if not LogEnabled then Exit;
  WriteLn(LogFile);
  WriteLn(LogFile, '+===============================================================+');
  
  paddedTitle := '|  ' + Title;
  while Length(paddedTitle) < 64 do
    paddedTitle := paddedTitle + ' ';
  paddedTitle := paddedTitle + '|';
  WriteLn(LogFile, paddedTitle);
  
  WriteLn(LogFile, '+===============================================================+');
  WriteLn(LogFile);
  Flush(LogFile);
end;

procedure LogArray(const Name: AnsiString; const Arr: TStrArray);
var
  i: LongInt;
  content: AnsiString;
begin
  if not LogEnabled then Exit;
  
  content := Name + ' = {';
  if Length(Arr) > 0 then
  begin
    for i := 0 to High(Arr) do
    begin
      if i > 0 then content := content + ', ';
      content := content + Arr[i];
    end;
  end;
  content := content + '}';
  
  LogLine(content);
end;

procedure LogTransitions(const Trans: TTransArray);
var
  i: LongInt;
begin
  if not LogEnabled then Exit;
  
  if Length(Trans) = 0 then
  begin
    LogLine('Transições: {} (nenhuma)');
  end
  else
  begin
    LogLine('Transições:');
    for i := 0 to High(Trans) do
      LogLine('  ' + Trans[i].src + ' --' + Trans[i].sym + '--> ' + Trans[i].dst);
  end;
end;

procedure LogEmptySet(const Name: AnsiString);
begin
  if not LogEnabled then Exit;
  LogLine(Name + ' = {} (conjunto vazio)');
end;

end.
