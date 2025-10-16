program teste;

{$mode fpc}{$H+}
{
 Compilação:
   fpc main.pas 
 Execução:
   ./main [caminho/para/automato.json] [exemplo: /home/joao/LFA-Automato/data/automato.json]
  
}
uses
  u_types, u_utils, u_automaton, u_io;

var
  path, json: AnsiString;
  pKey, iStart, iEnd: LongInt;
  ok: Boolean;
  section: AnsiString;
  alphabet, states, initials, finals: TStrArray;
  transicoes: TTransArray;

begin
  if ParamCount >= 1 then path := ParamStr(1) else path := 'data/automato.json';
  json := ReadAllText(path);
  if json = '' then
  begin
    WriteLn('Erro ao ler arquivo: ', path);
    Halt(1);
  end;

  { alfabeto }
  pKey := FindKeyPos(json, 'alfabeto');
  ExtractBracketIndices(json, pKey, iStart, iEnd, ok);
  if ok then
  begin
    section := Copy(json, iStart+1, iEnd - iStart - 1);
    ExtractStringsFromArray(section, alphabet);
  end;

  { estados }
  pKey := FindKeyPos(json, 'estados');
  ExtractBracketIndices(json, pKey, iStart, iEnd, ok);
  if ok then
  begin
    section := Copy(json, iStart+1, iEnd - iStart - 1);
    ExtractStringsFromArray(section, states);
  end;

  { iniciais (estadosI) }
  pKey := FindKeyPos(json, 'estadosI');
  ExtractBracketIndices(json, pKey, iStart, iEnd, ok);
  if ok then
  begin
    section := Copy(json, iStart+1, iEnd - iStart - 1);
    ExtractStringsFromArray(section, initials);
  end;

  { finais (estadoF) }
  pKey := FindKeyPos(json, 'estadoF');
  ExtractBracketIndices(json, pKey, iStart, iEnd, ok);
  if ok then
  begin
    section := Copy(json, iStart+1, iEnd - iStart - 1);
    ExtractStringsFromArray(section, finals);
  end;

  { transicoes }
  pKey := FindKeyPos(json, 'transicoes');
  ExtractBracketIndices(json, pKey, iStart, iEnd, ok);
  if ok then
  begin
    section := Copy(json, iStart+1, iEnd - iStart - 1);
    ExtractTransitions(section, transicoes);
  end;

  { mostrar menu }
  ShowMenu(alphabet, states, initials, finals, transicoes);
end.
