unit u_types;

{$mode fpc}{$H+}

interface

type
  TStrArray = array of AnsiString;

  TTransition = record
    src: AnsiString;
    dst: AnsiString;
    sym: AnsiString;
  end;

  TTransArray = array of TTransition;

{ Cria um TStrArray contendo um único elemento S }
function MakeArray1(const S: AnsiString): TStrArray;

{ Retorna o índice de S em A, ou -1 se não existir }
function IndexOfStr(const A: TStrArray; const S: AnsiString): LongInt;

implementation

function MakeArray1(const S: AnsiString): TStrArray;
var
  res: TStrArray;
begin
  SetLength(res, 1);
  res[0] := S;
  MakeArray1 := res;
end;

function IndexOfStr(const A: TStrArray; const S: AnsiString): LongInt;
var
  i: LongInt;
begin
  IndexOfStr := -1;
  if Length(A) = 0 then Exit;
  for i := 0 to High(A) do
    if A[i] = S then
    begin
      IndexOfStr := i;
      Exit;
    end;
end;

end.
