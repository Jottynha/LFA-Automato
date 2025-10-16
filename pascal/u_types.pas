unit u_types; 

{$mode fpc}{$H+} { Diretrizes de Compilação }

interface { Declaração de Tipos e Funções Auxiliares }

type
  TStrArray = array of AnsiString; { Vetor Dinâmico de Strings }

  TTransition = record { Estrutura de Transição }
    src: AnsiString; { Estado de Origem }
    dst: AnsiString; { Estado de Destino }
    sym: AnsiString; { Símbolo de Transição }
end;

TTransArray = array of TTransition; { Vetor Dinâmico de Transições }

{ Cria um TStrArray contendo um único elemento S }
function MakeArray1(const S: AnsiString): TStrArray;

{ Retorna o índice de S em A, ou -1 se não existir }
function IndexOfStr(const A: TStrArray; const S: AnsiString): LongInt;

implementation

  { Cria um TStrArray contendo um único elemento S }
  function MakeArray1(const S: AnsiString): TStrArray;
  var
    res: TStrArray; 
  begin
    SetLength(res, 1); 
    res[0] := S; 
    MakeArray1 := res; 
  end;

  { Retorna o índice de S em A, ou -1 se não existir }
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
