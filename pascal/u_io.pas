unit u_io;

{$mode fpc}{$H+}

interface

uses
  u_types, u_automaton;

procedure ShowMenu(var alphabet, states, initials, finals: TStrArray; var transicoes: TTransArray);

implementation

procedure ShowMenu(var alphabet, states, initials, finals: TStrArray; var transicoes: TTransArray);
var
  op: AnsiString;
begin
  while True do
  begin
    WriteLn;
    WriteLn('--- MENU ---');
    WriteLn('1) Converter múltiplos estados iniciais -> AFN-& (novo inicial)');
    WriteLn('2) Remover & (AFN-& -> AFN sem &)');
    WriteLn('3) Converter AFN para AFD (subconjuntos)');
    WriteLn('4) Minimizar AFD (Hopcroft)');
    WriteLn('5) Testar palavras (arquivo/terminal)');
    WriteLn('6) Mostrar autômato');
    WriteLn('0) Sair');
    Write('Escolha: '); ReadLn(op);
    case op of
      '1': begin ConvertMultipleInitialsToAFNEps(states, initials, transicoes); PrintAutomaton(alphabet, states, initials, finals, transicoes); end;
      '2': begin RemoveEpsilon(alphabet, states, initials, finals, transicoes); PrintAutomaton(alphabet, states, initials, finals, transicoes); end;
      '3': begin NFAToDFA(alphabet, states, initials, finals, transicoes); PrintAutomaton(alphabet, states, initials, finals, transicoes); end;
      '4': begin MinimizeDFAHopcroft(alphabet, states, initials, finals, transicoes); PrintAutomaton(alphabet, states, initials, finals, transicoes); end;
      '5': TestarPalavras(alphabet, states, initials, finals, transicoes);
      '6': PrintAutomaton(alphabet, states, initials, finals, transicoes);
      '0': Exit;
    else
      WriteLn('Opção inválida.');
    end;
  end;
end;

end.
