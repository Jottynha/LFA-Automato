unit u_io;

{$mode fpc}{$H+}

interface

uses
  u_types, u_automaton;

procedure ShowMenu(var alphabet, states, initials, finals: TStrArray; var transicoes: TTransArray);

implementation

procedure ShowMenu(var alphabet, states, initials, finals: TStrArray; var transicoes: TTransArray);
var
  op, autoType: AnsiString;
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
    WriteLn('7) Mostrar tipo do autômato');
    WriteLn('0) Sair');
    Write('Escolha: '); ReadLn(op);
    
    autoType := GetAutomatonType(transicoes);
    
    case op of
      '1': begin 
        ConvertMultipleInitialsToAFNEps(states, initials, transicoes); 
        PrintAutomaton(alphabet, states, initials, finals, transicoes); 
      end;
      '2': begin 
        if not HasEpsilonTransitions(transicoes) then
        begin
          WriteLn('ERRO: Esta operação requer um AFN-& (autômato com transições épsilon).');
          WriteLn('Tipo atual: ', autoType);
        end
        else
        begin
          RemoveEpsilon(alphabet, states, initials, finals, transicoes); 
          PrintAutomaton(alphabet, states, initials, finals, transicoes);
        end;
      end;
      '3': begin 
        if IsDeterministic(transicoes) and not HasEpsilonTransitions(transicoes) then
        begin
          WriteLn('AVISO: O autômato já é determinístico (AFD).');
          WriteLn('Tipo atual: ', autoType);
        end
        else
        begin
          if HasEpsilonTransitions(transicoes) then
            WriteLn('INFO: Conversão AFN-E -> AFD (tratará transições épsilon automaticamente)');
          NFAToDFA(alphabet, states, initials, finals, transicoes); 
          PrintAutomaton(alphabet, states, initials, finals, transicoes);
        end;
      end;
      '4': begin 
        if HasEpsilonTransitions(transicoes) then
        begin
          WriteLn('ERRO: Esta operação requer um AFD (autômato determinístico sem épsilon).');
          WriteLn('Tipo atual: ', autoType);
          WriteLn('Sugestão: Use opção 2 (remover &) e depois opção 3 (AFN->AFD).');
        end
        else if not IsDeterministic(transicoes) then
        begin
          WriteLn('ERRO: Esta operação requer um AFD (autômato determinístico).');
          WriteLn('Tipo atual: ', autoType);
          WriteLn('Sugestão: Use opção 3 (AFN->AFD) primeiro.');
        end
        else
        begin
          MinimizeDFAHopcroft(alphabet, states, initials, finals, transicoes); 
          PrintAutomaton(alphabet, states, initials, finals, transicoes);
        end;
      end;
      '5': TestarPalavras(alphabet, states, initials, finals, transicoes);
      '6': PrintAutomaton(alphabet, states, initials, finals, transicoes);
      '7': begin
        WriteLn('========================================');
        WriteLn('TIPO DO AUTÔMATO: ', autoType);
        WriteLn('========================================');
        if autoType = 'AFN-E' then
        begin
          WriteLn('• Possui transições épsilon (&)');
          WriteLn('• Pode ter não-determinismo');
          WriteLn('');
          WriteLn('Operações disponíveis:');
          WriteLn('  [2] Remover & → AFN (remove épsilon)');
          WriteLn('  [3] AFN-E → AFD (converte diretamente, trata & automaticamente)');
        end
        else if autoType = 'AFN' then
        begin
          WriteLn('• Não possui transições épsilon');
          WriteLn('• Possui não-determinismo');
          WriteLn('');
          WriteLn('Operações disponíveis:');
          WriteLn('  [3] AFN → AFD (construção de subconjuntos)');
        end
        else
        begin
          WriteLn('• Determinístico');
          WriteLn('• Sem transições épsilon');
          WriteLn('');
          WriteLn('Operações disponíveis:');
          WriteLn('  [4] Minimizar AFD (algoritmo de Hopcroft)');
        end;
        WriteLn('========================================');
      end;
      '0': Exit;
    else
      WriteLn('Opção inválida.');
    end;
  end;
end;

end.
