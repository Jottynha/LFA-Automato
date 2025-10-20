## 1. Objetivo
Este documento tem como objetivo introduzir novos colaboradores ao projeto de autômatos implementado em **Pascal**, explicando desde a **sintaxe básica da linguagem** até o funcionamento detalhado de **cada arquivo e função**. Além disso, será feita uma explicação simples e intuitiva sobre **como funcionam os algoritmos de autômatos** (como fecho-ε, conversão NFA→DFA, minimização, etc.).

---
## 2. Introdução à Linguagem Pascal

### 2.1 Estrutura de um programa

Em Pascal, um programa é dividido em duas partes principais: a **declaração** e a **implementação**.

```pascal
program Exemplo;

uses crt;

var
  nome: string;

begin
  write('Digite seu nome: ');
  readln(nome);
  writeln('Olá, ', nome, '!');
end.
```

- `program` indica o início do programa.
- `uses` importa bibliotecas.
- `var` declara variáveis.
- `begin` e `end.` delimitam o corpo do programa.
- `write` e `writeln` exibem texto.
- `readln` lê entrada do usuário.

---
### 2.2 Estrutura de um _unit_
Um _unit_ é um módulo reutilizável, semelhante a uma biblioteca. O projeto possui vários.

```pascal
unit u_exemplo;

interface
  procedure MostrarMensagem;

implementation
  procedure MostrarMensagem;
  begin
    writeln('Olá do unit!');
  end;
end.
```

- A parte `interface` declara o que será visível para outros arquivos.
- A parte `implementation` contém o código real.

---

### 2.3 Tipos básicos usados no projeto

| Tipo                  | Descrição                                          |
| --------------------- | -------------------------------------------------- |
| `AnsiString`          | Cadeia de caracteres (texto).                      |
| `array of AnsiString` | Vetor dinâmico de textos.                          |
| `record`              | Estrutura com vários campos (como um struct em C). |
| `function`            | Retorna um valor.                                  |
| `procedure`           | Executa uma ação, sem retorno.                     |

Exemplo de `record` usado no projeto:

```pascal
TTransition = record
  src: AnsiString;  // Estado de origem
  dst: AnsiString;  // Estado de destino
  sym: AnsiString;  // Símbolo da transição
end;
```

---

### 2.4 Diretivas de Compilação

O projeto usa algumas diretivas importantes para configurar o compilador:

```pascal
{$mode fpc}   // Ativa o modo Free Pascal (recursos modernos)
{$H+}         // Ativa strings longas (AnsiString ilimitadas)
```

| Diretiva | Significado |
|----------|-------------|
| `{$mode fpc}` | Modo de compatibilidade com Free Pascal, habilita recursos modernos |
| `{$H+}` | Strings são do tipo AnsiString (sem limite de 255 caracteres) |
| `{$H-}` | Strings são do tipo ShortString (máximo 255 caracteres) |
| `{$I+}` | Habilita verificação de erros de I/O |
| `{$I-}` | Desabilita verificação de erros de I/O |

**Por que usar `{$H+}`?**
- Permite trabalhar com strings grandes (arquivos JSON, etc.)
- Compatível com bibliotecas modernas
- Gerenciamento automático de memória

---

## 3. Estrutura do Projeto

O projeto é composto pelos seguintes arquivos:

| Arquivo           | Função principal                                    |
| ----------------- | --------------------------------------------------- |
| `u_types.pas`     | Define tipos, estruturas e funções auxiliares.      |
| `u_utils.pas`     | Faz a leitura e interpretação do arquivo JSON.      |
| `u_automaton.pas` | Implementa os algoritmos de autômatos.              |
| `u_io.pas`        | Responsável pela interface de entrada/saída e menu. |
| `main.pas`        | Arquivo principal que executa o programa.           |

### 3.1 Fluxo de Execução

```
┌─────────────┐
│  main.pas   │ ◄─── Ponto de entrada
└──────┬──────┘
       │
       ├──► ReadAllText (u_utils)        ◄─── Lê o JSON
       │
       ├──► ExtractStringsFromArray      ◄─── Parse do JSON
       │
       ├──► ShowMenu (u_io)              ◄─── Menu interativo
       │
       └──► Algoritmos (u_automaton)     ◄─── Transformações
            │
            ├─► EpsClosure
            ├─► RemoveEpsilon
            ├─► NFAToDFA
            ├─► MinimizeDFAHopcroft
            └─► Accepts
```

Para compilar, use:

```bash
cd pascal
fpc main.pas
```

Para executar:

```bash
./main ../data/automato.json
```

---

## 4. Descrição dos Arquivos

### 4.1 `u_types.pas`

Define estruturas básicas:
- `TStrArray`: vetor de strings.
- `TTransition`: representa uma transição do autômato.
- Funções auxiliares como `SetAdd`, `SetUnion`, `IndexOfStr`, usadas para manipular conjuntos e listas.

**Exemplo de uso:**
```pascal
var
  estados: TStrArray;
  trans: TTransition;
begin
  SetLength(estados, 3);
  estados[0] := 'q0';
  estados[1] := 'q1';
  estados[2] := 'q2';
  
  trans.src := 'q0';
  trans.dst := 'q1';
  trans.sym := 'a';
end;
```

### 4.2 `u_utils.pas`

Responsável pela leitura e interpretação do JSON que descreve o autômato:
- `ReadAllText`: lê o conteúdo de um arquivo.
- `FindKeyPos`: encontra a posição de uma chave no JSON.
- `ExtractStringsFromArray`: transforma um trecho do JSON em vetor de strings.
- `ExtractTransitions`: transforma a lista de transições do JSON em estruturas internas.

**Fluxo do Parser JSON:**
```
Arquivo JSON
    ↓
ReadAllText → String completa
    ↓
FindKeyPos → Localiza "alfabeto", "estados", etc.
    ↓
ExtractBracketIndices → Extrai conteúdo entre [ ]
    ↓
ExtractStringsFromArray → Converte para TStrArray
```

### 4.3 `u_automaton.pas`

Contém todos os **algoritmos principais**:
- `EpsClosure`: calcula o fecho-ε.
- `RemoveEpsilon`: remove transições ε (épsilon).
- `NFAToDFA`: converte autômato não-determinístico (AFN) em determinístico (AFD).
- `MinimizeDFAHopcroft`: minimiza o AFD com o algoritmo de Hopcroft.
- `Accepts`: verifica se uma palavra é aceita pelo autômato.

**Exemplo de Chamada:**
```pascal
var
  closure: TStrArray;
begin
  // Calcular fecho-ε do estado 'q0'
  closure := EpsClosure(transicoes, ['q0']);
  
  // closure agora contém todos os estados alcançáveis por ε
  // Exemplo: ['q0', 'q1', 'q3']
end;
```

### 4.4 `u_io.pas`

Apresenta um menu interativo com opções para o usuário:
1. Converter múltiplos iniciais em AFN-ε
2. Remover épsilon
3. Converter AFN→AFD
4. Minimizar AFD
5. Testar palavras
6. Imprimir autômato
7. Sair

**Interface do Menu:**
```
========================================
          SIMULADOR DE AUTOMATO
========================================
1. Converter múltiplos iniciais em AFN-ε
2. Remover épsilon (AFN-ε → AFN)
3. Converter AFN → AFD
4. Minimizar AFD
5. Testar palavras
6. Imprimir autômato
0. Sair
========================================
Escolha: _
```

### 4.5 `main.pas`

É o ponto de entrada do programa:
- Lê o caminho do JSON.
- Chama funções de `u_utils` para ler o autômato.
- Chama o menu em `u_io` para o usuário interagir.

**Fluxo de Execução:**
```pascal
begin
  // 1. Verifica argumentos da linha de comando
  if ParamCount >= 1 then 
    path := ParamStr(1)  // Usa caminho fornecido
  else 
    path := 'data/automato.json';  // Caminho padrão
  
  // 2. Lê o arquivo JSON
  json := ReadAllText(path);
  
  // 3. Faz o parse de cada seção
  ExtractStringsFromArray(section, alphabet);
  ExtractStringsFromArray(section, states);
  ExtractTransitions(section, transicoes);
  
  // 4. Inicia o menu interativo
  ShowMenu(alphabet, states, initials, finals, transicoes);
end.
```

---

## 5. Como Funcionam os Algoritmos de Autômato

### 5.1 Fecho-ε (Epsilon Closure)
**Função:** encontrar todos os estados alcançáveis a partir de um estado inicial apenas por transições ε.

**Pseudocódigo:**
```
função EpsClosure(estados_iniciais):
    resultado ← estados_iniciais
    pilha ← estados_iniciais
    
    enquanto pilha não vazia:
        estado ← desempilhar()
        para cada transição ε de estado:
            se destino não está em resultado:
                adicionar destino a resultado
                empilhar destino
    
    retornar resultado
```

**Exemplo Visual:**
```
Autômato:
q0 --ε--> q1 --ε--> q2
 |
 ε
 ↓
q3

EpsClosure([q0]) = {q0, q1, q2, q3}
```

**Implementação em Pascal:**
```pascal
function EpsClosure(const Trans: TTransArray; 
                    const Start: TStrArray): TStrArray;
var
  result, stack: TStrArray;
  current: AnsiString;
  targets: TStrArray;
begin
  result := Start;
  stack := Start;
  
  while Length(stack) > 0 do
  begin
    current := stack[High(stack)];
    SetLength(stack, Length(stack)-1);  // Pop
    
    targets := GetTargets(Trans, current, '&');  // '&' = epsilon
    for i := 0 to High(targets) do
      if not ContainsStr(result, targets[i]) then
      begin
        AddStr(result, targets[i]);
        AddStr(stack, targets[i]);
      end;
  end;
  
  EpsClosure := result;
end;
```

---
### 5.2 Conversão de AFN para AFD (Construção de Subconjunto)

**Ideia:** cada estado do DFA representa um conjunto de estados do NFA.

**Pseudocódigo:**
```
função NFAToDFA(afn):
    inicial_dfa ← EpsClosure(inicial_afn)
    fila ← [inicial_dfa]
    visitados ← conjunto vazio
    
    enquanto fila não vazia:
        conjunto_atual ← desenfileirar()
        marcar como visitado
        
        para cada símbolo do alfabeto:
            próximo ← conjunto vazio
            
            para cada estado em conjunto_atual:
                destinos ← transições(estado, símbolo)
                próximo ← próximo ∪ EpsClosure(destinos)
            
            se próximo não foi visitado:
                enfileirar próximo
                criar transição: conjunto_atual --símbolo--> próximo
    
    retornar dfa
```

**Exemplo Visual:**
```
AFN:
     a       b
q0 ----→ q1 ----→ q2
 |       |
 ε       ε
 ↓       ↓
q3      q4

Conversão para AFD:
Estados do DFA são conjuntos:
S0 = {q0, q3}  (inicial com fecho-ε)
S1 = {q1, q4}  (após ler 'a' de S0)
S2 = {q2}      (após ler 'b' de S1)

DFA resultante:
     a       b
S0 ----→ S1 ----→ S2
```

---
### 5.3 Remoção de Épsilon
**Função:** eliminar todas as transições ε mantendo o mesmo comportamento.

**Algoritmo:**
1. Para cada estado `q`, calcule seu fecho-ε
2. Para cada símbolo `a` (exceto ε):
   - Para cada estado `p` no fecho-ε de `q`:
     - Se existe transição `p --a--> r`:
       - Adicione transição `q --a--> r` (direta)
3. Remova todas as transições ε

**Exemplo:**
```
Antes:
q0 --ε--> q1 --a--> q2

Fecho-ε(q0) = {q0, q1}

Depois (ε removido):
q0 --a--> q2  (transição direta adicionada)
```

---
### 5.4 Minimização (Hopcroft)

**Função:** reduzir o DFA, removendo estados redundantes.

**Ideia:** agrupar estados equivalentes (que se comportam igual para todas as entradas).

**Algoritmo:**
```
1. Partição inicial: P = {Finais, Não-Finais}
2. W = P (fila de trabalho)

3. Enquanto W não vazio:
     Remover conjunto C de W
     
     Para cada símbolo a:
       X = estados que vão para C com 'a'
       
       Para cada conjunto B em P:
         Se X divide B:
           Dividir B em (B ∩ X) e (B - X)
           Substituir B pelas duas partes
           Atualizar W

4. Cada conjunto final em P vira um estado do DFA minimizado
```

**Exemplo Visual:**
```
DFA original:
q0 --a--> q1 --b--> q3 (final)
 |         |
 b         a
 ↓         ↓
q2 --a--> q4 --b--> q3 (final)

Estados q1 e q4 são equivalentes (ambos vão para q3 com 'b')

DFA minimizado:
q0 --a--> q1' --b--> q3 (final)
 |
 b
 ↓
q2 --a--> q1' (mesmo estado!)
```

---
### 5.5 Simulação de Palavra

Para testar se uma palavra é aceita:

**Algoritmo:**
```
função Accepts(automato, palavra):
    estados_atuais ← EpsClosure(inicial)
    
    para cada símbolo em palavra:
        próximos ← conjunto vazio
        
        para cada estado em estados_atuais:
            destinos ← transições(estado, símbolo)
            próximos ← próximos ∪ destinos
        
        estados_atuais ← EpsClosure(próximos)
    
    retornar (estados_atuais ∩ finais) ≠ vazio
```

**Exemplo de Simulação:**
```
Autômato: q0 --a--> q1 --b--> q2 (final)
Palavra: "ab"

Passo 1: Estados atuais = {q0}
Lê 'a' → {q1}

Passo 2: Estados atuais = {q1}
Lê 'b' → {q2}

Passo 3: q2 é final? SIM → Palavra aceita! ✓
```

---

## 6. Exemplo de Arquivo JSON

```json
{
  "alfabeto": ["a", "b"],
  "estados": ["S0", "S1", "S2"],
  "estadosI": ["S0"],
  "estadoF": ["S2"],
  "transicoes": [
    ["S0", "S1", "a"],
    ["S1", "S2", "b"],
    ["S0", "S2", "&"]
  ]
}
```

**Formato das transições:**
```json
["estado_origem", "estado_destino", "símbolo"]
```

**Símbolo especial:**
- `"&"` representa a transição épsilon (ε)

---

## 7. Exemplos Práticos de Uso

### 7.1 Testando uma palavra

```bash
$ ./main data/automato.json

Menu:
1. Converter múltiplos iniciais em AFN-ε
2. Remover épsilon
3. Converter AFN→AFD
4. Minimizar AFD
5. Testar palavras
6. Imprimir autômato
0. Sair

Escolha: 5

Modo de entrada:
(f) arquivo
(t) terminal
Escolha: t

Digite a palavra: ab
Resultado: ACEITA ✓

Digite a palavra: ba
Resultado: REJEITADA ✗
```

### 7.2 Convertendo AFN para AFD

```bash
Escolha: 3
Convertendo AFN para AFD...

Automato DFA:
Estados: S0 S1 S2
Inicial: S0
Finais: S2
Transições:
  S0 --(a)--> S1
  S1 --(b)--> S2
```

---

## 8. Dicas de Debugging

### 8.1 Imprimindo valores intermediários

Adicione `WriteLn` para debug:

```pascal
// Antes de processar
WriteLn('Estados atuais: ', Length(current), ' estados');
for i := 0 to High(current) do
  WriteLn('  ', current[i]);
```

### 8.2 Verificando o parse do JSON

```pascal
// No main.pas, adicione após cada ExtractStringsFromArray:
WriteLn('Alfabeto lido: ');
for i := 0 to High(alphabet) do
  WriteLn('  ', alphabet[i]);
```

### 8.3 Testando funções isoladamente

Crie um programa de teste:

```pascal
program teste_epsclosure;
uses u_types, u_automaton;

var
  trans: TTransArray;
  result: TStrArray;
begin
  // Criar transições de teste
  SetLength(trans, 2);
  trans[0].src := 'q0'; trans[0].dst := 'q1'; trans[0].sym := '&';
  trans[1].src := 'q1'; trans[1].dst := 'q2'; trans[1].sym := '&';
  
  // Testar
  result := EpsClosure(trans, ['q0']);
  
  // Deve retornar: q0, q1, q2
  WriteLn('Resultado: ', Length(result), ' estados');
end.
```

---

## 9. Troubleshooting

### Problema: "Error: Can't open file 'u_types.pas'"

**Solução:** Certifique-se de estar no diretório `pascal/` ao compilar:
```bash
cd pascal
fpc main.pas
```

---

### Problema: "Segmentation fault"

**Causas comuns:**
1. Array não inicializado
2. Acesso fora dos limites

**Solução:** Use `SetLength` antes de usar arrays:
```pascal
var
  arr: TStrArray;
begin
  SetLength(arr, 10);  // SEMPRE faça isso primeiro!
  arr[0] := 'valor';
end;
```

---

### Problema: String truncada em 255 caracteres

**Causa:** Falta a diretiva `{$H+}`

**Solução:** Adicione no início do arquivo:
```pascal
{$mode fpc}{$H+}
```

---

## 10. Referências e Recursos Adicionais

### 10.1 Livros e Materiais

- **"Introduction to Automata Theory"** - Hopcroft, Motwani, Ullman
- **Documentação do Free Pascal:** https://www.freepascal.org/docs.html

### 10.2 Ferramentas Úteis

- **JFLAP:** Software para visualizar autômatos
- **Graphviz:** Para gerar diagramas de autômatos

### 10.3 Comandos Úteis

```bash
# Compilar com warnings extras
fpc -vw main.pas

# Compilar com otimização
fpc -O3 main.pas

# Gerar informações de debug
fpc -g main.pas

# Limpar arquivos compilados
rm *.o *.ppu main
```

---

## 11. Contribuindo para o Projeto

### 11.1 Adicionando um novo algoritmo

1. Declare a função em `u_automaton.pas` (seção `interface`)
2. Implemente na seção `implementation`
3. Adicione uma opção no menu em `u_io.pas`
4. Documente o algoritmo neste README

### 11.2 Padrão de Código

- Use nomes descritivos em português
- Comente algoritmos complexos
- Teste com diferentes autômatos

---

**Última atualização:** Outubro de 2025
**Versão:** 1.0
