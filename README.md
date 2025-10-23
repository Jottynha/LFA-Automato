
# 1. Projeto de Autômato Finito em Pascal

Este projeto implementa um simulador de autômatos finitos (AF) em Pascal, capaz de ler a definição de um autômato a partir de um arquivo JSON, realizar diversas transformações (como conversão de AFN para AFD e minimização) e testar se palavras são aceitas pelo autômato.


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

## 8. Troubleshooting

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

## Visão Geral e Estrutura do Projeto

O código é modularizado em `units` (bibliotecas) para separar as responsabilidades, facilitando a manutenção e o entendimento.

- **`main.pas`**: O programa principal. É responsável por orquestrar a leitura do arquivo JSON, a montagem da estrutura de dados do autômato e a exibição do menu interativo.
- **`u_types.pas`**: Define os tipos de dados essenciais usados em todo o projeto, como vetores de strings e a estrutura para transições.
- **`u_utils.pas`**: Contém funções utilitárias para manipulação de strings, leitura de arquivos e, crucialmente, a análise (parsing) do arquivo JSON que define o autômato.
- **`u_automaton.pas`**: O coração do projeto. Contém toda a lógica para as operações com autômatos, incluindo a simulação da aceitação de palavras e os algoritmos de transformação.
- **`u_io.pas`**: Responsável pela interface com o usuário, exibindo o menu de opções e gerenciando a interação.

### Estrutura de Diretórios

```
.
├── data/
│   ├── automato.json        # Arquivo de definição do autômato
│   └── palavras_aceitas.txt # Exemplo de arquivo com palavras para teste
├── pascal/
│   ├── main.pas             # Programa principal
│   ├── u_automaton.pas      # Lógica do autômato
│   ├── u_io.pas             # Lógica de interface/menu
│   ├── u_types.pas          # Tipos de dados
│   └── u_utils.pas          # Funções utilitárias e parser JSON
└── README.md                # Este arquivo
```

## Compilação e Execução

### Pré-requisitos

- Compilador Free Pascal (`fpc`).

### Compilando

Navegue até o diretório `pascal/` e execute o comando:

```bash
fpc main.pas
```

Isso irá compilar o programa principal e todas as `units` associadas, gerando um executável chamado `main`.

### Executando

O programa pode ser executado de duas formas:

1.  **Usando o arquivo JSON padrão**:
    O programa procurará por `data/automato.json` relativo ao local de execução. Se você executar a partir do diretório `pascal/`, o caminho correto seria `../data/automato.json`.

    ```bash
    # Estando no diretório 'pascal/'
    ./main ../data/automato.json
    ```

2.  **Especificando um caminho para o arquivo JSON**:
    Você pode passar o caminho do arquivo como um argumento de linha de comando.

    ```bash
    ./main /caminho/completo/para/data/automato.json
    ```

Após a execução, um menu interativo será exibido no terminal.

## Análise Detalhada do Código-Fonte

### `u_types.pas` - As Fundações

Esta `unit` define os blocos de construção de dados do projeto.

- **`TStrArray = array of AnsiString;`**: Um tipo para representar um vetor (array dinâmico) de strings. É usado para armazenar o alfabeto, os conjuntos de estados, estados iniciais e finais. A diretiva `{$H+}` é crucial para que `AnsiString` funcione corretamente, permitindo strings de tamanho virtualmente ilimitado.

- **`TTransition = record ... end;`**: Uma estrutura (`record`) que representa uma única transição do autômato.
  - `src`: Estado de origem (ex: 'q0').
  - `dst`: Estado de destino (ex: 'q1').
  - `sym`: Símbolo que dispara a transição (ex: 'a').

- **`TTransArray = array of TTransition;`**: Um vetor de transições, usado para armazenar todas as transições do autômato.

- **Funções Auxiliares**:
  - `MakeArray1(const S: AnsiString): TStrArray;`: Cria um `TStrArray` com um único elemento. Útil para iniciar operações que trabalham com conjuntos de estados a partir de um único estado.
  - `IndexOfStr(const A: TStrArray; const S: AnsiString): LongInt;`: Procura uma string em um `TStrArray` e retorna seu índice, ou -1 se não for encontrada. É uma função de busca linear, fundamental e usada extensivamente em todo o projeto para verificar a existência de elementos em conjuntos.

### `u_utils.pas` - Utilitários e Parser JSON

Esta `unit` é crucial para a entrada de dados, pois contém o parser manual de JSON, escrito especificamente para este projeto.

- **`ReadAllText`**: Lê o conteúdo completo de um arquivo de texto e o retorna como uma única string, com quebras de linha preservadas (`#10`).
- **`FindKeyPos`**: Encontra a posição de uma chave (ex: `"alfabeto"`) dentro da string JSON. A busca é simples: procura a chave cercada por aspas.
- **`ExtractBracketIndices`**: Dada uma posição inicial, localiza os índices dos colchetes `[` e `]` que delimitam um array JSON. A lógica ignora colchetes dentro de strings para evitar erros de parsing.
- **`ExtractStringsFromArray`**: Recebe uma "fatia" de uma string JSON (o conteúdo de um array) e extrai todas as strings literais (delimitadas por aspas duplas `"`), adicionando-as a um `TStrArray`.
- **`ExtractTransitions`**: Uma variação da função anterior, especializada em extrair as transições. Ela lê as strings em grupos de três (`src`, `dst`, `sym`) para montar os `TTransition` e adicioná-los a um `TTransArray`.

O parser funciona de forma procedural e manual, evitando a necessidade de uma biblioteca externa:
1. O `main.pas` lê o arquivo JSON inteiro para uma string.
2. Para cada chave ("alfabeto", "estados", etc.), ele chama `FindKeyPos`.
3. Com a posição da chave, ele chama `ExtractBracketIndices` para encontrar o array correspondente.
4. A "fatia" do array é passada para `ExtractStringsFromArray` ou `ExtractTransitions` para popular as estruturas de dados.

### `main.pas` - O Ponto de Partida

O programa principal é simples e direto, servindo como o orquestrador do processo inicial.

1.  **Verifica Parâmetros**: Checa se um caminho de arquivo foi passado via linha de comando (`ParamCount`, `ParamStr`). Se não, usa um caminho padrão.
2.  **Lê o Arquivo**: Usa `ReadAllText` da `u_utils` para carregar o conteúdo do JSON para a memória.
3.  **Parsing do JSON**: Executa a sequência de `FindKeyPos`, `ExtractBracketIndices` e `ExtractStringsFromArray`/`ExtractTransitions` para cada parte do autômato (alfabeto, estados, etc.), preenchendo as variáveis locais que representarão o autômato.
4.  **Chama o Menu**: Uma vez que todas as estruturas de dados do autômato estão carregadas na memória, ele chama `ShowMenu` (da `unit` `u_io`), passando todas as informações do autômato para que a interação com o usuário possa começar.

### `u_automaton.pas` - O Cérebro do Projeto

Esta é a `unit` mais complexa, contendo toda a lógica dos autômatos.

#### Funções Auxiliares e Operações de Conjunto

- **`GetTargets`**: Dado um estado de origem e um símbolo, retorna um `TStrArray` com todos os estados de destino possíveis. É a função base para simular um passo de transição e lida naturalmente com o não determinismo (retornando múltiplos estados).
- **`UnionStr`**: Realiza a união de dois conjuntos de estados (dois `TStrArray`), garantindo que não haja elementos duplicados no resultado.
- **`IntersectsStr`**: Verifica se há interseção (elementos em comum) entre dois conjuntos de estados. Usado principalmente para checar se algum dos estados atuais de uma simulação é um estado final.
- **`SortStrArray` e `KeyFromSet`**: Funções cruciais para o algoritmo de subconjuntos. `SortStrArray` ordena um conjunto de estados, e `KeyFromSet` cria uma representação textual única e canônica desse conjunto (ex: `"{q0,q1,q3}"`). Isso permite que um conjunto de estados seja usado como uma "chave" ou "nome" para um novo estado no AFD.
- **`EpsClosure` (Fecho-Épsilon)**: Dado um conjunto de estados, calcula todos os estados alcançáveis a partir deles usando apenas transições épsilon (`&`). A implementação usa uma pilha (`stack`) para realizar uma busca em profundidade (DFS) não recursiva, garantindo que todos os caminhos épsilon sejam explorados.

#### Função de Aceitação

- **`Accepts`**: Simula a execução do autômato para verificar se uma palavra é aceita.
  1.  O conjunto de estados atuais (`current`) começa com o fecho-épsilon dos estados iniciais.
  2.  Para cada caractere da palavra de entrada:
      a. O `nextSet` é calculado encontrando todos os destinos possíveis (`GetTargets`) a partir de cada estado em `current` com o símbolo lido.
      b. O conjunto `current` é atualizado para ser o fecho-épsilon (`EpsClosure`) do `nextSet`. Este passo é vital para AFN-&, pois após cada transição normal, devemos seguir todas as transições épsilon possíveis.
  3.  Ao final da palavra, a função retorna `True` se o conjunto `current` tiver qualquer estado em comum com o conjunto de estados finais (`IntersectsStr`), e `False` caso contrário.

#### Algoritmos de Transformação

- **`ConvertMultipleInitialsToAFNEps`**: Se o autômato tiver múltiplos estados iniciais, esta função o converte para um AFN-& equivalente com um único estado inicial. Ela cria um novo estado (com um nome inédito, como `Qi0`), define-o como o único estado inicial e adiciona transições épsilon dele para todos os estados iniciais originais.

- **`RemoveEpsilon`**: Converte um AFN-& para um AFN sem transições épsilon.
  1.  **Novos Finais**: Um estado se torna final se ele pode alcançar um estado final original por um caminho de épsilons.
  2.  **Novas Transições**: Para cada estado `p` e símbolo `a`, a nova transição `p --a--> r` é criada se for possível ir de `p` para algum `q` via épsilons, de `q` para algum `s` com o símbolo `a`, e de `s` para `r` via épsilons. Essencialmente, "salta" sobre as transições épsilon.

- **`NFAToDFA` (Construção de Subconjuntos)**: Converte um AFN (com ou sem transições épsilon) para um AFD.
  1.  O estado inicial do AFD é o fecho-épsilon do(s) estado(s) inicial(is) do AFN. Este conjunto é o primeiro "macro-estado".
  2.  Uma fila (`queue`) de macro-estados a serem processados é mantida.
  3.  Enquanto a fila não está vazia, um macro-estado `T` é retirado. Para cada símbolo do alfabeto:
      a. Calcula-se o conjunto de destino `U`, que é o fecho-épsilon de todos os estados que `T` pode alcançar com aquele símbolo.
      b. O conjunto `U` se torna um novo macro-estado. `KeyFromSet` é usado para dar um nome a `T` e `U`.
      c. Uma transição é criada no AFD do estado `T` para o estado `U` com o símbolo correspondente.
      d. Se `U` é um macro-estado que nunca foi visto antes, ele é adicionado à fila para ser processado.
  4.  Um macro-estado no AFD é final se ele contiver pelo menos um estado final do AFN original.

- **`MinimizeDFAHopcroft` (Minimização de AFD)**: Implementa o algoritmo de Hopcroft para encontrar o AFD mínimo.
  1.  **Completar o AFD**: Primeiro, o algoritmo verifica se o AFD é completo (tem transições definidas para todos os estados e símbolos). Se não, um estado de "morte" (`DEAD`) é criado, para onde vão todas as transições indefinidas.
  2.  **Partição Inicial**: Os estados são divididos em dois grupos (partições): Finais e Não-Finais. Essa é a partição inicial `P`.
  3.  **Fila de Trabalho**: Uma fila de trabalho `W` é inicializada com os grupos de `P`.
  4.  **Refinamento**: O algoritmo itera enquanto `W` não está vazia.
      a. Um conjunto `C` é retirado de `W`.
      b. Para cada símbolo `a`, o algoritmo calcula o conjunto `X` de todos os estados que, com o símbolo `a`, transicionam para um estado em `C`.
      c. O algoritmo então "corta" cada conjunto `Y` na partição `P` usando `X`. Se `X` divide `Y` em duas partes não vazias (`Y ∩ X` e `Y - X`), o conjunto `Y` é substituído por essas duas novas partes em `P`, e a menor delas é adicionada a `W`.
  5.  **Construção do Autômato Minimizado**: Quando `W` fica vazia, os grupos na partição final `P` são os estados do novo AFD minimizado. As transições são reconstruídas entre esses novos estados-grupo.

###  `u_io.pas` - A Interface com o Usuário

- **`ShowMenu`**: Entra em um loop infinito que:
  1.  Imprime as opções do menu.
  2.  Lê a escolha do usuário.
  3.  Usa uma estrutura `case` para chamar a função de transformação ou teste correspondente da `unit` `u_automaton`.
  4.  Após a maioria das operações, chama `PrintAutomaton` para mostrar o estado atualizado do autômato, fornecendo feedback visual imediato ao usuário.
- **`TestarPalavras`**: Implementa a lógica para o item 5 do menu. Pergunta se o teste será feito via arquivo (`f`) ou terminal (`t`). Em seguida, lê as palavras uma a uma, chama a função `Accepts` e imprime o resultado (`True` ou `False`).

##  Formato do Arquivo `automato.json`

O arquivo JSON que define o autômato deve seguir esta estrutura:

```json
{
  "alfabeto": ["a", "b"],
  "estados": ["q0", "q1", "q2"],
  "estadosI": ["q0"],
  "estadoF": ["q2"],
  "transicoes": [
    "q0", "q1", "a",
    "q1", "q2", "b"
  ]
}
```

- **`alfabeto`**: Um array de strings, onde cada string é um símbolo do alfabeto.
- **`estados`**: Um array com os nomes de todos os estados.
- **`estadosI`**: Um array com os nomes dos estados iniciais.
- **`estadoF`**: Um array com os nomes dos estados finais.
- **`transicoes`**: Um array "achatado" de strings, onde cada grupo de três strings consecutivas representa uma transição no formato: `estado_origem`, `estado_destino`, `simbolo`.

**Observação**: O símbolo para a transição épsilon (vazia) deve ser representado como `&` no arquivo JSON.
