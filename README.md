# 📚 Simulador de Autômatos Finitos em Pascal

> Um projeto educacional completo para manipulação e transformação de autômatos finitos, implementado em Free Pascal.

---

## 📑 Índice

1. [Visão Geral](#-visão-geral)
2. [Introdução ao Pascal](#-introdução-ao-pascal)
3. [Arquitetura do Projeto](#-arquitetura-do-projeto)
4. [Compilação e Execução](#-compilação-e-execução)
5. [Algoritmos de Autômatos](#-algoritmos-de-autômatos)
6. [Formato do JSON](#-formato-do-arquivo-json)
7. [Exemplos Práticos](#-exemplos-práticos)
8. [Troubleshooting](#-troubleshooting)

---

## 🎯 Visão Geral

Este projeto implementa um **simulador completo de autômatos finitos** que permite:

```
┌─────────────────────────────────────────────────────────────┐
│  📥 ENTRADA                                                  │
│  • Arquivo JSON com definição do autômato                   │
└─────────────────────┬───────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────────────┐
│  ⚙️  TRANSFORMAÇÕES DISPONÍVEIS                              │
│                                                              │
│  1️⃣  AFN com múltiplos iniciais → AFN-ε (estado único)      │
│  2️⃣  AFN-ε → AFN (remoção de épsilon)                        │
│  3️⃣  AFN → AFD (construção de subconjuntos)                  │
│  4️⃣  AFD → AFD mínimo (algoritmo de Hopcroft)                │
│  5️⃣  Simulação: aceita palavra? (Sim/Não)                    │
└─────────────────────┬───────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────────────┐
│  📤 SAÍDA                                                    │
│  • Autômato transformado                                    │
│  • Resultado de aceitação de palavras                       │
└─────────────────────────────────────────────────────────────┘
```

### 🗂️ Estrutura de Diretórios

```
LFA-Automato/
│
├── 📂 data/
│   ├── automato.json         ← Definição do autômato em JSON
│   └── palavras_aceitas.txt  ← Palavras para teste em lote
│
├── 📂 pascal/
│   ├── main.pas              ← 🚀 Ponto de entrada do programa
│   ├── u_types.pas           ← 🏗️  Tipos e estruturas de dados
│   ├── u_utils.pas           ← 🔧 Parser JSON e utilitários
│   ├── u_automaton.pas       ← 🧠 Algoritmos de autômatos
│   └── u_io.pas              ← 💬 Interface e menu interativo
│
└── README.md                 ← 📖 Você está aqui!
```

---

## 🔤 Introdução ao Pascal

### Estrutura Básica de um Programa

```pascal
program MeuPrograma;        ← Nome do programa

uses                        ← Importação de bibliotecas
  SysUtils, Classes;        

var                         ← Declaração de variáveis
  nome: string;
  idade: integer;

begin                       ← Início do código executável
  WriteLn('Olá, Mundo!');
  ReadLn(nome);
end.                        ← Fim do programa (ponto obrigatório!)
```

### Estrutura de uma Unit (Módulo)

```
┌─────────────────────────────────────────┐
│  unit u_exemplo;                        │
│                                         │
│  {$mode fpc}{$H+}  ← Diretivas         │
├─────────────────────────────────────────┤
│  interface         ← O que é PÚBLICO   │
│    type                                 │
│      TMeuTipo = ...                     │
│    procedure MinhaFunc;                 │
├─────────────────────────────────────────┤
│  implementation    ← O que é PRIVADO   │
│    procedure MinhaFunc;                 │
│    begin                                │
│      // código...                       │
│    end;                                 │
├─────────────────────────────────────────┤
│  end.              ← Fim da unit        │
└─────────────────────────────────────────┘
```

### Diretivas de Compilação Importantes

| Diretiva       | Função                                              |
|----------------|-----------------------------------------------------|
| `{$mode fpc}`  | Ativa modo Free Pascal (recursos modernos)         |
| `{$H+}`        | Strings longas (AnsiString sem limite de 255 chars)|
| `{$I+}` / `{$I-}` | Habilita/desabilita verificação de erros I/O   |

**🔑 Por que usar `{$H+}`?**
```
┌─────────────────┬──────────────┬─────────────────┐
│ Sem {$H+}       │ Com {$H+}    │ Vantagem        │
├─────────────────┼──────────────┼─────────────────┤
│ ShortString     │ AnsiString   │ Tamanho ilimitado│
│ Máx 255 chars   │ Dinâmico     │ JSON grandes    │
│ Memória fixa    │ Gerenciado   │ Auto-expansão   │
└─────────────────┴──────────────┴─────────────────┘
```

### Tipos Usados no Projeto

```pascal
// Vetor dinâmico de strings
TStrArray = array of AnsiString;

// Estrutura para representar uma transição
TTransition = record
  src: AnsiString;  // Estado de origem
  dst: AnsiString;  // Estado de destino
  sym: AnsiString;  // Símbolo
end;

// Vetor de transições
TTransArray = array of TTransition;
```

**Exemplo Visual de TTransition:**
```
┌─────────────────────────────────┐
│  TTransition                    │
├─────────────────────────────────┤
│  src: "q0"    ← De onde sai     │
│  dst: "q1"    ← Para onde vai   │
│  sym: "a"     ← Com qual símbolo│
└─────────────────────────────────┘
         Representa: q0 --a--> q1
```

---

## 🏗️ Arquitetura do Projeto

### Mapa de Dependências

```
                    ┌──────────────┐
                    │   main.pas   │ ← Programa principal
                    └───────┬──────┘
                            │
        ┌───────────────────┼───────────────────┐
        ↓                   ↓                   ↓
   ┌─────────┐         ┌─────────┐        ┌──────────┐
   │u_types  │←────────│u_utils  │        │  u_io    │
   │(Tipos)  │         │(Parser) │        │ (Menu)   │
   └─────────┘         └─────────┘        └────┬─────┘
        ↑                   ↑                   ↓
        └───────────────────┴──────────┬────────┘
                                       ↓
                              ┌─────────────────┐
                              │  u_automaton    │
                              │  (Algoritmos)   │
                              └─────────────────┘
```

### Fluxo de Execução Completo

```
╔══════════════════════════════════════════════════════════════╗
║  FASE 1: INICIALIZAÇÃO                                       ║
╚══════════════════════════════════════════════════════════════╝
    │
    ├─→ [1] Verifica argumentos da linha de comando
    │        if ParamCount >= 1 then path := ParamStr(1)
    │
    ├─→ [2] Lê arquivo JSON completo
    │        json := ReadAllText(path)
    │
    └─→ [3] Valida se arquivo foi lido
             if json = '' then Halt(1)

╔══════════════════════════════════════════════════════════════╗
║  FASE 2: PARSING DO JSON                                     ║
╚══════════════════════════════════════════════════════════════╝
    │
    ├─→ [4] Extrai ALFABETO
    │        FindKeyPos → ExtractBracketIndices → ExtractStrings
    │        Resultado: ["a", "b", "c"]
    │
    ├─→ [5] Extrai ESTADOS
    │        Resultado: ["q0", "q1", "q2"]
    │
    ├─→ [6] Extrai ESTADOS INICIAIS
    │        Resultado: ["q0"]
    │
    ├─→ [7] Extrai ESTADOS FINAIS
    │        Resultado: ["q2"]
    │
    └─→ [8] Extrai TRANSIÇÕES
             Resultado: [(q0,q1,a), (q1,q2,b), ...]

╔══════════════════════════════════════════════════════════════╗
║  FASE 3: LOOP INTERATIVO                                     ║
╚══════════════════════════════════════════════════════════════╝
    │
    └─→ [9] ShowMenu() ─┐
                        │
         ┌──────────────┴──────────────┐
         │  MENU INTERATIVO             │
         │  ┌────────────────────────┐  │
         │  │ 1. Converter iniciais  │  │
         │  │ 2. Remover épsilon     │  │
         │  │ 3. AFN → AFD           │  │
         │  │ 4. Minimizar AFD       │  │
         │  │ 5. Testar palavras     │  │
         │  │ 6. Mostrar autômato    │  │
         │  │ 0. Sair                │  │
         │  └────────────────────────┘  │
         └─────────────┬────────────────┘
                       │
                       └─→ Loop até escolher "0"
```

### Detalhamento dos Módulos

#### 📦 `u_types.pas` - Fundações

```
┌─────────────────────────────────────────────────────┐
│  TIPOS BÁSICOS                                      │
├─────────────────────────────────────────────────────┤
│  TStrArray        = array of AnsiString             │
│  TTransArray      = array of TTransition            │
│                                                     │
│  TTransition = record                               │
│    src, dst, sym: AnsiString                        │
│  end;                                               │
├─────────────────────────────────────────────────────┤
│  FUNÇÕES AUXILIARES                                 │
├─────────────────────────────────────────────────────┤
│  • MakeArray1(S)     → Cria array [S]               │
│  • IndexOfStr(A, S)  → Busca S em A (-1 se ausente) │
└─────────────────────────────────────────────────────┘
```

#### 🔧 `u_utils.pas` - Parser JSON Manual

```
PIPELINE DE PARSING:
═══════════════════

  📄 Arquivo JSON
      │
      ↓ ReadAllText()
  📝 String completa em memória
      │
      ↓ FindKeyPos("alfabeto")
  📍 Posição da chave no texto
      │
      ↓ ExtractBracketIndices()
  🔢 Índices [início, fim] do array
      │
      ↓ ExtractStringsFromArray()
  📊 TStrArray populado
```

**Exemplo de Parsing:**
```json
{
  "alfabeto": ["a", "b", "c"]
}
```
↓ **FindKeyPos** localiza `"alfabeto"` na posição 5
↓ **ExtractBracketIndices** encontra `[` na pos 18, `]` na pos 32
↓ **ExtractStringsFromArray** extrai: `"a"`, `"b"`, `"c"`
↓ **Resultado:** `TStrArray = ['a', 'b', 'c']`

#### 🧠 `u_automaton.pas` - Motor de Transformações

```
╔════════════════════════════════════════════════════╗
║  OPERAÇÕES DE CONJUNTO                             ║
╠════════════════════════════════════════════════════╣
║  GetTargets(Trans, src, sym) → Retorna destinos    ║
║  UnionStr(A, B)              → A ∪ B               ║
║  IntersectsStr(A, B)         → A ∩ B ≠ ∅?          ║
║  KeyFromSet([q0,q1])         → "{q0,q1}"           ║
║  EpsClosure(Trans, [q0])     → Fecho-ε             ║
╠════════════════════════════════════════════════════╣
║  ALGORITMOS PRINCIPAIS                             ║
╠════════════════════════════════════════════════════╣
║  ✓ Accepts()                    ← Simulação        ║
║  ✓ ConvertMultipleInitials()    ← AFN → AFN-ε      ║
║  ✓ RemoveEpsilon()              ← AFN-ε → AFN      ║
║  ✓ NFAToDFA()                   ← AFN → AFD        ║
║  ✓ MinimizeDFAHopcroft()        ← AFD → AFD min    ║
╚════════════════════════════════════════════════════╝
```

#### 💬 `u_io.pas` - Interface do Usuário

```
┌─────────────────────────────────────────┐
│  MENU PRINCIPAL                         │
│  ┌───────────────────────────────────┐  │
│  │ while True do                     │  │
│  │   Exibir opções                   │  │
│  │   Ler escolha                     │  │
│  │   case escolha of                 │  │
│  │     '1': Converter iniciais       │  │
│  │     '2': Remover epsilon          │  │
│  │     '3': AFN → AFD                │  │
│  │     '4': Minimizar                │  │
│  │     '5': Testar palavras          │  │
│  │     '6': Imprimir autômato        │  │
│  │     '0': Exit                     │  │
│  │   end;                            │  │
│  └───────────────────────────────────┘  │
└─────────────────────────────────────────┘
```

---

## ⚙️ Compilação e Execução

### Pré-requisitos

```bash
# Verificar instalação do Free Pascal
fpc -version

# Saída esperada:
# Free Pascal Compiler version 3.x.x
```

### Compilação

```bash
# Navegar para o diretório do código
cd pascal/

# Compilar (gera executável 'main')
fpc main.pas

# Compilação bem-sucedida mostra:
# Linking main
# 123 lines compiled, 0.2 sec
```

### Execução

```bash
# Opção 1: Caminho padrão (data/automato.json)
./main ../data/automato.json

# Opção 2: Caminho customizado
./main /caminho/completo/para/automato.json

# Opção 3: Usar caminho relativo
./main ../../meus_automatos/teste.json
```

**Fluxo após Execução:**
```
$ ./main ../data/automato.json

📖 Carregando autômato de: ../data/automato.json
✓ Alfabeto lido: 2 símbolos
✓ Estados lidos: 3 estados
✓ Transições lidas: 5 transições

╔════════════════════════════════╗
║      MENU PRINCIPAL            ║
╠════════════════════════════════╣
║  1. Converter iniciais         ║
║  2. Remover épsilon            ║
║  3. AFN → AFD                  ║
║  4. Minimizar AFD              ║
║  5. Testar palavras            ║
║  6. Mostrar autômato           ║
║  0. Sair                       ║
╚════════════════════════════════╝
---
```
## 🧮 Algoritmos de Autômatos

### 1️⃣ Fecho-Épsilon (Epsilon Closure)

**Objetivo:** Encontrar todos os estados alcançáveis usando apenas transições ε.

#### Visualização do Processo

```
EXEMPLO DE AUTÔMATO:
                    ε           ε
        q0 ──────────→ q1 ──────────→ q2
         │
         │ ε
         ↓
        q3

```
PASSO A PASSO - EpsClosure({q0}):
```
┌─────────────────────────────────────────────────┐
│ INICIALIZAÇÃO                                   │
├─────────────────────────────────────────────────┤
│ Resultado = {q0}                                │
│ Pilha     = [q0]                                │
└─────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────┐
│ ITERAÇÃO 1                                      │
├─────────────────────────────────────────────────┤
│ Desempilha: q0                                  │
│ Transições ε de q0: {q1, q3}                    │
│ Adiciona q1 e q3 ao resultado e à pilha         │
│                                                 │
│ Resultado = {q0, q1, q3}                        │
│ Pilha     = [q1, q3]                            │
└─────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────┐
│ ITERAÇÃO 2                                      │
├─────────────────────────────────────────────────┤
│ Desempilha: q3                                  │
│ Transições ε de q3: ∅ (nenhuma)                 │
│                                                 │
│ Resultado = {q0, q1, q3}                        │
│ Pilha     = [q1]                                │
└─────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────┐
│ ITERAÇÃO 3                                      │
├─────────────────────────────────────────────────┤
│ Desempilha: q1                                  │
│ Transições ε de q1: {q2}                        │
│ Adiciona q2 ao resultado e à pilha              │
│                                                 │
│ Resultado = {q0, q1, q2, q3}                    │
│ Pilha     = [q2]                                │
└─────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────┐
│ ITERAÇÃO 4                                      │
├─────────────────────────────────────────────────┤
│ Desempilha: q2                                  │
│ Transições ε de q2: ∅                           │
│                                                 │
│ Resultado = {q0, q1, q2, q3}                    │
│ Pilha     = []  ← VAZIA!                        │
└─────────────────────────────────────────────────┘

✓ RESULTADO FINAL: EpsClosure({q0}) = {q0, q1, q2, q3}
```

#### Pseudocódigo

```pascal
function EpsClosure(Trans, StartStates):
  resultado ← StartStates
  pilha ← StartStates
  
  while pilha não vazia do:
    estado ← pop(pilha)
    
    para cada transição (estado --ε--> destino):
      se destino ∉ resultado:
        adicionar destino a resultado
        push(pilha, destino)
  
  return resultado
```

---

### 2️⃣ Remoção de Épsilon

**Objetivo:** Converter AFN-ε em AFN sem transições épsilon.

#### Visualização Completa

```
╔══════════════════════════════════════════════════════╗
║  AUTÔMATO ORIGINAL (AFN-ε)                           ║
╚══════════════════════════════════════════════════════╝

         ε           a           b
    q0 ────→ q1 ───────→ q2 ───────→ q3 (final)
                                       ↑
                                       │ ε
                                       │
                                      q4

Estados: {q0, q1, q2, q3, q4}
Iniciais: {q0}
Finais: {q3}


╔══════════════════════════════════════════════════════╗
║  PASSO 1: CALCULAR FECHOS-ε                          ║
╚══════════════════════════════════════════════════════╝

EpsClosure(q0) = {q0, q1}     ← q0 alcança q1 por ε
EpsClosure(q1) = {q1}
EpsClosure(q2) = {q2}
EpsClosure(q3) = {q3}
EpsClosure(q4) = {q3, q4}     ← q4 alcança q3 por ε


╔══════════════════════════════════════════════════════╗
║  PASSO 2: NOVOS ESTADOS FINAIS                       ║
╚══════════════════════════════════════════════════════╝

Para cada estado p:
  Se EpsClosure(p) ∩ Finais ≠ ∅, então p é final

q0: {q0,q1} ∩ {q3} = ∅        → NÃO final
q1: {q1} ∩ {q3} = ∅           → NÃO final
q2: {q2} ∩ {q3} = ∅           → NÃO final
q3: {q3} ∩ {q3} = {q3}        → FINAL ✓
q4: {q3,q4} ∩ {q3} = {q3}     → FINAL ✓

Novos Finais = {q3, q4}


╔══════════════════════════════════════════════════════╗
║  PASSO 3: NOVAS TRANSIÇÕES (SEM ε)                   ║
╚══════════════════════════════════════════════════════╝

Para cada estado p e símbolo a:
  Para cada q em EpsClosure(p):
    Se existe q --a--> r:
      Para cada s em EpsClosure(r):
        Adicionar: p --a--> s

De q0 com 'a':
  EpsClosure(q0) = {q0, q1}
  q1 --a--> q2
  EpsClosure(q2) = {q2}
  ✓ Adiciona: q0 --a--> q2

De q0 com 'b':
  Nenhuma transição 'b' de {q0, q1}
  (nada a adicionar)

De q2 com 'b':
  q2 --b--> q3
  EpsClosure(q3) = {q3}
  ✓ Adiciona: q2 --b--> q3


╔══════════════════════════════════════════════════════╗
║  AUTÔMATO RESULTANTE (AFN sem ε)                     ║
╚══════════════════════════════════════════════════════╝

         a           b
    q0 ────→ q2 ───────→ q3 (final)

Estados: {q0, q1, q2, q3, q4}
Iniciais: {q0}
Finais: {q3, q4}
Transições: {(q0,q2,a), (q2,q3,b)}
```

---

### 3️⃣ Conversão AFN → AFD (Construção de Subconjuntos)

**Objetivo:** Transformar autômato não-determinístico em determinístico.

#### Visualização Passo a Passo

```
╔══════════════════════════════════════════════════════╗
║  AFN ORIGINAL                                        ║
╚══════════════════════════════════════════════════════╝

         a         b
    q0 ───→ q1 ───→ q2 (final)
     │       │
     │ ε     │ ε
     ↓       ↓
    q3      q4

Alfabeto: {a, b}
Iniciais: {q0}
Finais: {q2}


╔══════════════════════════════════════════════════════╗
║  CONSTRUÇÃO DO AFD                                   ║
╚══════════════════════════════════════════════════════╝

INICIALIZAÇÃO:
━━━━━━━━━━━━━━
Estado inicial DFA = EpsClosure({q0}) = {q0, q3}
Nomeia como: S0
Fila = [S0]


ITERAÇÃO 1: Processar S0 = {q0, q3}
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

┌─────────────────────────────────────┐
│ Para símbolo 'a':                   │
├─────────────────────────────────────┤
│ De q0: GetTargets(q0, a) = {q1}     │
│ De q3: GetTargets(q3, a) = ∅        │
│                                     │
│ União: {q1}                         │
│ EpsClosure({q1}) = {q1, q4}         │
│                                     │
│ Novo estado: S1 = {q1, q4}          │
│ Transição: S0 --a--> S1             │
│                                     │
│ Adiciona S1 à fila                  │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ Para símbolo 'b':                   │
├─────────────────────────────────────┤
│ De q0: GetTargets(q0, b) = ∅        │
│ De q3: GetTargets(q3, b) = ∅        │
│                                     │
│ União: ∅                            │
│ (nenhuma transição)                 │
└─────────────────────────────────────┘

Estado da fila: [S1]


ITERAÇÃO 2: Processar S1 = {q1, q4}
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

┌─────────────────────────────────────┐
│ Para símbolo 'a':                   │
├─────────────────────────────────────┤
│ De q1: GetTargets(q1, a) = ∅        │
│ De q4: GetTargets(q4, a) = ∅        │
│                                     │
│ (nenhuma transição)                 │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│ Para símbolo 'b':                   │
├─────────────────────────────────────┤
│ De q1: GetTargets(q1, b) = {q2}     │
│ De q4: GetTargets(q4, b) = ∅        │
│                                     │
│ União: {q2}                         │
│ EpsClosure({q2}) = {q2}             │
│                                     │
│ Novo estado: S2 = {q2}              │
│ Transição: S1 --b--> S2             │
│                                     │
│ Adiciona S2 à fila                  │
└─────────────────────────────────────┘

Estado da fila: [S2]


ITERAÇÃO 3: Processar S2 = {q2}
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

S2 contém q2, que é final no AFN
Logo, S2 é final no AFD ✓

┌─────────────────────────────────────┐
│ Para símbolo 'a':                   │
│ GetTargets(q2, a) = ∅               │
│                                     │
│ Para símbolo 'b':                   │
│ GetTargets(q2, b) = ∅               │
└─────────────────────────────────────┘

Fila vazia → FIM


╔══════════════════════════════════════════════════════╗
║  AFD RESULTANTE                                      ║
╚══════════════════════════════════════════════════════╝

         a         b
    S0 ───→ S1 ───→ S2 (final)

Estados DFA:
  S0 = {q0, q3}  (inicial)
  S1 = {q1, q4}
  S2 = {q2}      (final)

Transições:
  S0 --a--> S1
  S1 --b--> S2

✓ Autômato totalmente determinístico!
```

---

### 4️⃣ Minimização de AFD (Hopcroft)

**Objetivo:** Reduzir AFD ao menor número de estados equivalentes.

#### Visualização do Processo

```
╔══════════════════════════════════════════════════════╗
║  AFD ORIGINAL                                        ║
╚══════════════════════════════════════════════════════╝

    q0 --a--> q1 --b--> q5 (final)
     │         │
     │ b       │ a
     ↓         ↓
    q2 --a--> q3 --b--> q5 (final)
     │         │
     │ b       │ a
     ↓         ↓
    q4 --a--> q4 --b--> q4

Estados: {q0, q1, q2, q3, q4, q5}
Finais: {q5}


╔══════════════════════════════════════════════════════╗
║  ETAPA 1: COMPLETAR AFD (adicionar DEAD)             ║
╚══════════════════════════════════════════════════════╝

Transições faltantes:
  q5 --a--> ? (indefinida)
  q5 --b--> ? (indefinida)

Cria estado DEAD:
  q5 --a--> DEAD
  q5 --b--> DEAD
  DEAD --a--> DEAD
  DEAD --b--> DEAD


╔══════════════════════════════════════════════════════╗
║  ETAPA 2: PARTIÇÃO INICIAL                           ║
╚══════════════════════════════════════════════════════╝

P = [ {q5}, {q0, q1, q2, q3, q4, DEAD} ]
      └─┬─┘  └──────────┬──────────────┘
      Finais        Não-Finais

W = [ {q5}, {q0, q1, q2, q3, q4, DEAD} ]


╔══════════════════════════════════════════════════════╗
║  ETAPA 3: REFINAMENTO                                ║
╚══════════════════════════════════════════════════════╝

ITERAÇÃO 1: Processar C = {q5}
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Para símbolo 'a':
  X = {estados que vão para q5 com 'a'} = ∅

Para símbolo 'b':
  X = {estados que vão para q5 com 'b'}
  = {q1, q3}  (verificar tabela de transições)

Dividir bloco {q0,q1,q2,q3,q4,DEAD}:
  Interseção: {q1, q3}
  Diferença:  {q0, q2, q4, DEAD}

Nova partição:
P = [ {q5}, {q1, q3}, {q0, q2, q4, DEAD} ]


ITERAÇÃO 2: Processar C = {q1, q3}
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Para símbolo 'a':
  q1 --a--> q1 (em bloco 1)
  q3 --a--> q3 (em bloco 1)
  (mesmo comportamento)

Para símbolo 'b':
  q1 --b--> q5 (em bloco 0)
  q3 --b--> q5 (em bloco 0)
  (mesmo comportamento)

Não divide!


ITERAÇÃO 3: Processar C = {q0, q2, q4, DEAD}
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Para símbolo 'a':
  q0 --a--> q1 (bloco 1)
  q2 --a--> q3 (bloco 1)
  q4 --a--> q4 (bloco 2)
  DEAD --a--> DEAD (bloco 2)

Divide em:
  {q0, q2}       (vão para bloco 1)
  {q4, DEAD}     (vão para bloco 2)

Partição final:
P = [ {q5}, {q1,q3}, {q0,q2}, {q4,DEAD} ]


╔══════════════════════════════════════════════════════╗
║  AFD MINIMIZADO                                      ║
╚══════════════════════════════════════════════════════╝

Estados minimizados:
  M0 = {q5}           (final)
  M1 = {q1, q3}
  M2 = {q0, q2}       (inicial)
  M3 = {q4, DEAD}

Transições:
  M2 --a--> M1
  M2 --b--> M3
  M1 --a--> M1
  M1 --b--> M0
  M3 --a--> M3
  M3 --b--> M3
  M0 --a--> M3
  M0 --b--> M3

Redução: 7 estados → 4 estados (-43%)
```

---

### 5️⃣ Simulação de Aceitação de Palavra

**Objetivo:** Verificar se uma palavra é aceita pelo autômato.

#### Exemplo Detalhado

```
╔══════════════════════════════════════════════════════╗
║  AUTÔMATO                                            ║
╚══════════════════════════════════════════════════════╝

         a         b
    q0 ───→ q1 ───→ q2 (final)
     │
     │ ε
     ↓
    q3 ───→ q4
         a

Palavra de entrada: "ab"


╔══════════════════════════════════════════════════════╗
║  SIMULAÇÃO PASSO A PASSO                             ║
╚══════════════════════════════════════════════════════╝

INICIALIZAÇÃO
━━━━━━━━━━━━━
Estados atuais = EpsClosure({q0})
               = {q0, q3}

┌────────────────────────────────┐
│  Posição na palavra: [⁰]ab    │
│  Estados atuais: {q0, q3}     │
└────────────────────────────────┘


LEITURA DO SÍMBOLO 'a' (posição 0)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

De q0 com 'a': GetTargets(q0, 'a') = {q1}
De q3 com 'a': GetTargets(q3, 'a') = {q4}

nextSet = {q1, q4}
Estados atuais = EpsClosure({q1, q4})
               = {q1, q4}

┌────────────────────────────────┐
│  Posição na palavra: a[¹]b    │
│  Estados atuais: {q1, q4}     │
└────────────────────────────────┘


LEITURA DO SÍMBOLO 'b' (posição 1)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

De q1 com 'b': GetTargets(q1, 'b') = {q2}
De q4 com 'b': GetTargets(q4, 'b') = ∅

nextSet = {q2}
Estados atuais = EpsClosure({q2})
               = {q2}

┌────────────────────────────────┐
│  Posição na palavra: ab[²]    │
│  Estados atuais: {q2}         │
│                   └── FINAL!  │
└────────────────────────────────┘


VERIFICAÇÃO FINAL
━━━━━━━━━━━━━━━━
Estados atuais ∩ Estados finais
= {q2} ∩ {q2}
= {q2} ≠ ∅

✅ PALAVRA ACEITA!


═══════════════════════════════════════════════════════

CONTRA-EXEMPLO: Palavra "ba"
━━━━━━━━━━━━━━━━━━━━━━━━━━━━

[⁰]ba  →  Estados: {q0, q3}
b[¹]a  →  GetTargets de q0 e q3 com 'b' = ∅
          nextSet = ∅
          Estados atuais = ∅

---
```
## 📋 Formato do Arquivo JSON

### Estrutura Completa

```json
{
  "alfabeto": ["a", "b"],
  "estados": ["q0", "q1", "q2"],
  "estadosI": ["q0"],
  "estadoF": ["q2"],
  "transicoes": [
    "q0", "q1", "a",
    "q1", "q2", "b",
    "q0", "q2", "&"
  ]
}
```

### Anatomia do JSON

```
┌──────────────────────────────────────────────────────┐
│  "alfabeto": ["a", "b"]                              │
│   └─┬─┘       └───┬───┘                              │
│   Chave      Array de símbolos                       │
│                                                      │
│  Símbolos válidos:                                   │
│  • Letras: "a", "b", "c", ...                        │
│  • Dígitos: "0", "1", "2", ...                       │
│  • Épsilon: "&" (transição vazia)                    │
└──────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────┐
│  "transicoes": [                                     │
│    "q0", "q1", "a",    ← Transição 1                 │
│     │     │     │                                    │
│     │     │     └─ Símbolo                           │
│     │     └─ Estado destino                          │
│     └─ Estado origem                                 │
│                                                      │
│    "q1", "q2", "b"     ← Transição 2                 │
│  ]                                                   │
│                                                      │
│  Representa:                                         │
│     q0 --a--> q1                                     │
│     q1 --b--> q2                                     │
└──────────────────────────────────────────────────────┘
```

### Exemplo Completo: AFN-ε

```json
{
  "alfabeto": ["0", "1"],
  "estados": ["q0", "q1", "q2", "q3"],
  "estadosI": ["q0"],
  "estadoF": ["q3"],
  "transicoes": [
    "q0", "q1", "&",
    "q0", "q2", "&",
    "q1", "q1", "0",
    "q1", "q3", "1",
    "q2", "q2", "1",
    "q2", "q3", "0"
  ]
}
```

**Representação Visual:**

```
         ε
    ┌────────→ q1 ─────┐
    │          ↺ 0     │ 1
   q0                   ↓
    │          ↺ 1     q3 (final)
    └────────→ q2 ─────┘
         ε         0
```

---

## 🎯 Exemplos Práticos

### Exemplo 1: Testando Palavras no Terminal

```bash
$ cd pascal
$ ./main ../data/automato.json

╔════════════════════════════════╗
║      MENU PRINCIPAL            ║
╠════════════════════════════════╣
║  1. Converter iniciais         ║
║  2. Remover épsilon            ║
║  3. AFN → AFD                  ║
║  4. Minimizar AFD              ║
║  5. Testar palavras            ║
║  6. Mostrar autômato           ║
║  0. Sair                       ║
╚════════════════════════════════╝

Escolha: 5

Testar palavras de (f)ile ou (t)erminal? [f/t]: t

Digite palavras ("sair" para terminar):

> ab
✓ ab → True (ACEITA)

> ba
✗ ba → False (REJEITADA)

> aab
✓ aab → True (ACEITA)

> (vazia)
✗ (vazia) → False (REJEITADA)

> sair
```

### Exemplo 2: Testando de Arquivo

**Conteúdo de `palavras_aceitas.txt`:**
```
ab
aab
aaab
abb
abbb
```

**Execução:**
```bash
Escolha: 5
Testar palavras de (f)ile ou (t)erminal? [f/t]: f
Caminho do arquivo: ../data/palavras_aceitas.txt

Resultados:
┌──────────┬──────────┐
│ Palavra  │ Aceita?  │
├──────────┼──────────┤
│ ab       │ ✓ True   │
│ aab      │ ✓ True   │
│ aaab     │ ✓ True   │
│ abb      │ ✗ False  │
│ abbb     │ ✗ False  │
└──────────┴──────────┘
```

### Exemplo 3: Fluxo Completo de Transformação

```
PASSO 1: Carregar AFN-ε
━━━━━━━━━━━━━━━━━━━━━━

Escolha: 6 (Mostrar autômato)

--- Automato ---
Alfabeto: a b
Estados: q0 q1 q2
Iniciais: q0 q1
Finais: q2
Transicoes:
  q0 --&--> q1
  q1 --a--> q2
  q0 --b--> q2


PASSO 2: Converter Múltiplos Iniciais
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Escolha: 1

✓ Novo inicial criado: Qi0

--- Automato ---
Iniciais: Qi0
Transicoes:
  Qi0 --&--> q0
  Qi0 --&--> q1
  q0 --&--> q1
  q1 --a--> q2
  q0 --b--> q2


PASSO 3: Remover Épsilon
━━━━━━━━━━━━━━━━━━━━━━━

Escolha: 2

✓ Removidas transições eps (&).

--- Automato ---
Transicoes:
  Qi0 --a--> q2
  Qi0 --b--> q2
  q0 --a--> q2
  q0 --b--> q2
  q1 --a--> q2


PASSO 4: Converter para AFD
━━━━━━━━━━━━━━━━━━━━━━━━

Escolha: 3

✓ Construído AFD por subconjuntos.

--- Automato ---
Estados: S0 S1
Iniciais: S0
Finais: S1
Transicoes:
  S0 --a--> S1
  S0 --b--> S1


PASSO 5: Minimizar AFD
━━━━━━━━━━━━━━━━━━━━━

Escolha: 4

✓ AFD minimizado (Hopcroft).

--- Automato ---
Estados: M0 M1
Iniciais: M0
Finais: M1
Transicoes:
  M0 --a--> M1
  M0 --b--> M1

✅ Autômato mínimo alcançado!
```
---

## 🔧 Troubleshooting

### ❌ Erro: "Can't open file 'u_types.pas'"

**Sintoma:**
```
Fatal: Can't open file "u_types.pas"
```

**Causa:** Compilando do diretório errado.

**Solução:**
```bash
# ✗ ERRADO (raiz do projeto)
$ fpc main.pas

# ✓ CORRETO (diretório pascal/)
$ cd pascal/
$ fpc main.pas
```

---

### ❌ Erro: "Segmentation fault"

**Sintoma:**
```bash
$ ./main
Segmentation fault (core dumped)
```

**Causas Comuns:**

1. **Array não inicializado**
   ```pascal
   var
     arr: TStrArray;
   begin
     arr[0] := 'valor';  // ✗ ERRO! Tamanho não definido
   end;
   ```

   **Correção:**
   ```pascal
   var
     arr: TStrArray;
   begin
     SetLength(arr, 10);  // ✓ Define tamanho primeiro
     arr[0] := 'valor';
   end;
   ```

2. **Acesso fora dos limites**
   ```pascal
   var
     arr: TStrArray;
   begin
     SetLength(arr, 5);
     arr[10] := 'x';  // ✗ ERRO! Índice 10 não existe
   end;
   ```

---

### ❌ Erro: String truncada em 255 caracteres

**Sintoma:**
Arquivo JSON grande não é lido corretamente.

**Causa:** Falta `{$H+}`.

**Solução:**
```pascal
{$mode fpc}{$H+}  // ✓ Adicionar no início de CADA unit

unit u_exemplo;
// ... resto do código
```

---

### ❌ Erro: "Arquivo não encontrado"

**Sintoma:**
```
Erro ao ler arquivo: data/automato.json
```

**Diagnóstico:**
```bash
# Verificar caminho atual
$ pwd
/home/user/LFA-Automato/pascal

# Verificar se arquivo existe
$ ls -la ../data/automato.json
-rw-r--r-- 1 user user 245 Oct 23 10:30 ../data/automato.json
```

**Soluções:**

```bash
# Opção 1: Usar caminho absoluto
$ ./main /home/user/LFA-Automato/data/automato.json

# Opção 2: Usar caminho relativo correto
$ ./main ../data/automato.json

# Opção 3: Executar do diretório raiz
$ cd ..
$ ./pascal/main data/automato.json
```

---

### ❌ Warning: "Function result variable does not seem to be initialized"

**Sintoma:**
```
Warning: Function result variable of a managed type does not seem to be initialized
```

**Causa:** Função não inicializa o retorno em todos os caminhos.

**Exemplo com Problema:**
```pascal
function BuscarEstado(const A: TStrArray; const S: AnsiString): LongInt;
var
  i: Integer;
begin
  for i := 0 to High(A) do
    if A[i] = S then
      BuscarEstado := i;  // ✗ E se não achar?
end;
```

**Correção:**
```pascal
function BuscarEstado(const A: TStrArray; const S: AnsiString): LongInt;
var
  i: Integer;
begin
  BuscarEstado := -1;  // ✓ Inicializa primeiro!
  
  for i := 0 to High(A) do
    if A[i] = S then
    begin
      BuscarEstado := i;
      Exit;  // ✓ Sai imediatamente
    end;
end;
```

---

## 📚 Referências e Recursos

### Livros

- **"Introduction to Automata Theory, Languages, and Computation"**  
  Hopcroft, Motwani, Ullman (3ª edição)

- **"Introdução à Teoria da Computação"**  
  Michael Sipser (tradução portuguesa)

### Documentação Online

- [Free Pascal Wiki](https://wiki.freepascal.org/)
- [Free Pascal Reference Guide](https://www.freepascal.org/docs-html/ref/ref.html)

### Ferramentas Úteis

| Ferramenta | Descrição | Link |
|------------|-----------|------|
| **JFLAP** | Simulador visual de autômatos | [jflap.org](http://www.jflap.org/) |
| **Graphviz** | Geração de diagramas de grafos | [graphviz.org](https://graphviz.org/) |
| **Automaton Simulator** | Simulador web interativo | [automatonsimulator.com](https://automatonsimulator.com/) |

---

## 🚀 Comandos Úteis

### Compilação Avançada

```bash
# Compilar com avisos detalhados
fpc -vw main.pas

# Compilar com otimização máxima
fpc -O3 main.pas

# Compilar com informações de debug
fpc -g main.pas

# Compilar e mostrar estatísticas
fpc -vl main.pas
```

### Limpeza de Arquivos

```bash
# Remover arquivos compilados
rm *.o *.ppu *.res main

# Ou criar um script de limpeza
cat > clean.sh << 'EOF'
#!/bin/bash
rm -f *.o *.ppu *.res link*.res main
echo "✓ Arquivos compilados removidos"
EOF

chmod +x clean.sh
./clean.sh
```

### Análise de Memória (Valgrind)

```bash
# Detectar vazamentos de memória
valgrind --leak-check=full ./main ../data/automato.json
```

## Compilação e Execução

## 🎓 Contribuindo

### Adicionando Novo Algoritmo

```
PASSO 1: Declarar em u_automaton.pas (interface)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

interface
  // ... funções existentes ...
  procedure MeuNovoAlgoritmo(var States: TStrArray; ...);


PASSO 2: Implementar em u_automaton.pas (implementation)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

implementation
  procedure MeuNovoAlgoritmo(var States: TStrArray; ...);
  begin
    // Código do algoritmo...
    WriteLn('Algoritmo executado!');
  end;


PASSO 3: Adicionar opção no menu (u_io.pas)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

procedure ShowMenu(...);
begin
  while True do
  begin
    WriteLn('7) Meu novo algoritmo');  // Nova opção
    case op of
      // ... casos existentes ...
      '7': MeuNovoAlgoritmo(states, ...);
    end;
  end;
end;


PASSO 4: Documentar no README.md
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Adicionar seção explicando o algoritmo com:
  • Objetivo
  • Pseudocódigo
  • Exemplo visual
  • Complexidade
```

Isso irá compilar o programa principal e todas as `units` associadas, gerando um executável chamado `main`.

## 📄 Licença

Este projeto foi desenvolvido para fins educacionais como parte da disciplina de **Linguagens Formais e Autômatos**.

---

## ✨ Agradecimentos

Desenvolvido com 💙 usando Free Pascal.

**Versão:** 2.0  
**Última atualização:** Outubro de 2025

---

<div align="center">

**[⬆ Voltar ao topo](#-simulador-de-autômatos-finitos-em-pascal)**

</div>
