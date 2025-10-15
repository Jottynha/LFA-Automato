# Projeto de Autômatos em Pascal (Disciplina de LFA)

Implementação de um manipulador de autômatos finitos (AFN, AFD, AFN-ε) em Pascal procedural puro. O programa é capaz de carregar um autômato a partir de um arquivo JSON, realizar uma série de transformações e testar a aceitação de palavras.

## Funcionalidades

*   Carregar um autômato de um arquivo `json`.
*   Converter múltiplos estados iniciais para um único estado inicial com transições épsilon (AFN-ε).
*   Remover transições épsilon (converter AFN-ε para AFN).
*   Converter um Autômato Finito Não Determinístico (AFN) para um Autômato Finito Determinístico (AFD).
*   Minimizar um Autômato Finito Determinístico (AFD).
*   Testar se palavras são aceitas pelo autômato, com entrada via terminal ou a partir de um arquivo.

## Pré-requisitos

Para compilar e executar este projeto, você precisa ter o compilador **Free Pascal (FPC)** instalado.

*   **Para instalar em sistemas baseados em Debian/Ubuntu:**
    ```bash
    sudo apt-get update
    sudo apt-get install fpc
    ```

## Estrutura dos Arquivos

*   `automato_app.pas`: O programa principal. Contém o laço de menu, a interação com o usuário e chama as funções de manipulação do autômato.
*   `automato_core.pas`: A unidade (unit) que contém toda a lógica principal, incluindo as estruturas de dados (`TAutomaton`), e os algoritmos para carregar, imprimir e transformar os autômatos.
*   `data/automato.json`: O arquivo de entrada que descreve o autômato a ser processado. O programa o carrega na inicialização.
*   `data/palavras_aceitas.txt`: Um arquivo de exemplo para testar a aceitação de múltiplas palavras de uma só vez.

## Como Compilar

O projeto é composto por um programa principal e uma unidade. O compilador Free Pascal lida com a compilação de ambos automaticamente quando você compila o arquivo principal.

1.  Abra um terminal na raiz do projeto.
2.  Execute o seguinte comando:

    ```bash
    fpc main.pas
    ```


## Como Executar

Após a compilação, você pode executar o programa diretamente pelo terminal.

1.  No mesmo terminal, execute o comando:

    ```bash
    ./main
    ```

2.  O programa carregará o autômato definido em `data/automato.json` e exibirá seu estado inicial.
3.  Um menu interativo será apresentado, permitindo que você escolha as operações de transformação ou teste de palavras. Siga as instruções na tela para usar o programa.
