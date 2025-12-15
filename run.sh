#!/bin/bash
# Script para facilitar a execução do simulador de autômatos

# Cores para output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Diretório base
BASE_DIR="/home/joao/Projetos/6º Periodo/LFA-Automato"

# Função para compilar
compile() {
    echo -e "${BLUE}📦 Compilando o projeto...${NC}"
    cd "$BASE_DIR/pascal"
    if fpc -Mobjfpc main.pas 2>&1 | tail -5; then
        echo -e "${GREEN}✅ Compilação bem-sucedida!${NC}"
        return 0
    else
        echo -e "${RED}❌ Erro na compilação!${NC}"
        return 1
    fi
}

# Função para executar
run() {
    local file=$1
    cd "$BASE_DIR"
    
    if [ ! -f "pascal/main" ]; then
        echo -e "${YELLOW}⚠️  Executável não encontrado. Compilando...${NC}"
        compile || return 1
    fi
    
    echo -e "${GREEN}🚀 Executando com arquivo: ${file}${NC}"
    echo ""
    ./pascal/main "$file"
}

# Função para listar arquivos disponíveis
list_files() {
    echo -e "${BLUE}📋 Arquivos JSON disponíveis:${NC}"
    echo ""
    local i=1
    for file in "$BASE_DIR/data/"*.json; do
        local basename=$(basename "$file")
        local tipo=""
        
        case "$basename" in
            *"afd"*) tipo="[AFD]" ;;
            *"afn_epsilon"*) tipo="[AFN-ε]" ;;
            *"afn"*) tipo="[AFN]" ;;
            *"multiplos"*) tipo="[Múltiplos Iniciais]" ;;
            *) tipo="[?]" ;;
        esac
        
        echo -e "  ${YELLOW}$i)${NC} $tipo $basename"
        i=$((i+1))
    done
    echo ""
}

# Função para menu interativo
interactive() {
    list_files
    
    echo -e "${BLUE}Escolha um arquivo (número) ou digite o caminho completo:${NC}"
    read -r choice
    
    if [[ "$choice" =~ ^[0-9]+$ ]]; then
        # É um número
        local files=("$BASE_DIR/data/"*.json)
        local index=$((choice - 1))
        
        if [ $index -ge 0 ] && [ $index -lt ${#files[@]} ]; then
            run "${files[$index]}"
        else
            echo -e "${RED}❌ Opção inválida!${NC}"
        fi
    else
        # É um caminho
        if [ -f "$choice" ]; then
            run "$choice"
        elif [ -f "$BASE_DIR/data/$choice" ]; then
            run "$BASE_DIR/data/$choice"
        else
            echo -e "${RED}❌ Arquivo não encontrado: $choice${NC}"
        fi
    fi
}

# Função para ajuda
show_help() {
    echo -e "${BLUE}🔧 Simulador de Autômatos Finitos${NC}"
    echo ""
    echo "Uso: $0 [opção] [arquivo]"
    echo ""
    echo "Opções:"
    echo "  -c, --compile       Apenas compila o projeto"
    echo "  -r, --run <arquivo> Executa com arquivo específico"
    echo "  -l, --list          Lista arquivos disponíveis"
    echo "  -i, --interactive   Modo interativo (padrão)"
    echo "  -h, --help          Mostra esta ajuda"
    echo ""
    echo "Exemplos:"
    echo "  $0                           # Modo interativo"
    echo "  $0 -r afd_simples.json       # Executa arquivo específico"
    echo "  $0 -r data/afn_epsilon.json  # Com caminho relativo"
    echo ""
    echo "Arquivos de exemplo disponíveis:"
    echo "  • afd_simples.json         - AFD básico (reconhece 0*1)"
    echo "  • afn_simples.json         - AFN com não-determinismo"
    echo "  • afn_epsilon.json         - AFN com transições ε"
    echo "  • multiplos_iniciais.json  - Múltiplos estados iniciais"
    echo "  • afn_epsilon_complexo.json - AFN-ε mais elaborado"
    echo "  • afd_minimizavel.json     - AFD que pode ser minimizado"
    echo ""
}

# Main
case "$1" in
    -c|--compile)
        compile
        ;;
    -r|--run)
        if [ -z "$2" ]; then
            echo -e "${RED}❌ Especifique um arquivo!${NC}"
            exit 1
        fi
        
        if [ -f "$2" ]; then
            run "$2"
        elif [ -f "$BASE_DIR/data/$2" ]; then
            run "$BASE_DIR/data/$2"
        else
            echo -e "${RED}❌ Arquivo não encontrado: $2${NC}"
            exit 1
        fi
        ;;
    -l|--list)
        list_files
        ;;
    -h|--help)
        show_help
        ;;
    -i|--interactive|"")
        interactive
        ;;
    *)
        echo -e "${RED}❌ Opção inválida: $1${NC}"
        echo ""
        show_help
        exit 1
        ;;
esac
