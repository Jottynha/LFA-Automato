# Script para visualizar autômato a partir de JSON. Uso: python visualize.py <caminho_json> [caminho_saida_png] 
# Se caminho_saida_png não for fornecido, usa 'output/automato.png'

import sys
import os
from automato import Automaton, visualize_automaton_networkx, open_file_with_default_app

def main():
    if len(sys.argv) < 2:
        print("Uso: python visualize.py <caminho_json> [caminho_saida_png]")
        sys.exit(1)
    
    json_path = sys.argv[1]
    
    # Verifica se o arquivo existe
    if not os.path.exists(json_path):
        print(f"Erro: Arquivo não encontrado: {json_path}")
        sys.exit(1)
    
    # Define o caminho de saída (sem extensão .png)
    if len(sys.argv) >= 3:
        out_path = sys.argv[2]
        # Remove .png se o usuário incluiu
        if out_path.endswith('.png'):
            out_path = out_path[:-4]
    else:
        out_path = 'output/automato'
    
    try:
        # Carrega o autômato do JSON
        print(f"Carregando autômato de: {json_path}")
        autom = Automaton.ler_json(json_path)
        
        # Gera a visualização
        print(f"Gerando visualização...")
        png_path = visualize_automaton_networkx(autom, out_path)
        
        print(f"Visualização salva em: {png_path}")
        
        # Tenta abrir com visualizador padrão
        print("Abrindo visualizador de imagens...")
        open_file_with_default_app(png_path)
        
    except Exception as e:
        print(f"Erro ao processar autômato: {e}")
        sys.exit(1)

if __name__ == '__main__':
    main()
