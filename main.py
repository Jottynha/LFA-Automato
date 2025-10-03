"""
Entrada principal do projeto.
Responsável por carregar o autômato e iniciar o loop do menu.
-> python main.py [caminho_para_automato]
-> Se nenhum caminho for fornecido, tenta carregar de data/automato.json
"""
import os
import sys
from automato import carregar_automato_interativo, menu_loop

DEFAULT_PATH = os.path.join('data', 'automato.json')

if __name__ == '__main__':
    caminho = DEFAULT_PATH 
    if len(sys.argv) > 1:
        caminho = sys.argv[1]
    if not os.path.exists(caminho):
        print(f"Arquivo não encontrado: {caminho}")
        print("Coloque seu automato na pasta data com nome 'automato.json' ou passe o caminho como argumento.")
        sys.exit(1)

    autom = carregar_automato_interativo(caminho)
    menu_loop(autom, caminho)
