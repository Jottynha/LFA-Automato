"""
Ferramenta para manipular autômatos LFA (AFN-&, AFN, AFD).
Visualização: usa NetworkX + Matplotlib. (Provisório)
Formato JSON esperado:
{
  "alfabeto": ["a","b"],
  "estados": ["q1","q2","qf"],
  "estadosI": ["q1","q2"],
  "estadoF": ["qf"],
  "transicoes": [["q1","q1","a"], ["q1","q2","&"], ["q2","qf","b"]]
}
'&' representa epsilon = vazio.
"""
from collections import defaultdict, deque # para filas e dicionários com valor default.
import json # para ler/escrever JSON.
import sys # para detectar plataforma.
import os # para manipulação de arquivos.
import subprocess # para abrir visualizador de imagens.
from typing import Set, Dict, Tuple # para estruturas de dados tipadas.

EPS = '&'

class Automaton:
    def __init__(self, alphabet: Set[str], states: Set[str], initials: Set[str], finals: Set[str],
                 transitions: Dict[Tuple[str,str], Set[str]]):
        self.alphabet = set(alphabet)
        self.states = set(states)
        self.initials = set(initials)
        self.finals = set(finals)
        # transitions: (state, symbol) -> set(states).
        self.transitions = defaultdict(set) # set padrão vazio.
        for (s,sym), targets in transitions.items():
            self.transitions[(s,sym)].update(targets) # copia os alvos.

    @classmethod # Método para verificar o json.
    def ler_json(cls, path: str):
        try:
            with open(path, 'r', encoding='utf-8') as f:
                data = json.load(f)
        except Exception as e:
            raise ValueError(f"Erro ao ler JSON: {e}")

        # Valida os valores.
        for k in ['alfabeto','estados','estadosI','estadoF','transicoes']:
            if k not in data:
                raise ValueError(f"Chave ausente no JSON: {k}")

        alphabet = set(data['alfabeto'])
        states = set(data['estados'])
        initials = set(data['estadosI'])
        finals = set(data['estadoF'])

        transitions_raw = data['transicoes']
        transitions = defaultdict(set)
        # Validação de Transição. (Tratamento de erro)
        for t in transitions_raw:
            if len(t) != 3:
                raise ValueError(f"Transição inválida (deve ter 3 elementos): {t}")
            a,b,c = t
            if a not in states:
                raise ValueError(f"Estado origem desconhecido na transição: {a}")
            if b not in states:
                raise ValueError(f"Estado destino desconhecido na transição: {b}")
            if c != EPS and c not in alphabet:
                raise ValueError(f"Símbolo de transição '{c}' não pertence ao alfabeto nem é '&'.")
            transitions[(a,c)].add(b)

        # Checagem básica de estado inicial.
        if not initials:
            raise ValueError("É necessário pelo menos um estado inicial.")
        return cls(alphabet, states, initials, finals, transitions)
    # Exportar para JSON (string)
    def to_json(self):
        trans_list = []
        for (s,sym), targets in self.transitions.items():
            for t in targets:
                trans_list.append([s,t,sym])
        return json.dumps({
            'alfabeto': sorted(list(self.alphabet)),
            'estados': sorted(list(self.states)),
            'estadosI': sorted(list(self.initials)),
            'estadoF': sorted(list(self.finals)),
            'transicoes': trans_list
        }, ensure_ascii=False, indent=2)

    # Outros utilitários (gerar estado único, adicionar estado, adicionar transição).
    def _unique_state(self, base='Qi'):
        i = 0
        while True:
            name = f"{base}{i}" if i>0 else base
            if name not in self.states:
                return name
            i += 1

    def add_state(self, q: str):
        self.states.add(q)

    def add_transition(self, src: str, dst: str, sym: str):
        if src not in self.states or dst not in self.states:
            raise ValueError(f"Transição usa estado desconhecido: {src} -> {dst}")
        if sym != EPS and sym not in self.alphabet:
            raise ValueError(f"Símbolo de transição '{sym}' não está no alfabeto")
        self.transitions[(src,sym)].add(dst)

    # Conversão múltiplos iniciais -> novo inicial com epsilon (1 no Menu).
    def converte_multiplos_iniciais_para_afn_eps(self):
        if len(self.initials) <= 1:
            print("Já há apenas um estado inicial. Nada feito.")
            return
        novo = self._unique_state('Qi')
        self.add_state(novo)
        # adiciona transições epsilon.
        for q in list(self.initials):
            self.add_transition(novo, q, EPS)
        self.initials = {novo}
        print(f"Novo estado inicial criado: {novo} com transições & para os antigos iniciais.")

    # Fecho epsilon (utilizado para remover epsilons em AFN->AFD). [http://www.decom.ufop.br/anderson/BCC242/AFN.pdf#:~:text=Para%20obter%20um%20AFN%20equivalente%20a%20um,e%20%E2%88%88%20Q%20e%20a%20%E2%88%88%20%CE%A3.]
    def epsilon_closure_of(self, states: Set[str]) -> Set[str]:
        stack = list(states)
        closure = set(states)
        while stack:
            s = stack.pop()
            for t in self.transitions.get((s,EPS), set()):
                if t not in closure:
                    closure.add(t)
                    stack.append(t)
        return closure

    # Remover epsilon transições (AFN-& -> AFN).
    def remove_epsilon(self):
        # novo conjunto de transições
        new_trans = defaultdict(set)
        new_finals = set(self.finals)
        for p in list(self.states):
            closure_p = self.epsilon_closure_of({p})
            # se qualquer estado no fechamento é final, p se torna final.
            if closure_p & self.finals:
                new_finals.add(p)
            # para cada símbolo não-epsilon, coleta destinos
            for sym in self.alphabet:
                dests = set()
                for q in closure_p:
                    dests |= self.transitions.get((q,sym), set())
                # fechar por epsilon depois
                dests_with_closure = set()
                for d in dests:
                    dests_with_closure |= self.epsilon_closure_of({d})
                if dests_with_closure:
                    new_trans[(p,sym)].update(dests_with_closure)

        self.transitions = new_trans
        self.finals = new_finals
        print("Removidas transições epsilon. Agora é um AFN sem '&'.")

    # NFA -> DFA (construção por subconjuntos). [https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton]
    def nfa_to_dfa(self):
        # se houver epsilons, tratar corretamente usando epsilon_closure. (tratamento de erro)
        start_closure = self.epsilon_closure_of(self.initials)
        queue = deque()
        queue.append(frozenset(start_closure))
        seen = set([frozenset(start_closure)])
        dfa_states = []
        dfa_trans = {}
        dfa_finals = set()

        while queue:
            T = queue.popleft()
            dfa_states.append(T)
            # é final se algum estado em T é final
            if set(T) & self.finals:
                dfa_finals.add(T)
            for sym in self.alphabet:
                # computa move(T, sym)
                move = set()
                for p in T:
                    move |= self.transitions.get((p,sym), set())
                # fechar por epsilon
                if move:
                    move = self.epsilon_closure_of(move)
                U = frozenset(move)
                dfa_trans[(T,sym)] = U
                if U and U not in seen:
                    seen.add(U)
                    queue.append(U)

        # renomear conjuntos para estados simbólicos
        mapping = {}
        for i, st in enumerate(dfa_states):
            mapping[st] = f"S{i}"
        # incluir estado morto (se necessário): se algum (T,sym) leva a frozenset()
        has_dead = any(dfa_trans.get((T,sym), frozenset())==frozenset() for T in dfa_states for sym in self.alphabet)
        if has_dead:
            dead = frozenset()
            if dead not in mapping:
                mapping[dead] = 'DEAD'
                dfa_states.append(dead)
        # construir novo automato determinístico
        new_states = set(mapping[st] for st in dfa_states)
        new_initials = {mapping[frozenset(start_closure)]}
        new_finals = set(mapping[st] for st in dfa_finals)
        new_trans = defaultdict(set)
        for (T,sym), U in dfa_trans.items():
            src = mapping[T]
            dst = mapping.get(U, mapping.get(frozenset(), 'DEAD'))
            new_trans[(src,sym)].add(dst)

        # se DEAD existir, adicionar transições self-loop para todos símbolos
        if 'DEAD' in new_states:
            for sym in self.alphabet:
                new_trans[('DEAD',sym)].add('DEAD')

        print("Construído AFD por subconjuntos.")
        return Automaton(self.alphabet, new_states, new_initials, new_finals, new_trans)

    # Minimização de DFA (Hopcroft).  [https://en.wikipedia.org/wiki/DFA_minimization]
    def minimize_dfa(self):
        # assume que o autômato é determinístico e completo (se não for, tente completá-lo primeiro).
        # construir tabela de transições determinísticas: state->sym->state.
        det = {}
        for s in self.states:
            det[s] = {}
            for sym in self.alphabet:
                dests = self.transitions.get((s,sym), set())
                if len(dests) > 1:
                    raise ValueError("Automato não é determinístico (mais de um destino). Minimize apenas AFDs completos.")
                det[s][sym] = next(iter(dests)) if dests else None
        # completar com estado morto se necessário.
        dead = self._unique_state('DEAD')
        need_dead = any(det[s][sym] is None for s in self.states for sym in self.alphabet)
        if need_dead:
            det[dead] = {sym: dead for sym in self.alphabet}
            for s in self.states:
                for sym in self.alphabet:
                    if det[s][sym] is None:
                        det[s][sym] = dead
            all_states = set(self.states) | {dead}
        else:
            all_states = set(self.states)
        # Hopcroft.
        P = [set(self.finals & all_states), set(all_states - self.finals)]
        P = [p for p in P if p]  # remover vazios.
        W = deque([p.copy() for p in P])

        while W:
            A = W.popleft()
            for c in self.alphabet:
                # X = {q | delta(q,c) in A}
                X = set(q for q in all_states if det[q][c] in A)
                newP = []
                for Y in P:
                    inter = Y & X
                    diff = Y - X
                    if inter and diff:
                        newP.append(inter)
                        newP.append(diff)
                        # substituir
                        if Y in W:
                            try:
                                W.remove(Y)
                            except ValueError:
                                pass
                            W.append(inter)
                            W.append(diff)
                        else:
                            if len(inter) <= len(diff):
                                W.append(inter)
                            else:
                                W.append(diff)
                    else:
                        newP.append(Y)
                P = newP

        # cada bloco de P é um estado do AFD minimizado
        block_map = {}
        for i, block in enumerate(P):
            name = f"M{i}"
            for s in block:
                block_map[s] = name

        new_states = set(block_map[s] for s in all_states)
        # inicial: mapear o(s) estado(s) inicial(is) — normalmente apenas 1
        if len(self.initials) == 1:
            new_initials = {block_map[next(iter(self.initials))]}
        else:
            new_initials = {block_map[s] for s in self.initials}
        new_finals = set(block_map[s] for s in all_states if s in self.finals)
        new_trans = defaultdict(set)
        for s in all_states:
            for sym in self.alphabet:
                new_trans[(block_map[s], sym)].add(block_map[det[s][sym]])

        print("AFD minimizado (Hopcroft).")
        return Automaton(self.alphabet, new_states, new_initials, new_finals, new_trans)

    # Teste de palavra.
    def accepts(self, word: str) -> bool:
        # decide se é AFN (usando epsilon-closure) ou AFD (assume determinístico se cada trans leva a único)
        # verificar se determinístico
        is_dfa = True
        for (s,sym), targets in self.transitions.items():
            if len(targets) > 1:
                is_dfa = False
                break
        if is_dfa and all(len(self.transitions.get((s,sym), set()))<=1 for s in self.states for sym in self.alphabet):
            # tratar como AFD
            # pegar inicial único
            if len(self.initials) != 1:
                raise ValueError("DFA deve ter exatamente 1 estado inicial para aceitar palavra deterministicamente.")
            cur = next(iter(self.initials))
            for ch in word:
                if ch not in self.alphabet:
                    raise ValueError(f"Símbolo '{ch}' não pertence ao alfabeto.")
                dests = self.transitions.get((cur,ch), set())
                if not dests:
                    return False
                cur = next(iter(dests))
            return cur in self.finals
        else:
            # tratar como AFN (com epsilon)
            current = self.epsilon_closure_of(self.initials)
            for ch in word:
                if ch not in self.alphabet:
                    raise ValueError(f"Símbolo '{ch}' não pertence ao alfabeto.")
                next_set = set()
                for p in current:
                    next_set |= self.transitions.get((p,ch), set())
                current = self.epsilon_closure_of(next_set)
            return bool(current & self.finals)


# Visualização com NetworkX + Matplotlib. (Teste)
def visualize_automaton_networkx(autom, out_filename='automato'):
    """
    Gera um PNG do autômato usando networkx + matplotlib.
    out_filename: sem extensão; ex: 'output/automato' -> gerará output/automato.png
    Retorna o caminho do arquivo gerado (.png).
    """
    # import local (só falhará aqui se pacotes ausentes)
    try:
        import networkx as nx
        import matplotlib
        # se não houver DISPLAY em sistemas unix, usa backend 'Agg' para poder salvar imagens
        if os.name != 'nt' and os.environ.get('DISPLAY','') == '':
            matplotlib.use('Agg')
        import matplotlib.pyplot as plt
    except Exception as e:
        raise RuntimeError("Para visualizar sem Graphviz instale: pip install networkx matplotlib. Erro: " + str(e))

    # montar grafo dirigido
    G = nx.DiGraph()
    for s in autom.states:
        G.add_node(s)

    # combinar rótulos de transições entre pares (src,dst) -> [syms]
    edges = {}
    for (src, sym), targets in autom.transitions.items():
        for dst in targets:
            edges.setdefault((src, dst), []).append(sym)

    for (src, dst), syms in edges.items():
        label = ','.join(sorted(syms))
        # guardamos label como atributo
        G.add_edge(src, dst, label=label)

    # posições: layout determinístico
    pos = nx.spring_layout(G, seed=42)

    # prepara figura
    plt.figure(figsize=(10, 6))
    ax = plt.gca()
    ax.set_axis_off()

    # nós: separar finais e não-finais
    finals = sorted([s for s in autom.states if s in autom.finals])
    normals = sorted([s for s in autom.states if s not in autom.finals])

    # desenhar nós normais
    if normals:
        nx.draw_networkx_nodes(G, pos, nodelist=normals, node_size=1000, node_color='white', edgecolors='black', linewidths=1)
    # desenhar nós finais com efeito double circle:
    # desenha círculos maiores (apenas borda) e depois os círculos internos preenchidos
    if finals:
        # outer (borda)
        nx.draw_networkx_nodes(G, pos, nodelist=finals, node_size=1400, node_color='white', edgecolors='black', linewidths=2)
        # inner (preenchido)
        nx.draw_networkx_nodes(G, pos, nodelist=finals, node_size=900, node_color='white', edgecolors='black', linewidths=1)

    # desenhar labels dos nós
    nx.draw_networkx_labels(G, pos, font_size=10)

    # desenhar arestas com setas
    nx.draw_networkx_edges(G, pos, arrows=True, connectionstyle='arc3,rad=0.1', arrowsize=20, width=1)

    # desenhar rótulos das arestas
    edge_labels = {(u,v): d['label'] for u,v,d in G.edges(data=True)}
    nx.draw_networkx_edge_labels(G, pos, edge_labels=edge_labels, font_size=9)

    # desenhar seta de início (nó invisível apontando para iniciais)
    # vamos desenhar uma seta manualmente em direção ao estado inicial (ou aos iniciais)
    initials = sorted(list(autom.initials))
    if initials:
        # colocar um ponto de início ligeiramente deslocado do primeiro inicial
        first_init = initials[0]
        x, y = pos[first_init]
        dx = x - 0.2
        dy = y + 0.2
        # desenhar uma seta sem nó de origem (linha com arrow)
        ax.annotate('', xy=(x,y), xytext=(dx,dy),
                    arrowprops=dict(arrowstyle='->', lw=1.5))
        # se houver vários inciais, desenhar setas adicionais pequenas
        for other in initials[1:]:
            x2, y2 = pos[other]
            dx2 = x2 - 0.2
            dy2 = y2 + 0.2
            ax.annotate('', xy=(x2,y2), xytext=(dx2,dy2),
                        arrowprops=dict(arrowstyle='->', lw=1.5))

    # garante diretório de saída
    dirname = os.path.dirname(out_filename)
    if dirname:
        os.makedirs(dirname, exist_ok=True)

    out_path = out_filename + '.png'
    plt.tight_layout()
    plt.savefig(out_path, dpi=200)
    plt.close()
    return os.path.abspath(out_path)


def open_file_with_default_app(path):
    """Abre o arquivo com o visualizador padrão do SO (cross-platform)."""
    try:
        if sys.platform.startswith('win'):
            os.startfile(path)
        elif sys.platform == 'darwin':
            subprocess.run(['open', path])
        else:
            # linux e similares
            subprocess.run(['xdg-open', path])
    except Exception:
        # falhar silenciosamente se não for possível abrir
        pass


# Funções de I/O e menu.
def salvar_em_json(autom: Automaton, caminho: str):
    with open(caminho, 'w', encoding='utf-8') as f:
        f.write(autom.to_json())
    print(f"Autômato salvo em {caminho}")

def carregar_automato_interativo(caminho: str) -> Automaton:
    try:
        autom = Automaton.ler_json(caminho)
        print(f"Autômato carregado de {caminho}")
        return autom
    except Exception as e:
        print(f"Erro: {e}")
        raise

def testar_palavras(autom: Automaton):
    modo = input("Testar palavras de (f)ile ou (t)erminal? [f/t]: ").strip().lower()
    if modo == 'f':
        caminho = input("Caminho do arquivo de palavras (uma por linha): ").strip()
        if not os.path.exists(caminho):
            print("Arquivo não encontrado.")
            return
        with open(caminho, 'r', encoding='utf-8') as f:
            for ln in f:
                w = ln.rstrip('\n\r')
                if w == '':
                    try:
                        print("(linha vazia) ->", autom.accepts(''))
                    except Exception as e:
                        print("(linha vazia) -> Erro:", e)
                else:
                    try:
                        print(f"{w} -> {autom.accepts(w)}")
                    except Exception as e:
                        print(f"{w} -> Erro: {e}")
    else:
        print("Digite uma palavra por linha (vazio para verificar palavra vazia). 'sair' para terminar.")
        while True:
            w = input('> ')
            if w.strip().lower() == 'sair':
                break
            try:
                print(autom.accepts(w))
            except Exception as e:
                print(f"Erro: {e}")

def menu_loop(autom: Automaton, caminho: str=None):
    while True:
        print('\n--- MENU ---')
        print('1) Converter múltiplos estados iniciais -> AFN-& (novo inicial com & para cada antigo)')
        print('2) Converter AFN-& para AFN (remover & )')
        print('3) Converter AFN para AFD (subconjuntos)')
        print('4) Minimizar AFD')
        print('5) Testar palavras (arquivo/terminal)')
        print('6) Salvar automato atual em JSON')
        print('7) Mostrar autômato atual (resumo)')
        print('8) Visualizar autômato em grafo (gera PNG e abre visualizador) [networkx/matplotlib]')
        print('0) Sair')
        op = input('Escolha: ').strip()
        try:
            if op == '1':
                autom.converte_multiplos_iniciais_para_afn_eps()
            elif op == '2':
                autom.remove_epsilon()
            elif op == '3':
                autom = autom.nfa_to_dfa()
            elif op == '4':
                autom = autom.minimize_dfa()
            elif op == '5':
                testar_palavras(autom)
            elif op == '6':
                caminho_salvar = input('Caminho para salvar JSON (enter para usar original): ').strip()
                if caminho_salvar == '' and caminho:
                    caminho_salvar = caminho
                elif caminho_salvar == '':
                    caminho_salvar = 'automato_out.json'
                salvar_em_json(autom, caminho_salvar)
            elif op == '7':
                print('Alfabeto:', self_print(autom.alphabet))
                print('Estados:', self_print(autom.states))
                print('Iniciais:', self_print(autom.initials))
                print('Finais:', self_print(autom.finals))
                print('Transições:')
                for (s,sym), targets in autom.transitions.items():
                    print(f"  {s} --{sym}--> {list(targets)}")
            elif op == '8':
                caminho_img = input('Nome/ caminho de saída (sem extensão) [automato]: ').strip()
                if caminho_img == '':
                    caminho_img = 'output/automato'
                try:
                    out = visualize_automaton_networkx(autom, out_filename=caminho_img)
                    print(f'Grafo gerado em: {out}')
                    open_file_with_default_app(out)
                except Exception as e:
                    print('Erro ao gerar/abrir grafo:', e)
            elif op == '0':
                print('Saindo...')
                break
            else:
                print('Opção inválida.')
        except Exception as e:
            print('Erro durante operação:', e)

def self_print(x):
    return ', '.join(sorted(list(x)))

# For convenience: se executar automatos.py diretamente cria um exemplo
if __name__ == '__main__':
    exemplo = {
        'alfabeto': ['a','b'],
        'estados': ['q1','q2','qf'],
        'estadosI': ['q1','q2'],
        'estadoF': ['qf'],
        'transicoes': [['q1','q1','a'], ['q1','q2','&'], ['q2','qf','b']]
    }
    os.makedirs('data', exist_ok=True)
    with open('data/automato.json','w',encoding='utf-8') as f:
        json.dump(exemplo,f,ensure_ascii=False,indent=2)
    print('Arquivo de exemplo "data/automato.json" criado. Rode main.py para começar.')
