% Declaração de predicados dinâmicos para permitir modificações durante a execução
% Isso permite alterar dinamicamente o estado do agente e do ambiente (corpo humano)
:- dynamic agente_local/1.
:- dynamic agente/1.
:- dynamic agente_interruptor/1.
:- dynamic celula/2.

% Definição inicial das células do corpo humano
% Cada célula pode ser normal (0), suspeita (1) ou cancerígena (2)
celula(corpo, celula1, s).
celula(corpo, celula2, s).
celula(corpo, celula3, s).
celula(corpo, celula4, s).
celula(corpo, celula5, s).
celula(corpo, celula6, s).
celula(corpo, celula7, s).
celula(corpo, celula8, s).
celula(corpo, celula9, s).

% Define as conexões entre as células (a adjacência aqui pode ser as conexões de células vizinhas)
% O agente pode se mover de uma célula para outra se elas forem adjacentes
adj(celula1, celula2).
adj(celula2, celula3).
adj(celula3, celula4).
adj(celula4, celula5).
adj(celula5, celula6).
adj(celula6, celula7).
adj(celula7, celula8).
adj(celula8, celula9).

% Define o estado inicial do agente (desligado) e sua posição inicial (celula1)
agente(desligado).
agente_local(celula1).

% Regra para gerar células com estado aleatório
% O valor 0 significa que a célula está normal, 1 que está suspeita e 2 que está cancerígena
:- celula(_, Celula, s),
   retract(celula(_, Celula, s)),
   random_between(0, 2, K),    % Gera um número aleatório 0 (normal), 1 (suspeita) ou 2 (cancerígena)
   assertz(celula(_, Celula, K)),
   fail.

% Função para iniciar a geração do ambiente e exibir a situação atual
gerar_ambiente :- print_ambiente, !.

% Função para exibir o estado das células
% Mostra na saída padrão quais células estão normais, suspeitas ou cancerígenas
print_ambiente :-
    celula(_, Celula, K),
    write(Celula), write(" -> "),
    (K = 2, writeln("Cancerígena") ;
     K = 1, writeln("Suspeita") ;
     K = 0, writeln("Normal")),
    fail.
print_ambiente.

% Implementação de uma busca em largura (BFS) para encontrar todas as células acessíveis
% BFS é usado para navegar pelo ambiente e determinar onde o agente deve ir
bfs(Origem, Descobertos) :- bfs_queue([Origem], [], Descobertos).

% Caso base da BFS: se a fila estiver vazia, retorna os vértices descobertos
bfs_queue([], Descobertos, Descobertos).

% Expande os vértices adjacentes e continua a busca
bfs_queue([Vertice | Fila], Visitado, Descobertos) :-
    findall(Adj, ((adj(Vertice, Adj); adj(Adj, Vertice)), \+ member(Adj, Visitado)), AdjVertices),
    append(Fila, AdjVertices, NovaFila),
    append(Visitado, AdjVertices, NovoVisitado),
    bfs_queue(NovaFila, NovoVisitado, Descobertos).

% Filtra a lista de células para encontrar apenas as que estão suspeitas ou cancerígenas
% Isso garante que o agente só se mova para locais que precisam de análise
filtra_lista(Lista, Filtered) :- include(esta_suspeita_ou_cancer, Lista, Filtered).
esta_suspeita_ou_cancer(X) :- celula(_, X, 1); celula(_, X, 2).
celulas_suspeitas_ou_cancer(Origem, Filtrados) :- bfs(Origem, ListaTodos),
                                                  filtra_lista(ListaTodos, Filtrados).

% Obtém o primeiro elemento de uma lista
get_head([H | _], H).

% Obtém a cauda da lista (todos os elementos menos o primeiro)
get_tail([H], [H]).
get_tail([_ | T], T).

% Alterna o estado do agente entre ligado e desligado
agente_interruptor:- agente(ligado),
                    retract(agente(ligado)),
                    asserta(agente(desligado)),
                    write("O agente está desligado"), !.

agente_interruptor:- agente(desligado),
                    retract(agente(desligado)),
                    asserta(agente(ligado)),
                    write("O agente está ligado"), !.

% Regras para identificar e marcar uma célula como cancerígena
% O agente deve estar ligado para que a identificação ocorra
agente_identificar :- agente(ligado),
                      agente_local(Celula),
                      celula(_, Celula, 2),
                      write("Célula cancerígena identificada em: "), writeln(Celula), !.
agente_identificar.

% Movimento do agente para uma célula adjacente
% O agente só pode se mover para locais diretamente conectados ao aposento atual
agente_mover(Origem, Dest) :- agente(ligado),
                               writeln(""),
                               writeln("Entrando em movimento..."),
                               (adj(Origem, Dest) ; adj(Dest, Origem)),
                               retract(agente_local(Origem)),
                               asserta(agente_local(Dest)),
                               format("Agente se moveu de ~w para ~w", [Origem, Dest]), nl.

% Regra principal para iniciar a rotina de exploração e identificação
% O agente verifica quais células precisam ser identificadas e segue uma ordem para analisá-las
agente_iniciar_rotina :- agente(ligado),
                         celula(_, _, 1); celula(_, _, 2), % Procura por células suspeitas ou cancerígenas
                         agente_identificar,
                         agente_local(Origem),
                         celulas_suspeitas_ou_cancer(Origem, CelulasSuspeitas),
                         CelulasSuspeitas \= [],
                         write("Células suspeitas ou cancerígenas: "), writeln(CelulasSuspeitas),
                         get_head(CelulasSuspeitas, Alvo),
                         path(Origem, Alvo, CaminhoTmp),
                         write("Caminho: "), writeln(CaminhoTmp),
                         get_tail(CaminhoTmp, Caminho),
                         get_head(Caminho, Dest),
                         agente_mover(Origem, Dest),
                         agente_iniciar_rotina.

% Caso em que a análise foi concluída
% Exibe uma mensagem informando que todos os exames foram realizados
agente_iniciar_rotina :- agente(ligado),
                         celula(_, _, 0),
                         writeln("\nANÁLISE COMPLETA!"),
                         writeln("Ambiente atual:"),
                         print_ambiente, !.

% Encontra o caminho entre duas células
path(Origem, Dest, Caminho) :-
    path_helper(Origem, Dest, [Origem], Caminho).
path_helper(Dest, Dest, Acc, Caminho) :-
    reverse(Acc, Caminho).
path_helper(Current, Dest, Acc, Caminho) :-
    (adj(Current, Next); adj(Next, Current)),
    \+ member(Next, Acc),
    path_helper(Next, Dest, [Next|Acc], Caminho).
