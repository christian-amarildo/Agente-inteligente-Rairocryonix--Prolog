% Declaração de predicados dinâmicos
:- dynamic agente_local/1.
:- dynamic agente/1.
:- dynamic agente_interruptor/1.
:- dynamic celula/2.
:- dynamic celula_visitada/1.

% Definição inicial das células
celula(corpo, celula1, s).
celula(corpo, celula2, s).
celula(corpo, celula3, s).
celula(corpo, celula4, s).
celula(corpo, celula5, s).
celula(corpo, celula6, s).
celula(corpo, celula7, s).
celula(corpo, celula8, s).
celula(corpo, celula9, s).

% Conexões entre as células
adj(celula1, celula2).
adj(celula2, celula3).
adj(celula3, celula4).
adj(celula4, celula5).
adj(celula5, celula6).
adj(celula6, celula7).
adj(celula7, celula8).
adj(celula8, celula9).

% Estado inicial do agente
agente(desligado).
agente_local(celula1).

% Geração aleatória do estado das células
:- celula(_, Celula, s),
   retract(celula(_, Celula, s)),
   random_between(0, 2, K),
   assertz(celula(_, Celula, K)),
   fail.

% Função para iniciar a geração do ambiente
gerar_ambiente :- print_ambiente, !.

% Função para exibir o estado das células
print_ambiente :-
    celula(_, Celula, K),
    write(Celula), write(" -> "),
    (K = 2, writeln("Cancerígena") ;
     K = 1, writeln("Suspeita") ;
     K = 0, writeln("Normal")),
    fail.
print_ambiente.

% Busca em largura (BFS)
bfs(Origem, Descobertos) :- bfs_queue([Origem], [], Descobertos).
bfs_queue([], Descobertos, Descobertos).
bfs_queue([Vertice | Fila], Visitado, Descobertos) :-
    findall(Adj, ((adj(Vertice, Adj); adj(Adj, Vertice)), \+ member(Adj, Visitado)), AdjVertices),
    append(Fila, AdjVertices, NovaFila),
    append(Visitado, AdjVertices, NovoVisitado),
    bfs_queue(NovaFila, NovoVisitado, Descobertos).

% Filtra células suspeitas ou cancerígenas
filtra_lista(Lista, Filtered) :- include(esta_suspeita_ou_cancer, Lista, Filtered).
esta_suspeita_ou_cancer(X) :- celula(_, X, 1); celula(_, X, 2).
celulas_suspeitas_ou_cancer(Origem, Filtrados) :- bfs(Origem, ListaTodos),
                                                  filtra_lista(ListaTodos, Filtrados).

% Alterna o estado do agente
agente_interruptor:- agente(ligado),
                    retract(agente(ligado)),
                    asserta(agente(desligado)),
                    write("O agente está desligado"), !.

agente_interruptor:- agente(desligado),
                    retract(agente(desligado)),
                    asserta(agente(ligado)),
                    write("O agente está ligado"), !.

% Identificação de células cancerígenas
agente_identificar :- agente(ligado),
                      agente_local(Celula),
                      celula(_, Celula, 2),
                      write("Célula cancerígena identificada em: "), writeln(Celula), !.
agente_identificar.

% Movimento do agente
agente_mover(Origem, Dest) :- agente(ligado),
                               writeln(""),
                               writeln("Entrando em movimento..."),
                               (adj(Origem, Dest) ; adj(Dest, Origem)),
                               retract(agente_local(Origem)),
                               asserta(agente_local(Dest)),
                               asserta(celula_visitada(Dest)),
                               format("Agente se moveu de ~w para ~w", [Origem, Dest]), nl.

% Rotina de exploração
agente_iniciar_rotina :- agente(ligado),
                         celula(_, _, 1); celula(_, _, 2),
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

% Conclusão da análise
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