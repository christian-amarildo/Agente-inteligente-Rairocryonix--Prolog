% ----------------------------------------------------------
% Declaração de predicados dinâmicos para permitir modificações durante a execução
% Isso possibilita que o estado de cada predicado possa ser alterado no decorrer do programa
% ----------------------------------------------------------
:- dynamic agente_local/1.         % Permite a modificação dinâmica do local do agente
:- dynamic agente/1.               % Permite a modificação dinâmica do estado (ligado/desligado) do agente
:- dynamic agente_interruptor/1.   % (Reservado para eventual uso de um interruptor externo do agente)
:- dynamic celula/2.               % Permite a modificação dinâmica do estado de cada célula do corpo

% ----------------------------------------------------------
% Definição inicial das células do corpo humano
% Cada célula é descrita por: celula(corpo, NomeDaCelula, Estado).
% O Estado "s" aqui significa que a célula ainda não foi definida (será substituído por 0, 1 ou 2 aleatoriamente).
% Onde 0 -> célula normal, 1 -> célula suspeita e 2 -> célula cancerígena.
% ----------------------------------------------------------
celula(corpo, celula1, s).  % Célula 1 está marcada como "s" (indefinida) a princípio
celula(corpo, celula2, s).  % Célula 2 está marcada como "s" (indefinida) a princípio
celula(corpo, celula3, s).  % Célula 3 está marcada como "s" (indefinida) a princípio
celula(corpo, celula4, s).  % Célula 4 está marcada como "s" (indefinida) a princípio
celula(corpo, celula5, s).  % Célula 5 está marcada como "s" (indefinida) a princípio
celula(corpo, celula6, s).  % Célula 6 está marcada como "s" (indefinida) a princípio
celula(corpo, celula7, s).  % Célula 7 está marcada como "s" (indefinida) a princípio
celula(corpo, celula8, s).  % Célula 8 está marcada como "s" (indefinida) a princípio
celula(corpo, celula9, s).  % Célula 9 está marcada como "s" (indefinida) a princípio

% ----------------------------------------------------------
% Define as conexões entre as células (adjacências).
% O agente pode se movimentar entre duas células se elas forem adjacentes.
% ----------------------------------------------------------
adj(celula1, celula2).  % Célula 1 é adjacente à célula 2
adj(celula2, celula3).  % Célula 2 é adjacente à célula 3
adj(celula3, celula4).  % Célula 3 é adjacente à célula 4
adj(celula4, celula5).  % Célula 4 é adjacente à célula 5
adj(celula5, celula6).  % Célula 5 é adjacente à célula 6
adj(celula6, celula7).  % Célula 6 é adjacente à célula 7
adj(celula7, celula8).  % Célula 7 é adjacente à célula 8
adj(celula8, celula9).  % Célula 8 é adjacente à célula 9

% ----------------------------------------------------------
% Define o estado inicial do agente e sua posição.
% agente/1 indica se está 'desligado' ou 'ligado'.
% agente_local/1 indica em qual célula o agente se encontra.
% ----------------------------------------------------------
agente(desligado).        % No início, o agente está desligado
agente_local(celula1).    % O agente começa na célula 1

% ----------------------------------------------------------
% Regra para gerar células com estado aleatório ao carregar o arquivo.
% Para cada célula marcada com 's', remove-se essa cláusula e insere-se uma nova
% com estado 0 (normal), 1 (suspeita) ou 2 (cancerígena), conforme número aleatório.
% O 'fail' ao final força o Prolog a buscar todas as soluções, aplicando o random
% em cada célula que inicialmente estava com 's'.
% ----------------------------------------------------------
:- celula(_, Celula, s),            % Se existir alguma célula com estado 's'...
   retract(celula(_, Celula, s)),   % ...remova esta cláusula...
   random_between(0, 2, K),         % ...gere um valor aleatório entre 0 e 2...
   assertz(celula(_, Celula, K)),   % ...e insira a célula com o novo valor (K).
   fail.                            % 'fail' força o backtracking para aplicar o processo em todas as células


% Função para iniciar a geração do ambiente e exibir a situação atual
gerar_ambiente :- print_ambiente, !.

% ------------------------------------------------------------------------------
% Função para iniciar a geração do ambiente e exibir a situação atual
% ------------------------------------------------------------------------------
gerar_ambiente :-          % Define o predicado 'gerar_ambiente' sem argumentos
    print_ambiente,        % Chama o predicado 'print_ambiente' para exibir o estado das células
    !.                     % O corte (!) evita backtracking após a primeira solução

% -------------------------------------------------------------------------------
% Função para exibir o estado das células
% Mostra no terminal quais células estão normais, suspeitas ou cancerígenas
% -------------------------------------------------------------------------------
print_ambiente :-                      % Define o predicado 'print_ambiente' sem argumentos
    celula(_, Celula, K),             % Verifica para cada célula (desconsiderando o primeiro argumento "corpo") o seu estado K
    write(Celula), write(" -> "),     % Escreve o nome da célula no terminal seguido de " -> "
    (K = 2, writeln("Cancerígena") ;  % Se K=2, escreve "Cancerígena"
     K = 1, writeln("Suspeita") ;     % Se K=1, escreve "Suspeita"
     K = 0, writeln("Normal")),       % Se K=0, escreve "Normal"
    fail.                             % O 'fail' força o Prolog a buscar mais soluções para 'celula(_, Celula, K)'
print_ambiente.                        % Caso não haja mais células, essa cláusula vazia finaliza o predicado

% ------------------------------------------------------------------------------
% Implementação de uma busca em largura (BFS) para encontrar todas as células acessíveis
% O BFS é usado para navegar pelo ambiente e determinar onde o agente pode ir
% ------------------------------------------------------------------------------
bfs(Origem, Descobertos) :-     % Define o predicado 'bfs' com origem e lista final de descobertos
    bfs_queue([Origem], [], Descobertos).  % Chama o auxiliar 'bfs_queue' iniciando a fila com o nó de origem

% ------------------------------------------------------------------------------
% Caso base da BFS: se a fila estiver vazia, retornamos a lista de vértices descobertos
% ------------------------------------------------------------------------------
bfs_queue([], Descobertos, Descobertos).  % Se a lista de vértices na fila estiver vazia, usamos os vértices visitados

% ------------------------------------------------------------------------------
% Expande os vértices adjacentes e continua a busca em largura
% ------------------------------------------------------------------------------
bfs_queue([Vertice | Fila], Visitado, Descobertos) :-           % Pega o primeiro vértice da fila
    findall(Adj, ((adj(Vertice, Adj); adj(Adj, Vertice)),       % Encontramos todos os vértices adjacentes (bidirecionais)
                  \+ member(Adj, Visitado)), AdjVertices),      % Garante que ainda não tenham sido visitados
    append(Fila, AdjVertices, NovaFila),                        % Anexa os novos vértices à fila
    append(Visitado, AdjVertices, NovoVisitado),                % Atualiza a lista de visitados com os recém-encontrados
    bfs_queue(NovaFila, NovoVisitado, Descobertos).             % Continua a busca com a fila e visitados atualizados

% ------------------------------------------------------------------------------
% Filtra a lista de células para encontrar apenas as que estão suspeitas (1) ou cancerígenas (2)
% ------------------------------------------------------------------------------
filtra_lista(Lista, Filtered) :-        % Define o predicado 'filtra_lista' que recebe a lista total e retorna uma lista filtrada
    include(esta_suspeita_ou_cancer,    % 'include' aplica um predicado de filtragem a cada elemento
            Lista,                      % Aplica sobre a lista de células
            Filtered).                  % Retorna apenas as que cumprem o critério

% ------------------------------------------------------------------------------
% Critério de filtragem: retorna true se a célula for suspeita (1) ou cancerígena (2)
% ------------------------------------------------------------------------------
esta_suspeita_ou_cancer(X) :- celula(_, X, 1); celula(_, X, 2). % Verifica se X é uma célula de estado 1 ou 2

% ------------------------------------------------------------------------------
% Obtém as células suspeitas ou cancerígenas acessíveis a partir de uma origem
% ------------------------------------------------------------------------------
celulas_suspeitas_ou_cancer(Origem, Filtrados) :-  % Define o predicado que encontra células suspeitas/cancerígenas
    bfs(Origem, ListaTodos),                       % Primeiro, faz BFS para obter todas as células alcançáveis a partir de Origem
    filtra_lista(ListaTodos, Filtrados).           % Em seguida, filtra apenas as suspeitas ou cancerígenas

% ------------------------------------------------------------------------------
% Obtém o primeiro elemento de uma lista
% ------------------------------------------------------------------------------
get_head([H | _], H).        % 'get_head' retorna o primeiro elemento de uma lista

% ------------------------------------------------------------------------------
% Obtém a cauda da lista (todos os elementos menos o primeiro)
% ------------------------------------------------------------------------------
get_tail([H], [H]).          % Caso a lista contenha somente um elemento, a cauda retorna a própria lista
get_tail([_ | T], T).        % Caso a lista tenha mais de um elemento, a cauda é T

% ------------------------------------------------------------------------------
% Alterna o estado do agente entre ligado e desligado
% ------------------------------------------------------------------------------
agente_interruptor :-                  % Define o predicado 'agente_interruptor' sem argumentos
    agente(ligado),                    % Verifica se o agente está atualmente ligado
    retract(agente(ligado)),          % Remove o fato de que o agente está ligado
    asserta(agente(desligado)),       % Informa que o agente está agora desligado
    write("O agente está desligado"), % Mensagem ao usuário
    !.                                % Corte para não tentar a próxima regra

agente_interruptor :-                  % Outra cláusula para o mesmo predicado
    agente(desligado),                % Verifica se o agente está atualmente desligado
    retract(agente(desligado)),       % Remove o fato de que o agente está desligado
    asserta(agente(ligado)),          % Informa que o agente está agora ligado
    write("O agente está ligado"),    % Mensagem ao usuário
    !.                                % Corte para não tentar a próxima regra

% ------------------------------------------------------------------------------
% Regras para identificar e marcar uma célula como cancerígena
% O agente deve estar ligado para que a identificação ocorra
% ------------------------------------------------------------------------------
agente_identificar :-                         % Define o predicado 'agente_identificar'
    agente(ligado),                           % O agente precisa estar ligado
    agente_local(Celula),                     % Obtém a célula em que o agente está
    celula(_, Celula, 2),                     % Verifica se essa célula tem estado 2 (cancerígena)
    write("Célula cancerígena identificada em: "),  % Mensagem informativa
    writeln(Celula),                          % Escreve qual célula foi identificada
    !.                                        % Corte para não tentar a próxima cláusula

agente_identificar.                           % Se a célula não for cancerígena ou o agente não estiver ligado, não faz nada


% ------------------------------------------------------------------------------
% Movimento do agente para uma célula adjacente
% O agente só pode se mover para locais diretamente conectados ao aposento atual
% ------------------------------------------------------------------------------
agente_mover(Origem, Dest) :-            % Define o predicado 'agente_mover' com origem e destino
    agente(ligado),                      % Verifica se o agente está ligado (condição para movimentar)
    writeln(""),                         % Imprime uma linha em branco para organizar a saída
    writeln("Entrando em movimento..."), % Mensagem indicando que o agente vai se mover
    (adj(Origem, Dest) ; adj(Dest, Origem)), % Verifica se Origem e Dest são adjacentes (bidirecional)
    retract(agente_local(Origem)),       % Remove o fato de que o agente estava em Origem
    asserta(agente_local(Dest)),         % Declara que agora o agente está em Dest
    format("Agente se moveu de ~w para ~w", [Origem, Dest]), nl.  % Mensagem de confirmação do movimento

% ------------------------------------------------------------------------------
% Regra principal para iniciar a rotina de exploração e identificação
% O agente verifica quais células precisam ser identificadas e segue uma ordem para analisá-las
% ------------------------------------------------------------------------------
agente_iniciar_rotina :-                 % Define o predicado 'agente_iniciar_rotina' sem argumentos
    agente(ligado),                      % Garante que o agente esteja ligado
    celula(_, _, 1); celula(_, _, 2),    % Verifica se há pelo menos uma célula suspeita (1) ou cancerígena (2)
    agente_identificar,                  % Tenta identificar uma célula cancerígena no local atual
    agente_local(Origem),                % Obtém a célula em que o agente se encontra atualmente
    celulas_suspeitas_ou_cancer(Origem, CelulasSuspeitas),   % Obtém a lista de células suspeitas/cancerígenas acessíveis
    CelulasSuspeitas \= [],             % Garante que a lista não está vazia
    write("Células suspeitas ou cancerígenas: "), writeln(CelulasSuspeitas), % Imprime a lista de células de interesse
    get_head(CelulasSuspeitas, Alvo),    % Pega o primeiro alvo (célula) na lista
    path(Origem, Alvo, CaminhoTmp),      % Encontra um caminho do local atual até o alvo
    write("Caminho: "), writeln(CaminhoTmp), % Imprime o caminho encontrado
    get_tail(CaminhoTmp, Caminho),       % Retira o ponto de origem para ficar só o trajeto subsequente
    get_head(Caminho, Dest),             % Obtém a próxima célula de destino imediata
    agente_mover(Origem, Dest),          % Movimenta o agente do local atual para o próximo destino
    agente_iniciar_rotina.               % Chama novamente a rotina para continuar analisando até esgotar as células

% ------------------------------------------------------------------------------
% Caso em que a análise foi concluída
% Exibe uma mensagem informando que todos os exames foram realizados
% ------------------------------------------------------------------------------
agente_iniciar_rotina :-                 % Outra cláusula para 'agente_iniciar_rotina'
    agente(ligado),                      % Verifica novamente se o agente está ligado
    celula(_, _, 0),                     % Checa se há célula normal (0) — sinal de que não há mais suspeitas/câncer
    writeln("\nANÁLISE COMPLETA!"),      % Imprime mensagem de conclusão
    writeln("Ambiente atual:"),          % Mensagem de contexto
    print_ambiente,                      % Imprime o estado atual de todas as células
    !.                                   % Usa corte para não voltar a outra cláusula do mesmo predicado

% ------------------------------------------------------------------------------
% Encontra o caminho entre duas células
% 'path' serve para orquestrar a busca chamando o auxiliar 'path_helper'
% ------------------------------------------------------------------------------
path(Origem, Dest, Caminho) :-                 % Define o predicado 'path' com origem, destino e caminho resultante
    path_helper(Origem, Dest, [Origem], Caminho).  % Chama o auxiliar passando a lista [Origem] como caminho inicial

% ------------------------------------------------------------------------------
% Caso base do path_helper: quando o destino é o mesmo que o atual, inverte-se a lista e finaliza
% ------------------------------------------------------------------------------
path_helper(Dest, Dest, Acc, Caminho) :-   % Se o Current for igual ao Dest, já chegamos ao destino
    reverse(Acc, Caminho).                 % Inverte a lista acumulada (Acc) para produzir o caminho correto

% ------------------------------------------------------------------------------
% Passo recursivo para encontrar o caminho
% Usa a adjacência e verifica se o próximo nó já não foi visitado
% ------------------------------------------------------------------------------
path_helper(Current, Dest, Acc, Caminho) :-                % Define o predicado auxiliar com Current, Dest, Acc e Caminho resultante
    (adj(Current, Next); adj(Next, Current)),              % Encontra uma célula 'Next' adjacente a 'Current'
    \+ member(Next, Acc),                                  % Garante que 'Next' não está na lista acumulada (evita ciclos)
    path_helper(Next, Dest, [Next|Acc], Caminho).          % Chama recursivamente adicionando 'Next' ao acumulador
