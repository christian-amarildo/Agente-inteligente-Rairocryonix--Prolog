:- encoding(utf8).                         % Define que o código usará a codificação UTF-8 (acentos e caracteres especiais)
:- set_prolog_flag(encoding, utf8).       % Força o Prolog a usar UTF-8 também nas mensagens e strings

% --------------------------------------------------------------------------
% Sistema de Nanorrobô de DNA em Prolog
% --------------------------------------------------------------------------

% -------- Predicados Dinâmicos (valores podem mudar durante a execução) --------
:- dynamic celula/4.                % Define que uma célula tem: nome, local, tipo (normal, suspeita, cancerígena) e receptor (0 ou 1)
:- dynamic agente/1.                % Representa o estado do robô: ligado ou desligado
:- dynamic agente_local/1.          % Guarda a parte do corpo onde o robô está no momento
:- dynamic ph_local/2.              % Guarda o pH de cada local: ph_local(Local, PH)
:- dynamic celula_morta/1.          % Armazena células destruídas
:- dynamic total_cancerigenas/1.    % Conta o total de células cancerígenas ativas
:- dynamic estado_iniciado/0.       % Indica se o robô está com estrutura ativada (ligantes expostos)
:- dynamic contador_celulas/1.      % Contador para gerar IDs únicos para as células

% Adjacências do corpo humano
adj(braco, mao).       adj(mao, braco).
adj(braco, ombro).     adj(ombro, braco).
adj(mao, dedo).        adj(dedo, mao).
adj(ombro, torax).     adj(torax, ombro).
adj(ombro, pescoco).   adj(pescoco, ombro).
adj(pescoco, cabeca).  adj(cabeca, pescoco).
adj(torax, abdomen).   adj(abdomen, torax).
adj(torax, pescoco).   adj(pescoco, torax).
adj(perna, pe).        adj(pe, perna).
adj(perna, joelho).    adj(joelho, perna).
adj(joelho, coxa).     adj(coxa, joelho).
adj(coxa, abdomen).    adj(abdomen, coxa).

% Início do processo de inicialização do sistema
iniciar :-                                     
    writeln("Inicializando sistema..."),    
    retractall(celula(_,_,_,_)),                % Remove todas as células existentes
    retractall(ph_local(_,_)),                  % Remove os valores de pH anteriores
    retractall(total_cancerigenas(_)),          % Zera o total de células cancerígenas
    retractall(estado_iniciado),                % Remove o estado ativo do robô
    retractall(agente(_)),                      % Remove o estado do robô (ligado/desligado)
    retractall(agente_local(_)),                % Remove o local atual do robô
    retractall(contador_celulas(_)),            % Remove o contador atual de células
    asserta(contador_celulas(1)),               % Inicia contador de células em 1
    asserta(total_cancerigenas(0)),             % Começa com zero células cancerígenas
    gerar_ph,                                   % Gera valores de pH aleatórios para os locais
    inicializar_celulas,                        % Cria células com base nos pH dos locais
    asserta(agente(desligado)),                 % Define que o robô começa desligado
    asserta(agente_local(braco)),               % Coloca o robô inicialmente no braço
    verificar_celulas_cancerigenas(Quantidade), % Recupera a quantidade de células cancerígenas
    writeln("Sistema pronto. Comandos disponíveis:"), 
    writeln("  - ligar."),
    writeln("  - desligar."),
    writeln("  - onde."),
    writeln("  - varredura."),
    writeln("  - verificar_ph."),
    writeln("  - listar_cancerigenas."),
    writeln("  - listar_suspeitas."),
    writeln("  - andar(De, Para)."),
    writeln("  - interagir(NomeDaCelula)."),
    format("Robô está em ~w.~n", [braco]),            
    format("Total de células cancerígenas: ~w~n", [Quantidade]).

% Gerar PH para os locais
% Lista de todos os locais possíveis
locais([braco, mao, ombro, torax, abdomen, perna, pe, cabeca, pescoco, coxa, joelho]). 

gerar_ph :-                         % Gera pH aleatório para todos os locais
    locais(Locais),                 % Obtém a lista de locais
    gerar_ph_para_locais(Locais).  % Inicia geração

gerar_ph_para_locais([]).          % Fim da lista: nada a fazer

gerar_ph_para_locais([Local|Resto]) :-               % Para cada local:
    random_between(55, 75, PH_temp),                 % Gera número inteiro entre 55 e 75
    PH is PH_temp / 10,                              % Converte para decimal (ex: 65 → 6.5)
    assertz(ph_local(Local, PH)),                    % Salva o pH gerado
    gerar_ph_para_locais(Resto).                     % Continua com o próximo local

% Listagem de Células
listar_suspeitas :-                                      % Lista todas as células do tipo "suspeita"
    findall(Nome, celula(Nome, _, suspeita, _), Lista), % Cria uma lista com os nomes das suspeitas
    length(Lista, Total),                                % Conta quantas são
    format("Total de células suspeitas (~w):~n", [Total]), 
    listar(Lista).                                       % Mostra os nomes

listar([]).                         % Caso a lista esteja vazia, não faz nada
listar([H|T]) :-                    % Caso contrário:
    writeln(H),                     % Escreve o nome da célula
    listar(T).                      % Continua com o restante da lista

listar_cancerigenas :-                              % Mostra o total de células cancerígenas
    total_cancerigenas(X),                          % Pega esse número
    format("Total de células cancerígenas: ~w~n", [X]).

verificar_celulas_cancerigenas(X) :-                % Permite acessar esse número fora
    total_cancerigenas(X).                          % Retorna X


% Criação de Células
inicializar_celulas :-                      % Começa o processo de criar células
    retractall(total_cancerigenas(_)),      % Remove contagem antiga
    assertz(total_cancerigenas(0)),         % Zera contador
    locais(Locais),                         % Pega todos os locais
    inicializar_celulas_para_locais(Locais), % Cria células em todos os locais
    garantir_uma_cancerigena.               % Garante que pelo menos uma cancerígena exista

inicializar_celulas_para_locais([]).  % Fim da lista de locais
inicializar_celulas_para_locais([Local|Resto]) :-    % Para cada local:
    ph_local(Local, PH),                             % Obtém o pH do local
    random_between(1, 3, Qtd),                        % Decide quantas células criar (1 a 3)
    iniciar_tipo_criacao(PH, Local, Qtd),             % Cria as células com base no pH
    inicializar_celulas_para_locais(Resto).           % Vai para o próximo local

iniciar_tipo_criacao(PH, Local, Qtd) :- PH >= 6.5, criar_celulas_normais(Local, Qtd).  % Se pH ≥ 6.5 → normais
iniciar_tipo_criacao(PH, Local, Qtd) :- PH < 6.5, criar_celulas_ph_baixo(Local, Qtd).   % Se pH < 6.5 → pode ter cancerígenas


% Criar células normais e em pH baixo
criar_celulas_normais(Local, Qtd) :- 
    criar_celulas_normais(Local, Qtd, 1).           % Chama a versão com contador auxiliar (não usado)

criar_celulas_normais(_, Qtd, _) :- 
    Qtd =< 0, !.                                    % Se quantidade for 0 ou menos, para

criar_celulas_normais(Local, Qtd, _) :-
    proximo_id(ID),                                 % Pega próximo ID único
    format(atom(Nome), 'celula_~w_~w', [ID, Local]),% Cria o nome da célula com ID e Local
    assertz(celula(Nome, Local, normal, 0)),        % Cria célula normal com receptor 0
    NewQtd is Qtd - 1,                              % Decrementa a quantidade
    criar_celulas_normais(Local, NewQtd, _).        % Continua criando o restante

criar_celulas_ph_baixo(Local, Qtd) :- 
    criar_celulas_ph_baixo(Local, Qtd, 1).          % Inicia criação de células com pH baixo

criar_celulas_ph_baixo(_, Qtd, _) :- 
    Qtd =< 0, !.                                    % Se quantidade for 0 ou menos, para

criar_celulas_ph_baixo(Local, Qtd, _) :-
    proximo_id(ID),                                 % Pega próximo ID
    format(atom(Nome), 'celula_~w_~w', [ID, Local]),% Cria nome da célula
    random_between(0, 1, Receptor),                 % Sorteia se receptor será 0 ou 1
    definir_tipo_celula(Receptor, Tipo),            % Define tipo com base no receptor
    tipo_cancerigena_contagem(Tipo),                % Atualiza contador se for cancerígena
    assertz(celula(Nome, Local, Tipo, Receptor)),   % Cria a célula
    NewQtd is Qtd - 1,                              % Decrementa
    criar_celulas_ph_baixo(Local, NewQtd, _).       % Continua criando

% Caso total de cancerígenas = 0, garante uma célula cancerígena no sistema.
garantir_uma_cancerigena :-
    total_cancerigenas(0),                          % Se não há nenhuma cancerígena
    locais(Todos),                                  % Pega todos os locais
    random_member(Local, Todos),                    % Escolhe um aleatoriamente
    proximo_id(ID),                                 % Gera ID
    format(atom(Nome), 'celula_~w_~w', [ID, Local]),% Gera nome
    assertz(celula(Nome, Local, cancerigena, 1)).   % Cria célula cancerígena com receptor 1

garantir_uma_cancerigena :- 
    total_cancerigenas(X), X > 0.                   % Se já tem uma ou mais, não faz nada

% Gerar IDs e contagem de cancerígenas
proximo_id(ID) :-
    retract(contador_celulas(N)),                   % Pega o valor atual e remove
    ID = N,                                         % Define o novo ID como o valor atual
    N1 is N + 1,                                    % Incrementa
    asserta(contador_celulas(N1)).                  % Salva o novo valor

tipo_cancerigena_contagem(cancerigena) :- 
    atualizar_contador(1).                          % Se for cancerígena, adiciona +1

tipo_cancerigena_contagem(_).                       % Se não for, ignora

definir_tipo_celula(1, cancerigena).                % Receptor 1 sempre é cancerígena

definir_tipo_celula(0, Tipo) :-                     % Se receptor for 0:
    random_between(0, 1, R),                        % Sorteia entre normal e suspeita
    definir_tipo_celula_por_receptor_zero(R, Tipo). % Define baseado no sorteio

definir_tipo_celula_por_receptor_zero(0, normal).   % 0 → normal
definir_tipo_celula_por_receptor_zero(1, suspeita). % 1 → suspeita

atualizar_contador(Delta) :-
    retract(total_cancerigenas(X)),                 % Pega o valor atual
    NewX is X + Delta,                              % Soma ou subtrai
    asserta(total_cancerigenas(NewX)).              % Salva novo valor

% COMANDOS DO USUARIO

% Comandos para ligar e desligar o robô
ligar :-
    agente(desligado),              % Se o robô estiver desligado
    retract(agente(desligado)),     % Remove o estado atual
    asserta(agente(ligado)),        % Define como ligado
    writeln("Robô ligado. Verificando pH..."),
    verificar_ph.                   % Verifica o pH do local

ligar :- agente(ligado), writeln("Robô já está ligado.").

desligar :- 
    agente(ligado),                 % Não tem muito o que dizer...
    retractall(agente(_)),
    retractall(estado_iniciado),
    asserta(agente(desligado)),
    writeln("Robô desligado.").

desligar :- agente(desligado), writeln("Robô já está desligado.").

onde :-
    agente_local(Local),                    % Pega o local atual
    format("Robô está em ~w.~n", [Local]).

andar(De, Para) :-
    agente(ligado),                         % Se o robô estiver ligado
    agente_local(De),                       % Se o robô estiver no local de origem
    adj(De, Para),                          % Se os locais forem adjacentes
    retract(agente_local(De)),              % Remove o local atual
    asserta(agente_local(Para)),            % Define o novo local
    format("Robô moveu-se de ~w para ~w.~n", [De, Para]), % Mostra a mudança
    verificar_ph.                           % Verifica o pH do novo local

andar(_, _) :- 
    agente(desligado), 
    writeln("Erro: Robô desligado.").

andar(De, Para) :-
    \+ adj(De, Para), % Se os locais não forem adjacentes
    format("Erro: Movimento inválido de ~w para ~w.~n", [De, Para]). % Mostra erro

verificar_ph :-                     % Verifica o pH do local atual
    agente_local(Local),            % Pega o local atual
    ph_local(Local, PH),            % Pega o pH do local
    format("pH em ~w: ~w~n", [Local, PH]), % Mostra o pH
    modificar_estrutura(PH).         % Modifica a estrutura do robô se necessário

modificar_estrutura(PH) :-          % Modifica a estrutura do robô com base no pH
    PH < 6.5, \+ estado_iniciado, assertz(estado_iniciado), writeln("Estrutura ativada: ligantes expostos."). % Se pH < 6.5 e estrutura não ativada

modificar_estrutura(PH) :-           % Se pH < 6.5 e estrutura já ativada
    PH >= 6.5, estado_iniciado, retractall(estado_iniciado), writeln("Estrutura desativada: ligantes recolhidos."). % Desativa

% Se pH ≥ 6.5, não faz nada
modificar_estrutura(_). 

varredura :- % Faz varredura no local atual
    agente(ligado),
    agente_local(Local),
    findall(C, celula(C, Local, _, _), TodasCelulas),   % Pega todas as células do local
    length(TodasCelulas, Total),                        % Conta quantas são
    findall(C, celula(C, Local, normal, _), Normais),   % Pega as normais
    length(Normais, QtdNormais),                        % Conta quantas são
    findall(C, celula(C, Local, suspeita, _), Suspeitas), % Pega as suspeitas
    length(Suspeitas, QtdSuspeitas),                    % Conta quantas são
    findall(C, celula(C, Local, cancerigena, _), Cancerigenas), % Pega as cancerígenas
    length(Cancerigenas, QtdCancerigenas),              % Conta quantas são
    format("Varredura no local ~w:~n", [Local]),        % Mostra o local
    format("Total de células: ~w~n", [Total]),  
    format("Células normais (~w): ~w~n", [QtdNormais, Normais]),
    format("Células suspeitas (~w): ~w~n", [QtdSuspeitas, Suspeitas]),
    format("Células cancerígenas (~w): ~w~n", [QtdCancerigenas, Cancerigenas]).

varredura :-
    agente(desligado),
    format("Erro: Robô desligado, não pode fazer varredura.~n").

interagir(Celula) :-
    agente(ligado), estado_iniciado,                % Se o robô estiver ligado e com estrutura ativada
    celula(Celula, _, normal, 0), format("Analisando ~w: Normal (receptor 0).~n", [Celula]). % Se for normal

interagir(Celula) :-
    agente(ligado), estado_iniciado, 
    celula(Celula, _, suspeita, 0), format("Analisando ~w: Suspeita é Normal (receptor 0).~n", [Celula]). % Se suspeita for normal
    
interagir(Celula) :-
    agente(ligado), estado_iniciado,
    celula(Celula, _, suspeita, 1), format("Analisando ~w: Suspeita é Cancerígena (receptor 1).~n", [Celula]), % Se suspeita for cancerígena
    celula(Celula, _, _, 1),                                                                                   % Se for cancerígena
    retract(celula(Celula, _, _, 1)),                                                                          % Remove célula
    assertz(celula_morta(Celula)),                                                                             % Adiciona à lista de mortas
    atualizar_contador(-1),                                                                                    % Atualiza contador
    format("Célula ~w destruída com sucesso!~n", [Celula]),                                                    % Mostra mensagem
    total_cancerigenas(Total),                                                                                 % Mostra total de cancerígenas
    format("Total de Células Cancerígenas: ~w~n", [Total]), !.                                                 

interagir(Celula) :-                                                                                           % Se for cancerígena
    agente(ligado), estado_iniciado,                                                                           % Se o robô estiver ligado e com estrutura ativada
    celula(Celula, _, cancerigena, 1), format("Analisando ~w: Cancerígena (receptor 1).~n", [Celula]),         % Se for cancerígena
    celula(Celula, _, _, 1),                                                                                   % Se for cancerígena
    retract(celula(Celula, _, _, 1)),                                                                          % Remove célula
    assertz(celula_morta(Celula)),                                                                             % Adiciona à lista de mortas
    atualizar_contador(-1),                                                                                    % Atualiza contador
    format("Célula ~w destruída com sucesso!~n", [Celula]),                                                    % Mostra mensagem
    total_cancerigenas(Total),                                                                                 % Mostra total de cancerígenas
    format("Total de Células Cancerígenas: ~w~n", [Total]), !.

interagir(_) :-
    agente(desligado), writeln("Erro: Robô desligado.").

interagir(C) :-
    \+ celula(C, _, _, _),                          % Se a célula não existir
    format("Erro: Célula ~w inexistente.~n", [C]).  % Mostra erro