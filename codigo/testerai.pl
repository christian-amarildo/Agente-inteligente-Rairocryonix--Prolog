:- encoding(utf8).
:- set_prolog_flag(encoding, utf8).

% --------------------------------------------------------------------------
% Sistema de Nanorrobô Anticâncer em Prolog (Versão Corrigida)
% --------------------------------------------------------------------------

% Predicados Dinâmicos
:- dynamic celula/4.                % celula(Nome, Local, Tipo, Receptor)
:- dynamic agente/1.                % agente(ligado/desligado)
:- dynamic agente_local/1.          % agente_local(Local)
:- dynamic ph_local/2.              % ph_local(Local, PH)
:- dynamic celula_morta/1.          % celula_morta(Nome)
:- dynamic total_cancerigenas/1.    % total_cancerigenas(Numero)
:- dynamic estado_iniciado/0.       % ligantes expostos

% Adjacências do corpo humano
adj(braco, mao).       adj(mao, braco).
adj(braco, ombro).     adj(ombro, braco).
adj(mao, dedo).        adj(dedo, mao).
adj(ombro, torax).     adj(torax, ombro).
adj(ombro, cabeca).    adj(cabeca, ombro).
adj(torax, abdomen).   adj(abdomen, torax).
adj(torax, pescoco).   adj(pescoco, torax).
adj(perna, pe).        adj(pe, perna).
adj(perna, joelho).    adj(joelho, perna).
adj(joelho, coxa).     adj(coxa, joelho).
adj(coxa, abdomen).    adj(abdomen, coxa).

% Inicialização do sistema
iniciar :-
    writeln("Inicializando sistema..."),
    retractall(celula(_,_,_,_)),
    retractall(ph_local(_,_)),
    retractall(total_cancerigenas(_)),
    retractall(estado_iniciado),
    retractall(agente(_)),
    retractall(agente_local(_)),
    gerar_ph,
    inicializar_celulas,
    asserta(agente(desligado)),
    asserta(agente_local(braco)),
    verificar_celulas_cancerigenas(Quantidade),
    writeln("Sistema pronto. Comandos disponíveis:"),
    writeln("  - ligar."),
    writeln("  - desligar."),
    writeln("  - onde."),
    writeln("  - andar(De, Para)."),
    writeln("  - varredura."),
    writeln("  - analisar(NomeCelula)."),
    writeln("  - matar."),
    writeln("  - verificar_ph.").
    writeln("  - verificar_cancerigenas.").
    format("Robô está em ~w.~n", [braco]).
    format("Total de células cancerígenas: ~w~n", [Quantidade]).

verificar_cancerigenas :-
    total_cancerigenas(X),
    format("Total de células cancerígenas: ~w~n", [X]).

verificar_celula_cancerigenas(X) :-
    total_cancerigenas(X).

% Gerar PH para os locais
locais([braco, mao, ombro, torax, abdomen, perna, pe, cabeca, pescoco, coxa, joelho]).

gerar_ph :-
    locais(Locais),
    gerar_ph_para_locais(Locais).

gerar_ph_para_locais([]).
gerar_ph_para_locais([Local|Resto]) :-
    random_between(55, 75, PH_temp),
    PH is PH_temp / 10,
    assertz(ph_local(Local, PH)),
    gerar_ph_para_locais(Resto).

inicializar_celulas :-
    assertz(total_cancerigenas(0)),
    locais(Locais),
    inicializar_celulas_para_locais(Locais).

inicializar_celulas_para_locais([]).
inicializar_celulas_para_locais([Local|Resto]) :-
    ph_local(Local, PH),
    random_between(1, 3, Qtd),
    iniciar_tipo_criacao(PH, Local, Qtd),
    inicializar_celulas_para_locais(Resto).

iniciar_tipo_criacao(PH, Local, Qtd) :- PH >= 6.5, criar_celulas_normais(Local, Qtd).
iniciar_tipo_criacao(PH, Local, Qtd) :- PH < 6.5, criar_celulas_ph_baixo(Local, Qtd).

criar_celulas_normais(Local, Qtd) :- criar_celulas_normais(Local, Qtd, 1).
criar_celulas_normais(_, Qtd, _) :- Qtd =< 0, !.
criar_celulas_normais(Local, Qtd, N) :-
    format(atom(Nome), 'celula_~w_~w', [N, Local]),
    assertz(celula(Nome, Local, normal, 0)),
    Next is N + 1,
    NewQtd is Qtd - 1,
    criar_celulas_normais(Local, NewQtd, Next).

criar_celulas_ph_baixo(Local, Qtd) :- criar_celulas_ph_baixo(Local, Qtd, 1).
criar_celulas_ph_baixo(_, Qtd, _) :- Qtd =< 0, !.
criar_celulas_ph_baixo(Local, Qtd, N) :-
    format(atom(Nome), 'celula_~w_~w', [N, Local]),
    random_between(0, 1, Receptor),
    definir_tipo_celula(Receptor, Tipo),
    tipo_cancerigena_contagem(Tipo),
    assertz(celula(Nome, Local, Tipo, Receptor)),
    Next is N + 1,
    NewQtd is Qtd - 1,
    criar_celulas_ph_baixo(Local, NewQtd, Next).

tipo_cancerigena_contagem(cancerigena) :- atualizar_contador(1).
tipo_cancerigena_contagem(_).

definir_tipo_celula(1, cancerigena).
definir_tipo_celula(0, suspeita) :- random_between(0, 1, 1).
definir_tipo_celula(0, normal) :- random_between(0, 1, 0).

atualizar_contador(Delta) :-
    retract(total_cancerigenas(X)),
    NewX is X + Delta,
    asserta(total_cancerigenas(NewX)).

ligar :-
    agente(desligado),
    retract(agente(desligado)),
    asserta(agente(ligado)),
    writeln("Robô ligado. Verificando pH..."),
    verificar_ph.

ligar :- agente(ligado), writeln("Robô já está ligado.").

desligar :-
    agente(ligado),
    retractall(agente(_)),
    retractall(estado_iniciado),
    asserta(agente(desligado)),
    writeln("Robô desligado.").

desligar :- agente(desligado), writeln("Robô já está desligado.").

onde :-
    agente_local(Local),
    format("Robô está em ~w.~n", [Local]).

andar(De, Para) :-
    agente(ligado),
    agente_local(De),
    adj(De, Para),
    retract(agente_local(De)),
    asserta(agente_local(Para)),
    format("Robô moveu-se de ~w para ~w.~n", [De, Para]),
    verificar_ph.

andar(_, _) :-
    agente(desligado),
    writeln("Erro: Robô desligado.").

andar(De, Para) :-
    \+ adj(De, Para),
    format("Erro: Movimento inválido de ~w para ~w.~n", [De, Para]).

verificar_ph :-
    agente_local(Local),
    ph_local(Local, PH),
    format("pH em ~w: ~w~n", [Local, PH]),
    modificar_estrutura(PH).

modificar_estrutura(PH) :-
    PH < 6.5, \+ estado_iniciado, assertz(estado_iniciado), writeln("Estrutura ativada: ligantes expostos.").
modificar_estrutura(PH) :-
    PH >= 6.5, estado_iniciado, retractall(estado_iniciado), writeln("Estrutura desativada: ligantes recolhidos.").
modificar_estrutura(_).

varredura :-
    agente(ligado),
    agente_local(Local),
    findall(C, celula(C, Local, _, _), TodasCelulas),
    length(TodasCelulas, Total),
    findall(C, celula(C, Local, normal, _), Normais),
    length(Normais, QtdNormais),
    findall(C, celula(C, Local, suspeita, _), Suspeitas),
    length(Suspeitas, QtdSuspeitas),
    findall(C, celula(C, Local, cancerigena, _), Cancerigenas),
    length(Cancerigenas, QtdCancerigenas),
    format("Varredura no local ~w:~n", [Local]),
    format("Total de células: ~w~n", [Total]),  
    format("Células normais (~w): ~w~n", [QtdNormais, Normais]),
    format("Células suspeitas (~w): ~w~n", [QtdSuspeitas, Suspeitas]),
    format("Células cancerígenas (~w): ~w~n", [QtdCancerigenas, Cancerigenas]).

varredura :-
    agente(desligado),
    format("Erro: Robô desligado, não pode fazer varredura.~n").


analisar(Celula) :-
    agente(ligado), estado_iniciado,
    celula(Celula, _, normal, 0), writeln("Analisando ~w: Normal (receptor 0)"), format("~w~n", [Celula]).
analisar(Celula) :-
    agente(ligado), estado_iniciado,
    celula(Celula, _, suspeita, 0), writeln("Analisando ~w: Suspeita é Normal (receptor 0)"), format("~w~n", [Celula]).
analisar(Celula) :-
    agente(ligado), estado_iniciado,
    celula(Celula, _, suspeita, 1), writeln("Analisando ~w: Suspeita é Cancerígena (receptor 1)"), format("~w~n", [Celula]).

analisar(Celula) :-
    agente(ligado), estado_iniciado,
    celula(Celula, _, cancerigena, 1), writeln("Analisando ~w: Cancerígena (receptor 1)"), format("~w~n", [Celula]).

analisar(_) :-
    agente(desligado), writeln("Erro: Robô desligado.").

analisar(C) :-
    \+ celula(C, _, _, _),
    format("Erro: Célula ~w inexistente.~n", [C]).

interagir(Celula) :-
    agente(ligado), estado_iniciado,
    agente_local(Local),
    celula(Celula, Local, Tipo, 1).
    retract(celula(C, Local, _, _)),
    assertz(celula_morta(C)),
    atualizar_contador(-1),
    format("Célula ~w destruída com sucesso!~n", [C]), !.

interagir :-
    agente(ligado), estado_iniciado,
    writeln("Nenhuma célula cancerígena encontrada no local atual.").

interagir :-
    agente(desligado), writeln("Erro: Robô desligado.").