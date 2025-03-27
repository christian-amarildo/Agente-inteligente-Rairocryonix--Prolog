% --------------------------------------------------------------------------
% Sistema de Nanorobo AnticAncer em Prolog (Versao Puramente Declarativa)
% --------------------------------------------------------------------------

% 1. Definiçao dos Predicados DinAmicos
:- dynamic celula/3.
:- dynamic agente/1.
:- dynamic agente_local/1.
:- dynamic celula_cancerigena/1.
:- dynamic ph_local/2.
:- dynamic celula_morta/1.
:- dynamic total_cancerigenas/1.
:- dynamic celula_visitada/1.
:- dynamic estado_iniciado/0.

% 2. Base de Conhecimento (Anatomia do Corpo)
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

% 3. Inicializaçao do Ambiente
gerar_ph :-
    Locais = [braco, mao, ombro, torax, abdomen, perna, pe, cabeca, pescoco, coxa, joelho],
    gerar_ph_para_locais(Locais).

gerar_ph_para_locais([]).
gerar_ph_para_locais([Local|Resto]) :-
    random_between(50, 75, PH_temp), 
    PH is PH_temp / 10,
    assertz(ph_local(Local, PH)),
    gerar_ph_para_locais(Resto).

inicializar_celulas :-
    retractall(celula(_,_,_)),
    retractall(total_cancerigenas(_)),
    assertz(total_cancerigenas(0)),
    Locais = [braco, mao, ombro, torax, abdomen, perna, pe, cabeca, pescoco, coxa, joelho],
    inicializar_celulas_para_locais(Locais).

inicializar_celulas_para_locais([]).
inicializar_celulas_para_locais([Local|Resto]) :-
    ph_local(Local, PH),
    random_between(2, 4, Qtd),
    decidir_tipo_criacao(PH, Local, Qtd),
    inicializar_celulas_para_locais(Resto).

decidir_tipo_criacao(PH, Local, _) :- 
    PH < 6.5, 
    criar_celulas_ph_baixo(Local).

decidir_tipo_criacao(PH, Local, Qtd) :- 
    PH >= 6.5, 
    criar_celulas_normais(Local, Qtd).

criar_celulas_ph_baixo(Local) :-
    random_between(1, 3, Qtd),
    asserta(celula(celula_1, Local, 1)),
    atualizar_contador(1),
    processar_quantidade_restante(Qtd, Local).

processar_quantidade_restante(Qtd, Local) :-
    Qtd > 1,
    Restantes is Qtd - 1,
    criar_celulas_extra(Local, Restantes).

processar_quantidade_restante(Qtd, _) :-
    Qtd =< 1.

criar_celulas_extra(_, 0).
criar_celulas_extra(Local, N) :-
    N > 0,
    random_between(0, 1, Receptor),
    atom_number(AtomN, N),
    atom_concat('celula_extra_', AtomN, Nome),
    asserta(celula(Nome, Local, Receptor)),
    atualizar_contador_se_receptor_1(Receptor),
    Next is N - 1,
    criar_celulas_extra(Local, Next).

atualizar_contador_se_receptor_1(1) :-
    atualizar_contador(1).
atualizar_contador_se_receptor_1(0).

criar_celulas_normais(_, 0).
criar_celulas_normais(Local, N) :-
    N > 0,
    atom_number(AtomN, N),
    atom_concat('celula_', AtomN, Nome),
    asserta(celula(Nome, Local, 0)),
    Next is N - 1,
    criar_celulas_normais(Local, Next).

atualizar_contador(Incremento) :-
    retract(total_cancerigenas(X)),
    NewX is X + Incremento,
    asserta(total_cancerigenas(NewX)).

% 4. Regras do Robo (sem if-then-else)
verificar_ph :-
    agente(ligado),
    agente_local(CelulaAtual),
    celula(CelulaAtual, Local, _),
    ph_local(Local, PH),
    verificar_estado_robo(PH, Local).

% Caso 1: pH baixo e robo nao iniciado → expõe ligantes
verificar_estado_robo(PH, Local) :-
    PH < 6.5,
    \+ estado_iniciado,
    assertz(estado_iniciado),
    format("Estrutura alterada! Ligantes expostos em pH=~w.~n", [PH]).

% Caso 2: pH alto e robo iniciado → recolhe ligantes
verificar_estado_robo(PH, Local) :-
    PH >= 6.5,
    estado_iniciado,
    retractall(estado_iniciado),
    format("pH alto (~w). Ligantes recolhidos.~n", [PH]).

% Caso 3: pH baixo e ja iniciado → mantem estado
verificar_estado_robo(PH, _) :-
    PH < 6.5,
    estado_iniciado.

% Caso 4: pH alto e nao iniciado → mantem estado
verificar_estado_robo(PH, _) :-
    PH >= 6.5,
    \+ estado_iniciado.

% 5. Comandos do Usuario
ligar :-
    retractall(agente(_)),
    asserta(agente(ligado)),
    agente_local(_),
    writeln("Robo LIGADO (energizado). Verificando ambiente..."),
    verificar_ph.

ligar :-
    \+ agente_local(_),
    writeln("ERRO: Robo nao esta em nenhuma celula. Nao pode ligar.").

desligar :-
    retractall(agente(_)),
    retractall(estado_iniciado),
    asserta(agente(desligado)),
    writeln("Robo DESLIGADO (totalmente inativo).").

analisar :-
    agente(ligado),
    estado_iniciado,
    agente_local(CelulaAtual),
    celula(CelulaAtual, Local, 1),
    format("Celula ~w em ~w: RECEPTOR ATIVO (CANCER)!~n", [CelulaAtual, Local]),
    writeln("Use 'eliminar.' para tentar destruir esta celula.").

analisar :-
    agente(ligado),
    estado_iniciado,
    agente_local(CelulaAtual),
    celula(CelulaAtual, Local, 0),
    format("Celula ~w em ~w: saudavel.~n. Não foi possível se conectar", [CelulaAtual, Local]).

analisar :-
    agente(ligado),
    \+ estado_iniciado,
    writeln("AVISO: Ligantes nao expostos (pH alto). Nao pode analisar.").

analisar :-
    agente(desligado),
    writeln("ERRO: Robo desligado. Nao pode analisar.").

eliminar :-
    agente(ligado),
    estado_iniciado,
    agente_local(CelulaAtual),
    celula(CelulaAtual, Local, 1),
    retract(celula(CelulaAtual, Local, 1)),
    asserta(celula_morta(CelulaAtual)),
    atualizar_contador(-1),
    format("Celula ~w em ~w ELIMINADA com sucesso!~n", [CelulaAtual, Local]).

eliminar :-
    agente(ligado),
    estado_iniciado,
    agente_local(CelulaAtual),
    celula(CelulaAtual, _, 0),
    format("ERRO: Celula ~w saudavel. Nao pode ser eliminada.~n", [CelulaAtual]).

eliminar :-
    agente(ligado),
    \+ estado_iniciado,
    writeln("AVISO: Ligantes nao expostos (pH alto). Nao pode eliminar.").

eliminar :-
    agente(desligado),
    writeln("ERRO: Robo desligado. Nao pode eliminar celulas.").

% 6. Inicializaçao do sistema
iniciar :-
    writeln("Inicializando sistema..."),
    gerar_ph,
    inicializar_celulas,
    asserta(agente(desligado)),
    retractall(estado_iniciado),
    asserta(agente_local(celula_1)),
    writeln("Sistema pronto. Comandos:"),
    writeln("  - ligar."),
    writeln("  - desligar."),
    writeln("  - status."),
    writeln("  - analisar."),
    writeln("  - eliminar."),