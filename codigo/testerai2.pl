% --------------------------------------------------------------------------
% Sistema de Nanorobô Anticâncer em Prolog (Versão Puramente Declarativa)
% --------------------------------------------------------------------------

% 1. Definição dos Predicados Dinâmicos
:- dynamic celula/3.                % celula(Nome, Localizacao, Receptor)
:- dynamic agente/1.                % agente(ligado) ou agente(desligado)
:- dynamic agente_local/1.          % agente_local(NomeDaCelula)
:- dynamic celula_cancerigena/1.    % celula_cancerigena(NomeDaCelula)
:- dynamic ph_local/2.              % ph_local(Local, PH)
:- dynamic celula_morta/1.          % celula_morta(NomeDaCelula)
:- dynamic total_cancerigenas/1.    % Contador de células cancerígenas
:- dynamic celula_visitada/1.       % celula_visitada(NomeDaCelula)

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


% 3. Inicialização do Ambiente
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
    random_between(3, 5, Qtd),  % Define a quantidade de células normais
    decidir_tipo_criacao(PH, Local, Qtd),
    inicializar_celulas_para_locais(Resto).

decidir_tipo_criacao(PH, Local, _) :- 
    PH < 6.5, 
    criar_celulas_ph_baixo(Local).

decidir_tipo_criacao(PH, Local, Qtd) :- 
    PH >= 6.5, 
    criar_celulas_normais(Local, Qtd).


decidir_tipo_criacao(PH, Local) :-
    PH < 6.5,
    criar_celulas_ph_baixo(Local).

decidir_tipo_criacao(PH, Local) :-
    PH >= 6.5,
    criar_celulas_ph_normal(Local).

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
    atom_concat('celula_', AtomN, Nome),
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

% 4. Regras do Robô
verificar_ph :-
    agente_local(CelulaAtual),
    celula(CelulaAtual, Local, _),
    ph_local(Local, PH),
    verificar_estado_robo(PH, Local).

verificar_estado_robo(PH, Local) :-
    PH < 6.5,
    retractall(agente(_)),
    asserta(agente(ligado)),
    format("Robô ativado em ~w (pH=~w). Ligantes expostos.~n", [Local, PH]).

verificar_estado_robo(PH, Local) :-
    PH >= 6.5,
    retractall(agente(_)),
    asserta(agente(desligado)),
    format("Robô desligado em ~w (pH=~w).~n", [Local, PH]).

agente_identificar :-
    agente(ligado),
    agente_local(CelulaAtual),
    celula(CelulaAtual, Local, Receptor),
    processar_receptor(Receptor, CelulaAtual, Local).

agente_identificar :-
    agente(desligado),
    agente_local(CelulaAtual),
    celula(CelulaAtual, Local, _),
    format("Robô desligado: não pode examinar célula ~w em ~w.~n", [CelulaAtual, Local]).

processar_receptor(1, CelulaAtual, Local) :-
    format("Célula ~w em ~w: RECEPTOR ATIVO - CÂNCER!~n", [CelulaAtual, Local]),
    matar_celula(CelulaAtual).

processar_receptor(0, CelulaAtual, Local) :-
    format("Célula ~w em ~w: receptor inativo (saudável).~n", [CelulaAtual, Local]).

agente_mover(Dest) :-
    agente(ligado),
    agente_local(CelulaAtual),
    celula(CelulaAtual, LocalAtual, _),
    celula(Dest, LocalDest, _),
    adj(LocalAtual, LocalDest),
    retract(agente_local(CelulaAtual)),
    asserta(agente_local(Dest)),
    format("Movimento: ~w (~w) → ~w (~w).~n", [CelulaAtual, LocalAtual, Dest, LocalDest]),
    verificar_ph.

agente_mover(_) :-
    agente(desligado),
    format("Robô desligado: não pode se mover.~n").

matar_celula(Nome) :-
    agente(ligado),
    retract(celula(Nome, Local, _)),
    asserta(celula_morta(Nome)),
    atualizar_contador(-1),
    format("APOPTOSE: Célula ~w em ~w eliminada!~n", [Nome, Local]),
    verificar_se_todas_eliminadas.

matar_celula(_) :-
    agente(desligado),
    format("Robô desligado: não pode eliminar células.~n").

verificar_se_todas_eliminadas :-
    total_cancerigenas(0),
    writeln("TODAS AS CÉLULAS CANCERÍGENAS FORAM ELIMINADAS!").

verificar_se_todas_eliminadas :-
    total_cancerigenas(X),
    X > 0.

% 5. Rotina Principal
agente_iniciar_rotina :-
    agente(ligado),
    writeln("Iniciando rotina de varredura..."),
    total_cancerigenas(Total),
    format("Células cancerígenas totais: ~w~n", [Total]),
    findall(C, celula(C, _, _), Celulas),
    percorrer_celulas(Celulas).

agente_iniciar_rotina :-
    agente(desligado),
    writeln("ERRO: Robô desligado. Não pode iniciar rotina.").

percorrer_celulas([]) :-
    agente(ligado),
    writeln("ANÁLISE COMPLETA! Todas as células foram verificadas."),
    total_cancerigenas(Total),
    format("Células cancerígenas restantes: ~w~n", [Total]).

percorrer_celulas([Celula | Resto]) :-
    agente(ligado),
    verificar_ou_definir_local(Celula),
    agente_mover(Celula),
    agente_identificar,
    percorrer_celulas(Resto).

verificar_ou_definir_local(Celula) :-
    agente_local(_).

verificar_ou_definir_local(Celula) :-
    \+ agente_local(_),
    asserta(agente_local(Celula)).

percorrer_celulas(_) :-
    agente(desligado),
    writeln("INTERRUPÇÃO: Robô desligado durante a varredura!").

% 6. Funções Auxiliares
listar_celulas :-
    findall((Nome, Local, Receptor), celula(Nome, Local, Receptor), Celulas),
    writeln("Lista de células:"),
    print_list(Celulas).

print_list([]).
print_list([(Nome, Local, Receptor)|T]) :-
    format("~w: Local=~w, Receptor=~w~n", [Nome, Local, Receptor]),
    print_list(T).

info_local(Local) :-
    findall(Receptor, celula(_, Local, Receptor), Receptores),
    length(Receptores, Qtd),
    count(1, Receptores, Cancerigenas),
    ph_local(Local, PH),
    format("Local: ~w (pH=~w)~n", [Local, PH]),
    format("Total de células: ~w~n", [Qtd]),
    format("Células cancerígenas: ~w~n", [Cancerigenas]).

count(_, [], 0).
count(X, [X|T], N) :- count(X, T, N1), N is N1 + 1.
count(X, [Y|T], N) :- X \= Y, count(X, T, N).

% 7. Inicialização e Execução
iniciar :-
    writeln("Inicializando sistema..."),
    gerar_ph,
    inicializar_celulas,
    asserta(agente(ligado)),
    writeln("Sistema pronto. Use 'agente_iniciar_rotina.' para começar.").

% 8. Exemplo de Uso
% ?- iniciar.
% ?- agente_iniciar_rotina.