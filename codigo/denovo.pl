:- encoding(utf8).
:- set_prolog_flag(encoding, utf8).
% --------------------------------------------------------------------------
% Sistema de Nanorobô Anticâncer em Prolog (Versão Final)
% --------------------------------------------------------------------------

% 1. Definição dos Predicados Dinâmicos
:- dynamic celula/4.                % celula(Nome, Localizacao, Tipo, Receptor)
:- dynamic agente/1.                % agente(ligado/desligado)
:- dynamic agente_local/1.          % agente_local(NomeDaCelula)
:- dynamic ph_local/2.              % ph_local(Local, PH)
:- dynamic celula_morta/1.          % celula_morta(NomeDaCelula)
:- dynamic total_cancerigenas/1.    % Contador de células cancerígenas
:- dynamic estado_iniciado/0.       % Indica se ligantes estão expostos
:- dynamic celula_ligada/1.         % celula_ligada(NomeDaCelula)

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
iniciar :-
    writeln("Inicializando sistema..."),
    retractall(celula(_,_,_,_)),
    retractall(ph_local(_,_)),
    retractall(total_cancerigenas(_)),
    retractall(estado_iniciado),
    retractall(agente(_)),
    retractall(agente_local(_)),
    retractall(celula_ligada(_)),
    gerar_ph,
    inicializar_celulas,
    asserta(agente(desligado)),
    asserta(agente_local(_)),
    writeln("Sistema pronto. Comandos disponíveis:"),
    writeln("  - ligar."),
    writeln("  - desligar."),
    writeln("  - onde."),
    writeln("  - andar(CelulaDestino)."),
    writeln("  - varredura."),
    writeln("  - analisar(Celula)."),
    writeln("  - matar."),
    writeln("  - verificar_ph.").

gerar_ph :-
    Locais = [braco, mao, ombro, torax, abdomen, perna, pe, cabeca, pescoco, coxa, joelho],
    gerar_ph_para_locais(Locais).

gerar_ph_para_locais([]).
gerar_ph_para_locais([Local|Resto]) :-
    random_between(55, 75, PH_temp),
    PH is PH_temp / 10,
    assertz(ph_local(Local, PH)),
    gerar_ph_para_locais(Resto).

inicializar_celulas :-
    assertz(total_cancerigenas(0)),
    Locais = [braco, mao, ombro, torax, abdomen, perna, pe, cabeca, pescoco, coxa, joelho],
    inicializar_celulas_para_locais(Locais).

inicializar_celulas_para_locais([]).
inicializar_celulas_para_locais([Local|Resto]) :-
    ph_local(Local, PH),
    random_between(1, 3, Qtd),
    iniciar_tipo_criacao(PH, Local, Qtd),
    inicializar_celulas_para_locais(Resto).

iniciar_tipo_criacao(PH, Local, Qtd) :-
    PH >= 6.5,
    criar_celulas_normais(Local, Qtd).

iniciar_tipo_criacao(PH, Local, Qtd) :-
    PH < 6.5,
    criar_celulas_ph_baixo(Local, Qtd).

criar_celulas_normais(Local, Qtd) :-
    criar_celulas_normais(Local, Qtd, 1).

criar_celulas_normais(_, 0, _).
criar_celulas_normais(Local, Qtd, N) :-
    format(atom(Nome), 'celula_~w_~w', [N, Local]),
    assertz(celula(Nome, Local, normal, 0)),
    Next is N + 1,
    NewQtd is Qtd - 1,
    criar_celulas_normais(Local, NewQtd, Next).

criar_celulas_ph_baixo(Local, Qtd) :-
    criar_celulas_ph_baixo(Local, Qtd, 1).

criar_celulas_ph_baixo(_, 0, _).
criar_celulas_ph_baixo(Local, Qtd, N) :-
    format(atom(Nome), 'celula_~w_~w', [N, Local]),
    random_between(0, 1, Receptor),
    definir_tipo_celula(Receptor, Tipo),
    (Tipo = cancerigena, atualizar_contador(1); true),
    assertz(celula(Nome, Local, Tipo, Receptor)),
    Next is N + 1,
    NewQtd is Qtd - 1,
    criar_celulas_ph_baixo(Local, NewQtd, Next).

definir_tipo_celula(1, cancerigena).
definir_tipo_celula(0, Tipo) :-
    random_between(0, 1, Suspeita),
    (Suspeita = 1, Tipo = suspeita; Tipo = normal).

atualizar_contador(Incremento) :-
    retract(total_cancerigenas(X)),
    NewX is X + Incremento,
    asserta(total_cancerigenas(NewX)).

% 4. Comandos do Usuário
ligar :-
    agente(desligado),
    retractall(agente(_)),
    asserta(agente(ligado)),
    writeln("Robô ligado. Verificando ambiente..."),
    verificar_ph.

ligar :-
    agente(ligado),
    writeln("Robô já está ligado.").

desligar :-
    agente(ligado),
    retractall(agente(_)),
    retractall(estado_iniciado),
    retractall(celula_ligada(_)),
    asserta(agente(desligado)),
    writeln("Robô desligado.").

desligar :-
    agente(desligado),
    writeln("Robô já está desligado.").

onde :-
    agente_local(Celula),
    celula(Celula, Local, _, _),
    format("Robô está na célula ~w (~w)~n", [Celula, Local]).

onde :-
    \+ agente_local(_),
    writeln("Robô não está em nenhuma célula.").

andar(Destino) :-
    agente(ligado),
    agente_local(Atual),
    celula(Atual, LocalAtual, _, _),
    celula(Destino, LocalDest, _, _),
    adj(LocalAtual, LocalDest),
    retractall(agente_local(_)),
    retractall(celula_ligada(_)),
    asserta(agente_local(Destino)),
    format("Movendo para ~w (~w)~n", [Destino, LocalDest]),
    verificar_ph.

andar(_) :-
    agente(desligado),
    writeln("Erro: Robô desligado, não pode se mover.").

andar(Destino) :-
    \+ celula(Destino, _, _, _),
    format("Erro: Célula ~w não existe.~n", [Destino]).

varredura :-
    agente_local(CelulaAtual),
    celula(CelulaAtual, Local, _, _),
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

analisar(Celula) :-
    agente(ligado),
    estado_iniciado,
    celula(Celula, _, Tipo, Receptor),
    format("Análise da célula ~w:~n", [Celula]),
    (Tipo = normal, writeln("Tipo: normal (receptor 0)"));
    (Tipo = suspeita, Receptor = 0, writeln("Tipo: suspeita (receptor 0)"));
    (Tipo = suspeita, Receptor = 1, writeln("Tipo: suspeita (receptor 1) - possível câncer"));
    (Tipo = cancerigena, writeln("Tipo: cancerígena (receptor 1)")).

analisar(_) :-
    agente(desligado),
    writeln("Erro: Robô desligado, não pode analisar.").

analisar(Celula) :-
    \+ celula(Celula, _, _, _),
    format("Erro: Célula ~w não existe.~n", [Celula]).

matar :-
    agente(ligado),
    estado_iniciado,
    celula_ligada(Celula),
    celula(Celula, _, cancerigena, 1),
    retract(celula(Celula, _, _, _)),
    assertz(celula_morta(Celula)),
    retractall(celula_ligada(_)),
    atualizar_contador(-1),
    format("Célula ~w eliminada com sucesso!~n", [Celula]).

matar :-
    agente(ligado),
    estado_iniciado,
    \+ celula_ligada(_),
    writeln("Erro: Nenhuma célula cancerígena ligada para eliminar.").

matar :-
    agente(desligado),
    writeln("Erro: Robô desligado, não pode matar células.").

verificar_ph :-
    agente_local(Celula),
    celula(Celula, Local, _, _),
    ph_local(Local, PH),
    format("pH em ~w: ~w~n", [Local, PH]),
    modificar_estrutura(PH).

% 5. Funcionalidades Automáticas do Robô
modificar_estrutura(PH) :-
    PH < 6.5,
    \+ estado_iniciado,
    assertz(estado_iniciado),
    writeln("Estrutura modificada: ligantes expostos!").

modificar_estrutura(PH) :-
    PH >= 6.5,
    estado_iniciado,
    retractall(estado_iniciado),
    retractall(celula_ligada(_)),
    writeln("Estrutura modificada: ligantes recolhidos!").

modificar_estrutura(_).