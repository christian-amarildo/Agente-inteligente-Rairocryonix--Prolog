# Agente-inteligente-Prolog--trabalho-final
Este projeto implementa um agente explorador utilizando a linguagem de programação Prolog. O objetivo do agente é explorar um ambiente virtual composto por células, identificar células cancerígenas e realizar a movimentação dentro do ambiente.

## Descrição do Ambiente

O ambiente no qual o agente explorador atua é composto por um conjunto de células, onde cada célula possui um estado específico. O estado das células pode ser "normal", "suspeita" ou "cancerígena". O agente tem a tarefa de explorar esse ambiente, identificar células com suspeita de câncer e realizar o tratamento adequado. O ambiente é modelado como um grafo, onde as células são representadas como nós e as conexões entre elas (que determinam os movimentos do agente) são as arestas.

O ambiente é inicialmente configurado com células em estados aleatórios. O agente pode se mover entre células adjacentes e interagir com elas para identificar o tipo de célula (normal, suspeita ou cancerígena). As células cancerígenas identificadas são "destruídas" pelo agente, e o estado do ambiente é atualizado conforme o agente interage com ele.

### Estrutura do Ambiente

- **Células**: Representadas por fatos no Prolog, cada célula possui um nome, uma localização e um estado (normal, suspeita ou cancerígena). O estado de cada célula é atualizado conforme o agente interage com ela.
  
- **Conexões**: As células estão conectadas entre si, e o agente pode se mover de uma célula para outra se elas forem adjacentes. As conexões entre as células são representadas usando fatos `adj/2`, que definem a relação de adjacência entre as células.

- **Agente**: O agente começa em uma célula inicial e é responsável por explorar o ambiente. Ele pode se mover entre células adjacentes, identificar células cancerígenas e realizar a "eliminar" quando necessário.

## Funcionalidades

O agente explorador implementado no sistema possui as seguintes funcionalidades:

- **Ligar e Desligar**: O agente pode ser ativado ou desativado através do comando `agente_interruptor/0`. Quando desligado, o agente não realiza nenhuma ação e permanece inativo. Quando ativado, o agente começa a explorar o ambiente e realizar as tarefas definidas.

- **Movimentação**: O agente é capaz de se mover entre células adjacentes do ambiente. A movimentação é controlada pelo predicado `agente_mover/2`, que permite ao agente transitar de uma célula de origem para uma célula de destino, desde que haja uma conexão definida entre elas (representada pelas adjacências no grafo).

- **Identificação de Células Cancerígenas**: O agente realiza a verificação das células em que está localizado através do predicado `agente_identificar/0`. Ele identifica células que são cancerígenas, suspeitas ou normais, e, com base nessa identificação, pode tomar ações apropriadas, como marcar as células como cancerígenas ou iniciar o processo de eliminação.

- **Atualização de Estado**: O agente atualiza dinamicamente o estado das células enquanto explora o ambiente. Isso é feito usando o predicado `assertz/1`, que adiciona fatos à base de conhecimento, e o `retract/1`, que remove fatos obsoletos. As células identificadas como cancerígenas ou suspeitas têm seus estados atualizados e podem ser removidas do ambiente após o tratamento.

- **Busca em Largura (BFS)**: O algoritmo de busca em largura (BFS) é utilizado para explorar o ambiente de maneira eficiente. Com isso, o agente é capaz de percorrer todas as células acessíveis, garantindo que ele não explore uma célula mais de uma vez e que todas as células cancerígenas ou suspeitas sejam encontradas. A busca é implementada pelo predicado `bfs/2` e gerenciada por `bfs_queue/3`, permitindo que o agente explore o ambiente de maneira sistemática e ordenada.

Essas funcionalidades permitem ao agente realizar tarefas complexas, como navegar por um ambiente, identificar células cancerígenas, atualizar seu estado, e interagir de forma inteligente com as células. O sistema é projetado para ser flexível, permitindo a expansão das tarefas do agente conforme novas funcionalidades forem necessárias.


## Estrutura do Projeto

O código está organizado da seguinte forma:

- **pre-textuais/**: Contém arquivos como capa, folha de rosto, resumo, etc.
- **textuais/**: Contém as seções principais do trabalho, como introdução, definição do ambiente, tarefas do agente, etc.
- **pos-textuais/**: Contém elementos pós-textuais como referências.

### Principais Arquivos de Código:

- `teste.pl`: Contém a implementação básica do agente e do ambiente, com a definição de células e conexões.
- `testerai.pl`: Contém a implementação do agente com funcionalidades mais avançadas, como a identificação de células cancerígenas e movimentação.
- `testerai2.pl`: Contém a versão declarativa do agente, que utiliza uma abordagem mais simples para manipular o estado das células.
- `denovo.pl`: Contém a versão final do código com a implementação completa das funcionalidades do agente.
- `rairocryonix.pl`: Contém a implementação relacionada ao controle do estado do agente e da movimentação.

## Como Executar

Para rodar o projeto, siga os passos abaixo:

1. Certifique-se de ter o Prolog instalado em sua máquina. Você pode usar o [SWI-Prolog](https://www.swi-prolog.org/) como ambiente de desenvolvimento.
2. Carregue os arquivos `.pl` no ambiente do Prolog.
3. Execute o predicado `iniciar/0` para começar a execução do agente e começar a exploração do ambiente.

```prolog
?- [teste].
?- iniciar.

