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

O agente pode executar as seguintes ações:

- **Ligar e Desligar**: O agente pode ser ativado ou desativado. Quando desligado, o agente não realiza nenhuma ação. Quando ligado, o agente começa a explorar o ambiente.
  
- **Movimentação**: O agente pode se mover entre células adjacentes. As células são conectadas entre si em uma estrutura de grafo, e o agente segue essas conexões para explorar diferentes áreas do ambiente.

- **Identificação de Células Cancerígenas**: O agente examina as células em que está localizado e pode identificar células cancerígenas ou suspeitas.

- **Atualização de Estado**: O agente atualiza o estado das células conforme as ações executadas, marcando-as como identificadas ou eliminadas.

- **Busca em Largura (BFS)**: O algoritmo de busca em largura (BFS) é utilizado para explorar o ambiente de forma eficiente e garantir que o agente percorra todas as células necessárias.

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

