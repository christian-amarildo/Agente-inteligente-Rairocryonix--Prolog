# Agente-inteligente-Prolog--trabalho-final
Este projeto implementa um agente explorador utilizando a linguagem de programação Prolog. O objetivo do agente é explorar um ambiente virtual composto por células, identificar células cancerígenas e realizar a movimentação dentro do ambiente.

## Descrição do Ambiente

O ambiente é representado por uma rede de células conectadas, onde cada célula pode ter um dos três estados possíveis:
- **Normal (0)**
- **Suspeita (1)**
- **Cancerígena (2)**

A estrutura do ambiente é gerada dinamicamente no início da execução do programa. O agente pode explorar esse ambiente, movendo-se entre as células e realizando a identificação de células cancerígenas.

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

