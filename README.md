portransp-panel
================

Painel da Transparência CCEI
----------------------------

Dashboard shiny (R) com dados extraídos do [Portal da Transparência](http://portaltransparencia.gov.br), para servir de painel da transparência do Campus Ceilândia (CCEI) do Instituto Federal de Brasília (IFB)

### Objetivos

O painel de transparência para o Campus Ceilândia do Instituto Federal de Brasília (IFB) tem por objetivo tornar acessíveis ao público informações relacionadas à gestão do campus e servir de fonte de informação para a tomada de decisão.

### Fonte de dados

O painel utiliza dados do Portal da Transparência do Governo Federal e dos formulários do google drive, utilizados pelo Campus para coleta de avaliações da gestão.

| Módulo    | Fonte de dados                                                                                   | Atualização |
|-----------|--------------------------------------------------------------------------------------------------|-------------|
| Orçamento | [Execução da Despesa](http://www.portaltransparencia.gov.br/download-de-dados/despesas-execucao) | Semanal     |
| Pessoal   | [Servidores Civis](http://www.portaltransparencia.gov.br/download-de-dados/servidores)           | Mensal      |
| Avaliação | [Formulários Google](https://www.google.com/forms/about/)                                        | Semestral   |

### Pacotes utilizados

O painel foi desenvolvido em R (R version 3.4.4 (2018-03-15)) no RStudio Server (1.1.442) utilizando os seguintes pacotes:

| Package            | Version | Depends                  |
|:-------------------|:--------|:-------------------------|
| highcharter        | 0.5.0   | R (&gt;= 2.10)           |
| lubridate          | 1.7.4   | methods, R (&gt;= 3.0.0) |
| packrat            | 0.4.9-2 | R (&gt;= 3.0.0)          |
| reshape2           | 1.4.3   | R (&gt;= 3.1)            |
| shiny              | 1.0.5   | R (&gt;= 3.0.2), methods |
| shinycssloaders    | 0.2.0   | NA                       |
| shinydashboard     | 0.7.1   | R (&gt;= 3.0)            |
| shinydashboardPlus | 0.6.0   | NA                       |
| shinyjs            | 0.5.2   | R (&gt;= 3.1.0)          |
| shinyWidgets       | 0.4.3   | R (&gt;= 3.1.0)          |
| tidyverse          | 1.2.1   | NA                       |
| treemap            | 2.4-2   | R (&gt;= 2.10)           |

Para o download da base de dados disponível no portal da transparência foi utilizado o pacote [portransp](https://github.com/pompylio/portransp), disponível no github, o que pode ser feito diretamente pelo portal <http://portaltransparencia.gov.br/download-de-dados>

    require(devtools)
    install_github(repo = "pompylio/portransp")

### Requisitos para utilização

Acesso à rede mundial e utilização dos navegadores testados:

-   Google Chrome
-   Firefox
-   Microsoft Edge
-   Internet Explorer

### Apresentação

#### Módulo Orçamento

O módulo orçamento contempla a execução da despesa orçamentária nas fases de Empenho, Liquidação e Pagamento. A base de dados utilizada é extraída do portal da transparência que, por sua vez, extrai do Sistema de Administração Financeira e Orçamentária do Poder Executivo Federal (SIAFI) periodicamente.

##### Evolução

Demonstra a evolução ano a ano e mês a mês (acumulado e não acumulado) da execução da despesa, contemplando o empenho, a liquidação, o pagamento e restos a pagar (RAP). Contém também a evolução do comparativo das despesas do exercício corrente pagas neste exercício com as despesas pagas neste exercício que se referem a despesas de exercícios anteriores.

![Execução da despesa da UG por ano e valores pagos por ano de referência](img/1%20Execucao%20por%20ano%202%20Valores%20pagos%20por%20ano%20de%20referencia.PNG)

![Execução da despesa por mês (acumulado)](img/3%20Execucao%20da%20despesa%20por%20mes%20acumulado.PNG)

![Execução da despesa por mês (não acumulado)](img/4%20Execucao%20da%20despesa%20por%20mes%20nao%20acumulado.PNG)

##### Categoria

Demonstra a execução da despesa do mês atual por programa, por grupo de despesa, por ação orçamentária e por elemento, maior nível de detalhamento da extração disponibilizada no portal da transparência. Este submódulo dispõe de informações sobre a destinação do gasto.

![Execução da despesa por programa e grupo de despesa](img/5%20Execucao%20por%20programa%206%20Execucao%20por%20grupo%20de%20despesa.PNG)

![Execução da despesa por ação orçamentária](img/7%20Execucao%20por%20acao%20orcamentaria.PNG)

![Execução da despesa por elemento de despesa](img/8%20Execucao%20por%20elemento%20de%20despesa.PNG)

##### Comparativo

Demonstra o comparativo da execução da despesa por unidade gestora e por campi vs reitoria, tanto em relação a empenhado, liquidado, pago e RAP, quanto em relação a despesas de exercícios anteriores pagas no exercício com despesas do exercício pagas no exercício.

![Execução da despesa por unidade orçamentária e por Campi vs Reitoria](img/9%20Execucao%20por%20UG%2010%20Execucao%20por%20campi%20e%20reitoria.PNG)

![Valores pagos por exercício de referência](img/11%20Valores%20pagos%20por%20exercicio%20de%20referencia%20e%20UG%2012%20Valores%20pagos%20por%20exercicio%20de%20referencia%20e%20campi%20reitoria.PNG)

#### Módulo Pessoal

O módulo 'Pessoal' apresenta a estrutura de pessoal do campus em número de servidores por categoria, por cargo, por vínculo, por lotação, por classe, por padrão, por nível de capacitação e por admissão e desligamento, ano a ano e mês a mês. A base de dados extraída do portal da transparência tem origem no Sistema Integrado de Administração de Pessoal (SIAPE) do Poder Executivo Federal.

##### Evolução

Demonstra a evolução do número e do percentual de servidores técnicos e docentes ano a ano e mês a mês.

![Número e percentual de técnicos e docentes por ano](img/PES01%20Numero%20de%20tecnicos%20e%20docentes%20PES02%20Percentual%20de%20tecnicos%20e%20docentes.PNG)

![Número de técnicos e docentes por mês](img/PES03%20Numero%20de%20tecnicos%20e%20docentes%20por%20mes.PNG)

![Percentual de técnicos e docentes por mês](img/PES04%20Percentual%20de%20tecnicos%20e%20docentes%20por%20mes.PNG)

##### Distribuição

Demonstra a distribuição de servidores por cargo, por situação do vínculo e por unidade de exercício, relativo ao último mês de referência disponibilizado pelo portal da transparência.

![Servidores por cargo e por vínculo](img/PES05%20Servidores%20por%20cargo%20PES06%20Servidores%20por%20vinculo.PNG)

![Servidores por unidade de exercício](img/PES07%20Servidores%20por%20unidade.PNG)

##### Perfil (Docente)

Demonstra o perfil do corpo docente em relação a classe, padrão e jornada de trabalho. Considera apenas cargos efetivos.

![Docentes por classe e Docentes por jornada de trabalho](img/PES08%20Cargos%20por%20classe%20PES09%20Cargos%20por%20jornada%20docente.PNG)

![Docentes por classe e padrão](img/PES10%20Cargos%20por%20classe%20e%20padrao%20docentes.PNG)

##### Perfil (Técnico)

Demonstra o perfil do corpo técnico em relação a classe, padrão, nível de capacitação e jornada de trabalho. Considera apenas cargos efetivos.

![Técnicos por classe e Técnicos por jornada de trabalho](img/PES11%20Cargos%20por%20classe%20PES12%20Cargos%20por%20jornada%20tecnico.PNG)

![Técnicos por classe, padrão e nível de capacitação](img/PES13%20Cargos%20por%20classe,%20padrao%20e%20nivel%20de%20capacitacao.PNG)

##### Rotatividade

Demonstra a evolução da relação de admissões e desligamentos por unidade, ano e tipo de cargo. Considera cargos efetivos para docentes e técnicos e temporários para professores temporários e substitutos.

![Admissões e desligamentos de docentes e técnicos por ano e tipo de cargo](img/PES14%20Admissoes%20e%20desligamentos%20por%20ano%20PES15%20por%20tipo%20de%20cargo.PNG)

![Admissões e desligamentos de prof temporarios e substitutos por ano](img/PES16%20Admissoes%20e%20desligamentos%20temporario%20por%20ano%20PES17%20por%20tipo%20de%20contrato.PNG)

#### Módulo Avaliação

O módulo 'Avaliação' contempla o resultado das avaliações da gestão do campus ceilândia realizadas nos exercícios de 2016 a 2018 pela comunidade local. Este módulo é alimentado a partir da coleta dos formulários do google, utilizados pelo campus para alimentação. Considerando o conteúdo singular deste módulo, ele não pode ser replicado para outras unidades do IFB.

### Filtros

**Unidade:** Seleciona a unidade de referência dos dados. Pode ser qualquer unidade que possua dados no portal da transparência tanto em relação a despesas (CODIGO\_UNIDADE\_GESTORA) quanto a servidores civis (CODIGO\_UORG\_EXERCICIO). Pré-seleção e filtro padrão atualmente contempla apenas a unidade criadora da plataforma - CCEI.

**Exercício:** Seleciona o ano de referência das bases de dados 'despesas' e 'servidores civis' do portal da transparência. O exercício de referência depende da base de dados e da unidade de referência, podendo ser de 2013 ao ano atual, no caso de 'pessoal', e de 2014 ao atual, no caso de 'despesas'.

**Grupo de despesa:** Seleciona os grupos de despesa para o módulo 'Orçamento'. Por padrão, considera 'Investimento' e 'Outras despesas correntes', no caso da Reitoria/IFB, pode considerar 'Pessoal'. Não afeta os módulos 'Pessoal' e 'Avaliação'.

### Desenvolvimento e manutenção

<pompylio.lima@ifb.edu.br>

<pompylio.lima@gmail.com>
