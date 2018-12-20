README
================

portransp-panel
===============

Dashboard shiny (R) com dados extraídos do Portal da Transparência (<http://portaltransparencia.gov.br>)

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
