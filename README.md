# portransp-panel
Dashboard shiny (R) com dados extraídos do Portal da Transparência (http://portaltransparencia.gov.br)

### Objetivos
O painel de transparência para o Campus Ceilândia do Instituto Federal de Brasília (IFB) tem por objetivo tornar acessíveis ao público informações relacionadas à gestão do campus e servir de fonte de informação para a tomada de decisão.

### Fonte de dados
O painel utiliza dados do Portal da Transparência do Governo Federal e dos formulários do google drive, utilizados pelo Campus para coleta de avaliações da gestão.

|Módulo|Fonte de dados|Atualização|
|------|--------------|-----------|
|Orçamento|[Execução da Despesa](http://www.portaltransparencia.gov.br/download-de-dados/despesas-execucao)|Semanal|
|Pessoal|[Servidores Civis](http://www.portaltransparencia.gov.br/download-de-dados/servidores)|Mensal|
|Avaliação|[Formulários Google](https://www.google.com/forms/about/)|Semestral|

### Requisitos para desenvolvimento
O painel foi desenvolvido em R a partir do RStudio Server dos seguintes pacotes:

|Pacote|
|------|
|shiny|
|shinydashboard|
|shinydashboardPlus|
|shinycssloaders|
|shinyWidgets|
|shinyjs|
|tidyverse|
|lubridate|
|highcharter|
|reshape2|
|treemap|

### Requisitos para utilização
Acesso à rede mundial e utilização dos navegadores testados:
- Google Chrome
- Firefox
- Microsoft Edge
- Internet Explorer
