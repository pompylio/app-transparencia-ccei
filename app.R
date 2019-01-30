# GLOBAL ------------------------------------------------------------------
# Library 
suppressMessages(require(shiny))
suppressMessages(require(shinydashboard))
suppressMessages(require(shinydashboardPlus))
suppressMessages(require(shinycssloaders))
suppressMessages(require(shinyWidgets))
suppressMessages(require(shinyjs))
suppressMessages(require(tidyverse))
suppressMessages(require(lubridate))
suppressMessages(require(highcharter))
suppressMessages(require(reshape2))
suppressMessages(require(treemap))
suppressMessages(require(DT))
# Functions
source("function/function.R",encoding = "UTF-8")
# Data
import_data(pasta = "data/", extensao = "rds")
options(highcharter.lang = hcoptslang())
# SHINY UI --------------------------------------------------------------------------------------------------------
ui <-
  dashboardPagePlus(
    dashboardHeaderPlus( 
      title = strong("Transparência CCEI"),
      titleWidth = 230,
      enable_rightsidebar = TRUE,
      rightSidebarIcon = "gears"), 
    dashboardSidebar(
      width = 230,
      collapsed = FALSE,
      sidebarMenu(
        menuItem(
          text = strong("ORÇAMENTO"),
          tabName = "orcamento",
          icon = icon("money"),
          startExpanded = TRUE,
          menuItem(
            text = strong("Execução da despesa"),
            tabName = "execucao_despesa",
            startExpanded = TRUE,
            menuSubItem(
              text = strong("Evolução"),
              tabName = "evolucao"),
            menuSubItem(
              text = strong("Categoria"),
              tabName = "categoria"),
            menuSubItem(
              text = strong("Comparativo"),
              tabName = "comparativo"))),
        menuItem(
          text = strong("PESSOAL"),
          tabName = "pessoal",
          icon = icon("group"),
          menuItem(
            text = strong("Evolução"),
            tabName = "evolucao_pessoal"),
          menuItem(
            text = strong("Distribuicao"),
            tabName = "distribuicao_pessoal"),
          menuItem(
            text = strong("Perfil"),
            tabName = "perfil_pessoal",
            menuSubItem(
              text = strong("Docente"),
              tabName = "perfil_docente"),
            menuSubItem(
              text = strong("Técnico"),
              tabName = "perfil_tecnico")),
          menuItem(
            text = strong("Rotatividade"),
            tabName = "rotatividade")),
        menuItem(
          text = strong("AVALIAÇÕES"),
          tabName = "avaliacoes",
          icon = icon("comments-o"),
          menuItem(
            text = strong("Avaliação Geral"), 
            tabName = "ava_ger"),
          menuItem(
            text = strong("Priorização do Orçamento"), 
            tabName = "ava_orc")))),
    dashboardBody(
      tags$head(
        tags$head(HTML(
          "<!-- Global site tag (gtag.js) - Google Analytics -->
          <script async src='https://www.googletagmanager.com/gtag/js?id=UA-103778910-3'></script>
          <script>
          window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
          gtag('js', new Date());
          
          gtag('config', 'UA-103778910-3');
          </script>"))),
      tags$head(tags$style(HTML(".direct-chat-contacts-open .direct-chat-contacts {color:#000000;opacity:.9;}"))),
      tabItems(
        tabItem(
          tabName = "evolucao",
          fluidRow(
            boxnew(
              inputId = "ORC01", # ORC01 Execução da despesa por ano ----
              width_box = 6,
              status = "danger",
              boxtitle = "Execução da despesa por ano",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = c(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "empty", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "ORC02", # ORC02 Valores pagos por exercício de referência ----
              width_box = 6,
              status = "warning",
              boxtitle = "Valores pagos por exercício de referência",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = c(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "percent", dimension = "empty")),
            boxnew(
              inputId = "ORC03", # ORC03 Execução da despesa por mês (acumulado) -----
              width_box = 12,
              status = "warning",
              boxtitle = "Execução da despesa por mês (acumulado)",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = c(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "areaspline", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "ORC04", # ORC04 Execução da despesa por mês (não acumulado) ----
              width_box = 12,
              status = "warning",
              boxtitle = "Execução da despesa por mês (não acumulado)",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "spline", groupplot = "empty", dimension = "empty")))),
        tabItem(
          tabName = "categoria", 
          fluidRow(
            boxnew(
              inputId = "ORC05", # ORC05 Execução da despesa por programa ----
              width_box = 6,
              status = "warning",
              boxtitle = "Execução por programa",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = c(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "ORC06", # ORC06 Execução da despesa por grupo de despesa ----
              width_box = 6,
              status = "warning",
              boxtitle = "Execução por grupo de despesa",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = c(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "ORC07", # ORC07 Execução da despesa por ação orçamentária ----
              width_box = 12,
              status = "warning",
              boxtitle = "Execução por ação orçamentária",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = c(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "ORC08", # ORC08 Execução da despesa por elemento de despesa ----
              width_box = 12,
              status = "warning",
              boxtitle = "Execução por elemento de despesa",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = c(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty")))),
        tabItem(
          tabName = "comparativo", 
          fluidRow(
            boxnew(
              inputId = "ORC09", # ORC09 Execução da despesa por unidade gestora ----
              width_box = 7,
              status = "warning",
              boxtitle = "Execução por unidade gestora",
              menu_selected = c("typeplot", "groupplot", "dimension","department"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D", department = "Grupo de unidades"),
              choices = list(typeplot = "empty", groupplot = "empty", department = c("Com Reitoria","Sem Reitoria")),
              selected = c(typeplot = "column", groupplot = "normal", dimension = "empty", department = "Sem Reitoria")),
            boxnew(
              inputId = "ORC10", # ORC10 Execução da despesa por campi e reitoria ----
              width_box = 5,
              status = "warning",
              boxtitle = "Execução por Campi e Reitoria",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "normal", dimension = "empty")),
            boxnew(
              inputId = "ORC11", # ORC11 Valores pagos por exercício de referência (por UG) ----
              width_box = 7,
              status = "warning",
              boxtitle = "Valores pagos por exercício de referência",
              menu_selected = c("typeplot", "groupplot", "dimension","department"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D", department = "Grupo de unidades"),
              choices = list(typeplot = "empty", groupplot = "empty", department = c("Com Reitoria","Sem Reitoria")),
              selected = c(typeplot = "column", groupplot = "percent", dimension = "empty", department = "Sem Reitoria")),
            boxnew(
              inputId = "ORC12", # ORC12 Valores pagos por exercício de referência (Campi e Reitoria) ----
              width_box = 5,
              status = "warning",
              boxtitle = "Valores pagos por exercício de referência (Campi e Reitoria)",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "percent", dimension = "empty")))),
        tabItem(
          tabName = "evolucao_pessoal", 
          fluidRow(
            boxnew(
              inputId = "PES01", # PES01 Número de técnicos e docentes por ano ----
              width_box = 6,
              status = "warning",
              boxtitle = "Número de técnicos e docentes por ano",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "spline", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "PES02", # PES02 Percentual de técnicos e docentes por ano ----
              width_box = 6,
              status = "warning",
              boxtitle = "Percentual de técnicos e docentes por ano",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "percent", dimension = "empty")),
            boxnew(
              inputId = "PES03", # PES03 Número de técnicos e docentes por mês ----
              width_box = 12,
              status = "warning",
              boxtitle = "Número de técnicos e docentes por mês",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "areaspline", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "PES04", # PES04 Percentual de técnicos e docentes por mês ----
              width_box = 12,
              status = "warning",
              boxtitle = "Percentual de técnicos e docentes por mês",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "percent", dimension = "empty")))),
        tabItem(
          tabName = "distribuicao_pessoal", 
          fluidRow(
            boxnew(
              inputId = "PES05", # PES05 Servidores por cargo ----
              width_box = 6,
              boxtitle = "Servidores por cargo"),
            boxnew(
              inputId = "PES06", # PES06 Servidores por situação do vínculo ----
              width_box = 6,
              boxtitle = "Servidores por situação do vínculo"),
            boxnew(
              inputId = "PES07", # PES07 Servidores por unidade de Exercício ----
              width_box = 12,
              boxtitle = "Servidores por unidade de exercício"))),
        tabItem(
          tabName = "perfil_docente",
          fluidRow(
            boxnew(
              inputId = "PES08", # PES08 Cargos por classe ----
              width_box = 6,
              status = "warning",
              boxtitle = "Cargos por classe",
              menu_selected = c("dimension"),
              label = c(dimension = "3D"),
              choices = "",
              selected = c(dimension = "3D")),
            boxnew(
              inputId = "PES09", # PES09 Cargos por Jornada de trabalho ----
              width_box = 6,
              status = "warning",
              boxtitle = "Cargos por Jornada de trabalho",
              menu_selected = c("dimension"),
              label = c(dimension = "3D"),
              choices = "",
              selected = c(dimension = "3D")),
            boxnew(
              inputId = "PES10", # PES10 Cargos por classe e padrão ----
              width_box = 12,
              status = "warning",
              boxtitle = "Cargos por classe e padrão",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = c(`<i class='fa fa-bar-chart'></i>`="column", `<i class='fa fa-align-left'></i>`="bar"), groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "normal", dimension = "empty"))
            )
          ),
        tabItem(
          tabName = "perfil_tecnico",
          fluidRow(
            boxnew(
              inputId = "PES11", # PES11 Cargos por classe ----
              width_box = 6,
              status = "warning",
              boxtitle = "Cargos por classe",
              menu_selected = c("dimension"),
              label = c(dimension = "3D"),
              choices = "",
              selected = c(dimension = "3D")),
            boxnew(
              inputId = "PES12", # PES12 Cargos por Jornada de trabalho ----
              width_box = 6,
              status = "warning",
              boxtitle = "Cargos por Jornada de trabalho",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = c(`<i class='fa fa-bar-chart'></i>`="column", `<i class='fa fa-align-left'></i>`="bar"), groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "PES13", # PES13 Cargos por classe, padrão e nível de capacitação ----
              width_box = 12,
              status = "warning",
              boxtitle = "Cargos por classe, padrão e nível de capacitação",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = c(`<i class='fa fa-bar-chart'></i>`="column", `<i class='fa fa-align-left'></i>`="bar"),groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "normal", dimension = "empty")))),
        tabItem(
          tabName = "rotatividade", 
          fluidRow(
            boxnew(
              inputId = "PES17", # PES17 Admissão e desligamento de docentes e técnicos por ano ----
              width_box = 6,
              status = "warning",
              boxtitle = "Admissão e desligamento de docentes e técnicos por ano",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "areaspline", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "PES18", # PES18 Admissão e desligamento de docentes e técnicos por tipo ----
              width_box = 6,
              status = "warning",
              boxtitle = "Admissão e desligamento de docentes e técnicos por tipo de cargo",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "spline", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "PES19", # PES19 Admissão e desligamento de prof. temporário e substituto por ano ----
              width_box = 6,
              status = "warning",
              boxtitle = "Admissão e desligamento de prof. temporário e substituto por ano",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "areaspline", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "PES20", # PES20 Admissão e desligamento de prof. temporário e substituto por tipo de contrato ----
              width_box = 6,
              status = "warning",
              boxtitle = "Admissão e desligamento de prof. temporário e substituto por tipo de contrato",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "spline", groupplot = "empty", dimension = "empty")))),
        tabItem(
          tabName = "ava_ger", # AVALIACAO GERAL ----
          uiOutput("item_avaliacao")),
        tabItem(
          tabName = "ava_orc", # AVALIACAO ORCAMENTO ----
          uiOutput("ava_orc_2019")
        )
        )
      ),
    rightSidebar( # Barra lateral direita ----
      background = "dark", 
      rightSidebarTabContent(
        active = TRUE,
        id = 1,
        title = "Filtros", 
        icon = "filter",
        selectInput(
          inputId = "geral_unidade", 
          label = "Unidade", 
          choices = "CCEI",
          selected = "CCEI"),
        selectInput(
          inputId = "geral_exercicio",
          label = "Exercício", 
          choices = as.character(sort(seq(from = 2013, to = year(Sys.Date()), by = 1), decreasing = TRUE)), 
          selected = "2018"),
        checkboxGroupInput(
          inputId = "grupodespesa",
          label = "Grupo de despesa",
          choices = list("Despesa Corrente" = unique(db_siafi$CODIGO_GRUPO_DE_DESPESA)[1], 
                         "Despesa de Capital" = unique(db_siafi$CODIGO_GRUPO_DE_DESPESA)[2], 
                         "Pessoal" = unique(db_siafi$CODIGO_GRUPO_DE_DESPESA)[3]),
          selected = unique(db_siafi$CODIGO_GRUPO_DE_DESPESA)[1:2])),
      rightSidebarTabContent(
        id = 2,
        title = strong("Informações"),
        icon = "info-circle",
        p(align = "justify", strong("Licenças"), style="color:white"),
        p(strong("HIGHCHARTS - License holder: ")),
        p(align = "justify", "INSTITUTO FEDERAL DE EDUCAÇÃO, CIÊNCIA E TECNOLOGIA DE BRASÍLIA"),
        p("Pompylio Lima"),
        p(align = "justify", "This license is valid for educational-institution for the following product(s): Highcharts, Highstock, Highmaps."),
        p(align = "justify", "This software is released under Creative Commons Attribution-NonCommercial 3.0, and is available for download at highcharts.com/download. No further activation or license key is required."),
        p(align = "justify", strong("Fonte de dados"), style="color:white"),
        p(align = "justify", tags$a(href = "http://portaltransparencia.gov.br/download-de-dados", target = "_new", "Portal da Transparência")),
        p(align = "justify", paste0("- Orçamento (", tail(substr(db_siafi$DATA_REFERENCIA, 1, 7), n = 1), ")")),
        p(align = "justify", paste0("- Pessoal (", tail(substr(db_siape$DATA_REFERENCIA, 1, 7), n = 1),")")),
        p(align = "justify", tags$a(href = "https://www.google.com/forms/about/", target = "_new", "Google Forms")),
        p(align = "justify", paste0("- Avaliação (", max(ymd(substr(file.info("data/db_orcano_av.rds")$mtime, 1, 10)), ymd(substr(file.info("data/db_avager_av.rds")$mtime, 1, 10))),")")),
        p(align = "justify", strong("Dúvidas e sugestões"), style="color:white"),
        p(align = "justify", tags$a(href="https://github.com/pompylio/portransp-panel", target="_new", icon("github"), style="font-size:20px;color:white"), tags$a(href="https://github.com/pompylio/portransp-panel/issues", target="_new", "pompylio.lima@ifb.edu.br"))
        )
    ), title = "Transparência CCEI"
  )
# SHINY SERVER ----------------------------------------------------------------------------------------------------
server <-
  function(session, input, output) {
    
    observeEvent(input$geral_unidade, {
      if(input$geral_unidade == "CCEI"){
        output$item_avaliacao <- renderUI({
          fluidRow(
            column(width = 12,
              column(width = 3,
                selectInput(
                  inputId = "avager_ano", 
                  label = "Ano", 
                  choices = unique(db_avager_av$ANO), 
                  selected = max(unique(db_avager_av$ANO)))),
              column(width = 3,
                selectInput(
                  inputId = "avager_unidade", 
                  label = "Unidade", 
                  choices = unique(db_avager_av$UNIDADE_AVALIADA), 
                  selected = unique(db_avager_av$UNIDADE_AVALIADA)[1]))),
            boxnew(
              inputId = "AVA01", # AVA01 Número de avaliadores por cargo ----
              width_box = 5,
              status = "warning",
              boxtitle = "Avaliadores por cargo",
              menu_selected = c("dimension"),
              label = c(dimension = "3D"),
              selected = c(dimension = "empty")),
            boxnew(
              inputId = "AVA02", # AVA02 Resultado da avaliação por critério ----
              width_box = 7,
              status = "warning",
              boxtitle = "Resultado da avaliação por critério",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty")),
            box(width = 12, solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,closable = FALSE,
              column(width = 4,
                selectInput(
                  inputId = "avager_cargo", 
                  label = "Quadro a que pertence", 
                  choices = c("TODOS", unique(db_avager_av[db_avager_av$UNIDADE_AVALIADA == "DG", ]$CARGO)),
                  selected = "TODOS",
                  width = "100%")),
              column(width = 8,
                selectInput(
                  inputId = "avager_questao", 
                  label = "Questão", 
                  choices = unique(db_avager_av[db_avager_av$UNIDADE_AVALIADA == "DG", ]$QUESTAO),
                  selected = unique(db_avager_av[db_avager_av$UNIDADE_AVALIADA == "DG", ]$QUESTAO)[1],
                  width = "100%")),
              boxnew(
                inputId = "AVA03", # AVA03 Resultado da avaliação por nota ----
                width_box = 4, 
                status = "warning",
                boxtitle = "Resultado da avaliação por nota",
                menu_selected = c("typeplot", "groupplot", "dimension"),
                label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
                choices = list(typeplot = "empty", groupplot = "empty"),
                selected = c(typeplot = "column", groupplot = "empty", dimension = "empty")),
              boxnew(
                inputId = "AVA04", # AVA04 Resultado da avaliação por questão ----
                width_box = 8,
                status = "warning",
                boxtitle = "Resultado da avaliação por questão",
                menu_selected = c("typeplot", "groupplot", "dimension"),
                label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
                choices = list(typeplot = "empty", groupplot = "empty"),
                selected = c(typeplot = "column", groupplot = "empty", dimension = "empty"))))
        })
      } else {
        output$item_avaliacao <- renderUI({
          fluidRow(
            column(width = 5, callout(title = "Informação", message = "Não há dados neste módulo relacionados a esta unidade")))
        })
      }
    })
    
    observeEvent(input$geral_unidade, {
      if(input$geral_unidade == "CCEI"){
        output$ava_orc_2019 <- renderUI({
          fluidRow(
            column(width = 12,
                   column(width = 3,
                          selectInput(
                            inputId = "avaliadores", 
                            label = "Avaliadores", 
                            choices = c("TODOS", unique(db_orcano_gr$CARGO)), 
                            selected = "TODOS")),
                   column(width = 3,
                          selectInput(
                            inputId = "ava_grupodespesa", 
                            label = "Grupo de despesa", 
                            choices = c("CUSTEIO","INVESTIMENTO"), 
                            selected = "CUSTEIO")),
                   column(width = 6,
                          callout(message = "Resultado da pesquisa sobre a priorização do orçamento 2019 do Campus
                                  Ceilândia, realizada entre 12 e 31 de dezembro de 2018, com a participação de 
                                  alunos, técnicos e docentes.", 
                                  type = "info"))),
            boxnew(
              inputId = "AVA05", # AVA05 Número de avaliadores por grupo ----
              width_box = 4,
              status = "warning",
              boxtitle = "Número de avaliadores por grupo",
              menu_selected = c("dimension"),
              label = c(dimension = "3D"),
              selected = c(dimension = "empty")),
            boxnew(
              inputId = "AVA06", # AVA06 Priorização do orçamento por categoria de gasto ----
              width_box = 8,
              status = "warning",
              boxtitle = "Priorização do orçamento por categoria de gasto",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "percent", dimension = "empty")),
            boxnew(
              inputId = "AVA07", # AVA07 Categoria de gasto por priorização do orçamento ----
              width_box = 12, 
              status = "warning",
              boxtitle = "Categoria de gasto por priorização do orçamento",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "spline", groupplot = "empty", dimension = "empty")),
            box(
              width = 12,      # AVA08 Tabela de dados - priorização do orçamento ----
              title = "Tabela de dados - priorização do orçamento",
              solidHeader = T,
              collapsible = T,
              collapsed = F,
                DT::dataTableOutput("datatable01")))
        })
      } else {
        output$ava_orc_2019 <- renderUI({
          fluidRow(
            column(width = 5, callout(title = "Informação", message = "Não há dados neste módulo relacionados a esta unidade")))
        })
      }
    })

# ORÇAMENTO ---------------------------------------------------------------
    color_plot <- eventReactive(input$action_color, {
      db <- input$color_select
      db <- strsplit(db, ",")
      db <- db[[1]]
      db
    })
    # ORC01 Execução da despesa por ano ----
    dbpORC01 <- reactive({
      if (input$geral_unidade == "IFB"){
        db <- db_siafi %>%
          filter(
            CODIGO_GRUPO_DE_DESPESA %in% c(input$grupodespesa)) %>%
          group_by(
            ANO = substr(LANCAMENTO, 1, 4), 
            SIGLA_UNIDADE_GESTORA = "IFB") %>%
          summarise(
            SERIE1 = sum(EMPENHADO, na.rm = TRUE),
            SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
            SERIE3 = sum(PAGO, na.rm = TRUE),
            SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
        }else{
          ano <- unique(substr(db_siafi[db_siafi$SIGLA_UNIDADE_GESTORA == input$geral_unidade, ]$LANCAMENTO, 1, 4))
          if (input$geral_unidade == "CCEI") ano <- ano[!ano == "2016"]
          db <- db_siafi %>%
            filter(
              CODIGO_GRUPO_DE_DESPESA %in% c(input$grupodespesa), 
              substr(LANCAMENTO, 1, 4) %in% ano, 
              SIGLA_UNIDADE_GESTORA == input$geral_unidade) %>%
            group_by(
              ANO = substr(LANCAMENTO, 1, 4), 
              SIGLA_UNIDADE_GESTORA) %>%
            summarise(
              SERIE1 = sum(EMPENHADO, na.rm = TRUE),
              SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
              SERIE3 = sum(PAGO, na.rm = TRUE),
              SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
          }
      db
      })
    output$plotORC01 <- renderHighchart({
      highchart_new(
        data = dbpORC01(),
        series = c(SERIE1 = "EMPENHADO", SERIE2 = "LIQUIDADO", SERIE3 = "PAGO", SERIE_RAP = "RAP PAGO"),
        subtitle = paste(unique(dbpORC01()$SIGLA_UNIDADE_GESTORA), " ", min(dbpORC01()$ANO), "a", max(dbpORC01()$ANO)),
        categories = unique(dbpORC01()$ANO),
        credits = "Portal da Transparência",
        input_plot = c(
          typeplot = input$typeplotORC01, 
          dimension = input$dimensionORC01, 
          groupplot = input$groupplotORC01),
        origin = "orcamento",
        colors = paste(hccolor$exeorc))
    })
    # ORC02 Valores pagos por exercício de referência ----
    dbpORC02 <- reactive({
      if (input$geral_unidade == "IFB"){
        db <- db_siafi %>%
          filter(
            CODIGO_GRUPO_DE_DESPESA %in% c(input$grupodespesa)) %>%
          group_by(
            ANO = substr(LANCAMENTO, 1, 4), 
            SIGLA_UNIDADE_GESTORA = "IFB") %>%
          summarise(
            SERIE1 = sum(PAGO, na.rm = TRUE),
            SERIE2 = sum(RAP_PAGO, na.rm = TRUE))
      }else{
        ano <- unique(substr(db_siafi[db_siafi$SIGLA_UNIDADE_GESTORA == input$geral_unidade, ]$LANCAMENTO, 1, 4))
        if (input$geral_unidade == "CCEI") ano <- ano[!ano == "2016"]
        db <- db_siafi %>%
          filter(
            CODIGO_GRUPO_DE_DESPESA %in% c(input$grupodespesa), 
            substr(LANCAMENTO, 1, 4) %in% ano, 
            SIGLA_UNIDADE_GESTORA == input$geral_unidade) %>%
          group_by(
            ANO = substr(LANCAMENTO, 1, 4), 
            SIGLA_UNIDADE_GESTORA) %>%
          summarise(
            SERIE1 = sum(PAGO, na.rm = TRUE),
            SERIE2 = sum(RAP_PAGO, na.rm = TRUE))
        }
      db
    })
    output$plotORC02 <- renderHighchart({
      highchart_new(
        data = dbpORC02(), 
        series = c(SERIE1 = "Despesas do exercício", SERIE2 = "Despesas de exercícios anteriores"),
        subtitle = paste(unique(dbpORC02()$SIGLA_UNIDADE_GESTORA), min(dbpORC02()$ANO), "a", max(dbpORC02()$ANO)),
        categories = unique(dbpORC02()$ANO),
        credits = "Portal da Transparência",
        input_plot = c( 
          typeplot = input$typeplotORC02, 
          dimension = input$dimensionORC02, 
          groupplot = input$groupplotORC02),
        origin = "orcamento",
        colors = paste(hccolor$desexe))
    })
    # ORC03 Execução da despesa por mês (acumulado) ----
    dbpORC03 <- reactive({
      if (input$geral_unidade == "IFB") {
        db <- db_siafi %>%
          filter(
            CODIGO_GRUPO_DE_DESPESA %in% c(input$grupodespesa), 
            substr(LANCAMENTO, 1, 4) == input$geral_exercicio) %>%
          group_by(
            LANCAMENTO, 
            SIGLA_UNIDADE_GESTORA = "IFB") %>%
          summarise(
            SERIE1 = sum(EMPENHADO, na.rm = TRUE),
            SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
            SERIE3 = sum(PAGO, na.rm = TRUE),
            SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
        db$SERIE1 <- cumsum(db$SERIE1)
        db$SERIE2 <- cumsum(db$SERIE2)
        db$SERIE3 <- cumsum(db$SERIE3)
        db$SERIE_RAP <- cumsum(db$SERIE_RAP)
        } else {
        db <- db_siafi %>%
          filter(
            CODIGO_GRUPO_DE_DESPESA %in% input$grupodespesa, 
            substr(LANCAMENTO, 1, 4) == input$geral_exercicio, 
            SIGLA_UNIDADE_GESTORA == input$geral_unidade) %>%
          group_by(
            LANCAMENTO, 
            SIGLA_UNIDADE_GESTORA) %>%
          summarise(
            SERIE1 = sum(EMPENHADO, na.rm = TRUE),
            SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
            SERIE3 = sum(PAGO, na.rm = TRUE),
            SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
        db$SERIE1 <- cumsum(db$SERIE1)
        db$SERIE2 <- cumsum(db$SERIE2)
        db$SERIE3 <- cumsum(db$SERIE3)
        db$SERIE_RAP <- cumsum(db$SERIE_RAP)
        }
      db
    })
    output$plotORC03 <- renderHighchart({
      highchart_new(
        data = dbpORC03(),
        series = c(SERIE1 = "EMPENHADO", SERIE2 = "LIQUIDADO", SERIE3 = "PAGO", SERIE_RAP = "RAP PAGO"),
        subtitle = unique(paste(dbpORC03()$SIGLA_UNIDADE_GESTORA, substr(dbpORC03()$LANCAMENTO, 1, 4))),
        categories = dbpORC03()$LANCAMENTO,
        credits = "Portal da Transparência",
        input_plot = c(
          typeplot = input$typeplotORC03, 
          dimension = input$dimensionORC03, 
          groupplot = input$groupplotORC03),
          origin = "orcamento",
          colors = paste(hccolor$exeorc))
    })
    # ORC04 Execução da despesa por mês (não acumulado) ----
    dbpORC04 <- reactive({
      if (input$geral_unidade == "IFB") {
        db <- db_siafi %>%
          filter(
            CODIGO_GRUPO_DE_DESPESA %in% input$grupodespesa, 
            substr(LANCAMENTO, 1, 4) == input$geral_exercicio) %>%
          group_by(
            LANCAMENTO, 
            SIGLA_UNIDADE_GESTORA = "IFB") %>%
          summarise(
            SERIE1 = sum(EMPENHADO, na.rm = TRUE),
            SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
            SERIE3 = sum(PAGO, na.rm = TRUE),
            SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
        } else{
        db <- db_siafi %>%
          filter(
            CODIGO_GRUPO_DE_DESPESA %in% input$grupodespesa,
            substr(LANCAMENTO, 1, 4) == input$geral_exercicio,
            SIGLA_UNIDADE_GESTORA == input$geral_unidade) %>%
          group_by(
            LANCAMENTO, 
            SIGLA_UNIDADE_GESTORA) %>%
          summarise(
            SERIE1 = sum(EMPENHADO, na.rm = TRUE),
            SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
            SERIE3 = sum(PAGO, na.rm = TRUE),
            SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
        }
      db
    })
    output$plotORC04 <- renderHighchart({
      highchart_new(
        data = dbpORC04(),
        series = c(SERIE1 = "EMPENHADO", SERIE2 = "LIQUIDADO", SERIE3 = "PAGO", SERIE_RAP = "RAP PAGO"),
        subtitle = unique(paste(dbpORC04()$SIGLA_UNIDADE_GESTORA, substr(dbpORC04()$LANCAMENTO, 1, 4))),
        categories = dbpORC04()$LANCAMENTO,
        credits = "Portal da Transparência",
        input_plot = c( 
          typeplot = input$typeplotORC04, 
          dimension = input$dimensionORC04, 
          groupplot = input$groupplotORC04),
        origin = "orcamento",
        colors = paste(hccolor$exeorc))
    })
    # ORC05 Execução da despesa por programa ----
    dbpORC05 <- reactive({
      if (input$geral_unidade == "IFB") {
        db <- db_siafi %>%
          filter(
            CODIGO_GRUPO_DE_DESPESA %in% input$grupodespesa,
            substr(LANCAMENTO, 1, 4) == input$geral_exercicio) %>%
          group_by(
            ANO = substr(LANCAMENTO, 1, 4),
            SIGLA_UNIDADE_GESTORA = "IFB", 
            PROGRAMA = paste(CODIGO_PROGRAMA_ORCAMENTARIO, NOME_PROGRAMA_RESUMO)) %>%
          summarise(
            SERIE1 = sum(EMPENHADO, na.rm = TRUE),
            SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
            SERIE3 = sum(PAGO, na.rm = TRUE),
            SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      } else{
        db <- db_siafi %>%
          filter(
            SIGLA_UNIDADE_GESTORA == input$geral_unidade,
            substr(LANCAMENTO, 1, 4) == input$geral_exercicio, 
            CODIGO_GRUPO_DE_DESPESA %in% input$grupodespesa) %>%
          group_by(
            ANO = substr(LANCAMENTO, 1, 4), 
            SIGLA_UNIDADE_GESTORA, 
            PROGRAMA = paste(CODIGO_PROGRAMA_ORCAMENTARIO, NOME_PROGRAMA_RESUMO)) %>%
          summarise(
            SERIE1 = sum(EMPENHADO, na.rm = TRUE),
            SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
            SERIE3 = sum(PAGO, na.rm = TRUE),
            SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      }
      db$PROGRAMA <- str_to_title(db$PROGRAMA)
      db
    })
    output$plotORC05 <- renderHighchart({
      highchart_new(
        data = dbpORC05(),
        series = c(SERIE1 = "EMPENHADO", SERIE2 = "LIQUIDADO", SERIE3 = "PAGO", SERIE_RAP = "RAP PAGO"),
        subtitle = paste(input$geral_unidade, input$geral_exercicio),
        categories = dbpORC05()$PROGRAMA,
        credits = "Portal da Transparência",
        input_plot = c(
          typeplot = input$typeplotORC05, 
          dimension = input$dimensionORC05, 
          groupplot = input$groupplotORC05),
        origin = "orcamento",
        colors = paste(hccolor$exeorc))
    })
    # ORC06 Execução da despesa por grupo de despesa ----
    dbpORC06 <- reactive({
      if (input$geral_unidade == "IFB") {
        db <- db_siafi %>%
          filter(
            CODIGO_GRUPO_DE_DESPESA %in% input$grupodespesa, 
            substr(LANCAMENTO, 1, 4) == input$geral_exercicio) %>%
          group_by(
            ANO = substr(LANCAMENTO, 1, 4), 
            SIGLA_UNIDADE_GESTORA = "IFB",
            NOME_GRUPO_DE_DESPESA) %>%
          summarise(
            SERIE1 = sum(EMPENHADO, na.rm = TRUE),
            SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
            SERIE3 = sum(PAGO, na.rm = TRUE),
            SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      } else{
        db <- db_siafi %>%
          filter(
            CODIGO_GRUPO_DE_DESPESA %in% input$grupodespesa,
            substr(LANCAMENTO, 1, 4) == input$geral_exercicio, 
            SIGLA_UNIDADE_GESTORA == input$geral_unidade) %>%
          group_by(
            ANO = substr(LANCAMENTO, 1, 4), 
            SIGLA_UNIDADE_GESTORA, 
            NOME_GRUPO_DE_DESPESA) %>%
          summarise(
            SERIE1 = sum(EMPENHADO, na.rm = TRUE),
            SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
            SERIE3 = sum(PAGO, na.rm = TRUE),
            SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      }
      db$NOME_GRUPO_DE_DESPESA <- str_to_title(db$NOME_GRUPO_DE_DESPESA)
      db
    })
    output$plotORC06 <- renderHighchart({
      highchart_new(
        data = dbpORC06(),
        series = c(SERIE1 = "EMPENHADO", SERIE2 = "LIQUIDADO", SERIE3 = "PAGO", SERIE_RAP = "RAP PAGO"),
        subtitle = paste(input$geral_unidade, input$geral_exercicio),
        categories = dbpORC06()$NOME_GRUPO_DE_DESPESA,
        credits = "Portal da Transparência",
        input_plot = c( 
          typeplot = input$typeplotORC06, 
          dimension = input$dimensionORC06, 
          groupplot = input$groupplotORC06),
        origin = "orcamento",
        colors = paste(hccolor$exeorc))
    })
    # ORC07 Execução da despesa por ação orçamentária ----
    dbpORC07 <- reactive({
      if (input$geral_unidade == "IFB") {
        db <- db_siafi %>%
          filter(
            CODIGO_GRUPO_DE_DESPESA %in% input$grupodespesa, 
            substr(LANCAMENTO, 1, 4) == input$geral_exercicio) %>%
          group_by(
            ANO = substr(LANCAMENTO, 1, 4),
            CODIGO_GRUPO_DE_DESPESA, 
            SIGLA_UNIDADE_GESTORA = "IFB", 
            NOME_ACAO_RESUMO) %>%
          summarise(
            SERIE1 = sum(EMPENHADO, na.rm = TRUE),
            SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
            SERIE3 = sum(PAGO, na.rm = TRUE),
            SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      } else{
        db <- db_siafi %>%
          filter(
            CODIGO_GRUPO_DE_DESPESA %in% input$grupodespesa, 
            substr(LANCAMENTO, 1, 4) == input$geral_exercicio, 
            SIGLA_UNIDADE_GESTORA == input$geral_unidade) %>%
          group_by(
            ANO = substr(LANCAMENTO, 1, 4), 
            SIGLA_UNIDADE_GESTORA, 
            CODIGO_ACAO, 
            NOME_ACAO_RESUMO) %>%
          summarise(
            SERIE1 = sum(EMPENHADO, na.rm = TRUE),
            SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
            SERIE3 = sum(PAGO, na.rm = TRUE),
            SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      }
      db
    })
    output$plotORC07 <- renderHighchart({
      highchart_new(
        data = dbpORC07(),  
        series = c(SERIE1 = "EMPENHADO", SERIE2 = "LIQUIDADO", SERIE3 = "PAGO", SERIE_RAP = "RAP PAGO"),
        subtitle = paste(input$geral_unidade, input$geral_exercicio),
        categories = dbpORC07()$NOME_ACAO_RESUMO,
        credits = "Portal da Transparência",
        input_plot = c( 
          typeplot = input$typeplotORC07, 
          dimension = input$dimensionORC07, 
          groupplot = input$groupplotORC07),
        origin = "orcamento",
        colors = paste(hccolor$exeorc))
    })
    # ORC08 Execução da despesa por elemento da despesa ----
    dbpORC08 <- reactive({
      if (input$geral_unidade == "IFB") {
        db <- db_siafi %>%
          filter(
            CODIGO_GRUPO_DE_DESPESA %in% input$grupodespesa, 
            substr(LANCAMENTO, 1, 4) == input$geral_exercicio) %>%
          group_by(
            ANO = substr(LANCAMENTO, 1, 4), 
            SIGLA_UNIDADE_GESTORA = "IFB", 
            NOME_ELEMENTO_DE_DESPESA) %>%
          summarise(
            SERIE1 = sum(EMPENHADO, na.rm = TRUE),
            SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
            SERIE3 = sum(PAGO, na.rm = TRUE),
            SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      } else{
        db <- db_siafi %>%
          filter(
            CODIGO_GRUPO_DE_DESPESA %in% input$grupodespesa, 
            substr(LANCAMENTO, 1, 4) == input$geral_exercicio, 
            SIGLA_UNIDADE_GESTORA == input$geral_unidade) %>%
          group_by(
            ANO = substr(LANCAMENTO, 1, 4), 
            SIGLA_UNIDADE_GESTORA, 
            NOME_ELEMENTO_DE_DESPESA) %>%
          summarise(
            SERIE1 = sum(EMPENHADO, na.rm = TRUE),
            SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
            SERIE3 = sum(PAGO, na.rm = TRUE),
            SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      }
      db$NOME_ELEMENTO_DE_DESPESA <- str_to_title(db$NOME_ELEMENTO_DE_DESPESA)
      db
    })
    output$plotORC08 <- renderHighchart({
      highchart_new(
        data = dbpORC08(),
        series = c(SERIE1 = "EMPENHADO", SERIE2 = "LIQUIDADO", SERIE3 = "PAGO", SERIE_RAP = "RAP PAGO"),
        subtitle = paste(input$geral_unidade, input$geral_exercicio),
        categories = dbpORC08()$NOME_ELEMENTO_DE_DESPESA,
        credits = "Portal da Transparência",
        input_plot = c(
          typeplot = input$typeplotORC08, 
          dimension = input$dimensionORC08, 
          groupplot = input$groupplotORC08),
        origin = "orcamento",
        colors = paste(hccolor$exeorc))
    })
    # ORC09 Execução da despesa por unidade gestora ----
    dbpORC09 <- reactive({
      if(input$departmentORC09 == "Sem Reitoria") {
        ug <- unique(db_siafi[!db_siafi$SIGLA_UNIDADE_GESTORA %in% c("RIFB", "OUTROS"), ]$SIGLA_UNIDADE_GESTORA)
      } else{
        ug <- unique(db_siafi[!db_siafi$SIGLA_UNIDADE_GESTORA == "OUTROS", ]$SIGLA_UNIDADE_GESTORA)
      }
      db <- db_siafi %>%
        filter(
          CODIGO_GRUPO_DE_DESPESA %in% input$grupodespesa, 
          substr(LANCAMENTO, 1, 4) == input$geral_exercicio, 
          SIGLA_UNIDADE_GESTORA %in% ug) %>%
        group_by(
          ANO = substr(LANCAMENTO, 1, 4), 
          SIGLA_UNIDADE_GESTORA) %>%
        summarise(
          SERIE1 = sum(EMPENHADO, na.rm = TRUE),
          SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
          SERIE3 = sum(PAGO, na.rm = TRUE),
          SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
    })
    output$plotORC09 <- renderHighchart({
      highchart_new(
        data = dbpORC09(),
        series = c(SERIE1 = "EMPENHADO", SERIE2 = "LIQUIDADO", SERIE3 = "PAGO", SERIE_RAP = "RAP PAGO"),
        subtitle = unique(dbpORC09()$ANO),
        categories = unique(dbpORC09()$SIGLA_UNIDADE_GESTORA),
        credits = "Portal da Transparência",
        input_plot = c(
          typeplot = input$typeplotORC09, 
          dimension = input$dimensionORC09, 
          groupplot = input$groupplotORC09),
        origin = "orcamento",
        colors = paste(hccolor$exeorc))
    })
    # ORC10 Execução da despesa por campi e reitoria ----
    dbpORC10 <- reactive({
      db <- db_siafi %>%
        filter(
          CODIGO_GRUPO_DE_DESPESA %in% input$grupodespesa,
          substr(LANCAMENTO, 1, 4) == input$geral_exercicio,
          SIGLA_UNIDADE_GESTORA %in% c("RIFB")) %>%
        group_by(
          ANO = substr(LANCAMENTO, 1, 4), 
          SIGLA_UNIDADE_GESTORA) %>%
        summarise(
          SERIE1 = sum(EMPENHADO, na.rm = TRUE),
          SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
          SERIE3 = sum(PAGO, na.rm = TRUE),
          SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      db1 <- db_siafi %>%
        filter(
          CODIGO_GRUPO_DE_DESPESA %in% input$grupodespesa,
          substr(LANCAMENTO, 1, 4) == input$geral_exercicio,
          !SIGLA_UNIDADE_GESTORA %in% c("RIFB", "OUTROS")) %>%
        group_by(
          ANO = substr(LANCAMENTO, 1, 4), 
          SIGLA_UNIDADE_GESTORA = "CAMPI") %>%
        summarise(
          SERIE1 = sum(EMPENHADO, na.rm = TRUE),
          SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
          SERIE3 = sum(PAGO, na.rm = TRUE),
          SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      db <- bind_rows(db, db1)
      db
    })
    output$plotORC10 <- renderHighchart({
      highchart_new(
        data = dbpORC10(),
        series = c(SERIE1 = "EMPENHADO", SERIE2 = "LIQUIDADO", SERIE3 = "PAGO", SERIE_RAP = "RAP PAGO"),
        subtitle = paste("CAMPI E REITORIA ", unique(dbpORC10()$ANO)),
        categories = dbpORC10()$SIGLA_UNIDADE_GESTORA,
        credits = "Portal da Transparência",
        input_plot = c( 
          typeplot = input$typeplotORC10, 
          dimension = input$dimensionORC10, 
          groupplot = input$groupplotORC10),
        origin = "orcamento",
        colors = paste(hccolor$exeorc))
    })
    # ORC11 Pagamento no exercício e de exercícios anteriores (RAP) ----
    dbpORC11 <- reactive({
      if(input$departmentORC11 == "Sem Reitoria") {
        ug <- unique(db_siafi[!db_siafi$SIGLA_UNIDADE_GESTORA %in% c("RIFB", "OUTROS"), ]$SIGLA_UNIDADE_GESTORA)
      } else{
        ug <- unique(db_siafi[!db_siafi$SIGLA_UNIDADE_GESTORA == "OUTROS", ]$SIGLA_UNIDADE_GESTORA)
      }
      db <- db_siafi %>%
        filter(
          CODIGO_GRUPO_DE_DESPESA %in% input$grupodespesa, 
          substr(LANCAMENTO, 1, 4) == input$geral_exercicio, 
          SIGLA_UNIDADE_GESTORA %in% ug) %>%
        group_by(
          ANO = substr(LANCAMENTO, 1, 4), 
          SIGLA_UNIDADE_GESTORA) %>%
        summarise(
          SERIE1 = sum(PAGO, na.rm = TRUE),
          SERIE2 = sum(RAP_PAGO, na.rm = TRUE))
      db
    })
    output$plotORC11 <- renderHighchart({
      highchart_new(
        data = dbpORC11(),
        series = c(SERIE1 = "Despesas do exercício", SERIE2 = "Despesas de exercícios anteriores"),
        subtitle = unique(dbpORC11()$ANO),
        categories = dbpORC11()$SIGLA_UNIDADE_GESTORA,
        credits = "Portal da Transparência",
        input_plot = c( 
          typeplot = input$typeplotORC11, 
          dimension = input$dimensionORC11, 
          groupplot = input$groupplotORC11),
        origin = "orcamento",
        colors = paste(hccolor$desexe))
    })
    # ORC12 Pagamento no exercício e de exercícios anteriores (Campi e Reitoria) ----
    dbpORC12 <- reactive({
      db <- db_siafi %>%
        filter(
          CODIGO_GRUPO_DE_DESPESA %in% input$grupodespesa,
          substr(LANCAMENTO, 1, 4) == input$geral_exercicio,
          SIGLA_UNIDADE_GESTORA %in% c("RIFB")) %>%
        group_by(
          ANO = substr(LANCAMENTO, 1, 4), 
          SIGLA_UNIDADE_GESTORA) %>%
        summarise(
          SERIE1 = sum(PAGO, na.rm = TRUE),
          SERIE2 = sum(RAP_PAGO, na.rm = TRUE))
      db1 <- db_siafi %>%
        filter(
          CODIGO_GRUPO_DE_DESPESA %in% input$grupodespesa,
          substr(LANCAMENTO, 1, 4) == input$geral_exercicio,
          !SIGLA_UNIDADE_GESTORA %in% c("RIFB", "OUTROS")) %>%
        group_by(
          ANO = substr(LANCAMENTO, 1, 4), 
          SIGLA_UNIDADE_GESTORA = "CAMPI") %>%
        summarise(
          SERIE1 = sum(PAGO, na.rm = TRUE),
          SERIE2 = sum(RAP_PAGO, na.rm = TRUE))
      db <- bind_rows(db, db1)
      db
    })
    output$plotORC12 <- renderHighchart({
      highchart_new(
        data = dbpORC12(),
        series = c(SERIE1 = "Despesas do exercício", SERIE2 = "Despesas de exercícios anteriores"),
        subtitle = paste("CAMPI E REITORIA ", unique(dbpORC12()$ANO)),
        categories = dbpORC12()$SIGLA_UNIDADE_GESTORA,
        credits = "Portal da Transparência",
        input_plot = c(
          typeplot = input$typeplotORC12, 
          dimension = input$dimensionORC12, 
          groupplot = input$groupplotORC12),
        origin = "orcamento",
        colors = paste(hccolor$desexe))
    })

# PESSOAL -----------------------------------------------------------------
    # PES01 Número de técnicos e docentes por ano ----
    dbpPES01 <- reactive({
      last <- max(sp_quadro$DATA_REFERENCIA)
      if (input$geral_unidade == "IFB"){
        db <- sp_quadro %>% 
          filter(
            DATA_REFERENCIA == last | grepl("*12$", DATA_REFERENCIA)) %>% 
          group_by(
            DATA_REFERENCIA) %>% 
          summarise(
            SERIE1 = sum(TOTAL, na.rm = TRUE),
            SERIE2 = sum(DOCENTE, na.rm = TRUE),
            SERIE3 = sum(TECNICO, na.rm = TRUE))
        db$UORG_LOTACAO_GRUPO <- "IFB"
      } else {
        db <- sp_quadro %>% 
          filter(
            UORG_LOTACAO_GRUPO == input$geral_unidade, 
            DATA_REFERENCIA == last | grepl("*12$", DATA_REFERENCIA)) %>% 
          group_by(
            DATA_REFERENCIA, 
            UORG_LOTACAO_GRUPO) %>% 
          summarise(
            SERIE1 = sum(TOTAL, na.rm = TRUE),
            SERIE2 = sum(DOCENTE, na.rm = TRUE),
            SERIE3 = sum(TECNICO, na.rm = TRUE))
      }
      db$DATA_REFERENCIA <- substr(db$DATA_REFERENCIA, 1, 4)
      db
    })
    output$plotPES01 <- renderHighchart({
      highchart_new(
        data = dbpPES01(),
        series = c(SERIE1 = "TOTAL",SERIE2 = "DOCENTE", SERIE3 = "TECNICO"),
        subtitle = unique(dbpPES01()$UORG_LOTACAO_GRUPO),
        categories = dbpPES01()$DATA_REFERENCIA,
        credits = "Portal da Transparência",
        input_plot = c(
          typeplot = input$typeplotPES01, 
          dimension = input$dimensionPES01, 
          groupplot = input$groupplotPES01),
        origin = "orcamento",
        colors = paste(hccolor$pesevo))
    })
    # PES02 Percentual de técnicos e docentes por ano ----
    dbpPES02 <- reactive({
      last <- max(sp_quadro$DATA_REFERENCIA)
      if (input$geral_unidade == "IFB"){
        db <- sp_quadro %>% 
          filter(
            DATA_REFERENCIA == last | grepl("*12$", DATA_REFERENCIA)) %>% 
          group_by(
            DATA_REFERENCIA) %>% 
          summarise(
            SERIE1 = sum(DOCENTE, na.rm = TRUE),
            SERIE2 = sum(TECNICO, na.rm = TRUE))
        db$UORG_LOTACAO_GRUPO <- "IFB"
      } else {
        db <- sp_quadro %>% 
          filter(
            UORG_LOTACAO_GRUPO == input$geral_unidade, 
            DATA_REFERENCIA == last | grepl("*12$", DATA_REFERENCIA)) %>% 
          group_by(
            DATA_REFERENCIA, 
            UORG_LOTACAO_GRUPO) %>% 
          summarise(
            SERIE1 = sum(DOCENTE, na.rm = TRUE),
            SERIE2 = sum(TECNICO, na.rm = TRUE))
      }
      db$DATA_REFERENCIA <- substr(db$DATA_REFERENCIA, 1, 4)
      db
    })
    output$plotPES02 <- renderHighchart({
      highchart_new(
        data = dbpPES02(),
        series = c(SERIE1 = "DOCENTE",SERIE2 = "TECNICO"),
        subtitle = unique(dbpPES02()$UORG_LOTACAO_GRUPO),
        categories = dbpPES02()$DATA_REFERENCIA,
        credits = "Portal da Transparência",
        input_plot = c(
          typeplot = input$typeplotPES02, 
          dimension = input$dimensionPES02, 
          groupplot = input$groupplotPES02),
        origin = "orcamento",
        colors = paste(hccolor$pesevo[c("doc", "tec")]))
    })
    # PES03 Múmero de técnicos e docentes por mês ----
    dbpPES03 <- reactive({
      if (input$geral_unidade == "IFB"){
        db <- sp_quadro %>% 
          group_by(
            REF = paste0(substr(DATA_REFERENCIA,1,4),"/",substr(DATA_REFERENCIA,5,6))) %>% 
          summarise(
            SERIE1 = sum(TOTAL, na.rm = TRUE),
            SERIE2 = sum(DOCENTE, na.rm = TRUE),
            SERIE3 = sum(TECNICO, na.rm = TRUE))
        db$UORG_LOTACAO_GRUPO <- "IFB"
      } else {
        db <- sp_quadro %>% 
          filter(
            UORG_LOTACAO_GRUPO == input$geral_unidade) %>% 
          group_by(
            REF = paste0(substr(DATA_REFERENCIA,1,4),"/",substr(DATA_REFERENCIA,5,6)), 
            UORG_LOTACAO_GRUPO) %>% 
          summarise(
            SERIE1 = sum(TOTAL, na.rm = TRUE),
            SERIE2 = sum(DOCENTE, na.rm = TRUE),
            SERIE3 = sum(TECNICO, na.rm = TRUE))
      }
      db
    })
    output$plotPES03 <- renderHighchart({
      highchart_new(
        data = dbpPES03(),
        series = c(SERIE1 = "TOTAL",SERIE2 = "DOCENTE", SERIE3 = "TECNICO"),
        subtitle = unique(dbpPES03()$UORG_LOTACAO_GRUPO),
        categories = dbpPES03()$REF,
        credits = "Portal da Transparência",
        input_plot = c(
          typeplot = input$typeplotPES03, 
          dimension = input$dimensionPES03, 
          groupplot = input$groupplotPES03),
        origin = "orcamento",
        colors = paste(hccolor$pesevo))
    })
    # PES04 Percentual de técnicos e docentes por mês ----
    dbpPES04 <- reactive({
      if (input$geral_unidade == "IFB"){
        db <- sp_quadro %>% 
          group_by(
            MES = paste0(substr(DATA_REFERENCIA,1,4),"/", substr(DATA_REFERENCIA,5,6))) %>% 
          summarise(
            SERIE1 = sum(DOCENTE, na.rm = TRUE),
            SERIE2 = sum(TECNICO, na.rm = TRUE))
        db$UORG_LOTACAO_GRUPO <- "IFB"
      } else {
        db <- sp_quadro %>% 
          filter(
            UORG_LOTACAO_GRUPO == input$geral_unidade) %>% 
          group_by(
            MES = paste0(substr(DATA_REFERENCIA,1,4),"/", substr(DATA_REFERENCIA,5,6)), 
            UORG_LOTACAO_GRUPO) %>% 
          summarise(
            SERIE1 = sum(DOCENTE, na.rm = TRUE),
            SERIE2 = sum(TECNICO, na.rm = TRUE))
      }
      db
    })
    output$plotPES04 <- renderHighchart({
      highchart_new(
        data = dbpPES04(),
        series = c(SERIE1 = "DOCENTE", SERIE2 = "TECNICO"),
        subtitle = unique(dbpPES04()$UORG_LOTACAO_GRUPO),
        categories = dbpPES04()$MES,
        credits = "Portal da Transparência",
        input_plot = c( 
          typeplot = input$typeplotPES04, 
          dimension = input$dimensionPES04, 
          groupplot = input$groupplotPES04),
        origin = "orcamento",
        colors = paste(hccolor$pesevo[c("doc", "tec")]))
    })
    # PES05 Servidores por cargo ----
    dbpPES05 <- reactive({
      if (input$geral_unidade == "IFB"){
        tm <- sp_cargo %>% 
          filter(
            DATA_REFERENCIA == max(sp_cargo$DATA_REFERENCIA)) %>% 
          group_by(
            DESCRICAO_CARGO) %>% 
          summarise(
            SERIE = sum(TOTAL, na.rm = TRUE))
      } else {
        tm <- sp_cargo %>% 
          filter(
            DATA_REFERENCIA == max(sp_cargo$DATA_REFERENCIA), 
            UORG_LOTACAO_GRUPO == input$geral_unidade) %>% 
          group_by(
            DESCRICAO_CARGO) %>% 
          summarise(
            SERIE = sum(TOTAL, na.rm = TRUE))
      }
      tm <- treemap(
        tm, 
        index = "DESCRICAO_CARGO", 
        vSize = "SERIE", 
        vColor =  "SERIE",
        type = "value", 
        palette = paste(hccolor$palblu[14:19]), 
        pdf(file = NULL))
      tm
    })
    output$plotPES05 <- renderHighchart({
      hctreemap(
          tm = dbpPES05()) %>% 
        hc_exporting(
          enabled = TRUE) %>% 
        hc_credits(
          enabled = TRUE, 
          text = paste("Fonte:", "Portal da Transparência"),
          href = "http://portaltransparencia.gov.br/download-de-dados") %>% 
        hc_title(
          text = paste(input$geral_unidade, "-", max(paste0(substr(db_siape$DATA_REFERENCIA, 1 , 4),"/",substr(db_siape$DATA_REFERENCIA, 6, 7)))),
          style = list(fontSize = "12px"))
    })
    # PES06 Servidores por situação do vínculo ----
    dbpPES06 <- reactive({
      if (input$geral_unidade == "IFB"){
        tm <- db_siape %>% 
          filter(
            DATA_REFERENCIA == max(db_siape$DATA_REFERENCIA), 
            SIGLA_FUNCAO == "-1", 
            COD_ORG_LOTACAO == "26428") %>% 
          group_by(
            SITUACAO_VINCULO) %>% 
          summarise(
            SERIE = n())
      } else {
        tm <- db_siape %>% 
          filter(
            DATA_REFERENCIA == max(db_siape$DATA_REFERENCIA), 
            SIGLA_FUNCAO == "-1", 
            COD_ORG_LOTACAO == "26428", 
            UORG_LOTACAO_GRUPO == input$geral_unidade) %>% 
          group_by(
            SITUACAO_VINCULO) %>% 
          summarise(
            SERIE = n())
      }
      tm <- treemap(
        tm, 
        index = "SITUACAO_VINCULO", 
        vSize = "SERIE", 
        vColor =  "SERIE",
        type = "value", 
        palette = paste(hccolor$palblu[14:19]), 
        pdf(file = NULL))
      tm
    })
    output$plotPES06 <- renderHighchart({
      hctreemap(
        tm = dbpPES06()) %>% 
        hc_exporting(
          enabled = TRUE) %>% 
        hc_credits(
          enabled = TRUE, 
          text = paste("Fonte:", "Portal da Transparência"), 
          href = "http://portaltransparencia.gov.br/download-de-dados") %>% 
        hc_title(
          text = paste(input$geral_unidade, "-", max(paste0(substr(db_siape$DATA_REFERENCIA, 1 , 4),"/",substr(db_siape$DATA_REFERENCIA, 6, 7)))),
          style = list(fontSize = "12px"))
    })
    # PES07 Servidores por unidade de exercício ----
    dbpPES07 <- reactive({
      if (input$geral_unidade == "IFB"){
        tm <- db_siape %>% 
          filter(
            DATA_REFERENCIA == max(db_siape$DATA_REFERENCIA), 
            SIGLA_FUNCAO == "-1", 
            COD_ORG_LOTACAO == "26428") %>% 
          group_by(
            UORG_EXERCICIO) %>% 
          summarise(
            SERIE = n())
      } else {
        tm <- db_siape %>% 
          filter(
            DATA_REFERENCIA == max(db_siape$DATA_REFERENCIA), 
            SIGLA_FUNCAO == "-1", 
            COD_ORG_LOTACAO == "26428", 
            UORG_LOTACAO_GRUPO == input$geral_unidade) %>% 
          group_by(
            UORG_EXERCICIO) %>% 
          summarise(
            SERIE = n())
      }
      tm <- treemap(
        tm, 
        index = "UORG_EXERCICIO", 
        vSize = "SERIE", 
        vColor =  "SERIE",
        type = "value", 
        palette = paste(hccolor$palblu[14:19]), 
        pdf(file = NULL))
      tm
    })
    output$plotPES07 <- renderHighchart({
      hctreemap(
        tm = dbpPES07()) %>% 
        hc_exporting(
          enabled = TRUE) %>% 
        hc_credits(
          enabled = TRUE, 
          text = paste("Fonte:", "Portal da Transparência"), 
          href = "http://portaltransparencia.gov.br/download-de-dados") %>% 
        hc_title(
          text = paste(input$geral_unidade, "-", max(paste0(substr(db_siape$DATA_REFERENCIA, 1 , 4),"/",substr(db_siape$DATA_REFERENCIA, 6, 7)))),
          style = list(fontSize = "12px"))
    })
    
    # PES08 Cargos por classe (Docentes) ----
    dbpPES08 <- reactive({
      if (input$geral_unidade == "IFB"){
        db <- db_siape %>% 
          filter(
            DATA_REFERENCIA == max(DATA_REFERENCIA), 
            SIGLA_FUNCAO == "-1", 
            COD_ORG_LOTACAO == "26428", 
            grepl("\\<PROF", DESCRICAO_CARGO), 
            !DESCRICAO_CARGO %in% "PROFESSOR TEMPORARIO") %>% 
          group_by(
            CLASSE = paste(CLASSE_CARGO, substr(NIVEL_CARGO, 1, 1))) %>% 
          summarise(
            TOTAL = n())
      } else {
        db <- db_siape %>% 
          filter(
            DATA_REFERENCIA == max(DATA_REFERENCIA), 
            UORG_LOTACAO_GRUPO == input$geral_unidade, 
            SIGLA_FUNCAO == "-1", COD_ORG_LOTACAO == "26428", 
            grepl("\\<PROF", DESCRICAO_CARGO), 
            !DESCRICAO_CARGO %in% "PROFESSOR TEMPORARIO") %>% 
          group_by(
            CLASSE = paste(CLASSE_CARGO, substr(NIVEL_CARGO, 1, 1))) %>% 
          summarise(
            TOTAL = n())
      }
      db$CLASSE <- str_replace_all(db$CLASSE, c("[1]" = "I", "[2]" = "II", "[3]" = "III", "[4]" = "IV"))
      db$CLASSE <- ifelse(db$CLASSE == "D 5","Titular", db$CLASSE)
      db
    })
    output$plotPES08 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = paste(input$geral_unidade, "-", max(paste0(substr(db_siape$DATA_REFERENCIA, 1 , 4),"/",substr(db_siape$DATA_REFERENCIA, 6, 7))))) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>% 
        hc_chart(type = 'pie', options3d = list(enabled = TRUE, alpha = 45)) %>% 
        hc_plotOptions(pie = list(innerSize = 100, depth = 45)) %>% 
        hc_add_series_labels_values(
          name = "Total",
          labels = dbpPES08()$CLASSE,
          values = dbpPES08()$TOTAL, 
          colors = hccolor$palgra[
            sort(
              seq(
                from = which(names(hccolor$palgra) == "gra95"),
                to = which(names(hccolor$palgra) == "gra10"), 
                by = 3), 
              decreasing = TRUE)]
          [1:length(dbpPES08()$CLASSE)])
      hc
    })
    # PES09 Jornada de trabalho dos docentes e técnicos ----
    dbpPES09 <- reactive({
      if (input$geral_unidade == "IFB"){
        db <- db_siape %>%
          filter(
            DATA_REFERENCIA == max(DATA_REFERENCIA),
            SIGLA_FUNCAO == "-1",
            COD_ORG_LOTACAO == "26428",
            grepl("\\<PROF", DESCRICAO_CARGO)
          ) %>%
          group_by(
            JORNADA_DE_TRABALHO) %>%
          summarise(
            SERIE1 = n())
      } else {
        db <- db_siape %>%
          filter(
            DATA_REFERENCIA == max(DATA_REFERENCIA),
            SIGLA_FUNCAO == "-1",
            COD_ORG_LOTACAO == "26428",
            UORG_LOTACAO_GRUPO == input$geral_unidade,
            grepl("\\<PROF", DESCRICAO_CARGO)
          ) %>%
          group_by(
            JORNADA_DE_TRABALHO) %>%
          summarise(
            SERIE1 = n())
      }
      db
    })
    output$plotPES09 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = paste(input$geral_unidade, "-", max(paste0(substr(db_siape$DATA_REFERENCIA, 1 , 4),"/",substr(db_siape$DATA_REFERENCIA, 6, 7))))) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>% 
        hc_chart(type = 'pie', options3d = list(enabled = TRUE, alpha = 45)) %>% 
        hc_plotOptions(pie = list(innerSize = 090, depth = 45)) %>% 
        hc_add_series_labels_values(
          name = "Total",
          labels = dbpPES09()$JORNADA_DE_TRABALHO,
          values = dbpPES09()$SERIE1, 
          colors = hccolor$palgra[
            sort(
              seq(
                from = which(names(hccolor$palgra) == "gra95"),
                to = which(names(hccolor$palgra) == "gra10"), 
                by = 4), 
              decreasing = TRUE)]
          [1:length(dbpPES09()$JORNADA_DE_TRABALHO)])
      hc
    })
    # PES10 Cargos por classe e padrão ----
    dbpPES10 <- reactive({
      if (input$geral_unidade == "IFB"){
        db <- db_siape %>% 
          filter(
            DATA_REFERENCIA == max(DATA_REFERENCIA),
            SIGLA_FUNCAO == "-1",
            COD_ORG_LOTACAO == "26428",
            grepl("\\<PROF", DESCRICAO_CARGO),
            !DESCRICAO_CARGO == "Inválido") %>% 
          group_by(
            CLASSE = paste(CLASSE_CARGO, substr(NIVEL_CARGO, 1, 1)),
            PADRAO = substr(NIVEL_CARGO, 2, nchar(NIVEL_CARGO))) %>% 
          summarise(
            TOTAL = n())
      } else {
        db <- db_siape %>% 
          filter(
            DATA_REFERENCIA == max(DATA_REFERENCIA),
            SIGLA_FUNCAO == "-1",
            COD_ORG_LOTACAO == "26428",
            UORG_LOTACAO_GRUPO == input$geral_unidade,
            grepl("\\<PROF", DESCRICAO_CARGO),
            !DESCRICAO_CARGO == "Inválido") %>% 
          group_by(
            CLASSE = paste(CLASSE_CARGO, substr(NIVEL_CARGO, 1, 1)),
            PADRAO = substr(NIVEL_CARGO, 2, nchar(NIVEL_CARGO))) %>% 
          summarise(
            TOTAL = n())
      }
      db$CLASSE <- str_replace_all(db$CLASSE, c("[1]" = "I", "[2]" = "II", "[3]" = "III", "[4]" = "IV"))
      db$CLASSE <- ifelse(db$CLASSE == "D 5","Titular", db$CLASSE)
      db <- spread(data = db, key = PADRAO, value = TOTAL, fill = 0)
      nm <- seq(from = 1, to = ncol(db)-1)
      nm <- paste0("SERIE", nm)
      colnames(db) <- c("CLASSE", nm)
      db
    })
    output$plotPES10 <- renderHighchart({
      highchart_new(
        data = dbpPES10(),
        series = c(SERIE1 = "Padrão 1", SERIE2 = "Padrão 2", SERIE3 = "Padrão 3", SERIE4 = "Padrão 4"),
        subtitle = paste(input$geral_unidade, "-", max(paste0(substr(db_siape$DATA_REFERENCIA, 1 , 4),"/",substr(db_siape$DATA_REFERENCIA, 6, 7)))),
        categories = dbpPES10()$CLASSE,
        credits = "Portal da Transparência",
        input_plot = c(
          typeplot = input$typeplotPES10,
          dimension = input$dimensionPES10,
          groupplot = input$groupplotPES10),
        origin = "orcamento",
        colors = paste(hccolor$exeorc))
    })
    # PES11 Cargos por Classe (Técnico) ----
    dbpPES11 <- reactive({
      if (input$geral_unidade == "IFB"){
        db <- db_siape %>% 
          filter(
            DATA_REFERENCIA == max(DATA_REFERENCIA),
            SIGLA_FUNCAO == "-1",
            COD_ORG_LOTACAO == "26428",
            !grepl("\\<PROF", DESCRICAO_CARGO),
            !DESCRICAO_CARGO == "Inválido") %>% 
          group_by(
            CLASSE_CARGO) %>% 
          summarise(
            TOTAL = n())
      } else {
        db <- db_siape %>% 
          filter(
            DATA_REFERENCIA == max(DATA_REFERENCIA),
            SIGLA_FUNCAO == "-1",
            COD_ORG_LOTACAO == "26428",
            UORG_LOTACAO_GRUPO == input$geral_unidade,
            !grepl("\\<PROF", DESCRICAO_CARGO),
            !DESCRICAO_CARGO == "Inválido") %>% 
          group_by(
            CLASSE_CARGO) %>% 
          summarise(
            TOTAL = n())
      }
      db
    })
    output$plotPES11 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = paste(input$geral_unidade, "-", max(paste0(substr(db_siape$DATA_REFERENCIA, 1 , 4),"/",substr(db_siape$DATA_REFERENCIA, 6, 7))))) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>% 
        hc_chart(type = 'pie', options3d = list(enabled = TRUE, alpha = 45)) %>% 
        hc_plotOptions(pie = list(innerSize = 140, depth = 45)) %>% 
        hc_add_series_labels_values(
          name = "Total",
          labels = dbpPES11()$CLASSE_CARGO,
          values = dbpPES11()$TOTAL, 
          colors = hccolor$palgra[
            sort(
              seq(
                from = which(names(hccolor$palgra) == "gra90"),
                to = which(names(hccolor$palgra) == "gra10"), 
                by = 4), 
              decreasing = TRUE)]
          [1:length(dbpPES11()$CLASSE_CARGO)])
      hc
    })
    # PES12 Jornada de trabalho dos docentes e técnicos ----
    dbpPES12 <- reactive({
      if (input$geral_unidade == "IFB"){
        db <- db_siape %>%
          filter(
            DATA_REFERENCIA == max(DATA_REFERENCIA),
            SIGLA_FUNCAO == "-1",
            COD_ORG_LOTACAO == "26428",
            !grepl("\\<PROF", DESCRICAO_CARGO)
          ) %>%
          group_by(JORNADA_DE_TRABALHO) %>%
          summarise(SERIE1 = n())
      } else {
        db <- db_siape %>%
          filter(
            DATA_REFERENCIA == max(DATA_REFERENCIA),
            SIGLA_FUNCAO == "-1",
            COD_ORG_LOTACAO == "26428",
            UORG_LOTACAO_GRUPO == input$geral_unidade,
            !grepl("\\<PROF", DESCRICAO_CARGO)
          ) %>%
          group_by(
            JORNADA_DE_TRABALHO) %>%
          summarise(
            SERIE1 = n())
      }
      db
    })
    output$plotPES12 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = paste(input$geral_unidade, "-", max(paste0(substr(db_siape$DATA_REFERENCIA, 1 , 4),"/",substr(db_siape$DATA_REFERENCIA, 6, 7))))) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>% 
        hc_chart(type = 'pie', options3d = list(enabled = TRUE, alpha = 45)) %>% 
        hc_plotOptions(pie = list(innerSize = 120, depth = 45)) %>% 
        hc_add_series_labels_values(
          name = "Total",
          labels = dbpPES12()$JORNADA_DE_TRABALHO,
          values = dbpPES12()$SERIE1, 
          colors = hccolor$palgra[
            sort(
              seq(
                from = which(names(hccolor$palgra) == "gra90"),
                to = which(names(hccolor$palgra) == "gra10"), 
                by = 4), 
              decreasing = TRUE)]
          [1:length(dbpPES12()$JORNADA_DE_TRABALHO)])
      hc
    })
    # PES13 Cargos por classe, padrão e nível de capacitação ----
    dbpPES13 <- reactive({
      if (input$geral_unidade == "IFB"){
        db <- db_siape %>% 
          filter(
            DATA_REFERENCIA == max(DATA_REFERENCIA), 
            SIGLA_FUNCAO == "-1", 
            COD_ORG_LOTACAO == "26428", 
            !grepl("\\<PROF", DESCRICAO_CARGO),
            !DESCRICAO_CARGO == "Inválido") %>% 
          group_by(
            CLASSE_CARGO, 
            NIVEL_CARGO = substr(PADRAO_CARGO, 1, 1), 
            PADRAO_CARGO = substr(PADRAO_CARGO, 2, nchar(PADRAO_CARGO))) %>% 
          summarise(
            TOTAL = n())
      } else {
        db <- db_siape %>% 
          filter(
            DATA_REFERENCIA == max(DATA_REFERENCIA), 
            SIGLA_FUNCAO == "-1", 
            COD_ORG_LOTACAO == "26428", 
            UORG_LOTACAO_GRUPO == input$geral_unidade, 
            !grepl("\\<PROF", DESCRICAO_CARGO),
            !DESCRICAO_CARGO == "Inválido") %>% 
          group_by(
            CLASSE_CARGO, 
            NIVEL_CARGO = substr(PADRAO_CARGO, 1, 1), 
            PADRAO_CARGO = substr(PADRAO_CARGO, 2, nchar(PADRAO_CARGO))) %>% 
          summarise(
            TOTAL = n())
      }
      db$PADRAO_CARGO <- paste0(db$CLASSE_CARGO, db$PADRAO_CARGO)
      db <- spread(data = db, key = NIVEL_CARGO, value = TOTAL, fill = 0)
      nm <- seq(from = 1, to = ncol(db)-2)
      nm <- paste0("SERIE", nm)
      colnames(db) <- c("CLASSE_CARGO", "PADRAO_CARGO", nm)
      db
    })
    output$plotPES13 <- renderHighchart({
      highchart_new(
        data = dbpPES13(),
        series = c(SERIE1 = "Nível Capacitação I", SERIE2 = "Nível Capacitação II", SERIE3 = "Nível Capacitação III", SERIE4 = "Nível Capacitação IV"),
        subtitle = paste(input$geral_unidade, "-", max(paste0(substr(db_siape$DATA_REFERENCIA, 1 , 4),"/",substr(db_siape$DATA_REFERENCIA, 6, 7)))),
        categories = dbpPES13()$PADRAO_CARGO,
        credits = "Portal da Transparência",
        input_plot = c(
          typeplot = input$typeplotPES13,
          dimension = input$dimensionPES13,
          groupplot = input$groupplotPES13),
        origin = "orcamento",
        colors = paste(hccolor$exeorc))
    })
    # PES17 Admissão e desligamento de docentes e técnicos por ano ----
    dbpPES17 <- reactive({
      if (input$geral_unidade == "IFB"){
        db <- sp_rot_efetivo %>% 
          group_by(
            DATA_REFERENCIA = substr(FIM, 1, 4), 
            TIPO = ifelse(TIPO == "ADMISSAO", "SERIE1", "SERIE2")) %>% 
          summarise_at(
            .vars = "TOTAL", 
            .funs = sum) %>% 
          mutate(
            UORG_LOTACAO_GRUPO = "IFB")
      } else {
        db <- sp_rot_unidade %>% 
          filter(
            UORG_LOTACAO_GRUPO == input$geral_unidade) %>% 
          group_by(
            UORG_LOTACAO_GRUPO, 
            DATA_REFERENCIA = substr(FIM, 1, 4), 
            TIPO = ifelse(TIPO == "ADMISSAO", "SERIE1", "SERIE2")) %>%  
          summarise_at(
            .vars = "TOTAL", 
            .funs = sum)
      }
      db <- spread(data = db, key = TIPO, value = TOTAL, fill = 0)
      db
    })
    output$plotPES17 <- renderHighchart({
      highchart_new(
        data = dbpPES17(),
        series = c(SERIE1 = "Admissões", SERIE2 = "Desligamentos"),
        subtitle = paste(input$geral_unidade, "-", min(dbpPES17()$DATA_REFERENCIA), "a", max(dbpPES17()$DATA_REFERENCIA)),
        categories = dbpPES17()$DATA_REFERENCIA,
        credits = "Portal da Transparência",
        input_plot = c(
          typeplot = input$typeplotPES17,
          dimension = input$dimensionPES17,
          groupplot = input$groupplotPES17),
        origin = "orcamento",
        colors = paste(hccolor$exeorc[c("emp","pag")]))
    })
    # PES18 Admissão e desligamento de docentes e técnicos por ano e tipo de cargo ----
    dbpPES18 <- reactive({
      if (input$geral_unidade == "IFB"){
        db <- sp_rot_efetivo %>% 
          mutate(
            TIPO_CARGO = ifelse(grepl("\\<PROF", DESCRICAO_CARGO), "DOCENTE", "TECNICO"))
        db1 <- db %>% 
          filter(
            TIPO_CARGO == "DOCENTE") %>% 
          group_by(
            DATA_REFERENCIA = substr(FIM, 1, 4),
            TIPO) %>% 
          summarise_at(
            .vars = "TOTAL", 
            .funs = sum)
        db1 <- spread(data = db1, key = TIPO, value = TOTAL, fill = 0)
        colnames(db1) <- c("DATA_REFERENCIA", "SERIE1", "SERIE3")
        db2 <- db %>% 
          filter(
            TIPO_CARGO == "TECNICO") %>% 
          group_by(
            DATA_REFERENCIA = substr(FIM, 1, 4),
            TIPO) %>% 
          summarise_at(
            .vars = "TOTAL", 
            .funs = sum)
        db2 <- spread(data = db2, key = TIPO, value = TOTAL, fill = 0)
        colnames(db2) <- c("DATA_REFERENCIA", "SERIE2", "SERIE4")
        db2$UORG_LOTACAO_GRUPO <- "IFB"
        db <- left_join(db1, db2, by = "DATA_REFERENCIA")
      } else {
        db <- sp_rot_efetivo %>% 
          mutate(TIPO_CARGO = ifelse(grepl("\\<PROF", DESCRICAO_CARGO), "DOCENTE", "TECNICO"))
        db1 <- db %>% 
          filter(
            UORG_LOTACAO_GRUPO == input$geral_unidade, 
            TIPO_CARGO == "DOCENTE") %>% 
          group_by(
            DATA_REFERENCIA = substr(FIM, 1, 4),
            TIPO) %>% 
          summarise_at(
            .vars = "TOTAL", 
            .funs = sum)
        db1 <- spread(data = db1, key = TIPO, value = TOTAL, fill = 0)
        colnames(db1) <- c("DATA_REFERENCIA", "SERIE1", "SERIE3")
        db2 <- db %>% 
          filter(
            UORG_LOTACAO_GRUPO == input$geral_unidade, 
            TIPO_CARGO == "TECNICO") %>% 
          group_by(
            DATA_REFERENCIA = substr(FIM, 1, 4),
            TIPO) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum)
        db2 <- spread(data = db2, key = TIPO, value = TOTAL, fill = 0)
        colnames(db2) <- c("DATA_REFERENCIA", "SERIE2", "SERIE4")
        db2$UORG_LOTACAO_GRUPO <- "IFB"
        db <- left_join(db1, db2, by = "DATA_REFERENCIA")
      }
      db
    })
    output$plotPES18 <- renderHighchart({
      highchart_new(
        data = dbpPES18(),
        series = c(SERIE1 = "Admissão (Docente)", SERIE2 = "Admissão (Técnico)", SERIE3 = "Desligamento (Docente)", SERIE4 = "Desligamento (Técnico)"),
        subtitle = paste(input$geral_unidade, "-", min(dbpPES18()$DATA_REFERENCIA), "a", max(dbpPES18()$DATA_REFERENCIA)),
        categories = dbpPES18()$DATA_REFERENCIA,
        credits = "Portal da Transparência",
        input_plot = c(
          typeplot = input$typeplotPES18, 
          dimension = input$dimensionPES18, 
          groupplot = input$groupplotPES18),
        origin = "orcamento",
        colors = paste(hccolor$exeorc))
    })
    # PES19 Admissão e desligamento de prof. temporários e substitutos por ano ----
    dbpPES19 <- reactive({
      if (input$geral_unidade == "IFB"){
        db <- sp_rot_vinculo %>% 
          filter(
            grepl("\\<CONT.PROF", 
                  SITUACAO_VINCULO)) %>% 
          group_by(
            DATA_REFERENCIA = substr(FIM, 1, 4), 
            TIPO = ifelse(TIPO == "ADMISSAO", "SERIE1", "SERIE2")) %>% 
          summarise_at(
            .vars = "TOTAL", 
            .funs = sum) %>% 
          mutate(UORG_LOTACAO_GRUPO = "IFB")
      } else {
        db <- sp_rot_vinculo %>% 
          filter(
            grepl("\\<CONT.PROF", SITUACAO_VINCULO), 
            UORG_LOTACAO_GRUPO == input$geral_unidade) %>% 
          group_by(
            UORG_LOTACAO_GRUPO, 
            DATA_REFERENCIA = substr(FIM, 1, 4), 
            TIPO = ifelse(TIPO == "ADMISSAO", "SERIE1", "SERIE2")) %>%  
          summarise_at(
            .vars = "TOTAL", 
            .funs = sum)
      }
      db <- spread(data = db, key = TIPO, value = TOTAL, fill = 0)
      db
    })
    output$plotPES19 <- renderHighchart({
      highchart_new(
        data = dbpPES19(),
        series = c(SERIE1 = "Admissões", SERIE2 = "Desligamentos"),
        subtitle = paste(input$geral_unidade, "-", min(dbpPES19()$DATA_REFERENCIA), "a", max(dbpPES19()$DATA_REFERENCIA)),
        categories = dbpPES19()$DATA_REFERENCIA,
        credits = "Portal da Transparência",
        input_plot = c(
          typeplot = input$typeplotPES19, 
          dimension = input$dimensionPES19, 
          groupplot = input$groupplotPES19),
        origin = "orcamento",
        colors = paste(hccolor$exeorc[c("emp","pag")]))
    })
    # PES20 Admissão e desligamento de prof. temporários e substitutos por ano e vínculo ----
    dbpPES20 <- reactive({
      if (input$geral_unidade == "IFB"){
        db <- sp_rot_vinculo %>% 
          filter(
            grepl("\\<CONT.PROF", SITUACAO_VINCULO)) %>% 
          mutate(
            VINCULO = ifelse(grepl("TEMPORARIO$", SITUACAO_VINCULO), "TEMPORARIO", "SUBSTITUTO"))
        db1 <- db %>% 
          filter(
            VINCULO == "TEMPORARIO") %>% 
          group_by(
            DATA_REFERENCIA = substr(FIM, 1, 4),
            TIPO) %>% 
          summarise_at(
            .vars = "TOTAL", 
            .funs = sum)
        db1 <- spread(data = db1, key = TIPO, value = TOTAL, fill = 0)
        colnames(db1) <- c("DATA_REFERENCIA", "SERIE1", "SERIE3")
        db2 <- db %>% 
          filter(
            VINCULO == "SUBSTITUTO") %>% 
          group_by(
            DATA_REFERENCIA = substr(FIM, 1, 4),
            TIPO) %>% 
          summarise_at(
            .vars = "TOTAL", 
            .funs = sum)
        db2 <- spread(data = db2, key = TIPO, value = TOTAL, fill = 0)
        colnames(db2) <- c("DATA_REFERENCIA", "SERIE2", "SERIE4")
        db2$UORG_LOTACAO_GRUPO <- "IFB"
        db <- left_join(db1, db2, by = "DATA_REFERENCIA")
      } else {
        db <- sp_rot_vinculo %>% 
          filter(
            grepl("\\<CONT.PROF", SITUACAO_VINCULO)) %>% 
          mutate(
            VINCULO = ifelse(grepl("TEMPORARIO$", SITUACAO_VINCULO), "TEMPORARIO", "SUBSTITUTO"))
        db1 <- db %>% 
          filter(
            UORG_LOTACAO_GRUPO == input$geral_unidade, 
            VINCULO == "TEMPORARIO") %>% 
          group_by(
            DATA_REFERENCIA = substr(FIM, 1, 4),
            TIPO) %>% 
          summarise_at(
            .vars = "TOTAL", 
            .funs = sum)
        db1 <- spread(data = db1, key = TIPO, value = TOTAL, fill = 0)
        colnames(db1) <- c("DATA_REFERENCIA", "SERIE1", "SERIE3")
        db2 <- db %>% 
          filter(
            UORG_LOTACAO_GRUPO == input$geral_unidade, 
            VINCULO == "SUBSTITUTO") %>% 
          group_by(
            DATA_REFERENCIA = substr(FIM, 1, 4),
            TIPO) %>% 
          summarise_at(
            .vars = "TOTAL", 
            .funs = sum)
        db2 <- spread(data = db2, key = TIPO, value = TOTAL, fill = 0)
        colnames(db2) <- c("DATA_REFERENCIA", "SERIE2", "SERIE4")
        db2$UORG_LOTACAO_GRUPO <- "IFB"
        db <- left_join(db1, db2, by = "DATA_REFERENCIA")
      }
      db
    })
    output$plotPES20 <- renderHighchart({
      highchart_new(
        data = dbpPES20(),  
        series = c(SERIE1 = "Admissão (Temporário)", SERIE2 = "Admissão (Substituto)", SERIE3 = "Desligamento (Temporário)", SERIE4 = "Desligamento (Substituto)"),
        subtitle = paste(input$geral_unidade, "-", min(dbpPES20()$DATA_REFERENCIA), "a", max(dbpPES20()$DATA_REFERENCIA)),
        categories = dbpPES20()$DATA_REFERENCIA,
        credits = "Portal da Transparência",
        input_plot = c(
          typeplot = input$typeplotPES20, 
          dimension = input$dimensionPES20, 
          groupplot = input$groupplotPES20),
        origin = "orcamento",
        colors = paste(hccolor$exeorc))
    })

# AVALIAÇÃO ---------------------------------------------------------------
    # AVA01 Número de avaliadores por cargo ----
    dbpAVA01 <- reactive({
      db <- db_avager_av %>% 
        filter(
          ANO == input$avager_ano, 
          UNIDADE_AVALIADA == input$avager_unidade) %>% 
        group_by(
          ANO, 
          CARGO, 
          QUESTAO) %>% 
        summarise(
          SERIE1 = n()) %>% 
        group_by(
          ANO, 
          CARGO, 
          SERIE1) %>% 
        summarise()
      db
    })
    
    output$plotAVA01 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = paste(input$avager_unidade, unique(dbpAVA01()$ANO))) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>% 
        hc_chart(type = 'pie', options3d = list(enabled = FALSE, alpha = 45)) %>% 
        hc_add_series_labels_values(
          name = "Total",
          labels = dbpAVA01()$CARGO,
          values = dbpAVA01()$SERIE1, colors = hccolor$palgre[
            sort(
              seq(
                from = which(names(hccolor$palgre) == "gre95"),
                to = which(names(hccolor$palgre) == "gre10"), 
                by = 2), 
              decreasing = TRUE)]
          [1:length(dbpAVA01()$CARGO)]) %>% 
        hc_plotOptions(pie = list(showInLegend = TRUE, dataLabels = list(enabled = FALSE)))
      hc
    })
    # AVA02 Resultado da avaliação por critério ----
    dbpAVA02 <- reactive({
      db <- db_avager_av %>% 
        filter(
          UNIDADE_AVALIADA == input$avager_unidade, 
          ANO == input$avager_ano) %>% 
        group_by(
          UNIDADE_AVALIADA, 
          ANO, 
          AVALIACAO) %>% summarise(
            TOTAL = n()) %>% 
        mutate(
          SERIE1 = (TOTAL/sum(TOTAL))*100)
      if (input$avager_ano == "2016"){
        db$AVALIACAO <- factor(
          x = db$AVALIACAO, 
          levels = c("5", "4", "3", "2", "1"))
        if(any(is.na(db$AVALIACAO))) db <- db[!is.na(db$AVALIACAO), ]
      } else if (input$avager_ano == "2017"){
        db$AVALIACAO <- factor(
          x = db$AVALIACAO, 
          levels = c("Concordo plenamente", "Concordo parcialmente", "Algumas vezes", "Discordo parcialmente", "Discordo totalmente", "Não sei avaliar"))
      } else if (input$avager_ano == "2018"){
        db$AVALIACAO <- factor(
          x = db$AVALIACAO, 
          levels = c("Concordo plenamente", "Concordo parcialmente", "Nem concordo nem discordo", "Discordo totalmente", "Não sei avaliar"))
      }
      db <- db[order(db$AVALIACAO), ]
      db$SERIE1 <- as.numeric(format(x = db$SERIE1, digits = 2, nsmall = 2))
      db
    })
    output$plotAVA02 <- renderHighchart({
      highchart_new(
        data = dbpAVA02(),
        series = c(SERIE1 = "% Avaliadores"),
        subtitle = paste(input$avager_unidade, input$avager_ano),
        categories = as.character(dbpAVA02()$AVALIACAO),
        credits = "Googledrive",
        input_plot = c(
          typeplot = input$typeplotAVA02, 
          dimension = input$dimensionAVA02, 
          groupplot = input$groupplotAVA02),
        origin = "orcamento",
        colors = hccolor$pessch$h40)
    })
    observeEvent(c(input$avager_unidade, input$avager_ano), {
      updateSelectInput(
        session,
        inputId = "avager_cargo", 
        label = "Quadro a que pertence", 
        choices = c("TODOS", 
                    unique(db_avager_av[db_avager_av$UNIDADE_AVALIADA == input$avager_unidade & 
                                          db_avager_av$ANO == input$avager_ano, ]$CARGO)),
        selected = "TODOS")
    })
    observeEvent(c(input$avager_unidade, input$avager_ano), {
      updateSelectInput(
        session,
        inputId = "avager_questao", 
        label = "Questão", 
        choices = unique(db_avager_av[db_avager_av$UNIDADE_AVALIADA == input$avager_unidade & 
                                        db_avager_av$ANO == input$avager_ano, ]$QUESTAO),
        selected = unique(db_avager_av[db_avager_av$UNIDADE_AVALIADA == input$avager_unidade & 
                                         db_avager_av$ANO == input$avager_ano, ]$QUESTAO)[1])
    })
    # AVA03 Resultado da avaliação por nota ----
    dbpAVA03 <- eventReactive(c(input$avager_cargo, input$avager_unidade, input$avager_ano), {
      if(input$avager_cargo == "TODOS"){
        db <- db_avager_nt %>% 
          filter(
            !is.na(AVALIACAO),
            UNIDADE_AVALIADA == input$avager_unidade, 
            ANO == input$avager_ano) %>% 
          group_by(
            ANO, 
            AVALIACAO) %>% 
          summarise(
            TOTAL = n()) %>% 
          mutate(
            SERIE1 = (TOTAL/sum(TOTAL))*100)
      }else{
        db <- db_avager_nt %>% 
          filter(
            !is.na(AVALIACAO), 
            CARGO == input$avager_cargo,
            UNIDADE_AVALIADA == input$avager_unidade, 
            ANO == input$avager_ano) %>% 
          group_by(
            ANO, 
            AVALIACAO) %>% 
          summarise(
            TOTAL = n()) %>% 
          mutate(
            SERIE1 = (TOTAL/sum(TOTAL))*100)
      }
      db$AVALIACAO <- factor(
        x = db$AVALIACAO, 
        levels = c(1:10))
      db <- db[order(db$AVALIACAO, decreasing = TRUE), ]
      db$SERIE1 <- as.numeric(format(x = db$SERIE1, digits = 2, nsmall = 2))
      db
    })
    output$plotAVA03 <- renderHighchart({
      highchart_new(
        data = dbpAVA03(),
        series = c(SERIE1 = "% Avaliadores"),
        subtitle = paste(input$avager_unidade, input$avager_ano),
        categories = as.character(dbpAVA03()$AVALIACAO),
        credits = "Googledrive",
        input_plot = c( 
          typeplot = input$typeplotAVA03, 
          dimension = input$dimensionAVA03, 
          groupplot = input$groupplotAVA03),
        origin = "orcamento",
        colors = hccolor$pessch$h40)
    })
    # AVA04 Resultado da avaliação por questão ----
    dbpAVA04 <- eventReactive(c(input$avager_unidade, input$avager_questao, input$avager_cargo, input$avager_ano), {
      if(input$avager_cargo == "TODOS"){
        db <- db_avager_av %>% 
          filter(
            QUESTAO == input$avager_questao, 
            UNIDADE_AVALIADA == input$avager_unidade, 
            ANO == input$avager_ano) %>% 
          group_by(
            ANO, 
            AVALIACAO) %>% 
          summarise(
            TOTAL = n()) %>% 
          mutate(
            SERIE1 = (TOTAL/sum(TOTAL))*100)
      }else{
        db <- db_avager_av %>% 
          filter(
            CARGO == input$avager_cargo, 
            QUESTAO == input$avager_questao, 
            UNIDADE_AVALIADA == input$avager_unidade, 
            ANO == input$avager_ano) %>% 
          group_by(
            ANO, 
            AVALIACAO) %>% 
          summarise(
            TOTAL = n()) %>% 
          mutate(
            SERIE1 = (TOTAL/sum(TOTAL))*100)
      }
      if (input$avager_ano == "2016"){
        db$AVALIACAO <- factor(
          x = db$AVALIACAO, 
          levels = c("5", "4", "3", "2", "1"))
        if(any(is.na(db$AVALIACAO))) db <- db[!is.na(db$AVALIACAO), ]
      } else if (input$avager_ano == "2017"){
        db$AVALIACAO <- factor(
          x = db$AVALIACAO, 
          levels = c("Concordo plenamente", "Concordo parcialmente", "Algumas vezes", "Discordo parcialmente", "Discordo totalmente", "Não sei avaliar"))
      } else if (input$avager_ano == "2018"){
        db$AVALIACAO <- factor(
          x = db$AVALIACAO, 
          levels = c("Concordo plenamente", "Concordo parcialmente", "Nem concordo nem discordo", "Discordo totalmente", "Não sei avaliar"))
      }
      db <- db[order(db$AVALIACAO), ]
      db$SERIE1 <- as.numeric(format(x = db$SERIE1, digits = 2, nsmall = 2))
      db
    })
    output$plotAVA04 <- renderHighchart({
      highchart_new(
        data = dbpAVA04(),
        series = c(SERIE1 = "% Avaliadores"),
        subtitle = paste(input$avager_unidade, input$avager_ano),
        categories = as.character(dbpAVA04()$AVALIACAO),
        credits = "Googledrive",
        input_plot = c( 
          typeplot = input$typeplotAVA04, 
          dimension = input$dimensionAVA04, 
          groupplot = input$groupplotAVA04),
        origin = "orcamento",
        colors = hccolor$pessch$h40)
    })

    # AVA05 Número de avaliadores por grupo ----
    dbpAVA05 <- reactive({
      db <- db_orcano_gr %>% 
        filter(
          ANO == "2019") %>% 
        group_by(
          ANO, 
          CARGO) %>% 
        summarise(
          SERIE1 = sum(TOTAL, na.rm = TRUE))
      db
    })
    output$plotAVA05 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = paste(input$avager_unidade, unique(dbpAVA05()$ANO))) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>% 
        hc_chart(type = 'pie', options3d = list(enabled = FALSE, alpha = 45)) %>% 
        hc_add_series_labels_values(
          name = "Total",
          labels = dbpAVA05()$CARGO,
          values = dbpAVA05()$SERIE1, colors = hccolor$palgre[
            sort(
              seq(
                from = which(names(hccolor$palgre) == "gre95"),
                to = which(names(hccolor$palgre) == "gre10"), 
                by = 2), 
              decreasing = TRUE)]
          [1:length(dbpAVA05()$CARGO)]) %>% 
        hc_plotOptions(pie = list(showInLegend = TRUE, dataLabels = list(enabled = FALSE)))
      hc
    })
    # AVA06 Resultado por destinação e nível de prioridade ----
    dbpAVA06 <- reactive({
      if(input$avaliadores == "TODOS"){
        db <- db_orcano_av %>% 
          filter(
            CATEGORIA == input$ava_grupodespesa) %>% 
          group_by(
            QUESTAO, 
            AVALIACAO) %>% 
          summarise(
            TOTAL = n()) %>% 
          spread(
            key = AVALIACAO, 
            value = TOTAL, 
            fill = 0)
      } else {
        db <- db_orcano_av %>% 
          filter(
            CATEGORIA == input$ava_grupodespesa, 
            CARGO == input$avaliadores) %>% 
          group_by(
            QUESTAO, 
            AVALIACAO) %>% 
          summarise(
            TOTAL = n()) %>% 
          spread(
            key = AVALIACAO, 
            value = TOTAL, 
            fill = 0)
        }
      nm <- seq(from = 1, to = ncol(db)-1)
      nm <- paste0("SERIE", nm)
      colnames(db) <- c("QUESTAO", nm)
      db
    })
    output$plotAVA06 <- renderHighchart({
      highchart_new(
        data = dbpAVA06(),
        series = c(SERIE1 = "P1", SERIE2 = "P2", SERIE3 = "P3", SERIE4 = "P4", SERIE5 = "P5", SERIE6 = "P6", SERIE7 = "P7", SERIE8 = "P8"),
        subtitle = paste(if(input$avaliadores == "TODOS"){""}else{paste(input$avaliadores,"|")}, input$ava_grupodespesa, "2019"),
        categories = dbpAVA06()$QUESTAO,
        credits = "Portal da Transparência",
        input_plot = c(
          typeplot = input$typeplotAVA06,
          dimension = input$dimensionAVA06,
          groupplot = input$groupplotAVA06),
        origin = "orcamento",
        colors = c(hccolor$palblu$blu05, hccolor$palblu$blu15, hccolor$palblu$blu25, hccolor$palblu$blu35, 
                       hccolor$palblu$blu45, hccolor$palblu$blu55, hccolor$palblu$blu65, hccolor$palblu$blu75))
    })
    # AVA07 Priorização do orçamento por categoria de gasto ----
    dbpAVA07 <- reactive({
      if(input$avaliadores == "TODOS"){
        db <- db_orcano_av %>% 
          filter(
            CATEGORIA == input$ava_grupodespesa) %>% 
          group_by(
            QUESTAO, 
            AVALIACAO) %>% 
          summarise(
            TOTAL = n()) %>% 
          spread(
            key = QUESTAO, 
            value = TOTAL, 
            fill = 0)
      } else {
        db <- db_orcano_av %>% 
          filter(
            CATEGORIA == input$ava_grupodespesa, 
            CARGO == input$avaliadores) %>% 
          group_by(
            QUESTAO, 
            AVALIACAO) %>% 
          summarise(
            TOTAL = n()) %>% 
          spread(
            key = QUESTAO, 
            value = TOTAL, 
            fill = 0)
      }
      db$AVALIACAO <- paste0("P", db$AVALIACAO)
      db
    })
    output$plotAVA07 <- renderHighchart({
      db <- dbpAVA07()
      nm_ser <- colnames(db[, c(2:ncol(db))])
      nm <- seq(from = 1, to = ncol(db)-1)
      nm <- paste0("SERIE", nm)
      colnames(db) <- c("AVALIACAO", nm)
      names(nm_ser) <- paste0("SERIE", seq(from = 1, to = length(nm_ser)))
      highchart_new(
        data = db,
        series = nm_ser,
        subtitle = paste(if(input$avaliadores == "TODOS"){""}else{paste(input$avaliadores,"|")}, input$ava_grupodespesa, "2019"),
        categories = db$AVALIACAO,
        credits = "Portal da Transparência",
        input_plot = c( 
          typeplot = input$typeplotAVA07, 
          dimension = input$dimensionAVA07, 
          groupplot = input$groupplotAVA07),
        origin = "orcamento",
        colors = hccolor$palgre[
          sort(
            seq(
              from = which(names(hccolor$palgre) == "gre95"),
              to = which(names(hccolor$palgre) == "gre10"), 
              by = 2), 
            decreasing = TRUE)]
        [1:ncol(db)-1])
    })
    # AVA08 Tabela de dados priorização do orçamento ----
    output$datatable01 <- renderDataTable({
      db <- db_orcano_av %>%
        group_by(CATEGORIA, QUESTAO, DESCRICAO, AVALIACAO) %>% 
        summarise(TOTAL = n()) %>% 
        spread(key = AVALIACAO, value = TOTAL, fill = 0)
      if(any(db$QUESTAO == "Outro" & db$DESCRICAO == "")) db <- db[-which(db$QUESTAO == "Outro" & db$DESCRICAO == ""),]
      db$QUESTAO <- factor(db$QUESTAO, levels = c(unique(db[!db$QUESTAO == "Outro",]$QUESTAO), "Outro"))
      db <- db[order(x = db$QUESTAO, decreasing = FALSE),]
      db$QUESTAO <- as.character(as.factor(db$QUESTAO))
      db[db$QUESTAO == "Outro",]$QUESTAO <- paste0(db[db$QUESTAO == "Outro",]$QUESTAO, " (", db[db$QUESTAO == "Outro",]$DESCRICAO,")")
      db <- db[,c(1,2,4:ncol(db))]
      db$CATEGORIA <- str_to_title(db$CATEGORIA)
      dttb <- 
        datatable(
          db,
          rownames = FALSE, 
          escape = FALSE,
          colnames = c("Grupo de Despesa", "Destinação proposta", "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8"),
          options = list(
            pageLength = 11,
            language = list(url ='http://cdn.datatables.net/plug-ins/1.10.7/i18n/Portuguese-Brasil.json'))) %>% 
        formatStyle(
          c("1","2","3","4","5","6","7","8"), 
          color = "white",
          backgroundColor = 
            styleInterval(
              cuts = c(1, 4, 7, 10, 13), 
              values = c(hccolor$palgra$gra50, hccolor$palgra$gra40, hccolor$palgra$gra30, hccolor$palgra$gra20, 
                         hccolor$palgra$gra10, hccolor$palgra$gra0)))
      dttb
    })
  }
shinyApp(ui, server)

