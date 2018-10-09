# GLOBAL ------------------------------------------------------------------
# Library 
suppressMessages(require(shiny))
suppressMessages(require(shinydashboard))
suppressMessages(require(shinydashboardPlus))
suppressMessages(require(shinycssloaders))
suppressMessages(require(shinyWidgets))
suppressMessages(require(shinyjs))
suppressMessages(require(tidyverse))
suppressMessages(require(highcharter))
suppressMessages(require(reshape2))
suppressMessages(require(treemap))
# Functions
source("function/function.R",encoding = "UTF-8")
# Data
import_data(pasta = "data/", extensao = "rds")
options(highcharter.lang = hcoptslang())
# SHINY UI --------------------------------------------------------------------------------------------------------
ui <-
  dashboardPagePlus(
    dashboardHeaderPlus( 
      title = strong("Painel CCEI"),
      titleWidth = 200,
      enable_rightsidebar = TRUE,
      rightSidebarIcon = "gears"), 
    dashboardSidebar(
      width = 200,
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
              tabName = "evolucao"
              ),
            menuSubItem(
              text = strong("Categoria"),
              tabName = "categoria"),
            menuSubItem(
              text = strong("Comparativo"),
              tabName = "comparativo")
            )
          ),
        menuItem(
          text = strong("PESSOAL"),
          tabName = "pessoal",
          icon = icon("group"),
          menuSubItem(
            text = strong("Evolução"),
            tabName = "evolucao_pessoal"
            ),
          menuSubItem(
            text = strong("Categoria"),
            tabName = "categoria_pessoal"
          ),
          menuSubItem(
            text = strong("Rotatividade"),
            tabName = "rotatividade"
            )
          ),
        menuItem(
          text = strong("AVALIAÇÕES"),
          tabName = "avaliacoes",
          icon = icon("comments-o"),
          menuItem(
            text = strong("Geral"),
            tabName = "avaliacao_geral",
            menuSubItem(
              text = "2017",
              tabName = "avager_2017"
            )
          )
        )
        )
      ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "evolucao",
          fluidRow(
            boxnew(
              inputId = "01", # 01 Execução da despesa por ano ----
              width_box = 6,
              status = "warning",
              boxtitle = "Execução da despesa por ano",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = c(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "empty", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "02", # 02 Valores pagos por exercício de referência ----
              width_box = 6,
              status = "warning",
              boxtitle = "Valores pagos por exercício de referência",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = c(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "percent", dimension = "empty")),
            boxnew(
              inputId = "03", # 03 Execução da despesa por mês (acumulado) -----
              width_box = 12,
              status = "warning",
              boxtitle = "Execução da despesa por mês (acumulado)",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = c(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "areaspline", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "04", # 04 Execução da despesa por mês (não acumulado) ----
              width_box = 12,
              status = "warning",
              boxtitle = "Execução da despesa por mês (não acumulado)",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "spline", groupplot = "empty", dimension = "empty"))
            )
          ),
        tabItem(
          tabName = "categoria", 
          fluidRow(
            boxnew(
              inputId = "05", # 05 Execução da despesa por grupo de despesa ----
              width_box = 6,
              status = "warning",
              boxtitle = "Execução por grupo de despesa",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = c(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "06", # 06 Execução da despesa por programa ----
              width_box = 6,
              status = "warning",
              boxtitle = "Execução por programa",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = c(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "07", # 07 Execução da despesa por ação orçamentária ----
              width_box = 12,
              status = "warning",
              boxtitle = "Execução por ação orçamentária",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = c(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "08", # 08 Execução da despesa por elemento de despesa ----
              width_box = 12,
              status = "warning",
              boxtitle = "Execução por elemento de despesa",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = c(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty"))
            )
          ),
        tabItem(
          tabName = "comparativo", 
          fluidRow(
            boxnew(
              inputId = "09", # 09 Execução da despesa por unidade gestora ----
              width_box = 7,
              status = "warning",
              boxtitle = "Execução por unidade gestora",
              menu_selected = c("typeplot", "groupplot", "dimension","department"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D", department = "Grupo de unidades"),
              choices = list(typeplot = "empty", groupplot = "empty", department = c("Com Reitoria","Sem Reitoria")),
              selected = c(typeplot = "column", groupplot = "normal", dimension = "empty", department = "Sem Reitoria")),
            boxnew(
              inputId = "10", # 10 Execução da despesa por campi e reitoria ----
              width_box = 5,
              status = "warning",
              boxtitle = "Execução por Campi e Reitoria",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "normal", dimension = "empty")),
            boxnew(
              inputId = "11", # 11 Valores pagos por exercício de referência (por UG) ----
              width_box = 7,
              status = "warning",
              boxtitle = "Valores pagos por exercício de referência",
              menu_selected = c("typeplot", "groupplot", "dimension","department"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D", department = "Grupo de unidades"),
              choices = list(typeplot = "empty", groupplot = "empty", department = c("Com Reitoria","Sem Reitoria")),
              selected = c(typeplot = "column", groupplot = "percent", dimension = "empty", department = "Sem Reitoria")),
            boxnew(
              inputId = "12", # 12 Valores pagos por exercício de referência (Campi e Reitoria) ----
              width_box = 5,
              status = "warning",
              boxtitle = "Valores pagos por exercício de referência (Campi e Reitoria)",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "percent", dimension = "empty"))
          )
        ),
        tabItem(
          tabName = "evolucao_pessoal", 
          fluidRow(
            boxnew(
              inputId = "13", # 13 Número de técnicos e docentes por ano ----
              width_box = 6,
              status = "warning",
              boxtitle = "Número de técnicos e docentes por ano",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "spline", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "14", # 14 Percentual de técnicos e docentes por ano ----
              width_box = 6,
              status = "warning",
              boxtitle = "Percentual de técnicos e docentes por ano",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "percent", dimension = "empty")),
            boxnew(
              inputId = "15", # 15 Número de técnicos e docentes por mês ----
              width_box = 12,
              status = "warning",
              boxtitle = "Número de técnicos e docentes por mês",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "areaspline", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "16", # 16 Percentual de técnicos e docentes por mês ----
              width_box = 12,
              status = "warning",
              boxtitle = "Percentual de técnicos e docentes por mês",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "percent", dimension = "empty")))),
        tabItem(
          tabName = "categoria_pessoal", 
          fluidRow(
            boxnew(
              inputId = "17", # 17 Servidores por cargo ----
              width_box = 6,
              status = "warning",
              boxtitle = "Servidores por cargo"),
            boxnew(
              inputId = "18", # 18 Servidores por situação do vínculo ----
              width_box = 6,
              status = "warning",
              boxtitle = "Servidores por situação do vínculo"),
            boxnew(
              inputId = "19", # 19 Jornada de trabalho dos docentes e técnicos ----
              width_box = 6,
              status = "warning",
              boxtitle = "Jornada de trabalho dos docentes e técnicos",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = c(`<i class='fa fa-bar-chart'></i>`="column", `<i class='fa fa-align-left'></i>`="bar"), groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "20", # 20 Classificação dos cargos técnicos ----
              width_box = 6,
              status = "warning",
              boxtitle = "Classificação dos cargos técnicos",
              menu_selected = c("dimension"),
              label = c(dimension = "3D"),
              choices = "",
              selected = c(dimension = "3D")),
            boxnew(
              inputId = "21", # 21 Nível de capacitação por classificação dos cargos técnicos ----
              width_box = 6,
              status = "warning",
              boxtitle = "Nível de capacitação por classificação dos cargos técnicos",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = c(`<i class='fa fa-bar-chart'></i>`="column", `<i class='fa fa-align-left'></i>`="bar"),
                             groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "22", # 22 Cargos em comissão e funções gratificadas ocupadas ----
              width_box = 6,
              status = "warning",
              boxtitle = "Cargos em comissão e funções gratificadas ocupadas",
              menu_selected = c("typeplot", "groupplot","dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty"))
            )
          ),
        tabItem(
          tabName = "rotatividade", 
          fluidRow(
            boxnew(
              inputId = "23", # 23 Admissão e desligamento de docentes e técnicos por ano ----
              width_box = 6,
              status = "warning",
              boxtitle = "Admissão e desligamento de docentes e técnicos por ano",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "areaspline", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "24", # 24 Admissão e desligamento de docentes e técnicos por tipo ----
              width_box = 6,
              status = "warning",
              boxtitle = "Admissão e desligamento de docentes e técnicos por tipo de cargo",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "spline", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "25", # 25 Admissão e desligamento de prof. temporário e substituto por ano ----
              width_box = 6,
              status = "warning",
              boxtitle = "Admissão e desligamento de prof. temporário e substituto por ano",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "areaspline", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "26", # 26 Admissão e desligamento de prof. temporário e substituto por tipo de contrato ----
              width_box = 6,
              status = "warning",
              boxtitle = "Admissão e desligamento de prof. temporário e substituto por tipo de contrato",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "spline", groupplot = "empty", dimension = "empty"))
            )
          ),
        tabItem(
          tabName = "avager_2017",
          fluidRow(
            column(
              width = 12,
              radioGroupButtons(
                width = "20%",
                inputId = "avager_unidade", 
                label = "", 
                choices = c("DG", "DRAP", "DREP"), selected = "DG",
                justified = TRUE, 
                checkIcon = list(yes = icon("ok", lib = "glyphicon"))
              )
            ),
            boxnew(
              inputId = "27", # 27 Número de avaliadores por cargo ----
              width_box = 5,
              status = "warning",
              boxtitle = "Avaliadores por cargo",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "28", # 28 Resultado da avaliação por critério ----
              width_box = 7,
              status = "warning",
              boxtitle = "Resultado da avaliação por critério",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty")),
            column(
              width = 4,
              selectInput(
                inputId = "avager_cargo", 
                label = "Quadro a que pertence", 
                choices = c("TODOS", unique(db_avager_av[db_avager_av$UNIDADE_AVALIADA == "DG", ]$CARGO)),
                selected = "TODOS",
                width = "100%")),
            column(
              width = 8,
              selectInput(
                inputId = "avager_questao", 
                label = "Questão", 
                choices = unique(db_avager_av[db_avager_av$UNIDADE_AVALIADA == "DG", ]$QUESTAO),
                selected = unique(db_avager_av[db_avager_av$UNIDADE_AVALIADA == "DG", ]$QUESTAO)[1],
                width = "100%")),
            boxnew(
              inputId = "29", # 29 Resultado da avaliação por nota ----
              width_box = 4, 
              status = "warning",
              boxtitle = "Resultado da avaliação por nota",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "30", # 30 Resultado da avaliação por questão ----
              width_box = 8,
              status = "warning",
              boxtitle = "Resultado da avaliação por questão",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty"))
            )
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
        tags$p(strong("MÓDULO ORÇAMENTO")),
        selectInput(
          inputId = "unidade_orcamento", 
          label = "Unidade", 
          choices = c("IFB", unique(db_siafi[order(db_siafi$SG_UG),]$SG_UG)), 
          selected = "CCEI"),
        selectInput(
          inputId = "exercicio_orcamento",
          label = "Exercício", 
          choices = unique(substr(db_siafi[order(db_siafi$LANCAMENTO, decreasing = T),]$LANCAMENTO,1,4)), 
          selected = unique(substr(db_siafi[order(db_siafi$LANCAMENTO, decreasing = T),]$LANCAMENTO,1,4)[1])),
        checkboxGroupInput(
          inputId = "grupodespesa",
          label = "Grupo de despesa",
          choices = list("Despesa Corrente" = unique(db_siafi$CO_GD)[1], 
                         "Despesa de Capital" = unique(db_siafi$CO_GD)[2], 
                         "Pessoal" = unique(db_siafi$CO_GD)[3]),
          selected = unique(db_siafi$CO_GD)[1:2]),
        tags$p(strong("MÓDULO PESSOAL")),
        selectInput(
          inputId = "unidade_pessoal", 
          label = "Unidade", 
          choices = c("IFB", unique(db_siape[order(db_siape$UORG_LOTACAO_GRUPO), ]$UORG_LOTACAO_GRUPO)), 
          selected = "CCEI"),
        selectInput(
          inputId = "exercicio_pessoal",
          label = "Exercício", 
          choices = unique(substr(db_siape$REFERENCIA, 1, 4)), 
          selected = "2018")
      ),
      rightSidebarTabContent(
        id = 2,
        title = "Licenças",
        icon = "info-circle",
        p(strong("HIGHCHARTS - License holder: ")),
        p("INSTITUTO FEDERAL DE EDUCAÇÃO, CIÊNCIA E TECNOLOGIA DE BRASÍLIA"),
        p("Pompylio Lima"),
        p("This license is valid for educational-institution for the following product(s): Highcharts, Highstock, Highmaps"),
        p("This software is released under Creative Commons Attribution-NonCommercial 3.0, and is available for download at highcharts.com/download. No further activation or license key is required.")
        )
    ), title = "Painel CCEI"
  )
# SHINY SERVER ----------------------------------------------------------------------------------------------------
server <-
  function(session, input, output) {

# ORÇAMENTO ---------------------------------------------------------------
    # 01 Execução da despesa por ano ----
    dbplot01 <- reactive({
      if (input$unidade_orcamento == "IFB"){
        db <- db_siafi %>%
          filter(CO_GD %in% c(input$grupodespesa)) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG = "IFB") %>%
          summarise(SERIE1 = sum(EMPENHADO, na.rm = TRUE),
                    SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
                    SERIE3 = sum(PAGO, na.rm = TRUE),
                    SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
        }else{
          ano <- unique(substr(db_siafi[db_siafi$SG_UG == input$unidade_orcamento, ]$LANCAMENTO, 1, 4))
          if (input$unidade_orcamento == "CCEI") ano <- ano[!ano == "2016"]
          db <- db_siafi %>%
            filter(CO_GD %in% c(input$grupodespesa), substr(LANCAMENTO, 1, 4) %in% ano, SG_UG == input$unidade_orcamento) %>%
            group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG) %>%
            summarise(SERIE1 = sum(EMPENHADO, na.rm = TRUE),
                      SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
                      SERIE3 = sum(PAGO, na.rm = TRUE),
                      SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
        }
      db
      })
    output$plot01 <- renderHighchart({
      highchart_new(data = dbplot01(), 
                    series = c(SERIE1 = "EMPENHADO", SERIE2 = "LIQUIDADO", SERIE3 = "PAGO", SERIE_RAP = "RAP PAGO"),
                    subtitle = paste(unique(dbplot01()$SG_UG), " ", min(dbplot01()$ANO), "a", max(dbplot01()$ANO)),
                    categories = unique(dbplot01()$ANO),
                    credits = "Portal da Transparência",
                    input_plot = c(
                      typeplot = input$typeplot01, 
                      dimension = input$dimension01, 
                      groupplot = input$groupplot01))
    })
    # 02 Valores pagos por exercício de referência ----
    dbplot02 <- reactive({
      if (input$unidade_orcamento == "IFB"){
        db <- db_siafi %>%
          filter(CO_GD %in% c(input$grupodespesa)) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG = "IFB") %>%
          summarise(SERIE1 = sum(PAGO, na.rm = TRUE),
                    SERIE2 = sum(RAP_PAGO, na.rm = TRUE))
      }else{
        ano <- unique(substr(db_siafi[db_siafi$SG_UG == input$unidade_orcamento, ]$LANCAMENTO, 1, 4))
        if (input$unidade_orcamento == "CCEI") ano <- ano[!ano == "2016"]
        db <- db_siafi %>%
          filter(CO_GD %in% c(input$grupodespesa), substr(LANCAMENTO, 1, 4) %in% ano, SG_UG == input$unidade_orcamento) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG) %>%
          summarise(SERIE1 = sum(PAGO, na.rm = TRUE),
                    SERIE2 = sum(RAP_PAGO, na.rm = TRUE))
      }
      db
    })
    output$plot02 <- renderHighchart({
      highchart_new(data = dbplot02(), 
                    series = c(SERIE1 = "Despesas do exercício", SERIE2 = "Despesas de exercícios anteriores"),
                    subtitle = paste(unique(dbplot02()$SG_UG), min(dbplot02()$ANO), "a", max(dbplot02()$ANO)),
                    categories = unique(dbplot02()$ANO),
                    credits = "Portal da Transparência",
                    input_plot = c( 
                      typeplot = input$typeplot02, 
                      dimension = input$dimension02, 
                      groupplot = input$groupplot02))
    })
    # 03 Execução da despesa por mês (acumulado) ----
    dbplot03 <- reactive({
      if (input$unidade_orcamento == "IFB") {
        db <- db_siafi %>%
          filter(CO_GD %in% c(input$grupodespesa), substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento) %>%
          group_by(LANCAMENTO, SG_UG = "IFB") %>%
          summarise(SERIE1 = sum(EMPENHADO, na.rm = TRUE),
                    SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
                    SERIE3 = sum(PAGO, na.rm = TRUE),
                    SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
        db$SERIE1 <- cumsum(db$SERIE1)
        db$SERIE2 <- cumsum(db$SERIE2)
        db$SERIE3 <- cumsum(db$SERIE3)
        db$SERIE_RAP <- cumsum(db$SERIE_RAP)
        } else {
        db <- db_siafi %>%
          filter(CO_GD %in% input$grupodespesa, substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento, SG_UG == input$unidade_orcamento) %>%
          group_by(LANCAMENTO, SG_UG) %>%
          summarise(SERIE1 = sum(EMPENHADO, na.rm = TRUE),
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
    output$plot03 <- renderHighchart({
      highchart_new(data = dbplot03(),  
                    series = c(SERIE1 = "EMPENHADO", SERIE2 = "LIQUIDADO", SERIE3 = "PAGO", SERIE_RAP = "RAP PAGO"),
                    subtitle = unique(paste(dbplot03()$SG_UG, substr(dbplot03()$LANCAMENTO, 1, 4))),
                    categories = dbplot03()$LANCAMENTO,
                    credits = "Portal da Transparência",
                    input_plot = c(
                      typeplot = input$typeplot03, 
                      dimension = input$dimension03, 
                      groupplot = input$groupplot03))
    })
    # 04 Execução da despesa por mês (não acumulado) ----
    dbplot04 <- reactive({
      if (input$unidade_orcamento == "IFB") {
        db <- db_siafi %>%
          filter(CO_GD %in% input$grupodespesa, substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento) %>%
          group_by(LANCAMENTO, SG_UG = "IFB") %>%
          summarise(SERIE1 = sum(EMPENHADO, na.rm = TRUE),
                    SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
                    SERIE3 = sum(PAGO, na.rm = TRUE),
                    SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
        } else{
        db <- db_siafi %>%
          filter(CO_GD %in% input$grupodespesa,substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento,SG_UG == input$unidade_orcamento) %>%
          group_by(LANCAMENTO, SG_UG) %>%
          summarise(SERIE1 = sum(EMPENHADO, na.rm = TRUE),
                    SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
                    SERIE3 = sum(PAGO, na.rm = TRUE),
                    SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      }
      db
    })
    output$plot04 <- renderHighchart({
      highchart_new(data = dbplot04(),  
                   series = c(SERIE1 = "EMPENHADO", SERIE2 = "LIQUIDADO", SERIE3 = "PAGO", SERIE_RAP = "RAP PAGO"),
                   subtitle = unique(paste(dbplot04()$SG_UG, substr(dbplot04()$LANCAMENTO, 1, 4))),
                   categories = dbplot04()$LANCAMENTO,
                   credits = "Portal da Transparência",
                   input_plot = c( 
                     typeplot = input$typeplot04, 
                     dimension = input$dimension04, 
                     groupplot = input$groupplot04))
    })
    # 05 Execução da despesa por grupo de despesa ----
    dbplot05 <- reactive({
      if (input$unidade_orcamento == "IFB") {
        db <- db_siafi %>%
          filter(CO_GD %in% input$grupodespesa, substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), CO_GD, SG_UG = "IFB", NO_GD) %>%
          summarise(SERIE1 = sum(EMPENHADO, na.rm = TRUE),
                    SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
                    SERIE3 = sum(PAGO, na.rm = TRUE),
                    SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      } else{
        db <- db_siafi %>%
          filter(CO_GD %in% input$grupodespesa,substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento, SG_UG == input$unidade_orcamento) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG, CO_GD, NO_GD) %>%
          summarise(SERIE1 = sum(EMPENHADO, na.rm = TRUE),
                    SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
                    SERIE3 = sum(PAGO, na.rm = TRUE),
                    SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      }
      db$NO_GD <- str_to_title(db$NO_GD)
      db
    })
    output$plot05 <- renderHighchart({
      highchart_new(data = dbplot05(),  
                    series = c(SERIE1 = "EMPENHADO", SERIE2 = "LIQUIDADO", SERIE3 = "PAGO", SERIE_RAP = "RAP PAGO"),
                   subtitle = paste(unique(dbplot05()$SG_UG), unique(dbplot05()$ANO)),
                   categories = dbplot05()$NO_GD,
                   credits = "Portal da Transparência",
                   input_plot = c( 
                     typeplot = input$typeplot05, 
                     dimension = input$dimension05, 
                     groupplot = input$groupplot05))
    })
    # 06 Execução da despesa por programa ----
    dbplot06 <- reactive({
      db <- db_siafi %>% left_join(tb_siafi_programa, by = "CO_PROGRAMA")
      if (input$unidade_orcamento == "IFB") {
        db <- db %>%
          filter(substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG = "IFB", PROGRAMA = paste(CO_PROGRAMA, NO_PROGRAMA_RESUMO)) %>%
          summarise(SERIE1 = sum(EMPENHADO, na.rm = TRUE),
                    SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
                    SERIE3 = sum(PAGO, na.rm = TRUE),
                    SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      } else{
        db <- db %>%
          filter(substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento, SG_UG == input$unidade_orcamento) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG, PROGRAMA = paste(CO_PROGRAMA, NO_PROGRAMA_RESUMO)) %>%
          summarise(SERIE1 = sum(EMPENHADO, na.rm = TRUE),
                    SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
                    SERIE3 = sum(PAGO, na.rm = TRUE),
                    SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      }
      db$PROGRAMA <- str_to_title(db$PROGRAMA)
      db
    })
    output$plot06 <- renderHighchart({
      highchart_new(data = dbplot06(),  
                    series = c(SERIE1 = "EMPENHADO", SERIE2 = "LIQUIDADO", SERIE3 = "PAGO", SERIE_RAP = "RAP PAGO"),
                   subtitle = paste(unique(dbplot06()$SG_UG), unique(dbplot06()$ANO)),
                   categories = dbplot06()$PROGRAMA,
                   credits = "Portal da Transparência",
                   input_plot = c(
                     typeplot = input$typeplot06, 
                     dimension = input$dimension06, 
                     groupplot = input$groupplot06))
    })
    # 07 Execução da despesa por ação orçamentária ----
    dbplot07 <- reactive({
      db <- db_siafi %>% left_join(tb_siafi_acao, by = "CO_ACAO")
      if (input$unidade_orcamento == "IFB") {
        db <- db %>%
          filter(
            CO_GD %in% input$grupodespesa, 
            substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4),CO_GD, SG_UG = "IFB", NO_ACAO_RESUMO) %>%
          summarise(SERIE1 = sum(EMPENHADO, na.rm = TRUE),
                    SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
                    SERIE3 = sum(PAGO, na.rm = TRUE),
                    SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      } else{
        db <- db %>%
          filter(
            CO_GD %in% input$grupodespesa, 
            substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento, 
            SG_UG == input$unidade_orcamento) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG, CO_ACAO, NO_ACAO_RESUMO) %>%
          summarise(SERIE1 = sum(EMPENHADO, na.rm = TRUE),
                    SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
                    SERIE3 = sum(PAGO, na.rm = TRUE),
                    SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      }
      db
    })
    output$plot07 <- renderHighchart({
      highchart_new(data = dbplot07(),  
                    series = c(SERIE1 = "EMPENHADO", SERIE2 = "LIQUIDADO", SERIE3 = "PAGO", SERIE_RAP = "RAP PAGO"),
                   subtitle = paste(unique(dbplot07()$SG_UG), unique(dbplot07()$ANO)),
                   categories = dbplot07()$NO_ACAO_RESUMO,
                   credits = "Portal da Transparência",
                   input_plot = c( 
                     typeplot = input$typeplot07, 
                     dimension = input$dimension07, 
                     groupplot = input$groupplot07))
    })
    # 08 Execução da despesa por elemento da despesa ----
    dbplot08 <- reactive({
      if (input$unidade_orcamento == "IFB") {
        db <- db_siafi %>%
          filter(CO_GD %in% input$grupodespesa, substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG = "IFB", NO_ED) %>%
          summarise(SERIE1 = sum(EMPENHADO, na.rm = TRUE),
                    SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
                    SERIE3 = sum(PAGO, na.rm = TRUE),
                    SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      } else{
        db <- db_siafi %>%
          filter(CO_GD %in% input$grupodespesa, substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento, SG_UG == input$unidade_orcamento) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG, NO_ED) %>%
          summarise(SERIE1 = sum(EMPENHADO, na.rm = TRUE),
                    SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
                    SERIE3 = sum(PAGO, na.rm = TRUE),
                    SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      }
      db$NO_ED <- str_to_title(db$NO_ED)
      db
    })
    output$plot08 <- renderHighchart({
      highchart_new(data = dbplot08(),  
                    series = c(SERIE1 = "EMPENHADO", SERIE2 = "LIQUIDADO", SERIE3 = "PAGO", SERIE_RAP = "RAP PAGO"),
                   subtitle = paste(unique(dbplot08()$SG_UG), unique(dbplot08()$ANO)),
                   categories = dbplot08()$NO_ED,
                   credits = "Portal da Transparência",
                   input_plot = c(
                     typeplot = input$typeplot08, 
                     dimension = input$dimension08, 
                     groupplot = input$groupplot08))
    })
    
    # 09 Execução da despesa por unidade gestora ----
    dbplot09 <- reactive({
      if(input$department09 == "Sem Reitoria") {
        ug <- unique(db_siafi[!db_siafi$SG_UG == "REITORIA", ]$SG_UG)
      } else{
        ug <- unique(db_siafi$SG_UG)
      }
      db <- db_siafi %>%
        filter(CO_GD %in% input$grupodespesa, substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento, SG_UG %in% ug) %>%
        group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG) %>%
        summarise(SERIE1 = sum(EMPENHADO, na.rm = TRUE),
                  SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
                  SERIE3 = sum(PAGO, na.rm = TRUE),
                  SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
    })
    output$plot09 <- renderHighchart({
      highchart_new(data = dbplot09(),  
                    series = c(SERIE1 = "EMPENHADO", SERIE2 = "LIQUIDADO", SERIE3 = "PAGO", SERIE_RAP = "RAP PAGO"),
                   subtitle = paste(input$unidade_orcamento, unique(dbplot09()$ANO)),
                   categories = dbplot09()$SG_UG,
                   credits = "Portal da Transparência",
                   input_plot = c(
                     typeplot = input$typeplot09, 
                     dimension = input$dimension09, 
                     groupplot = input$groupplot09))
    })
    # 10 Execução da despesa por campi e reitoria ----
    dbplot10 <- reactive({
      db <- db_siafi %>%
        filter(CO_GD %in% input$grupodespesa,
               substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento,
               SG_UG == "REITORIA") %>%
        group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG) %>%
        summarise(SERIE1 = sum(EMPENHADO, na.rm = TRUE),
                  SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
                  SERIE3 = sum(PAGO, na.rm = TRUE),
                  SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      db1 <- db_siafi %>%
        filter(CO_GD %in% input$grupodespesa,
               substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento,
               !SG_UG == "REITORIA") %>%
        group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG = "CAMPI") %>%
        summarise(SERIE1 = sum(EMPENHADO, na.rm = TRUE),
                  SERIE2 = sum(LIQUIDADO, na.rm = TRUE),
                  SERIE3 = sum(PAGO, na.rm = TRUE),
                  SERIE_RAP = sum(RAP_PAGO, na.rm = TRUE))
      db <- bind_rows(db, db1)
      db
    })
    output$plot10 <- renderHighchart({
      highchart_new(data = dbplot10(),  
                    series = c(SERIE1 = "EMPENHADO", SERIE2 = "LIQUIDADO", SERIE3 = "PAGO", SERIE_RAP = "RAP PAGO"),
                   subtitle = paste("CAMPI E REITORIA ", unique(dbplot10()$ANO)),
                   categories = dbplot10()$SG_UG,
                   credits = "Portal da Transparência",
                   input_plot = c( 
                     typeplot = input$typeplot10, 
                     dimension = input$dimension10, 
                     groupplot = input$groupplot10))
    })
    # 11 Pagamento no exercício e de exercícios anteriores (RAP) ----
    dbplot11 <- reactive({
      if(input$department11 == "Sem Reitoria") {
        ug <- unique(db_siafi[!db_siafi$SG_UG == "REITORIA", ]$SG_UG)
      } else{
        ug <- unique(db_siafi$SG_UG)
      }
      db <- db_siafi %>%
        filter(CO_GD %in% input$grupodespesa, substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento, SG_UG %in% ug) %>%
        group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG) %>%
        summarise(SERIE1 = sum(PAGO, na.rm = TRUE),
                  SERIE2 = sum(RAP_PAGO, na.rm = TRUE))
      db
    })
    output$plot11 <- renderHighchart({
      highchart_new(data = dbplot11(),  
                    series = c(SERIE1 = "Despesas do exercício", SERIE2 = "Despesas de exercícios anteriores"),
                   subtitle = paste(input$unidade_orcamento, unique(dbplot11()$ANO)),
                   categories = dbplot11()$SG_UG,
                   credits = "Portal da Transparência",
                   input_plot = c( 
                     typeplot = input$typeplot11, 
                     dimension = input$dimension11, 
                     groupplot = input$groupplot11))
    })
    # 12 Pagamento no exercício e de exercícios anteriores (Campi e Reitoria) ----
    dbplot12 <- reactive({
      db <- db_siafi %>%
        filter(CO_GD %in% input$grupodespesa,
               substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento,
               SG_UG == "REITORIA") %>%
        group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG) %>%
        summarise(SERIE1 = sum(PAGO, na.rm = TRUE),
                  SERIE2 = sum(RAP_PAGO, na.rm = TRUE))
      db1 <- db_siafi %>%
        filter(CO_GD %in% input$grupodespesa,
               substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento,
               !SG_UG == "REITORIA") %>%
        group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG = "CAMPI") %>%
        summarise(SERIE1 = sum(PAGO, na.rm = TRUE),
                  SERIE2 = sum(RAP_PAGO, na.rm = TRUE))
      db <- bind_rows(db, db1)
      db
    })
    output$plot12 <- renderHighchart({
      highchart_new(data = dbplot12(),  
                    series = c(SERIE1 = "Despesas do exercício", SERIE2 = "Despesas de exercícios anteriores"),
                   subtitle = paste("CAMPI E REITORIA ", unique(dbplot12()$ANO)),
                   categories = dbplot12()$SG_UG,
                   credits = "Portal da Transparência",
                   input_plot = c(
                     typeplot = input$typeplot12, 
                     dimension = input$dimension12, 
                     groupplot = input$groupplot12))
    })

# PESSOAL -----------------------------------------------------------------
    # 13 Número de técnicos e docentes por ano ----
    dbplot13 <- reactive({
      last <- max(sp_quadro$REFERENCIA)
      if (input$unidade_pessoal == "IFB"){
        db <- sp_quadro %>% 
          filter(REFERENCIA == last | grepl("*12$", REFERENCIA)) %>% 
          group_by(REFERENCIA, UORG_LOTACAO_GRUPO = "IFB") %>% 
          summarise(SERIE1 = sum(TOTAL, na.rm = TRUE),
                    SERIE2 = sum(DOCENTE, na.rm = TRUE),
                    SERIE3 = sum(TECNICO, na.rm = TRUE))
      } else {
        db <- sp_quadro %>% 
          filter(UORG_LOTACAO_GRUPO == input$unidade_pessoal, REFERENCIA == last | grepl("*12$", REFERENCIA)) %>% 
          group_by(REFERENCIA, UORG_LOTACAO_GRUPO) %>% 
          summarise(SERIE1 = sum(TOTAL, na.rm = TRUE),
                    SERIE2 = sum(DOCENTE, na.rm = TRUE),
                    SERIE3 = sum(TECNICO, na.rm = TRUE))
      }
      db$REFERENCIA <- substr(db$REFERENCIA, 1, 4)
      db
    })
    output$plot13 <- renderHighchart({
      highchart_new(data = dbplot13(),  
                   series = c(SERIE1 = "TOTAL",SERIE2 = "DOCENTE", SERIE3 = "TECNICO"),
                   subtitle = paste(unique(dbplot13()$UORG_LOTACAO_GRUPO), "-", min(dbplot13()$REFERENCIA), "a", max(dbplot13()$REFERENCIA)),
                   categories = dbplot13()$REFERENCIA,
                   credits = "Portal da Transparência",
                   input_plot = c(
                     typeplot = input$typeplot13, 
                     dimension = input$dimension13, 
                     groupplot = input$groupplot13))
    })
    # 14 Percentual de técnicos e docentes por ano ----
    dbplot14 <- reactive({
      last <- max(sp_quadro$REFERENCIA)
      if (input$unidade_pessoal == "IFB"){
        db <- sp_quadro %>% 
          filter(REFERENCIA == last | grepl("*12$", REFERENCIA)) %>% 
          group_by(REFERENCIA, UORG_LOTACAO_GRUPO = "IFB") %>% 
          summarise(SERIE1 = sum(DOCENTE, na.rm = TRUE),
                    SERIE2 = sum(TECNICO, na.rm = TRUE))
      } else {
        db <- sp_quadro %>% 
          filter(UORG_LOTACAO_GRUPO == input$unidade_pessoal, REFERENCIA == last | grepl("*12$", REFERENCIA)) %>% 
          group_by(REFERENCIA, UORG_LOTACAO_GRUPO) %>% 
          summarise(SERIE1 = sum(DOCENTE, na.rm = TRUE),
                    SERIE2 = sum(TECNICO, na.rm = TRUE))
      }
      db$REFERENCIA <- substr(db$REFERENCIA, 1, 4)
      db
    })
    output$plot14 <- renderHighchart({
      highchart_new(data = dbplot14(),  
                    series = c(SERIE1 = "DOCENTE",SERIE2 = "TECNICO"),
                   subtitle = paste(unique(dbplot14()$UORG_LOTACAO_GRUPO), "-", min(dbplot14()$REFERENCIA), "a", max(dbplot14()$REFERENCIA)),
                   categories = dbplot14()$REFERENCIA,
                   credits = "Portal da Transparência",
                   input_plot = c( 
                     typeplot = input$typeplot14, 
                     dimension = input$dimension14, 
                     groupplot = input$groupplot14))
    })
    # 15 Múmero de técnicos e docentes por mês ----
    dbplot15 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        db <- sp_quadro %>% 
          group_by(REFERENCIA, UORG_LOTACAO_GRUPO = "IFB") %>% 
          summarise(SERIE1 = sum(TOTAL, na.rm = TRUE),
                    SERIE2 = sum(DOCENTE, na.rm = TRUE),
                    SERIE3 = sum(TECNICO, na.rm = TRUE))
      } else {
        db <- sp_quadro %>% 
          filter(UORG_LOTACAO_GRUPO == input$unidade_pessoal) %>% 
          group_by(REFERENCIA, UORG_LOTACAO_GRUPO) %>% 
          summarise(SERIE1 = sum(TOTAL, na.rm = TRUE),
                    SERIE2 = sum(DOCENTE, na.rm = TRUE),
                    SERIE3 = sum(TECNICO, na.rm = TRUE))
      }
      db$REFERENCIA <- paste0(substr(db$REFERENCIA,1,4),"/",substr(db$REFERENCIA,5,6))
      db
    })
    output$plot15 <- renderHighchart({
      highchart_new(data = dbplot15(),  
                    series = c(SERIE1 = "TOTAL",SERIE2 = "DOCENTE", SERIE3 = "TECNICO"),
                   subtitle = paste(unique(dbplot15()$UORG_LOTACAO_GRUPO), "-", min(dbplot15()$REFERENCIA), "a", max(dbplot15()$REFERENCIA)),
                   categories = dbplot15()$REFERENCIA,
                   credits = "Portal da Transparência",
                   input_plot = c(
                     typeplot = input$typeplot15, 
                     dimension = input$dimension15, 
                     groupplot = input$groupplot15))
    })
    # 16 Percentual de técnicos e docentes por mês ----
    dbplot16 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        db <- sp_quadro %>% 
          group_by(MES = substr(REFERENCIA, 1, 6), UORG_LOTACAO_GRUPO = "IFB") %>% 
          summarise(SERIE1 = sum(DOCENTE, na.rm = TRUE),
                    SERIE2 = sum(TECNICO, na.rm = TRUE))
      } else {
        db <- sp_quadro %>% 
          filter(UORG_LOTACAO_GRUPO == input$unidade_pessoal) %>% 
          group_by(MES = substr(REFERENCIA, 1, 6), UORG_LOTACAO_GRUPO) %>% 
          summarise(SERIE1 = sum(DOCENTE, na.rm = TRUE),
                    SERIE2 = sum(TECNICO, na.rm = TRUE))
      }
      db$MES <- paste0(substr(db$MES,1,4),"/",substr(db$MES,5,6))
      db
    })
    output$plot16 <- renderHighchart({
      highchart_new(data = dbplot16(),  
                   series = c(SERIE1 = "DOCENTE", SERIE2 = "TECNICO"),
                   subtitle = paste(unique(dbplot16()$UORG_LOTACAO_GRUPO), "-", min(dbplot16()$MES), "a", max(dbplot16()$MES)),
                   categories = dbplot16()$MES,
                   credits = "Portal da Transparência",
                   input_plot = c( 
                     typeplot = input$typeplot16, 
                     dimension = input$dimension16, 
                     groupplot = input$groupplot16))
    })
    # 17 Servidores por cargo ----
    dbplot17 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        tm <- sp_cargo %>% 
          filter(REFERENCIA == max(sp_cargo$REFERENCIA)) %>% 
          group_by(DESCRICAO_CARGO) %>% 
          summarise(SERIE = sum(TOTAL, na.rm = TRUE)) %>% 
          left_join(tb_siape_cargos, by = "DESCRICAO_CARGO")
      } else {
        tm <- sp_cargo %>% 
          filter(REFERENCIA == max(sp_cargo$REFERENCIA), UORG_LOTACAO_GRUPO == input$unidade_pessoal) %>% 
          group_by(DESCRICAO_CARGO) %>% 
          summarise(SERIE = sum(TOTAL, na.rm = TRUE)) %>% 
          left_join(tb_siape_cargos, by = "DESCRICAO_CARGO")
      }
      tm <- treemap(tm, index = "DESCRICAO_CARGO", vSize = "SERIE", vColor =  "SERIE",
                    type = "value", palette = c("#19334d", "#264d73", "#336699", "#4080bf"), pdf(file = NULL))
      tm
    })
    output$plot17 <- renderHighchart({
      hctreemap(tm = dbplot17()) %>% 
        hc_exporting(enabled = TRUE) %>% 
        hc_credits(enabled = TRUE, text = paste("Fonte:", "Portal da Transparência"), 
                   href = "http://portaltransparencia.gov.br/download-de-dados") %>% 
        hc_title(text = paste(input$unidade_pessoal, "-", max(paste0(substr(db_siape$REFERENCIA, 1 , 4),"/",substr(db_siape$REFERENCIA, 5, 6)))),
                 style = list(fontSize = "12px"))
    })
    # 18 Servidores por situação ----
    dbplot18 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        tm <- db_siape %>% 
          filter(REFERENCIA == max(db_siape$REFERENCIA), SIGLA_FUNCAO == "-1", COD_ORG_LOTACAO == "26428") %>% 
          group_by(SITUACAO_VINCULO) %>% 
          summarise(SERIE = n())
      } else {
        tm <- db_siape %>% 
          filter(REFERENCIA == max(db_siape$REFERENCIA), SIGLA_FUNCAO == "-1", COD_ORG_LOTACAO == "26428", UORG_LOTACAO_GRUPO == input$unidade_pessoal) %>% 
          group_by(SITUACAO_VINCULO) %>% 
          summarise(SERIE = n())
      }
      tm <- treemap(tm, index = "SITUACAO_VINCULO", vSize = "SERIE", vColor =  "SERIE",
                    type = "value", palette = c("#19334d", "#264d73", "#336699", "#4080bf"), pdf(file = NULL))
      tm
    })
    output$plot18 <- renderHighchart({
      hctreemap(tm = dbplot18()) %>% 
        hc_exporting(enabled = TRUE) %>% 
        hc_credits(enabled = TRUE, text = paste("Fonte:", "Portal da Transparência"), 
                   href = "http://portaltransparencia.gov.br/download-de-dados") %>% 
        hc_title(text = paste(input$unidade_pessoal, "-", max(paste0(substr(db_siape$REFERENCIA, 1 , 4),"/",substr(db_siape$REFERENCIA, 5, 6)))),
                 style = list(fontSize = "12px"))
    })
    # 19 Jornada de trabalho dos docentes e técnicos ----
    dbplot19 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        db <- db_siape %>% 
          filter(REFERENCIA == max(REFERENCIA), SIGLA_FUNCAO == "-1", COD_ORG_LOTACAO == "26428") %>% 
          group_by(JORNADA_DE_TRABALHO, TIPO_CARGO = ifelse(grepl("\\<PROF", DESCRICAO_CARGO), "DOCENTE", "TÉCNICO")) %>% 
          summarise(TOTAL = n())
      } else {
        db <- db_siape %>% 
          filter(REFERENCIA == max(REFERENCIA), SIGLA_FUNCAO == "-1", COD_ORG_LOTACAO == "26428", UORG_LOTACAO_GRUPO == input$unidade_pessoal) %>% 
          group_by(JORNADA_DE_TRABALHO, TIPO_CARGO = ifelse(grepl("\\<PROF", DESCRICAO_CARGO), "DOCENTE", "TÉCNICO")) %>% 
          summarise(TOTAL = n())
      }
      db1 <- tibble(JORNADA_DE_TRABALHO = rep(c("20 HORAS SEMANAIS", "25 HORAS SEMANAIS", "30 HORAS SEMANAIS", "40 HORAS SEMANAIS", "DEDICACAO EXCLUSIVA"),2),
                    TIPO_CARGO = rep(c("TÉCNICO", "DOCENTE"), 5))
      db <- full_join(db, db1, by = c("JORNADA_DE_TRABALHO", "TIPO_CARGO"))
      if(any(is.na(db$TOTAL))) db[is.na(db$TOTAL), ]$TOTAL <- 0
      db <- db[order(db$JORNADA_DE_TRABALHO), ]
      db <- spread(data = db, key = JORNADA_DE_TRABALHO, value = TOTAL, fill = 0)
      nm <- seq(from = 1, to = ncol(db)-1)
      nm <- paste0("SERIE", nm)
      colnames(db) <- c("TIPO_CARGO", nm)
      db
    })
    output$plot19 <- renderHighchart({
      highchart_new(data = dbplot19(),  
                    series = c(SERIE1 = "20 HORAS", SERIE2 = "25 HORAS", SERIE3 = "30 HORAS", SERIE4 = "40 HORAS", SERIE5 = "DE"),
                    subtitle = paste(input$unidade_pessoal, "-", max(paste0(substr(db_siape$REFERENCIA, 1 , 4),"/",substr(db_siape$REFERENCIA, 5, 6)))),
                    categories = dbplot19()$TIPO_CARGO,
                    credits = "Portal da Transparência",
                    input_plot = c(
                      typeplot = input$typeplot19, 
                      dimension = input$dimension19, 
                      groupplot = input$groupplot19))
      })
    
    # 20 Classificação dos cargos técnicos ----
    dbplot20 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        db <- db_siape %>% 
          filter(REFERENCIA == max(REFERENCIA), SIGLA_FUNCAO == "-1", COD_ORG_LOTACAO == "26428", !grepl("\\<PROF", DESCRICAO_CARGO)) %>% 
          group_by(CLASSE_CARGO) %>% 
          summarise(TOTAL = n())
      } else {
        db <- db_siape %>% 
          filter(REFERENCIA == max(REFERENCIA), SIGLA_FUNCAO == "-1", COD_ORG_LOTACAO == "26428", UORG_LOTACAO_GRUPO == input$unidade_pessoal, !grepl("\\<PROF", DESCRICAO_CARGO)) %>% 
          group_by(CLASSE_CARGO) %>% 
          summarise(TOTAL = n())
      }
      db
    })
    output$plot20 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = paste(input$unidade_pessoal, "-", max(paste0(substr(db_siape$REFERENCIA, 1 , 4),"/",substr(db_siape$REFERENCIA, 5, 6))))) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>% 
        hc_chart(type = 'pie', options3d = list(enabled = TRUE, alpha = 45)) %>% 
        hc_plotOptions(pie = list(innerSize = 100, depth = 45)) %>% 
        hc_add_series_labels_values(
          name = "Total",
          labels = dbplot20()$CLASSE_CARGO,
          values = dbplot20()$TOTAL)
      hc
    })
    # 21 Nível de capacitação por classificação dos cargos técnicos ----
    dbplot21 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        db <- db_siape %>% 
          filter(REFERENCIA == max(REFERENCIA), 
                 SIGLA_FUNCAO == "-1", 
                 COD_ORG_LOTACAO == "26428", 
                 !grepl("\\<PROF", DESCRICAO_CARGO),
                 !DESCRICAO_CARGO == "Inválido") %>% 
          group_by(CLASSE_CARGO, PADRAO_CARGO = substr(PADRAO_CARGO, 1, 1)) %>% 
          summarise(TOTAL = n())
        db1 <- db_siape %>% 
          filter(REFERENCIA == max(REFERENCIA), 
                 SIGLA_FUNCAO == "-1", 
                 COD_ORG_LOTACAO == "26428", 
                 !grepl("\\<PROF", DESCRICAO_CARGO),
                 !DESCRICAO_CARGO == "Inválido") %>% 
          group_by(CLASSE_CARGO, PADRAO_CARGO = "TOTAL") %>% 
          summarise(TOTAL = n())
        db <- bind_rows(db, db1)
      } else {
        db <- db_siape %>% 
          filter(REFERENCIA == max(REFERENCIA), 
                 SIGLA_FUNCAO == "-1", 
                 COD_ORG_LOTACAO == "26428", 
                 UORG_LOTACAO_GRUPO == input$unidade_pessoal, 
                 !grepl("\\<PROF", DESCRICAO_CARGO),
                 !DESCRICAO_CARGO == "Inválido") %>% 
          group_by(CLASSE_CARGO, PADRAO_CARGO = substr(PADRAO_CARGO, 1, 1)) %>% 
          summarise(TOTAL = n())
        db1 <- db_siape %>% 
          filter(REFERENCIA == max(REFERENCIA), 
                 SIGLA_FUNCAO == "-1", 
                 COD_ORG_LOTACAO == "26428", 
                 UORG_LOTACAO_GRUPO == input$unidade_pessoal, 
                 !grepl("\\<PROF", DESCRICAO_CARGO),
                 !DESCRICAO_CARGO == "Inválido") %>% 
          group_by(CLASSE_CARGO, PADRAO_CARGO = "TOTAL") %>% 
          summarise(TOTAL = n())
        db <- bind_rows(db, db1)
      }
      db1 <- tibble(CLASSE_CARGO = rep(c("A", "B", "C", "D", "E"), 4), PADRAO_CARGO = rep(c("1", "2", "3", "4"), 5))
      db1 <- db1[order(db1$CLASSE_CARGO), ]
      db <- full_join(db, db1, by = c("CLASSE_CARGO", "PADRAO_CARGO"))
      if(any(is.na(db$TOTAL))) db[is.na(db$TOTAL), ]$TOTAL <- 0
      db <- db[order(db$CLASSE_CARGO), ]
      db <- spread(data = db, key = PADRAO_CARGO, value = TOTAL, fill = 0)
      nm <- seq(from = 1, to = ncol(db)-2)
      nm <- paste0("SERIE", nm)
      colnames(db) <- c("CLASSE_CARGO", nm, "SERIE_TYPE")
      db
    })
    output$plot21 <- renderHighchart({
      highchart_new(data = dbplot21(),  
                    series = c(SERIE1 = "Nível I", SERIE2 = "Nível II", SERIE3 = "Nível III", SERIE4 = "Nível IV"),
                    subtitle = paste(input$unidade_pessoal, "-", max(paste0(substr(db_siape$REFERENCIA, 1 , 4),"/",substr(db_siape$REFERENCIA, 5, 6)))),
                    categories = dbplot21()$CLASSE_CARGO,
                    credits = "Portal da Transparência",
                    input_plot = c( 
                      typeplot = input$typeplot21, 
                      dimension = input$dimension21, 
                      groupplot = input$groupplot21))
    })
    # 22 Cargos em comissão e funções gratificadas ----
    dbplot22 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        db <- db_siape %>% 
          filter(REFERENCIA == max(REFERENCIA), !SIGLA_FUNCAO == "-1", COD_ORG_EXERCICIO == "26428") %>% 
          group_by(FUNCAO = paste(SIGLA_FUNCAO, NIVEL_FUNCAO)) %>% 
          summarise(SERIE1 = n())
        db$UORG_LOTACAO_GRUPO <- "IFB"
      } else {
        db <- db_siape %>% 
          filter(REFERENCIA == max(REFERENCIA), !SIGLA_FUNCAO == "-1", COD_ORG_EXERCICIO == "26428", UORG_LOTACAO_GRUPO == input$unidade_pessoal) %>% 
          group_by(FUNCAO = paste(SIGLA_FUNCAO, NIVEL_FUNCAO), UORG_LOTACAO_GRUPO) %>% 
          summarise(SERIE1 = n())
      }
      db
    })
    output$plot22 <- renderHighchart({
      highchart_new(data = dbplot22(),  
                    series = c(SERIE1 = "Nº de cargos e funções"),
                    subtitle = paste(input$unidade_pessoal, "-", max(paste0(substr(db_siape$REFERENCIA, 1 , 4),"/",substr(db_siape$REFERENCIA, 5, 6)))),
                    categories = dbplot22()$FUNCAO,
                    credits = "Portal da Transparência",
                    input_plot = c(
                      typeplot = input$typeplot22, 
                      dimension = input$dimension22, 
                      groupplot = input$groupplot22))
      })
    # 23 Admissão e desligamento de docentes e técnicos por ano ----
    dbplot23 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        db <- sp_rot_efetivo %>% 
          group_by(REFERENCIA = substr(FIM, 1, 4), TIPO = ifelse(TIPO == "ADMISSAO", "SERIE1", "SERIE2")) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum) %>% mutate(UORG_LOTACAO_GRUPO = "IFB")
      } else {
        db <- sp_rot_unidade %>% 
          filter(UORG_LOTACAO_GRUPO == input$unidade_pessoal) %>% 
          group_by(UORG_LOTACAO_GRUPO, REFERENCIA = substr(FIM, 1, 4), TIPO = ifelse(TIPO == "ADMISSAO", "SERIE1", "SERIE2")) %>%  
          summarise_at(.vars = "TOTAL", .funs = sum)
      }
      db <- spread(data = db, key = TIPO, value = TOTAL, fill = 0)
      db
    })
    output$plot23 <- renderHighchart({
      highchart_new(data = dbplot23(),  
                    series = c(SERIE1 = "Admissões", SERIE2 = "Desligamentos"),
                    subtitle = paste(input$unidade_pessoal, "-", min(dbplot23()$REFERENCIA), "a", max(dbplot23()$REFERENCIA)),
                    categories = dbplot23()$REFERENCIA,
                    credits = "Portal da Transparência",
                    input_plot = c( 
                      typeplot = input$typeplot23, 
                      dimension = input$dimension23, 
                      groupplot = input$groupplot23))
    })
    # 24 Admissão e desligamento de docentes e técnicos por ano e tipo de cargo ----
    dbplot24 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        db <- sp_rot_efetivo %>% mutate(TIPO_CARGO = ifelse(grepl("\\<PROF", DESCRICAO_CARGO), "DOCENTE", "TECNICO"))
        db1 <- db %>% 
          filter(TIPO_CARGO == "DOCENTE") %>% 
          group_by(REFERENCIA = substr(FIM, 1, 4),
                   TIPO) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum)
        db1 <- spread(data = db1, key = TIPO, value = TOTAL, fill = 0)
        colnames(db1) <- c("REFERENCIA", "SERIE1", "SERIE3")
        db2 <- db %>% 
          filter(TIPO_CARGO == "TECNICO") %>% 
          group_by(REFERENCIA = substr(FIM, 1, 4),
                   TIPO) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum)
        db2 <- spread(data = db2, key = TIPO, value = TOTAL, fill = 0)
        colnames(db2) <- c("REFERENCIA", "SERIE2", "SERIE4")
        db2$UORG_LOTACAO_GRUPO <- "IFB"
        db <- left_join(db1, db2, by = "REFERENCIA")
      } else {
        db <- sp_rot_efetivo %>% mutate(TIPO_CARGO = ifelse(grepl("\\<PROF", DESCRICAO_CARGO), "DOCENTE", "TECNICO"))
        db1 <- db %>% 
          filter(UORG_LOTACAO_GRUPO == input$unidade_pessoal, TIPO_CARGO == "DOCENTE") %>% 
          group_by(REFERENCIA = substr(FIM, 1, 4),
                   TIPO) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum)
        db1 <- spread(data = db1, key = TIPO, value = TOTAL, fill = 0)
        colnames(db1) <- c("REFERENCIA", "SERIE1", "SERIE3")
        db2 <- db %>% 
          filter(UORG_LOTACAO_GRUPO == input$unidade_pessoal, TIPO_CARGO == "TECNICO") %>% 
          group_by(REFERENCIA = substr(FIM, 1, 4),
                   TIPO) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum)
        db2 <- spread(data = db2, key = TIPO, value = TOTAL, fill = 0)
        colnames(db2) <- c("REFERENCIA", "SERIE2", "SERIE4")
        db2$UORG_LOTACAO_GRUPO <- "IFB"
        db <- left_join(db1, db2, by = "REFERENCIA")
      }
      db
    })
    output$plot24 <- renderHighchart({
      highchart_new(data = dbplot24(),  
                    series = c(SERIE1 = "Admissão (Docente)", SERIE2 = "Admissão (Técnico)", SERIE3 = "Desligamento (Docente)", SERIE4 = "Desligamento (Técnico)"),
                    subtitle = paste(input$unidade_pessoal, "-", min(dbplot23()$REFERENCIA), "a", max(dbplot23()$REFERENCIA)),
                    categories = dbplot24()$REFERENCIA,
                    credits = "Portal da Transparência",
                    input_plot = c(
                      typeplot = input$typeplot24, 
                      dimension = input$dimension24, 
                      groupplot = input$groupplot24))
    })
    # 25 Admissão e desligamento de prof. temporários e substitutos por ano ----
    dbplot25 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        db <- sp_rot_vinculo %>% 
          filter(grepl("\\<CONT.PROF", SITUACAO_VINCULO)) %>% 
          group_by(REFERENCIA = substr(FIM, 1, 4), TIPO = ifelse(TIPO == "ADMISSAO", "SERIE1", "SERIE2")) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum) %>% 
          mutate(UORG_LOTACAO_GRUPO = "IFB")
      } else {
        db <- sp_rot_vinculo %>% 
          filter(grepl("\\<CONT.PROF", SITUACAO_VINCULO), UORG_LOTACAO_GRUPO == input$unidade_pessoal) %>% 
          group_by(UORG_LOTACAO_GRUPO, REFERENCIA = substr(FIM, 1, 4), TIPO = ifelse(TIPO == "ADMISSAO", "SERIE1", "SERIE2")) %>%  
          summarise_at(.vars = "TOTAL", .funs = sum)
      }
      db <- spread(data = db, key = TIPO, value = TOTAL, fill = 0)
      db
    })
    output$plot25 <- renderHighchart({
      highchart_new(data = dbplot25(),  
                    series = c(SERIE1 = "Admissões", SERIE2 = "Desligamentos"),
                    subtitle = paste(input$unidade_pessoal, "-", min(dbplot23()$REFERENCIA), "a", max(dbplot23()$REFERENCIA)),
                    categories = dbplot25()$REFERENCIA,
                    credits = "Portal da Transparência",
                    input_plot = c(
                      typeplot = input$typeplot25, 
                      dimension = input$dimension25, 
                      groupplot = input$groupplot25))
    })
    # 26 Admissão e desligamento de prof. temporários e substitutos por ano e vínculo ----
    dbplot26 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        db <- sp_rot_vinculo %>% filter(grepl("\\<CONT.PROF", SITUACAO_VINCULO)) %>% mutate(VINCULO = ifelse(grepl("TEMPORARIO$", SITUACAO_VINCULO), "TEMPORARIO", "SUBSTITUTO"))
        db1 <- db %>% 
          filter(VINCULO == "TEMPORARIO") %>% 
          group_by(REFERENCIA = substr(FIM, 1, 4),
                   TIPO) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum)
        db1 <- spread(data = db1, key = TIPO, value = TOTAL, fill = 0)
        colnames(db1) <- c("REFERENCIA", "SERIE1", "SERIE3")
        db2 <- db %>% 
          filter(VINCULO == "SUBSTITUTO") %>% 
          group_by(REFERENCIA = substr(FIM, 1, 4),
                   TIPO) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum)
        db2 <- spread(data = db2, key = TIPO, value = TOTAL, fill = 0)
        colnames(db2) <- c("REFERENCIA", "SERIE2", "SERIE4")
        db2$UORG_LOTACAO_GRUPO <- "IFB"
        db <- left_join(db1, db2, by = "REFERENCIA")
      } else {
        db <- sp_rot_vinculo %>% filter(grepl("\\<CONT.PROF", SITUACAO_VINCULO)) %>% mutate(VINCULO = ifelse(grepl("TEMPORARIO$", SITUACAO_VINCULO), "TEMPORARIO", "SUBSTITUTO"))
        db1 <- db %>% 
          filter(UORG_LOTACAO_GRUPO == input$unidade_pessoal, VINCULO == "TEMPORARIO") %>% 
          group_by(REFERENCIA = substr(FIM, 1, 4),
                   TIPO) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum)
        db1 <- spread(data = db1, key = TIPO, value = TOTAL, fill = 0)
        colnames(db1) <- c("REFERENCIA", "SERIE1", "SERIE3")
        db2 <- db %>% 
          filter(UORG_LOTACAO_GRUPO == input$unidade_pessoal, VINCULO == "SUBSTITUTO") %>% 
          group_by(REFERENCIA = substr(FIM, 1, 4),
                   TIPO) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum)
        db2 <- spread(data = db2, key = TIPO, value = TOTAL, fill = 0)
        colnames(db2) <- c("REFERENCIA", "SERIE2", "SERIE4")
        db2$UORG_LOTACAO_GRUPO <- "IFB"
        db <- left_join(db1, db2, by = "REFERENCIA")
      }
      db
    })
    output$plot26 <- renderHighchart({
      highchart_new(data = dbplot26(),  
                    series = c(SERIE1 = "Admissão (Temporário)", SERIE2 = "Admissão (Substituto)", SERIE3 = "Desligamento (Temporário)", SERIE4 = "Desligamento (Substituto)"),
                    subtitle = paste(input$unidade_pessoal, "-", min(dbplot23()$REFERENCIA), "a", max(dbplot23()$REFERENCIA)),
                    categories = dbplot26()$REFERENCIA,
                    credits = "Portal da Transparência",
                    input_plot = c(
                      typeplot = input$typeplot26, 
                      dimension = input$dimension26, 
                      groupplot = input$groupplot26))
    })

# AVALIAÇÃO ---------------------------------------------------------------
    # 27 Número de avaliadores por cargo ----
    dbplot27 <- reactive({
      db <- db_avager_av %>% group_by(ANO, CARGO, QUESTAO) %>% summarise(SERIE1 = n())
      db <- db %>% group_by(ANO, CARGO, SERIE1) %>% summarise()
      db
    })
    
    output$plot27 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = paste(input$avager_unidade, unique(dbplot28()$ANO))) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>% 
        hc_chart(type = 'pie', options3d = list(enabled = FALSE, alpha = 45)) %>% 
        hc_add_series_labels_values(
          name = "Total",
          labels = dbplot27()$CARGO,
          values = dbplot27()$SERIE1) %>% 
        hc_plotOptions(pie = list(showInLegend = TRUE, dataLabels = list(enabled = FALSE)))
      hc
    })
    # 28 Resultado da avaliação por critério ----
    dbplot28 <- reactive({
      db <- db_avager_av %>% 
        filter(UNIDADE_AVALIADA == input$avager_unidade) %>% 
        group_by(UNIDADE_AVALIADA, ANO, AVALIACAO) %>% summarise(TOTAL = n()) %>% 
        mutate(SERIE1 = (TOTAL/sum(TOTAL))*100)
      db$AVALIACAO <- factor(
        x = db$AVALIACAO, 
        levels = c("Concordo plenamente", "Concordo parcialmente", "Algumas vezes", "Discordo parcialmente", "Discordo totalmente", "Não sei avaliar"))
      db <- db[order(db$AVALIACAO), ]
      db$SERIE1 <- as.numeric(format(x = db$SERIE1, digits = 2, nsmall = 2))
      db
    })
    output$plot28 <- renderHighchart({
      highchart_new(data = dbplot28(),  
                    series = c(SERIE1 = "% Avaliadores"),
                    subtitle = paste(input$avager_unidade, unique(dbplot28()$ANO)),
                    categories = as.character(dbplot28()$AVALIACAO),
                    credits = "Googledrive",
                    input_plot = c( 
                      typeplot = input$typeplot28, 
                      dimension = input$dimension28, 
                      groupplot = input$groupplot28))
    })
    observeEvent(input$avager_unidade, {
      updateSelectInput(
        session,
        inputId = "avager_cargo", 
        label = "Quadro a que pertence", 
        choices = c("TODOS", unique(db_avager_av[db_avager_av$UNIDADE_AVALIADA == input$avager_unidade, ]$CARGO)),
        selected = "TODOS")
    })
    observeEvent(input$avager_unidade, {
      updateSelectInput(
        session,
        inputId = "avager_questao", 
        label = "Questão", 
        choices = unique(db_avager_av[db_avager_av$UNIDADE_AVALIADA == input$avager_unidade, ]$QUESTAO),
        selected = unique(db_avager_av[db_avager_av$UNIDADE_AVALIADA == input$avager_unidade, ]$QUESTAO)[1])
    })
    # 29 Resultado da avaliação por nota ----
    dbplot29 <- eventReactive(c(input$avager_cargo, input$avager_unidade), {
      if(input$avager_cargo == "TODOS"){
        db <- db_avager_nt %>% 
          filter(!is.na(AVALIACAO), UNIDADE_AVALIADA == input$avager_unidade) %>% 
          group_by(ANO, AVALIACAO) %>% summarise(TOTAL = n()) %>% 
          mutate(SERIE1 = (TOTAL/sum(TOTAL))*100)
      }else{
        db <- db_avager_nt %>% 
          filter(!is.na(AVALIACAO), CARGO == input$avager_cargo, UNIDADE_AVALIADA == input$avager_unidade) %>% 
          group_by(ANO, AVALIACAO) %>% summarise(TOTAL = n()) %>% 
          mutate(SERIE1 = (TOTAL/sum(TOTAL))*100)
      }
      db$AVALIACAO <- factor(
        x = db$AVALIACAO, 
        levels = c(1:10))
      db <- db[order(db$AVALIACAO, decreasing = TRUE), ]
      db$SERIE1 <- as.numeric(format(x = db$SERIE1, digits = 2, nsmall = 2))
      db
    })
    output$plot29 <- renderHighchart({
      highchart_new(data = dbplot29(),  
                    series = c(SERIE1 = "% Avaliadores"),
                    subtitle = paste(input$avager_unidade, unique(dbplot29()$ANO)),
                    categories = as.character(dbplot29()$AVALIACAO),
                    credits = "Googledrive",
                    input_plot = c( 
                      typeplot = input$typeplot29, 
                      dimension = input$dimension29, 
                      groupplot = input$groupplot29))
    })
    # 30 Resultado da avaliação por questão ----
    dbplot30 <- eventReactive(c(input$avager_unidade, input$avager_questao, input$avager_cargo), {
      if(input$avager_cargo == "TODOS"){
        db <- db_avager_av %>% 
          filter(QUESTAO == input$avager_questao, UNIDADE_AVALIADA == input$avager_unidade) %>% 
          group_by(ANO, AVALIACAO) %>% summarise(TOTAL = n()) %>% 
          mutate(SERIE1 = (TOTAL/sum(TOTAL))*100)
      }else{
        db <- db_avager_av %>% 
          filter(CARGO == input$avager_cargo, QUESTAO == input$avager_questao, UNIDADE_AVALIADA == input$avager_unidade) %>% 
          group_by(ANO, AVALIACAO) %>% summarise(TOTAL = n()) %>% 
          mutate(SERIE1 = (TOTAL/sum(TOTAL))*100)
      }
      db$AVALIACAO <- factor(
        x = db$AVALIACAO, 
        levels = c("Concordo plenamente", "Concordo parcialmente", "Algumas vezes", "Discordo parcialmente", "Discordo totalmente", "Não sei avaliar"))
      db <- db[order(db$AVALIACAO), ]
      db$SERIE1 <- as.numeric(format(x = db$SERIE1, digits = 2, nsmall = 2))
      db
    })
    output$plot30 <- renderHighchart({
      highchart_new(data = dbplot30(),  
                    series = c(SERIE1 = "% Avaliadores"),
                    subtitle = paste(input$avager_unidade, unique(dbplot30()$ANO)),
                    categories = as.character(dbplot30()$AVALIACAO),
                    credits = "Googledrive",
                    input_plot = c( 
                      typeplot = input$typeplot30, 
                      dimension = input$dimension30, 
                      groupplot = input$groupplot30))
    })

  }
shinyApp(ui,server)

