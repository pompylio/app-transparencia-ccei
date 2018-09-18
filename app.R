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
# Functions
source("function/function.R",encoding = "UTF-8")
# Data
import_data(pasta = "data/", extensao = "rds")
options(highcharter.lang = hcoptslang())
# SHINY UI --------------------------------------------------------------------------------------------------------
ui <-
  dashboardPagePlus(
    dashboardHeaderPlus( 
      title = strong("Gestão CCEI"),
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
          startExpanded = FALSE,
          menuSubItem(
            text = strong("Evolução"),
            tabName = "evolucao",
            icon = icon("angle-right")),
          menuSubItem(
            text = strong("Categoria"),
            tabName = "categoria",
            icon = icon("angle-right")),
          menuSubItem(
            text = strong("Comparativo"),
            tabName = "comparativo",
            icon = icon("angle-right"))),
        menuItem(
          text = strong("PESSOAL"),
          tabName = "pessoal",
          icon = icon("group"),
          menuSubItem(
            text = strong("Evolução"),
            tabName = "evolucao_pessoal",
            icon = icon("angle-right")
            ),
          menuSubItem(
            text = strong("Categoria"),
            tabName = "categoria_pessoal",
            icon = icon("angle-right")
          ),
          menuSubItem(
            text = strong("Rotatividade"),
            tabName = "rotatividade",
            icon = icon("angle-right")
            ),
          menuSubItem(
            text = strong("Comparativo"),
            tabName = "comparativo_pessoal",
            icon = icon("angle-right")
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
              width_box = 5,
              status = "warning",
              boxtitle = "Execução da despesa por ano",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = c(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "empty", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "02", # 02 Execução da despesa por mês (acumulado) -----
              width_box = 7,
              status = "warning",
              boxtitle = "Execução da despesa por mês (acumulado)",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = c(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "areaspline", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "03", # 03 Execução da despesa por mês (não acumulado) ----
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
              inputId = "04", # 04 Execução da despesa por grupo de despesa ----
              width_box = 5,
              status = "warning",
              boxtitle = "Execução da despesa por grupo de despesa",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = c(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "05", # 05 Execução da despesa por ação orçamentária ----
              width_box = 7,
              status = "warning",
              boxtitle = "Execução da despesa por ação orçamentária",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = c(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "06", # 06 Execução da despesa por elemento de despesa ----
              width_box = 12,
              status = "warning",
              boxtitle = "Execução da despesa por elemento de despesa",
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
              inputId = "07", # 07 Execução da despesa por unidade gestora ----
              width_box = 7,
              status = "warning",
              boxtitle = "Execução da despesa por unidade gestora",
              menu_selected = c("typeplot", "groupplot", "dimension","department"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D", department = "Grupo de unidades"),
              choices = list(typeplot = "empty", groupplot = "empty", department = c("Com Reitoria","Sem Reitoria")),
              selected = c(typeplot = "column", groupplot = "normal", dimension = "empty", department = "Sem Reitoria")),
            boxnew(
              inputId = "08", # 08 Execução da despesa por campi e reitoria ----
              width_box = 5,
              status = "warning",
              boxtitle = "Execução da despesa por campi e reitoria",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "normal", dimension = "empty"))
          )
        ),
        tabItem(
          tabName = "evolucao_pessoal", 
          fluidRow(
            boxnew(
              inputId = "09", # 09 Evolução do nº de técnicos e docentes por ano ----
              width_box = 6,
              status = "warning",
              boxtitle = "Evolução do nº de técnicos e docentes por ano",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "10", # 10 Evolução do % de técnicos e docentes por ano ----
              width_box = 6,
              status = "warning",
              boxtitle = "Evolução do % de técnicos e docentes por ano",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "normal", dimension = "empty")),
            boxnew(
              inputId = "11", # 11 Evolução do nº de técnicos e docentes por mês ----
              width_box = 12,
              status = "warning",
              boxtitle = "Evolução do número de cargos por mês",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "areaspline", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "23", # 23 Evolução do % de técnicos e docentes por ano ----
              width_box = 12,
              status = "warning",
              boxtitle = "Evolução do % de técnicos e docentes por ano",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "normal", dimension = "empty")))),
        tabItem(
          tabName = "categoria_pessoal", 
          fluidRow(
            boxnew(
              inputId = "12", # 12 Servidores por cargo ----
              width_box = 6,
              status = "warning",
              boxtitle = "Servidores por cargo",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "areaspline", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "13", # 13 Servidores por situação do vínculo ----
              width_box = 6,
              status = "warning",
              boxtitle = "Servidores por situação do vínculo",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "areaspline", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "16", # 16 Classificação dos cargos técnicos ----
              width_box = 6,
              status = "warning",
              boxtitle = "Classificação dos cargos técnicos",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "areaspline", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "17", # 17 Nível de capacitação por classificação dos cargos técnicos ----
              width_box = 6,
              status = "warning",
              boxtitle = "Nível de capacitação por classificação dos cargos técnicos",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "normal", dimension = "empty")),
            boxnew(
              inputId = "14", # 14 Jornada de trabalho dos docentes e técnicos ----
              width_box = 6,
              status = "warning",
              boxtitle = "Jornada de trabalho dos docentes e técnicos",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "3D")),
            boxnew(
              inputId = "18", # 18 Cargos em comissão e funções gratificadas ocupadas ----
              width_box = 6,
              status = "warning",
              boxtitle = "Cargos em comissão e funções gratificadas ocupadas",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty"))
            )
          ),
        tabItem(
          tabName = "rotatividade", 
          fluidRow(
            boxnew(
              inputId = "19", # 19 Admissão e desligamento de docentes e técnicos por ano ----
              width_box = 6,
              status = "warning",
              boxtitle = "Admissão e desligamento de docentes e técnicos por ano",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "areaspline", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "20", # 20 Admissão e desligamento de docentes e técnicos por ano e tipo de cargo ----
              width_box = 6,
              status = "warning",
              boxtitle = "Admissão e desligamento de docentes e técnicos por ano e tipo de cargo",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "spline", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "21", # 21 Admissão e desligamento de prof. temporários e substitutos por ano ----
              width_box = 6,
              status = "warning",
              boxtitle = "Admissão e desligamento de prof. temporários e substitutos por ano",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "areaspline", groupplot = "empty", dimension = "empty")),
            boxnew(
              inputId = "22", # 22 Admissão e desligamento de prof. temporários e substitutos por ano e vínculo ----
              width_box = 6,
              status = "warning",
              boxtitle = "Admissão e desligamento de prof. temporários e substitutos por ano e vínculo",
              menu_selected = c("typeplot", "groupplot", "dimension"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D"),
              choices = list(typeplot = "empty", groupplot = "empty"),
              selected = c(typeplot = "spline", groupplot = "empty", dimension = "empty"))
            )
          )
    )
      ),
    rightSidebar( # Barra lateral direita ----
      background = "dark",
      rightSidebarTabContent(
        id = 1,
        title = "Orçamento", icon = "money",
        selectInput(
          inputId = "unidade_orcamento", 
          label = "Unidade", 
          choices = c("IFB",unique(db_siafi[order(db_siafi$SG_UG),]$SG_UG)), 
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
        materialSwitch(
          inputId = "restoapagar",
          label = "Incluir restos a pagar?",
          value = FALSE,
          status = "primary")
      ),
      rightSidebarTabContent(
        id = 2,
        title = "Pessoal", icon = "group",
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
        id = 3,
        title = "Licenças",
        icon = "file-text-o",
        p(strong("HIGHCHARTS - License holder: ")),
        p("INSTITUTO FEDERAL DE EDUCAÇÃO, CIÊNCIA E TECNOLOGIA DE BRASÍLIA"),
        p("Pompylio Lima"),
        p("This license is valid for educational-institution for the following product(s): Highcharts, Highstock, Highmaps"),
        p("This software is released under Creative Commons Attribution-NonCommercial 3.0, and is available for download at highcharts.com/download. No further activation or license key is required.")
        
      )
    ),title = "Painel CCEI"
  )
# SHINY SERVER ----------------------------------------------------------------------------------------------------
server <-
  function(input, output, session) {

# ORÇAMENTO ---------------------------------------------------------------
    
    # 01 Evolução da despesa por ano ----
    dbplot01 <- reactive({
      if (input$unidade_orcamento == "IFB"){
        db <- db_siafi %>%
          filter(CO_GD %in% c(input$grupodespesa)) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG = "IFB") %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO","RAP_PAGO","RAP_CANCELADO"), .funs = sum)
        }else{
          ano <- unique(substr(db_siafi[db_siafi$SG_UG == input$unidade_orcamento, ]$LANCAMENTO, 1, 4))
          if (input$unidade_orcamento == "CCEI") ano <- ano[!ano == "2016"]
          db <- db_siafi %>%
            filter(CO_GD %in% c(input$grupodespesa), substr(LANCAMENTO, 1, 4) %in% ano, SG_UG == input$unidade_orcamento) %>%
            group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG) %>%
            summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO","RAP_PAGO","RAP_CANCELADO"), .funs = sum)
        }
      db
      })
    output$plot01 <- renderHighchart({
      HighchartOrc(data = dbplot01(), 
                   subtitle = unique(dbplot01()$SG_UG),
                   categories = unique(dbplot01()$ANO),
                   input_plot = c( 
                     rap = input$restoapagar, 
                     typeplot = input$typeplot01, 
                     dimension = input$dimension01, 
                     groupplot = input$groupplot01))
    })
    # 02 Evolução da despesa por mês ----
    dbplot02 <- reactive({
      if (input$unidade_orcamento == "IFB") {
        db <- db_siafi %>%
          filter(CO_GD %in% c(input$grupodespesa), substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento) %>%
          group_by(LANCAMENTO, SG_UG = "IFB") %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO","RAP_PAGO","RAP_CANCELADO"),.funs = sum)
        db$EMPENHADO <- cumsum(db$EMPENHADO)
        db$LIQUIDADO <- cumsum(db$LIQUIDADO)
        db$PAGO <- cumsum(db$PAGO)
        db$RAP_PAGO <- cumsum(db$RAP_PAGO)
        db$RAP_CANCELADO <- cumsum(db$RAP_CANCELADO)
        } else {
        db <- db_siafi %>%
          filter(CO_GD %in% input$grupodespesa, substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento, SG_UG == input$unidade_orcamento) %>%
          group_by(LANCAMENTO, SG_UG) %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO","RAP_PAGO","RAP_CANCELADO"),.funs = sum)
        db$EMPENHADO <- cumsum(db$EMPENHADO)
        db$LIQUIDADO <- cumsum(db$LIQUIDADO)
        db$PAGO <- cumsum(db$PAGO)
        db$RAP_PAGO <- cumsum(db$RAP_PAGO)
        db$RAP_CANCELADO <- cumsum(db$RAP_CANCELADO)
        }
      db
    })
    output$plot02 <- renderHighchart({
      HighchartOrc(data = dbplot02(), 
                   subtitle = unique(paste(dbplot02()$SG_UG, substr(dbplot02()$LANCAMENTO, 1, 4))),
                   categories = dbplot02()$LANCAMENTO,
                   input_plot = c( 
                     rap = input$restoapagar, 
                     typeplot = input$typeplot02, 
                     dimension = input$dimension02, 
                     groupplot = input$groupplot02))
    })
    # 03 Evolução da despesa por mês acumulado ----
    dbplot03 <- reactive({
      if (input$unidade_orcamento == "IFB") {
        db <- db_siafi %>%
          filter(CO_GD %in% input$grupodespesa, substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento) %>%
          group_by(LANCAMENTO, SG_UG = "IFB") %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO","RAP_PAGO","RAP_CANCELADO"),.funs = sum)
        } else{
        db <- db_siafi %>%
          filter(CO_GD %in% input$grupodespesa,substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento,SG_UG == input$unidade_orcamento) %>%
          group_by(LANCAMENTO, SG_UG) %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO","RAP_PAGO","RAP_CANCELADO"),.funs = sum)
      }
      db
    })
    output$plot03 <- renderHighchart({
      HighchartOrc(data = dbplot03(), 
                   subtitle = unique(dbplot03()$SG_UG),
                   categories = dbplot03()$LANCAMENTO,
                   input_plot = c( 
                     rap = input$restoapagar, 
                     typeplot = input$typeplot03, 
                     dimension = input$dimension03, 
                     groupplot = input$groupplot03))
    })
    # 04 Execução da despesa por grupo de despesa ----
    dbplot04 <- reactive({
      if (input$unidade_orcamento == "IFB") {
        db <- db_siafi %>%
          filter(CO_GD %in% input$grupodespesa, substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), CO_GD, SG_UG = "IFB",NO_GD) %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO","RAP_PAGO","RAP_CANCELADO"),.funs = sum)
      } else{
        db <- db_siafi %>%
          filter(CO_GD %in% input$grupodespesa,substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento,SG_UG == input$unidade_orcamento) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG, CO_GD, NO_GD) %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO","RAP_PAGO","RAP_CANCELADO"),.funs = sum)
      }
      db
    })
    output$plot04 <- renderHighchart({
      HighchartOrc(data = dbplot04(), 
                   subtitle = paste(unique(dbplot04()$SG_UG), unique(dbplot04()$ANO)),
                   categories = dbplot04()$NO_GD,
                   input_plot = c( 
                     rap = input$restoapagar, 
                     typeplot = input$typeplot04, 
                     dimension = input$dimension04, 
                     groupplot = input$groupplot04))
    })
    # 05 Execução da despesa por ação orçamentária ----
    dbplot05 <- reactive({
      db <- db_siafi %>% left_join(tb_siafi_acao, by = "CO_ACAO")
      if (input$unidade_orcamento == "IFB") {
        db <- db %>%
          filter(CO_GD %in% input$grupodespesa, substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4),CO_GD, SG_UG = "IFB", NO_ACAO_RESUMO) %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO", "RAP_PAGO", "RAP_CANCELADO"), .funs = sum)
      } else{
        db <- db %>%
          filter(CO_GD %in% input$grupodespesa, substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento, SG_UG == input$unidade_orcamento
          ) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG, CO_ACAO, NO_ACAO_RESUMO) %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO", "RAP_PAGO", "RAP_CANCELADO"), .funs = sum)
      }
      db
    })
    output$plot05 <- renderHighchart({
      HighchartOrc(data = dbplot05(), 
                   subtitle = paste(unique(dbplot05()$SG_UG), unique(dbplot05()$ANO)),
                   categories = dbplot05()$NO_ACAO_RESUMO,
                   input_plot = c( 
                     rap = input$restoapagar, 
                     typeplot = input$typeplot05, 
                     dimension = input$dimension05, 
                     groupplot = input$groupplot05))
    })
    # 06 Execução da despesa por elemento da despesa ----
    dbplot06 <- reactive({
      if (input$unidade_orcamento == "IFB") {
        db <- db_siafi %>%
          filter(CO_GD %in% input$grupodespesa, substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG = "IFB", NO_ED) %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO", "RAP_PAGO", "RAP_CANCELADO"), .funs = sum)
      } else{
        db <- db_siafi %>%
          filter(CO_GD %in% input$grupodespesa, substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento, SG_UG == input$unidade_orcamento) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG, NO_ED) %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO", "RAP_PAGO", "RAP_CANCELADO"), .funs = sum)
      }
      db
    })
    output$plot06 <- renderHighchart({
      HighchartOrc(data = dbplot06(), 
                   subtitle = paste(unique(dbplot06()$SG_UG), unique(dbplot06()$ANO)),
                   categories = dbplot06()$NO_ED,
                   input_plot = c( 
                     rap = input$restoapagar, 
                     typeplot = input$typeplot06, 
                     dimension = input$dimension06, 
                     groupplot = input$groupplot06))
    })
    
    # 07 Execução da despesa por unidade gestora ----
    dbplot07 <- reactive({
      if(input$department07 == "Sem Reitoria") {
        ug <- unique(db_siafi[!db_siafi$SG_UG == "REITORIA", ]$SG_UG)
      } else{
        ug <- unique(db_siafi$SG_UG)
      }
      db <- db_siafi %>%
        filter(CO_GD %in% input$grupodespesa, substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento, SG_UG %in% ug) %>%
        group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG) %>%
        summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO", "RAP_PAGO", "RAP_CANCELADO"), .funs = sum
        )
    })
    output$plot07 <- renderHighchart({
      HighchartOrc(data = dbplot07(), 
                   subtitle = paste(input$unidade_orcamento, unique(dbplot07()$ANO)),
                   categories = dbplot07()$SG_UG,
                   input_plot = c( 
                     rap = input$restoapagar, 
                     typeplot = input$typeplot07, 
                     dimension = input$dimension07, 
                     groupplot = input$groupplot07))
    })
    # 08 Execução da despesa campi e reitoria ----
    dbplot08 <- reactive({
      db <- db_siafi %>%
        filter(CO_GD %in% input$grupodespesa,
               substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento,
               SG_UG == "REITORIA") %>%
        group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG) %>%
        summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO", "RAP_PAGO", "RAP_CANCELADO"),
                     .funs = sum)
      db1 <- db_siafi %>%
        filter(CO_GD %in% input$grupodespesa,
               substr(LANCAMENTO, 1, 4) == input$exercicio_orcamento,
               !SG_UG == "REITORIA") %>%
        group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG = "CAMPI") %>%
        summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO", "RAP_PAGO", "RAP_CANCELADO"),
                     .funs = sum)
      db <- bind_rows(db, db1)
      db
    })
    output$plot08 <- renderHighchart({
      HighchartOrc(data = dbplot08(), 
                   subtitle = unique(dbplot08()$ANO),
                   categories = dbplot08()$SG_UG,
                   input_plot = c( 
                     rap = input$restoapagar, 
                     typeplot = input$typeplot08, 
                     dimension = input$dimension08, 
                     groupplot = input$groupplot08))
    })

# PESSOAL -----------------------------------------------------------------
    # 09 Evolução do número de cargos por ano ----
    dbplot09 <- reactive({
      last <- max(sp_quadro$REFERENCIA)
      if (input$unidade_pessoal == "IFB"){
        db <- sp_quadro %>% 
          filter(REFERENCIA == last | grepl("*12$", REFERENCIA)) %>% 
          group_by(REFERENCIA, UORG_LOTACAO_GRUPO = "IFB") %>% 
          summarise_at(.vars = c("DOCENTE", "TECNICO", "TOTAL"), .funs = sum)
      } else {
        db <- sp_quadro %>% 
          filter(UORG_LOTACAO_GRUPO == input$unidade_pessoal, REFERENCIA == last | grepl("*12$", REFERENCIA)) %>% 
          group_by(REFERENCIA, UORG_LOTACAO_GRUPO) %>% 
          summarise_at(.vars = c("DOCENTE", "TECNICO", "TOTAL"), .funs = sum)
      }
      db$REFERENCIA <- substr(db$REFERENCIA, 1, 4)
      db
    })
    output$plot09 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = unique(dbplot09()$UORG_LOTACAO_GRUPO)) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = ""), categories = dbplot09()$REFERENCIA) %>%
        hc_exporting(enabled = TRUE) %>% 
        hc_chart(type = input$typeplot09) %>% 
        hc_add_series(name = "TOTAL", data = dbplot09()$TOTAL) %>%
        hc_add_series(name = "DOCENTES", data = dbplot09()$DOCENTE) %>%
        hc_add_series(name = "TECNICOS", data = dbplot09()$TECNICO)
      if(input$dimension09 == TRUE){
        hc <- hc %>% 
          hc_chart(options3d = list(enabled = input$dimension09, beta = 10, alpha = 10))
      }
      if (input$groupplot09 != FALSE) {
        hc <- hc %>% 
          hc_plotOptions(series = list(stacking = input$groupplot09))
      }
      hc
    })
    # 10 Evolução do percentual de técnicos e docentes por ano ----
    dbplot10 <- reactive({
      last <- max(sp_quadro$REFERENCIA)
      if (input$unidade_pessoal == "IFB"){
        db <- sp_quadro %>% 
          filter(REFERENCIA == last | grepl("*12$", REFERENCIA)) %>% 
          group_by(REFERENCIA, UORG_LOTACAO_GRUPO = "IFB") %>% 
          summarise_at(.vars = c("DOCENTE", "TECNICO", "TOTAL"), .funs = sum)
        db$DOCENTE <- (db$DOCENTE/db$TOTAL)*100
        db$TECNICO <- (db$TECNICO/db$TOTAL)*100
      } else {
        db <- sp_quadro %>% 
          filter(UORG_LOTACAO_GRUPO == input$unidade_pessoal, REFERENCIA == last | grepl("*12$", REFERENCIA)) %>% 
          group_by(REFERENCIA, UORG_LOTACAO_GRUPO) %>% 
          summarise_at(.vars = c("DOCENTE", "TECNICO", "TOTAL"), .funs = sum)
        db$DOCENTE <- (db$DOCENTE/db$TOTAL)*100
        db$TECNICO <- (db$TECNICO/db$TOTAL)*100
      }
      db$REFERENCIA <- substr(db$REFERENCIA, 1, 4)
      db
    })
    output$plot10 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = unique(dbplot10()$UORG_LOTACAO_GRUPO)) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = ""), categories = dbplot10()$REFERENCIA) %>%
        hc_exporting(enabled = TRUE) %>% 
        hc_chart(type = input$typeplot10) %>% 
        hc_add_series(name = "DOCENTES", data = dbplot10()$DOCENTE) %>%
        hc_add_series(name = "TECNICOS", data = dbplot10()$TECNICO)
      if(input$dimension10 == TRUE){
        hc <- hc %>% 
          hc_chart(options3d = list(enabled = input$dimension10, beta = 10, alpha = 10))
      }
      if (input$groupplot10 != FALSE) {
        hc <- hc %>% 
          hc_plotOptions(series = list(stacking = input$groupplot10))
      }
      hc
    })
    # 11 Evolução do número de cargos por mês ----
    dbplot11 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        db <- sp_quadro %>% 
          group_by(REFERENCIA, UORG_LOTACAO_GRUPO = "IFB") %>% 
          summarise_at(.vars = c("DOCENTE", "TECNICO", "TOTAL"), .funs = sum)
      } else {
        db <- sp_quadro %>% 
          filter(UORG_LOTACAO_GRUPO == input$unidade_pessoal) %>% 
          group_by(REFERENCIA, UORG_LOTACAO_GRUPO) %>% 
          summarise_at(.vars = c("DOCENTE", "TECNICO", "TOTAL"), .funs = sum)
      }
      db
    })
    output$plot11 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = unique(dbplot11()$UORG_LOTACAO_GRUPO)) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = ""), categories = dbplot11()$REFERENCIA) %>%
        hc_exporting(enabled = TRUE) %>% 
        hc_chart(type = input$typeplot11) %>% 
        hc_add_series(name = "TOTAL", data = dbplot11()$TOTAL) %>%
        hc_add_series(name = "DOCENTES", data = dbplot11()$DOCENTE) %>%
        hc_add_series(name = "TECNICOS", data = dbplot11()$TECNICO)
      if(input$dimension11 == TRUE){
        hc <- hc %>% 
          hc_chart(options3d = list(enabled = input$dimension11, beta = 10, alpha = 10))
      }
      if (input$groupplot11 != FALSE) {
        hc <- hc %>% 
          hc_plotOptions(series = list(stacking = input$groupplot11))
      }
      hc
    })
    # 23 Evolução do % de técnicos e docentes por mês ----
    dbplot23 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        db <- sp_quadro %>% 
          group_by(MES = substr(REFERENCIA, 1, 6), UORG_LOTACAO_GRUPO = "IFB") %>% 
          summarise_at(.vars = c("DOCENTE", "TECNICO", "TOTAL"), .funs = sum)
        db$DOCENTE <- (db$DOCENTE/db$TOTAL)*100
        db$TECNICO <- (db$TECNICO/db$TOTAL)*100
      } else {
        db <- sp_quadro %>% 
          filter(UORG_LOTACAO_GRUPO == input$unidade_pessoal) %>% 
          group_by(MES = substr(REFERENCIA, 1, 6), UORG_LOTACAO_GRUPO) %>% 
          summarise_at(.vars = c("DOCENTE", "TECNICO", "TOTAL"), .funs = sum)
        db$DOCENTE <- (db$DOCENTE/db$TOTAL)*100
        db$TECNICO <- (db$TECNICO/db$TOTAL)*100
      }
      db
    })
    output$plot23 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = unique(dbplot23()$UORG_LOTACAO_GRUPO)) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = ""), categories = dbplot23()$MES) %>%
        hc_exporting(enabled = TRUE) %>% 
        hc_chart(type = input$typeplot23) %>% 
        hc_add_series(name = "DOCENTES", data = dbplot23()$DOCENTE) %>%
        hc_add_series(name = "TECNICOS", data = dbplot23()$TECNICO)
      if(input$dimension23 == TRUE){
        hc <- hc %>% 
          hc_chart(options3d = list(enabled = input$dimension23, beta = 23, alpha = 23))
      }
      if (input$groupplot23 != FALSE) {
        hc <- hc %>% 
          hc_plotOptions(series = list(stacking = input$groupplot23))
      }
      hc
    })
    # 12 Servidores por cargo ----
    dbplot12 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        db <- sp_cargo %>% 
          filter(REFERENCIA == max(sp_cargo$REFERENCIA)) %>% 
          group_by(DESCRICAO_CARGO) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum) %>% 
          left_join(tb_siape_cargos, by = "DESCRICAO_CARGO")
        db$UORG_LOTACAO_GRUPO <- "IFB"
      } else {
        db <- sp_cargo %>% 
          filter(REFERENCIA == max(sp_cargo$REFERENCIA), UORG_LOTACAO_GRUPO == input$unidade_pessoal) %>% 
          group_by(UORG_LOTACAO_GRUPO, DESCRICAO_CARGO) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum) %>% 
          left_join(tb_siape_cargos, by = "DESCRICAO_CARGO")
      }
      db <- db[order(db$TOTAL), ]
      db
    })
    output$plot12 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = unique(dbplot12()$UORG_LOTACAO_GRUPO)) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>% 
        hc_chart(type = 'pie', options3d = list(enabled = TRUE, alpha = 45)) %>% 
        hc_plotOptions(pie = list(innerSize = 100, depth = 45)) %>% 
        hc_add_series_labels_values(
          name = "Total",
          labels = dbplot12()$DESCRICAO_CARGO_SIGLA,
          values = dbplot12()$TOTAL)
      hc
    })
    # 13 Servidores por situação ----
    dbplot13 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        db <- db_siape %>% 
          filter(REFERENCIA == max(db_siape$REFERENCIA), SIGLA_FUNCAO == "-1", COD_ORG_LOTACAO == "26428") %>% 
          group_by(SITUACAO_VINCULO) %>% 
          summarise(TOTAL = n())
      } else {
        db <- db_siape %>% 
          filter(REFERENCIA == max(db_siape$REFERENCIA), SIGLA_FUNCAO == "-1", COD_ORG_LOTACAO == "26428", UORG_LOTACAO_GRUPO == input$unidade_pessoal) %>% 
          group_by(SITUACAO_VINCULO) %>% 
          summarise(TOTAL = n())
      }
      db <- db[order(db$TOTAL), ]
      db
    })
    output$plot13 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = unique(dbplot13()$UORG_LOTACAO_GRUPO)) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>% 
        hc_chart(type = 'pie', options3d = list(enabled = TRUE, alpha = 45)) %>% 
        hc_plotOptions(pie = list(innerSize = 100, depth = 45)) %>% 
        hc_add_series_labels_values(
          name = "Total",
          labels = dbplot13()$SITUACAO_VINCULO,
          values = dbplot13()$TOTAL)
      hc
    })
    # 14 Jornada de trabalho dos técnicos ----
    dbplot14 <- reactive({
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
      db <- spread(data = db, key = JORNADA_DE_TRABALHO, value = TOTAL, fill = 0)
      db
    })
    output$plot14 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = unique(dbplot14()$UORG_LOTACAO_GRUPO)) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = ""), categories = dbplot14()$TIPO_CARGO) %>%
        hc_exporting(enabled = TRUE) %>% 
        hc_chart(type = input$typeplot14) %>% 
        hc_add_series(name = "20 HORAS", data = dbplot14()$`20 HORAS SEMANAIS`) %>%
        hc_add_series(name = "25 HORAS", data = dbplot14()$`25 HORAS SEMANAIS`) %>%
        hc_add_series(name = "30 HORAS", data = dbplot14()$`30 HORAS SEMANAIS`) %>%
        hc_add_series(name = "40 HORAS", data = dbplot14()$`40 HORAS SEMANAIS`) %>% 
        hc_add_series(name = "DEDICAÇÃO EXCLUSIVA", data = dbplot14()$`DEDICACAO EXCLUSIVA`)
      if(input$dimension14 == TRUE){
        hc <- hc %>% 
          hc_chart(options3d = list(enabled = input$dimension14, beta = 10, alpha = 10))
      }
      if (input$groupplot14 != FALSE) {
        hc <- hc %>% 
          hc_plotOptions(series = list(stacking = input$groupplot14))
      }
      hc
    })
    
    # 16 Servidores técnicos por classe ----
    dbplot16 <- reactive({
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
    output$plot16 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = unique(dbplot16()$UORG_LOTACAO_GRUPO)) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>% 
        hc_chart(type = 'pie', options3d = list(enabled = TRUE, alpha = 45)) %>% 
        hc_plotOptions(pie = list(innerSize = 100, depth = 45)) %>% 
        hc_add_series_labels_values(
          name = "Total",
          labels = dbplot16()$CLASSE_CARGO,
          values = dbplot16()$TOTAL)
      hc
    })
    # 17 Servidores técnicos por padrao ----
    dbplot17 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        db <- db_siape %>% 
          filter(REFERENCIA == max(REFERENCIA), SIGLA_FUNCAO == "-1", COD_ORG_LOTACAO == "26428", !grepl("\\<PROF", DESCRICAO_CARGO)) %>% 
          group_by(CLASSE_CARGO, PADRAO_CARGO = substr(PADRAO_CARGO, 1, 1)) %>% 
          summarise(TOTAL = n())
      } else {
        db <- db_siape %>% 
          filter(REFERENCIA == max(REFERENCIA), SIGLA_FUNCAO == "-1", COD_ORG_LOTACAO == "26428", UORG_LOTACAO_GRUPO == input$unidade_pessoal, !grepl("\\<PROF", DESCRICAO_CARGO)) %>% 
          group_by(CLASSE_CARGO, PADRAO_CARGO = substr(PADRAO_CARGO, 1, 1)) %>% 
          summarise(TOTAL = n())
      }
      db
    })
    output$plot17 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = unique(dbplot17()$UORG_LOTACAO_GRUPO)) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = ""), categories = unique(dbplot17()$CLASSE_CARGO)) %>%
        hc_exporting(enabled = TRUE) %>% 
        hc_chart(type = input$typeplot17) %>% 
        hc_add_series(name = "Capacitação I",   data = dbplot17()[dbplot17()$PADRAO_CARGO == "1", ]$TOTAL) %>%
        hc_add_series(name = "Capacitação II",  data = dbplot17()[dbplot17()$PADRAO_CARGO == "2", ]$TOTAL) %>%
        hc_add_series(name = "Capacitação III", data = dbplot17()[dbplot17()$PADRAO_CARGO == "3", ]$TOTAL) %>%
        hc_add_series(name = "Capacitação IV",  data = dbplot17()[dbplot17()$PADRAO_CARGO == "4", ]$TOTAL)
      if(input$dimension17 == TRUE){
        hc <- hc %>% 
          hc_chart(options3d = list(enabled = input$dimension17, beta = 10, alpha = 10))
      }
      if (input$groupplot17 != FALSE) {
        hc <- hc %>% 
          hc_plotOptions(series = list(stacking = input$groupplot17))
      }
      hc
    })
    # 18 Cargos em comissão e funções gratificadas ----
    dbplot18 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        db <- db_siape %>% 
          filter(REFERENCIA == max(REFERENCIA), !SIGLA_FUNCAO == "-1", COD_ORG_EXERCICIO == "26428") %>% 
          group_by(FUNCAO = paste(SIGLA_FUNCAO, NIVEL_FUNCAO)) %>% 
          summarise(TOTAL = n())
      } else {
        db <- db_siape %>% 
          filter(REFERENCIA == max(REFERENCIA), !SIGLA_FUNCAO == "-1", COD_ORG_EXERCICIO == "26428", UORG_LOTACAO_GRUPO == input$unidade_pessoal) %>% 
          group_by(FUNCAO = paste(SIGLA_FUNCAO, NIVEL_FUNCAO)) %>% 
          summarise(TOTAL = n())
      }
      db
    })
    output$plot18 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = unique(dbplot18()$UORG_LOTACAO_GRUPO)) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = ""), categories = dbplot18()$FUNCAO) %>%
        hc_exporting(enabled = TRUE) %>% 
        hc_chart(type = input$typeplot18) %>% 
        hc_add_series(name = "Total",   data = dbplot18()$TOTAL)
      if(input$dimension18 == TRUE){
        hc <- hc %>% 
          hc_chart(options3d = list(enabled = input$dimension18, beta = 10, alpha = 10))
      }
      if (input$groupplot18 != FALSE) {
        hc <- hc %>% 
          hc_plotOptions(series = list(stacking = input$groupplot18))
      }
      hc
    })
    # 19 Admissão e desligamento de docentes e técnicos por ano ----
    dbplot19 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        db <- sp_rot_efetivo %>% 
          group_by(REFERENCIA = substr(FIM, 1, 4), TIPO) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum)
        db$UORG_LOTACAO_GRUPO == "IFB"
      } else {
        db <- sp_rot_unidade %>% 
          filter(UORG_LOTACAO_GRUPO == input$unidade_pessoal) %>% 
          group_by(UORG_LOTACAO_GRUPO, REFERENCIA = substr(FIM, 1, 4), TIPO) %>%  
          summarise_at(.vars = "TOTAL", .funs = sum)
      }
      db <- spread(data = db, key = TIPO, value = TOTAL, fill = 0)
      db
    })
    output$plot19 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = unique(dbplot19()$UORG_LOTACAO_GRUPO)) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = ""), categories = dbplot19()$REFERENCIA) %>%
        hc_exporting(enabled = TRUE) %>% 
        hc_chart(type = input$typeplot19) %>% 
        hc_add_series(name = "ADMISSÃO",     data = dbplot19()$ADMISSAO) %>% 
        hc_add_series(name = "DESLIGAMENTO", data = dbplot19()$DESLIGAMENTO) 
      if (input$dimension19 == TRUE){
        hc <- hc %>% 
          hc_chart(options3d = list(enabled = input$dimension19, beta = 10, alpha = 10))
      }
      if (input$groupplot19 != FALSE) {
        hc <- hc %>% 
          hc_plotOptions(series = list(stacking = input$groupplot19))
      }
      hc
    })
    # 20 Admissão e desligamento de docentes e técnicos por ano e tipo de cargo ----
    dbplot20 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        db <- sp_rot_efetivo %>% mutate(TIPO_CARGO = ifelse(grepl("\\<PROF", DESCRICAO_CARGO), "DOCENTE", "TECNICO"))
        db1 <- db %>% 
          filter(TIPO_CARGO == "DOCENTE") %>% 
          group_by(REFERENCIA = substr(FIM, 1, 4),
                   TIPO) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum)
        db1 <- spread(data = db1, key = TIPO, value = TOTAL, fill = 0)
        colnames(db1) <- c("REFERENCIA", "ADMISSAO_DOCENTE", "DESLIGAMENTO_DOCENTE")
        db2 <- db %>% 
          filter(TIPO_CARGO == "TECNICO") %>% 
          group_by(REFERENCIA = substr(FIM, 1, 4),
                   TIPO) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum)
        db2 <- spread(data = db2, key = TIPO, value = TOTAL, fill = 0)
        colnames(db2) <- c("REFERENCIA", "ADMISSAO_TECNICO", "DESLIGAMENTO_TECNICO")
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
        colnames(db1) <- c("REFERENCIA", "ADMISSAO_DOCENTE", "DESLIGAMENTO_DOCENTE")
        db2 <- db %>% 
          filter(UORG_LOTACAO_GRUPO == input$unidade_pessoal, TIPO_CARGO == "TECNICO") %>% 
          group_by(REFERENCIA = substr(FIM, 1, 4),
                   TIPO) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum)
        db2 <- spread(data = db2, key = TIPO, value = TOTAL, fill = 0)
        colnames(db2) <- c("REFERENCIA", "ADMISSAO_TECNICO", "DESLIGAMENTO_TECNICO")
        db2$UORG_LOTACAO_GRUPO <- "IFB"
        db <- left_join(db1, db2, by = "REFERENCIA")
      }
      db
    })
    output$plot20 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = unique(dbplot20()$UORG_LOTACAO_GRUPO)) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = ""), categories = dbplot20()$REFERENCIA) %>%
        hc_exporting(enabled = TRUE) %>% 
        hc_chart(type = input$typeplot20) %>% 
        hc_add_series(name = "ADMIS DOCENTE",  data = dbplot20()$ADMISSAO_DOCENTE) %>% 
        hc_add_series(name = "ADMIS TÉCNICO", data = dbplot20()$ADMISSAO_TECNICO) %>% 
        hc_add_series(name = "DESLI DOCENTE", data = dbplot20()$DESLIGAMENTO_DOCENTE) %>% 
        hc_add_series(name = "DESLI TÉCNICO", data = dbplot20()$DESLIGAMENTO_TECNICO)
      if (input$dimension20 == TRUE){
        hc <- hc %>% 
          hc_chart(options3d = list(enabled = input$dimension20, beta = 10, alpha = 10))
      }
      if (input$groupplot20 != FALSE) {
        hc <- hc %>% 
          hc_plotOptions(series = list(stacking = input$groupplot20))
      }
      hc
    })
    # 21 Admissão e desligamento de prof. temporários e substitutos por ano ----
    dbplot21 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        db <- sp_rot_vinculo %>% 
          filter(grepl("\\<CONT.PROF", SITUACAO_VINCULO)) %>% 
          group_by(REFERENCIA = substr(FIM, 1, 4), TIPO) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum)
        db$UORG_LOTACAO_GRUPO == "IFB"
      } else {
        db <- sp_rot_vinculo %>% 
          filter(grepl("\\<CONT.PROF", SITUACAO_VINCULO), UORG_LOTACAO_GRUPO == input$unidade_pessoal) %>% 
          group_by(UORG_LOTACAO_GRUPO, REFERENCIA = substr(FIM, 1, 4), TIPO) %>%  
          summarise_at(.vars = "TOTAL", .funs = sum)
      }
      db <- spread(data = db, key = TIPO, value = TOTAL, fill = 0)
      db
    })
    output$plot21 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = unique(dbplot21()$UORG_LOTACAO_GRUPO)) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = ""), categories = dbplot21()$REFERENCIA) %>%
        hc_exporting(enabled = TRUE) %>% 
        hc_chart(type = input$typeplot21) %>% 
        hc_add_series(name = "ADMISSÃO",     data = dbplot21()$ADMISSAO) %>% 
        hc_add_series(name = "DESLIGAMENTO", data = dbplot21()$DESLIGAMENTO) 
      if (input$dimension21 == TRUE){
        hc <- hc %>% 
          hc_chart(options3d = list(enabled = input$dimension21, beta = 10, alpha = 10))
      }
      if (input$groupplot21 != FALSE) {
        hc <- hc %>% 
          hc_plotOptions(series = list(stacking = input$groupplot21))
      }
      hc
    })
    # 22 Admissão e desligamento de prof. temporários e substitutos por ano e vínculo ----
    dbplot22 <- reactive({
      if (input$unidade_pessoal == "IFB"){
        db <- sp_rot_vinculo %>% filter(grepl("\\<CONT.PROF", SITUACAO_VINCULO)) %>% mutate(VINCULO = ifelse(grepl("TEMPORARIO$", SITUACAO_VINCULO), "TEMPORARIO", "SUBSTITUTO"))
        db1 <- db %>% 
          filter(VINCULO == "TEMPORARIO") %>% 
          group_by(REFERENCIA = substr(FIM, 1, 4),
                   TIPO) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum)
        db1 <- spread(data = db1, key = TIPO, value = TOTAL, fill = 0)
        colnames(db1) <- c("REFERENCIA", "ADMISSAO_TEMPORARIO", "DESLIGAMENTO_TEMPORARIO")
        db2 <- db %>% 
          filter(VINCULO == "SUBSTITUTO") %>% 
          group_by(REFERENCIA = substr(FIM, 1, 4),
                   TIPO) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum)
        db2 <- spread(data = db2, key = TIPO, value = TOTAL, fill = 0)
        colnames(db2) <- c("REFERENCIA", "ADMISSAO_SUBSTITUTO", "DESLIGAMENTO_SUBSTITUTO")
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
        colnames(db1) <- c("REFERENCIA", "ADMISSAO_TEMPORARIO", "DESLIGAMENTO_TEMPORARIO")
        db2 <- db %>% 
          filter(UORG_LOTACAO_GRUPO == input$unidade_pessoal, VINCULO == "SUBSTITUTO") %>% 
          group_by(REFERENCIA = substr(FIM, 1, 4),
                   TIPO) %>% 
          summarise_at(.vars = "TOTAL", .funs = sum)
        db2 <- spread(data = db2, key = TIPO, value = TOTAL, fill = 0)
        colnames(db2) <- c("REFERENCIA", "ADMISSAO_SUBSTITUTO", "DESLIGAMENTO_SUBSTITUTO")
        db2$UORG_LOTACAO_GRUPO <- "IFB"
        db <- left_join(db1, db2, by = "REFERENCIA")
      }
      db
    })
    output$plot22 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = unique(dbplot22()$UORG_LOTACAO_GRUPO)) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = ""), categories = dbplot22()$REFERENCIA) %>%
        hc_exporting(enabled = TRUE) %>% 
        hc_chart(type = input$typeplot22) %>% 
        hc_add_series(name = "ADMIS TEMPORÁRIO",  data = dbplot22()$ADMISSAO_TEMPORARIO) %>% 
        hc_add_series(name = "ADMIS SUBSTITUTO", data = dbplot22()$ADMISSAO_SUBSTITUTO) %>% 
        hc_add_series(name = "DESLI TEMPORÁRIO", data = dbplot22()$DESLIGAMENTO_TEMPORARIO) %>% 
        hc_add_series(name = "DESLI SUBSTITUTO", data = dbplot22()$DESLIGAMENTO_SUBSTITUTO)
      if (input$dimension22 == TRUE){
        hc <- hc %>% 
          hc_chart(options3d = list(enabled = input$dimension22, beta = 10, alpha = 10))
      }
      if (input$groupplot22 != FALSE) {
        hc <- hc %>% 
          hc_plotOptions(series = list(stacking = input$groupplot22))
      }
      hc
    })
    # 24 
  }
shinyApp(ui,server)

