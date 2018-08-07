# LIBRARY ---------------------------------------------------------------------------------------------------------
suppressMessages(require(shiny))
suppressMessages(require(shinydashboard))
suppressMessages(require(shinydashboardPlus))
suppressMessages(require(shinyWidgets))
suppressMessages(require(shinyjs))
suppressMessages(require(tidyverse))
suppressMessages(require(highcharter))
suppressMessages(require(reshape2))
suppressMessages(library(shinycssloaders))
# FUNCTIONS -------------------------------------------------------------------------------------------------------
source("function/function.R",encoding = "UTF-8")
boxnew <- function(inputId,boxtitle,menu_selected,label,choices,selected,status,width_box,plot_highchart){
  year <- unique(substr(des_evo[order(des_evo$LANCAMENTO,decreasing = T),]$LANCAMENTO,1,4))
  yearmonth <- unique(des_evo[order(des_evo$LANCAMENTO,decreasing = T),]$LANCAMENTO)
  department <- c("IFB",unique(des_evo[order(des_evo$SG_UG),]$SG_UG))
  if(missing(menu_selected)){stop()}
  if(any(menu_selected == "typeplot")){
    typeplot <- shinyWidgets::radioGroupButtons(
      inputId = paste0("typeplot",inputId),
      label = if(label["typeplot"]=="empty"){NULL}else{label["typeplot"]},
      choices = if(any(choices[["typeplot"]]=="empty")){c(`<i class='fa fa-bar-chart'></i>`="column",`<i class='fa fa-align-left'></i>`="bar",`<i class='fa fa-line-chart'></i>`="spline",`<i class='fa fa-area-chart'></i>`="areaspline")}else{choices[["typeplot"]]},
      selected = if(selected["typeplot"]=="empty"){"column"}else{selected["typeplot"]})}
  if(any(menu_selected == "groupplot")){
    groupplot <- shinyWidgets::radioGroupButtons(
      inputId = paste0("groupplot",inputId),
      label =  if(label["groupplot"]=="empty"){NULL}else{label["groupplot"]},
      choices = if(any(choices[["groupplot"]]=="empty")){c(`<i class='fa fa-square-o'></i>`=FALSE,`<i class='fa fa-object-ungroup'></i>`="normal",`<i class='fa fa-object-group'></i>`="percent")}else{choices[["groupplot"]]},
      selected = if(selected["groupplot"]=="empty"){FALSE}else{selected["groupplot"]},
      width = "100%")}
  if(any(menu_selected == "dimension")){
    dimension <- shinyWidgets::materialSwitch(
      inputId = paste0("dimension",inputId), 
      label = if(label["dimension"]=="empty"){FALSE}else{label["dimension"]}, 
      status = status,
      value = if(selected["dimension"]=="empty"){FALSE}else{TRUE})}
  if(any(menu_selected == "year")){
    year <- shiny::selectInput(
      inputId = paste0("year",inputId), 
      label = if(label["year"]=="empty"){NULL}else{label["year"]},
      choices = if(any(choices[["year"]]=="empty")){year}else{choices[["year"]]},
      selected = if(selected["year"]=="empty"){year[1]}else{selected["year"]})}
  if(any(menu_selected == "yearmonth")){
    yearmonth <- shiny::selectInput(
      inputId = paste0("yearmonth",inputId), 
      label = if(label["yearmonth"]=="empty"){NULL}else{label["yearmonth"]},
      choices = if(any(choices[["yearmonth"]]=="empty")){yearmonth}else{choices[["yearmonth"]]},
      selected = if(selected["yearmonth"]=="empty"){yearmonth[1]}else{selected["yearmonth"]})}
  if(any(menu_selected == "department")){
    department <- shiny::selectInput(
      inputId = paste0("department",inputId), 
      label = if(label["department"]=="empty"){NULL}else{label["department"]},
      choices = if(any(choices[["department"]]=="empty")){department}else{choices[["department"]]},
      selected = if(selected["department"]=="empty"){department}else{selected["department"]})}
  if(any(menu_selected == "selectX")){
    selectX <- shiny::selectInput(
      inputId = paste0("selectX",inputId), 
      label = if(label["selectX"]=="empty"){NULL}else{label["selectX"]},
      choices = if(any(choices[["selectX"]]=="empty")){"X"}else{choices[["selectX"]]},
      selected = if(selected["selectX"]=="empty"){"X"}else{selected["selectX"]})}
  if(any(menu_selected == "selectY")){
    selectY <- shiny::selectInput(
      inputId = paste0("selectY",inputId), 
      label = if(label["selectY"]=="empty"){NULL}else{label["selectY"]},
      choices = if(any(choices[["selectY"]]=="empty")){"Y"}else{choices[["selectY"]]},
      selected = if(selected["selectY"]=="empty"){"Y"}else{selected["selectY"]})}
  boxnew <- 
    shinydashboard::box(
      title = NULL,
      status = status,
      width = if(missing(width_box)){6}else{width_box},
      collapsible = F,
      collapsed = F,
      column(
        width = 12,
        column(
          width = 11,
          h4(boxtitle)),
        column(
          width = 1,
          shinyWidgets::dropdownButton(
            size = "sm",
            circle = FALSE, 
            status = status, 
            icon = icon("filter"), 
            width = "200px",
            right = TRUE,
            tooltip = tooltipOptions(title = "Filtro"),
            if(any(menu_selected == "typeplot")){typeplot}else{NULL},
            if(any(menu_selected == "groupplot")){groupplot}else{NULL},
            if(any(menu_selected == "dimension")){dimension}else{NULL},
            if(any(menu_selected == "year")){year}else{NULL},
            if(any(menu_selected == "yearmonth")){yearmonth}else{NULL},
            if(any(menu_selected == "department")){department}else{NULL},
            if(any(menu_selected == "selectX")){selectX}else{NULL},
            if(any(menu_selected == "selectY")){selectY}else{NULL}))),
    column(
      width = 12,
      shinycssloaders::withSpinner(highchartOutput(paste0("plot",inputId)),type = 8L,color = "#3c8dbc")))
  return(boxnew)
  }
# SHINY DATABASE --------------------------------------------------------------------------------------------------
des_evo <- readRDS("data/despesa_evolucao.rds")
load("data/tb_acao.rda")
hcoptslang <- getOption("highcharter.lang")
hcoptslang$decimalPoint <- ","
hcoptslang$thousandsSep <- "."
hcoptslang$downloadCSV <- "Baixar CSV"
hcoptslang$downloadJPEG <- "Baixar JPEG"
hcoptslang$downloadPDF <- "Baixar PDF"
hcoptslang$downloadPNG <- "Baixar PNG"
hcoptslang$downloadSVG <- "Baixar SVG"
hcoptslang$downloadXLS <- "Baixar XLS"
hcoptslang$loading <- "Carregando ..."
hcoptslang$printChart <- "Imprimir"
hcoptslang$viewData <- "Ver dados"
options(highcharter.lang = hcoptslang)
# SHINY UI --------------------------------------------------------------------------------------------------------
ui <-
  dashboardPagePlus(
    dashboardHeaderPlus(
      title = "Painel de Gestão CCEI",
      titleWidth = 240,
      enable_rightsidebar = TRUE,
      rightSidebarIcon = "gears"), 
    dashboardSidebar(
      width = 240,
      collapsed = FALSE,
      sidebarMenu(
        menuItem(
          text = strong("ORÇAMENTO"),
          tabName = "orcamento",
          icon = icon("money"),
          startExpanded = TRUE,
          menuSubItem(
            text = strong("Evolução"),
            tabName = "evolucao",
            icon = icon("angle-right"),
            selected = TRUE),
          menuSubItem(
            text = strong("Categoria"),
            tabName = "categoria",
            icon = icon("angle-right")),
          menuSubItem(
            text = strong("Comparativo"),
            tabName = "comparativo",
            icon = icon("angle-right")))
        )
      ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "evolucao",
          fluidRow(
            boxnew(
              inputId = "01",
              width_box = 5,
              status = "warning",
              boxtitle = "Execução da Despesa - Ano",
              menu_selected = c("typeplot", "groupplot", "dimension", "department"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D", department = "empty"),
              choices = c(typeplot = "empty", groupplot = "empty", department = "empty"),
              selected = c(typeplot = "empty", groupplot = "empty", dimension = "empty", department = "CCEI")),
            boxnew(
              inputId = "02",
              width_box = 7,
              status = "warning",
              boxtitle = "Execução da Despesa - Mês",
              menu_selected = c("typeplot", "groupplot", "dimension", "year", "department"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D", year = "empty", department = "empty"),
              choices = c(typeplot = "empty", groupplot = "empty", year = "empty", department = "empty"),
              selected = c(typeplot = "areaspline", groupplot = "empty", dimension = "empty", year = "empty", department = "CCEI")),
            boxnew(
              inputId = "03",
              width_box = 12,
              status = "warning",
              boxtitle = "Execução da Despesa - Ano e Mês não acumulado (%)",
              menu_selected = c("typeplot", "groupplot", "dimension", "year", "department"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D", year = "empty", department = "empty"),
              choices = list(typeplot = "empty", groupplot = "empty", year = c("Todos", unique(substr(des_evo[order(des_evo$LANCAMENTO,decreasing = T),]$LANCAMENTO,1,4))), department = "empty"),
              selected = c(typeplot = "spline", groupplot = "empty", dimension = "empty", year = "Todos", department = "CCEI"))
            )
          ),
        tabItem(
          tabName = "categoria",
          fluidRow(
            boxnew(
              inputId = "04",
              width_box = 6,
              status = "warning",
              boxtitle = "Execução da Despesa - Grupo de Despesa",
              menu_selected = c("typeplot", "groupplot", "dimension", "year", "department"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D", year = "empty", department = "empty"),
              choices = c(typeplot = "empty", groupplot = "empty", year = "empty", department = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty", year = "empty", department = "CCEI")),
            boxnew(
              inputId = "05",
              width_box = 6,
              status = "warning",
              boxtitle = "Execução da Despesa - Ação Orçamentária",
              menu_selected = c("typeplot", "groupplot", "dimension", "year", "department"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D", year = "empty", department = "empty"),
              choices = c(typeplot = "empty", groupplot = "empty", year = "empty", department = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty", year = "empty", department = "CCEI")),
            boxnew(
              inputId = "06",
              width_box = 12,
              status = "warning",
              boxtitle = "Execução da Despesa - Elemento de Despesa",
              menu_selected = c("typeplot", "groupplot", "dimension", "year", "department"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D", year = "empty", department = "empty"),
              choices = c(typeplot = "empty", groupplot = "empty", year = "empty", department = "empty"),
              selected = c(typeplot = "column", groupplot = "empty", dimension = "empty", year = "empty", department = "CCEI"))
            )
          ),
        tabItem(
          tabName = "comparativo",
          fluidRow(
            boxnew(
              inputId = "07",
              width_box = 7,
              status = "warning",
              boxtitle = "Execução da Despesa - UG",
              menu_selected = c("typeplot", "groupplot", "dimension", "year", "department"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D", year = "empty", department = "empty"),
              choices = list(typeplot = "empty", groupplot = "empty", year = "empty", department = c("IFB","CAMPI")),
              selected = c(typeplot = "column", groupplot = "normal", dimension = "empty", year = "empty", department = "CAMPI")),
            boxnew(
              inputId = "08",
              width_box = 5,
              status = "warning",
              boxtitle = "Execução da Despesa - Campi e Reitoria",
              menu_selected = c("typeplot", "groupplot", "dimension", "year"),
              label = c(typeplot = "empty", groupplot = "empty", dimension = "3D", year = "empty"),
              choices = list(typeplot = "empty", groupplot = "empty", year = "empty"),
              selected = c(typeplot = "column", groupplot = "normal", dimension = "empty", year = "empty"))
          )
        )
        )
      ),
    rightSidebar(
      background = "dark",
      rightSidebarTabContent(
        id = 1,
        title = "Tab 1",
        icon = "desktop",
        active = TRUE,
        sliderInput(
          "obs",
          "Number of observations:",
          min = 0, max = 1000, value = 500
        )
      ),
      rightSidebarTabContent(
        id = 2,
        title = "Tab 2",
        textInput("caption", "Caption", "Data Summary")
      ),
      rightSidebarTabContent(
        id = 3,
        icon = "paint-brush",
        title = "Tab 3",
        numericInput("obs", "Observations:", 10, min = 1, max = 100)
      )
    ),title = "Painel CCEI"
  )
# SHINY SERVER ----------------------------------------------------------------------------------------------------
server <-
  function(input, output, session) {
    dbplot01 <- reactive({
      if (input$department01 == "IFB"){
        db <- des_evo %>%
          filter(CO_GD %in% c("3", "4")) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG = "IFB") %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO"), .funs = sum)
      }else{
        ano <- unique(substr(des_evo[des_evo$SG_UG == input$department01, ]$LANCAMENTO, 1, 4))
        if (input$department01 == "CCEI") ano <- ano[!ano == "2016"]
        db <- des_evo %>%
          filter(CO_GD %in% c("3", "4"), substr(LANCAMENTO, 1, 4) %in% ano, SG_UG == input$department01) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG) %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO"), .funs = sum)
      }
      db
    })
    output$plot01 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = unique(dbplot01()$SG_UG)) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = ""), categories = dbplot01()$ANO) %>%
        hc_add_series(name = "EMPENHADO", data = dbplot01()$EMPENHADO) %>%
        hc_add_series(name = "LIQUIDADO", data = dbplot01()$LIQUIDADO) %>%
        hc_add_series(name = "PAGO", data = dbplot01()$PAGO) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_chart(
          type = input$typeplot01,
          options3d = list(
            enabled = input$dimension01,
            beta = 10,
            alpha = 10
          )
        )
      if (input$groupplot01 != FALSE) {
        hc <-
          hc %>% hc_plotOptions(series = list(stacking = input$groupplot01))
      }
      hc
    })
    dbplot02 <- reactive({
      if (input$department02 == "IFB") {
        db <- des_evo %>%
          filter(CO_GD %in% c("3", "4"), substr(LANCAMENTO, 1, 4) == input$year02) %>%
          group_by(LANCAMENTO, SG_UG = "IFB") %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO"),
                       .funs = sum)
        db$EMPENHADO <- cumsum(db$EMPENHADO)
        db$LIQUIDADO <- cumsum(db$LIQUIDADO)
        db$PAGO <- cumsum(db$PAGO)
      } else{
        db <- des_evo %>%
          filter(
            CO_GD %in% c("3", "4"),
            substr(LANCAMENTO, 1, 4) == input$year02,
            SG_UG == input$department02
          ) %>%
          group_by(LANCAMENTO, SG_UG) %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO"),
                       .funs = sum)
        db$EMPENHADO <- cumsum(db$EMPENHADO)
        db$LIQUIDADO <- cumsum(db$LIQUIDADO)
        db$PAGO <- cumsum(db$PAGO)
      }
      db
    })
    output$plot02 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = unique(paste(dbplot02()$SG_UG, substr(dbplot02()$LANCAMENTO, 1, 4)))) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_xAxis(categories = dbplot02()$LANCAMENTO) %>%
        hc_add_series(name = "EMPENHADO", data = dbplot02()$EMPENHADO) %>%
        hc_add_series(name = "LIQUIDADO", data = dbplot02()$LIQUIDADO) %>%
        hc_add_series(name = "PAGO", data = dbplot02()$PAGO) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_chart(
          type = input$typeplot02,
          options3d = list(
            enabled = input$dimension02,
            beta = 10,
            alpha = 10
          )
        )
      if (input$groupplot02 != FALSE) {
        hc <-
          hc %>% hc_plotOptions(series = list(stacking = input$groupplot02))
      }
      hc
    })
    dbplot03 <- reactive({
      if(input$year03 == "Todos") {
        ano <- unique(substr(des_evo$LANCAMENTO, 1, 4))
        if (input$department03 == "CCEI") {
          ano <- ano[!ano == "2016"]
        }
      } else{
        ano = input$year03
      }
      if (input$department03 == "IFB") {
        db <- des_evo %>%
          filter(CO_GD %in% c("3", "4"), substr(LANCAMENTO, 1, 4) %in% ano) %>%
          group_by(LANCAMENTO, SG_UG = "IFB") %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO"),
                       .funs = sum)
      } else{
        db <- des_evo %>%
          filter(CO_GD %in% c("3", "4"),
                 substr(LANCAMENTO, 1, 4) %in% ano,
                 SG_UG == input$department03) %>%
          group_by(LANCAMENTO, SG_UG) %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO"),
                       .funs = sum)
      }
      db
    })
    output$plot03 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = unique(dbplot03()$SG_UG)) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_xAxis(categories = dbplot03()$LANCAMENTO) %>%
        hc_add_series(name = "EMPENHADO", data = dbplot03()$EMPENHADO) %>%
        hc_add_series(name = "LIQUIDADO", data = dbplot03()$LIQUIDADO) %>%
        hc_add_series(name = "PAGO", data = dbplot03()$PAGO) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_chart(
          type = input$typeplot03,
          options3d = list(
            enabled = input$dimension03,
            beta = 10,
            alpha = 10
          )
        )
      if (input$groupplot03 != FALSE) {
        hc <-
          hc %>% hc_plotOptions(series = list(stacking = input$groupplot03))
      }
      hc
    })
    
    
# Por Grupo de Despesa ---------------------------------------------------
    dbplot04 <- reactive({
      if (input$department04 == "IFB") {
        db <- des_evo %>%
          filter(CO_GD %in% c("3", "4"), substr(LANCAMENTO, 1, 4) == input$year04) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4),
                   CO_GD,
                   SG_UG = "IFB",
                   NO_GD) %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO"),
                       .funs = sum)
      } else{
        db <- des_evo %>%
          filter(
            CO_GD %in% c("3", "4"),
            substr(LANCAMENTO, 1, 4) == input$year04,
            SG_UG == input$department04
          ) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG, CO_GD, NO_GD) %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO"),
                       .funs = sum)
      }
      db
    })
    output$plot04 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = paste(unique(dbplot04()$SG_UG), unique(dbplot04()$ANO))) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_xAxis(categories = dbplot04()$NO_GD) %>%
        hc_add_series(name = "EMPENHADO", data = dbplot04()$EMPENHADO) %>%
        hc_add_series(name = "LIQUIDADO", data = dbplot04()$LIQUIDADO) %>%
        hc_add_series(name = "PAGO", data = dbplot04()$PAGO) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_chart(
          type = input$typeplot04,
          options3d = list(
            enabled = input$dimension04,
            beta = 10,
            alpha = 10
          )
        )
      if (input$groupplot04 != FALSE) {
        hc <-
          hc %>% hc_plotOptions(series = list(stacking = input$groupplot04))
      }
      hc
    })
    
# Por Ação Orçamentária ---------------------------------------------------
    dbplot05 <- reactive({
      db <- des_evo %>% left_join(tb_acao, by = "CO_ACAO")
      if (input$department03 == "IFB") {
        db <- db %>%
          filter(CO_GD %in% c("3", "4"), substr(LANCAMENTO, 1, 4) == input$year05) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4),
                   CO_GD,
                   SG_UG = "IFB",
                   NO_ACAO_RESUMO) %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO"),
                       .funs = sum)
      } else{
        db <- db %>%
          filter(
            CO_GD %in% c("3", "4"),
            substr(LANCAMENTO, 1, 4) == input$year05,
            SG_UG == input$department05
          ) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG, CO_ACAO, NO_ACAO_RESUMO) %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO"),
                       .funs = sum)
      }
      db
    })
    output$plot05 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = paste(unique(dbplot05()$SG_UG), unique(dbplot05()$ANO))) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_xAxis(categories = dbplot05()$NO_ACAO_RESUMO) %>%
        hc_add_series(name = "EMPENHADO", data = dbplot05()$EMPENHADO) %>%
        hc_add_series(name = "LIQUIDADO", data = dbplot05()$LIQUIDADO) %>%
        hc_add_series(name = "PAGO", data = dbplot05()$PAGO) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_chart(
          type = input$typeplot05,
          options3d = list(
            enabled = input$dimension05,
            beta = 10,
            alpha = 10
          )
        )
      if (input$groupplot05 != FALSE) {
        hc <-
          hc %>% hc_plotOptions(series = list(stacking = input$groupplot05))
      }
      hc
    })

# Por Elemento de Despesa -------------------------------------------------
    dbplot06 <- reactive({
      if (input$department06 == "IFB") {
        db <- des_evo %>%
          filter(CO_GD %in% c("3", "4"), substr(LANCAMENTO, 1, 4) == input$year06) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG = "IFB", NO_ED) %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO"),
                       .funs = sum)
      } else{
        db <- des_evo %>%
          filter(
            CO_GD %in% c("3", "4"),
            substr(LANCAMENTO, 1, 4) == input$year06,
            SG_UG == input$department06
          ) %>%
          group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG, NO_ED) %>%
          summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO"),
                       .funs = sum)
      }
      db
    })
    output$plot06 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = paste(unique(dbplot06()$SG_UG), unique(dbplot06()$ANO))) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_xAxis(categories = dbplot06()$NO_ED) %>%
        hc_add_series(name = "EMPENHADO", data = dbplot06()$EMPENHADO) %>%
        hc_add_series(name = "LIQUIDADO", data = dbplot06()$LIQUIDADO) %>%
        hc_add_series(name = "PAGO", data = dbplot06()$PAGO) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_chart(
          type = input$typeplot06,
          options3d = list(
            enabled = input$dimension06,
            beta = 10,
            alpha = 10
          )
        )
      if (input$groupplot06 != FALSE) {
        hc <-
          hc %>% hc_plotOptions(series = list(stacking = input$groupplot06))
      }
      hc
    })

# Comparativo - Por Execução da Despesa -----------------------------------
    dbplot07 <- reactive({
      if(input$department07 == "CAMPI") {
        ug <- unique(des_evo[!des_evo$SG_UG == "REITORIA", ]$SG_UG)
      } else{
        ug <- unique(des_evo$SG_UG)
      }
      db <- des_evo %>%
        filter(CO_GD %in% c("3", "4"),
               substr(LANCAMENTO, 1, 4) == input$year07,
               SG_UG %in% ug) %>%
        group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG) %>%
        summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO"),
                     .funs = sum
        )
    })
    output$plot07 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = paste(input$department07, unique(dbplot07()$ANO))) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_xAxis(categories = dbplot07()$SG_UG) %>%
        hc_add_series(name = "EMPENHADO", data = dbplot07()$EMPENHADO) %>%
        hc_add_series(name = "LIQUIDADO", data = dbplot07()$LIQUIDADO) %>%
        hc_add_series(name = "PAGO", data = dbplot07()$PAGO) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_chart(
          type = input$typeplot07,
          options3d = list(
            enabled = input$dimension07,
            beta = 10,
            alpha = 10
          )
        )
      if (input$groupplot07 != FALSE) {
        hc <-
          hc %>% hc_plotOptions(series = list(stacking = input$groupplot07))
      }
      hc
    })
    dbplot08 <- reactive({
      db <- des_evo %>%
        filter(CO_GD %in% c("3", "4"),
               substr(LANCAMENTO, 1, 4) == input$year08,
               SG_UG == "REITORIA") %>%
        group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG) %>%
        summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO"),
                     .funs = sum)
      db1 <- des_evo %>%
        filter(CO_GD %in% c("3", "4"),
               substr(LANCAMENTO, 1, 4) == input$year08,
               !SG_UG == "REITORIA") %>%
        group_by(ANO = substr(LANCAMENTO, 1, 4), SG_UG = "CAMPI") %>%
        summarise_at(.vars = c("EMPENHADO", "LIQUIDADO", "PAGO"),
                     .funs = sum)
      db <- bind_rows(db, db1)
      db
    })
    output$plot08 <- renderHighchart({
      hc <- highchart() %>%
        hc_title(text = "") %>%
        hc_subtitle(text = unique(dbplot08()$ANO)) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_xAxis(categories = dbplot08()$SG_UG) %>%
        hc_add_series(name = "EMPENHADO", data = dbplot08()$EMPENHADO) %>%
        hc_add_series(name = "LIQUIDADO", data = dbplot08()$LIQUIDADO) %>%
        hc_add_series(name = "PAGO", data = dbplot08()$PAGO) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_chart(
          type = input$typeplot08,
          options3d = list(
            enabled = input$dimension08,
            beta = 10,
            alpha = 10
          )
        )
      if (input$groupplot08 != FALSE) {
        hc <-
          hc %>% hc_plotOptions(series = list(stacking = input$groupplot08))
      }
      hc
    })
  }
shinyApp(ui,server)

