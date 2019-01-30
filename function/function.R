# DATABASE ----------------------------------------------------------------

extract_orc <- function(file, reference){
  require(openxlsx)
  UG <- data.frame(
    CO_UG = c("152140", "152141", '152142', "152143", "152144", "152145", "152146", "152147", "155145", "155150", "155151", "158501", "158143", '152139', "152143"), 
    SG_UG = c("CTAG", "CSAM", "CBRA", "CTGC", "CSSB", "CCEI", "CEST", "CRFI", "CBRZ", "CCAN", 'CSOB', "CPLA", "RIFB", "CGAM", "CREM"), 
    CO_UO = "26428",
    SG_UO = "IFB",
    stringsAsFactors = FALSE)
  dbTitle <- read.xlsx(xlsxFile = if(missing(file)){file.choose()}else{file},sheet = 1,startRow = 1,rows = 1,rowNames = FALSE,colNames = TRUE,check.names = TRUE)
  dbTitle <- dbTitle[, c(16:ncol(dbTitle))]
  dbTitle <- gsub(pattern = "[.]",replacement = "_",x = colnames(dbTitle))
  db <- read.xlsx(xlsxFile = if(missing(file)){file.choose()}else{file},sheet = 1,startRow = 3,rowNames = FALSE,colNames = FALSE, na.strings = 0)
  col_names <- c("LANCAMENTO", 
                 "CO_UO", "NO_UO", "CO_UGE", "NO_UGE", "CO_UGR", "NO_UGR", 
                 "CO_CATEGORIA_ECONOMICA", "NO_CATEGORIA_ECONOMICA", "CO_GD", "NO_GD", "CO_MODALIDADE_APLICACAO", "NO_MODALIDADE_APLICACAO", "CO_ED", "NO_ED", "CO_SUBITEM", "NO_SUBITEM", "CO_ACAO", "NO_ACAO", "PTRES", "CO_NATUREZA_DESPESA","NO_NATUREZA_DESPESA", "CO_NATUREZA_DESPESA_DETALHADA", "NO_NATUREZA_DESPESA_DETALHADA", "CO_PLANO_INTERNO", "NO_PLANO_INTERNO",
                   dbTitle)
  colnames(db) <- col_names
  db$CO_SUBITEM <- ifelse(nchar(db$CO_SUBITEM) == 1, paste0("0", db$CO_SUBITEM), db$CO_SUBITEM)
  db$LANCAMENTO <- str_replace_all(
    string = db$LANCAMENTO, 
    c("JAN" = "01", "FEV" = "02", "MAR" = "03", "ABR" = "04", "MAI" = "05", "JUN" = "06",
      "JUL" = "07", "AGO" = "08", "SET" = "09", "OUT" = "10", "NOV" = "11", "DEZ" = "12"))
  db$LANCAMENTO <- paste0(substr(db$LANCAMENTO, 4, 7), "/", substr(db$LANCAMENTO, 1, 2))
  db$CO_PLANO_INTERNO <- gsub(pattern = "[-]", replacement = "", db$CO_PLANO_INTERNO)
  db$CO_UGR <- ifelse(db$CO_UGR == "-8", db$CO_UGE, db$CO_UGR)
  db$NO_UGR <- ifelse(db$NO_UGR == "SEM INFORMACAO", db$NO_UGE, db$NO_UGR)
  db <- left_join(x = db, y = UG[, c("CO_UG", "SG_UG")], by = c("CO_UGE" = "CO_UG"))
  db$SG_UO <- ifelse(db$CO_UO == "26428", "IFB", "OUTRO")
  db <- left_join(x = db, y = UG[, c("CO_UG", "SG_UG")], by = c("CO_UGR" = "CO_UG"), suffix = c("", "R"))
  db$CLASSIFICACAO <-
    ifelse(db$CO_ED %in% c("14","33"),"DIARIAS E PASSAGENS",
    ifelse(db$CO_ED %in% c("18","20"),"AUXILIO FINANCEIRO",
    ifelse(db$CO_ED %in% c("30"),"MATERIAL DE CONSUMO",
    ifelse(db$CO_ED %in% c("31"),"OUTROS MATERIAIS",
    ifelse(db$CO_ED %in% c("32"),"OUTROS MATERIAIS",
    ifelse(db$CO_ED %in% c("35"),"OUTROS SERVICOS",
    ifelse(db$CO_ED %in% c("36"),"OUTROS SERVICOS",
    ifelse(db$CO_ED %in% c("37"),"OUTROS SERVICOS",
    ifelse(db$CO_ED %in% c("39"),"OUTROS SERVICOS",
    ifelse(db$CO_ED %in% c("47"),"TRIBUTOS E OBRIGACOES",
    ifelse(db$CO_ED %in% c("51"),"OBRAS INSTALACOES",
    ifelse(db$CO_ED %in% c("52"),"MATERIAL PERMANENTE",""))))))))))))
  db$CLASSIFICACAO <-
    ifelse(db$CO_ED %in% c("30") & db$CO_SUBITEM %in% c("17","47") |
           db$CO_ED %in% c("35") & db$CO_SUBITEM %in% c("04") |
           db$CO_ED %in% c("36") & db$CO_SUBITEM %in% c("46") & db$CO_GD %in% c("4") |
           db$CO_ED %in% c("36") & db$CO_SUBITEM %in% c("54","57") |
           db$CO_ED %in% c("37") & db$CO_SUBITEM %in% c("09","27","28") & db$CO_GD %in% c("3") |
           db$CO_ED %in% c("37") & db$CO_SUBITEM %in% c("92","93") & db$CO_GD %in% c("4") |
           db$CO_ED %in% c("39") & db$CO_SUBITEM %in% c("08","11","26","27","28","30","31","56") & db$CO_GD %in% c("3") |
           db$CO_ED %in% c("39") & db$CO_SUBITEM %in% c("57","97") |
           db$CO_ED %in% c("39") & db$CO_SUBITEM %in% c("92","93","95") & db$CO_GD %in% c("4") |
           db$CO_ED %in% c("52") & db$CO_SUBITEM %in% c("35")& db$CO_GD %in% c("4"),
           "TECNOLOGIA DA INFORMACAO",
    ifelse(db$CO_ED %in% c("36") & db$CO_SUBITEM %in% c("07","26","35") |
           db$CO_ED %in% c("37") & db$CO_SUBITEM %in% c("01","19","08") |
           db$CO_ED %in% c("39") & db$CO_SUBITEM %in% c("79"),
           "APOIO ADMINISTRATIVO",
    ifelse(db$CO_ED %in% c("36") & db$CO_SUBITEM %in% c("15") |
           db$CO_ED %in% c("36") & db$CO_SUBITEM %in% c("16","17") & db$CO_GD %in% c("3") |
           db$CO_ED %in% c("39") & db$CO_SUBITEM %in% c("10","12","14") |
           db$CO_ED %in% c("39") & db$CO_SUBITEM %in% c("13") & db$CO_GD %in% c("4"),
           "LOCACAO DE BENS",
    ifelse(db$CO_ED %in% c("36") & db$CO_SUBITEM %in% c("18","20","21","22") |
           db$CO_ED %in% c("36") & db$CO_SUBITEM %in% c("19") & db$CO_GD %in% c("4") |
           db$CO_ED %in% c("37") & db$CO_SUBITEM %in% c("04","06") & db$CO_GD %in% c("3") |
           db$CO_ED %in% c("39") & db$CO_SUBITEM %in% c("18") & db$CO_GD %in% c("4") |
           db$CO_ED %in% c("39") & db$CO_SUBITEM %in% c("19") & db$CO_GD %in% c("3") |
           db$CO_ED %in% c("39") & db$CO_SUBITEM %in% c("16","17","20","21"),
           "MANUTENCAO E CONSERVACAO",
    ifelse(db$CO_ED %in% c("36") & db$CO_SUBITEM %in% c("28") |
           db$CO_ED %in% c("39") & db$CO_SUBITEM %in% c("48"),
           "SELECAO E TREINAMENTO",
    ifelse(db$CO_ED %in% c("36") & db$CO_SUBITEM %in% c("19") & db$CO_GD %in% c("3") |
           db$CO_ED %in% c("37") & db$CO_SUBITEM %in% c("03")|
           db$CO_ED %in% c("39") & db$CO_SUBITEM %in% c("77"),
           "VIGILANCIA OSTENSIVA",
    ifelse(db$CO_ED %in% c("36") & db$CO_SUBITEM %in% c("25") |
           db$CO_ED %in% c("37") & db$CO_SUBITEM %in% c("02") |
           db$CO_ED %in% c("39") & db$CO_SUBITEM %in% c("78"),
           "LIMPEZA E CONSERVACAO",
    ifelse(db$CO_ED %in% c("36") & db$CO_SUBITEM %in% c("27") |
           db$CO_ED %in% c("36") & db$CO_SUBITEM %in% c("59","63") & db$CO_GD %in% c("3")|
           db$CO_ED %in% c("39") & db$CO_SUBITEM %in% c("47","58","59") |
           db$CO_ED %in% c("39") & db$CO_SUBITEM %in% c("49","91","92","93") & db$CO_GD %in% c("3"),
           "COMUNICACAO",
    ifelse(db$CO_ED %in% c("39") & db$CO_SUBITEM %in% c("44"),
           "AGUA E ESGOTO",
    ifelse(db$CO_ED %in% c("39") & db$CO_SUBITEM %in% c("90"),
           "COMUNICACAO OFICIAL",
    ifelse(db$CO_ED %in% c("39") & db$CO_SUBITEM %in% c("43") |
           db$CO_ED %in% c("47") & db$CO_SUBITEM %in% c("22"),
           "ENERGIA ELETRICA",
           db$CLASSIFICACAO)))))))))))
  db$REFERENCIA <- reference
  col_names <- c("LANCAMENTO", 
                 "CO_UO", "NO_UO", "SG_UO", "CO_UGE", "NO_UGE", "SG_UG", "CO_UGR", "NO_UGR", "SG_UGR", 
                 "CO_CATEGORIA_ECONOMICA", "NO_CATEGORIA_ECONOMICA", "CO_GD", "NO_GD", "CO_MODALIDADE_APLICACAO", "NO_MODALIDADE_APLICACAO", "CO_ED", "NO_ED", "CO_SUBITEM", "NO_SUBITEM", "CO_ACAO", "NO_ACAO", "PTRES", "CO_NATUREZA_DESPESA","NO_NATUREZA_DESPESA", "CO_NATUREZA_DESPESA_DETALHADA", "NO_NATUREZA_DESPESA_DETALHADA", "CO_PLANO_INTERNO", "NO_PLANO_INTERNO", 
                 "CLASSIFICACAO", "REFERENCIA",
                 dbTitle)
  db <- db[, col_names]
  db[, sapply(db, class) == "character"] <- apply(X = db[, sapply(db, class) == "character"], MARGIN = 2, FUN = function(y){iconv(x = y, from = "latin1", to = "UTF-8")})
  for(i in 1:length(dbTitle)){
    if(any(is.na(db[, dbTitle[i]]))) db[, dbTitle[i]][which(is.na(db[, dbTitle[i]]))] <- 0
  }
  db$RAP_PAGO <- db$RESTOS_A_PAGAR_PROCESSADOS_PAGOS + db$RESTOS_A_PAGAR_NAO_PROCESSADOS_PAGOS
  return(db)
}

extract_redmine <- function(pool_connect, select_project, year_project, col_names, col_select, select_custom_values){
  require(pool)
  require(RMySQL)
  require(tidyr)
  require(stringr)
  verify_pool <- function(){
    yes <- c("S","s","Y","y","Sim","sim","SIM","Yes","yes","YES")
    message("The connection data was not informed or not found. Want to inform? (Y or N)")
    question <- scan(what = "character",nmax = 1,quiet = TRUE,encoding = "Latin-1")
    if(question %in% yes){
      message("Inform a name, host, username and password:")
      pool_connect <- scan(what = "character",nmax = 4,quiet = TRUE,encoding = "Latin-1")
      names(pool_connect) <- c("name","host","username","password")
      return(pool_connect)
    }else{
      stop()
    }
  }
  if(missing(pool_connect)){
    pool_connect <- verify_pool()
  }else{
    if(!length(pool_connect) == 4){
      pool_connect <- verify_pool()
    }else{
      if(class(pool_connect)=="list"){
        pool_connect <- unlist(pool_connect)
        names(pool_connect) <- c("name","host","username","password")
      }
    }
  }
  if(missing(select_custom_values)){
    select_custom_values <- FALSE
  }
  connect <- dbPool(drv = MySQL(),dbname = pool_connect["name"],host = pool_connect["host"],username = pool_connect["username"],password = pool_connect["password"],idleTimeout = 3600000)
  conn <- poolCheckout(connect)
  date_dbi <- dbWithTransaction(conn, {dbGetQuery(conn, "SELECT updated_on FROM issues ORDER BY updated_on DESC LIMIT 1")})
  update_dbi <- as.POSIXct(strptime(date_dbi$updated_on,"%Y-%m-%d %H:%M:%S"))
  tb <- c("custom_fields","custom_values","issues","issue_statuses","projects","trackers","users")
  for(i in 1:NROW(tb)){assign(tb[i],dbReadTable(conn = conn,name = tb[i]))}
  custom_fields[,sapply(custom_fields, class) == "character"] <- apply(X = custom_fields[,sapply(custom_fields, class) == "character"],MARGIN = 2,FUN = function(y){iconv(x = y,from = "latin1",to = "UTF-8")})
  custom_values[,sapply(custom_values, class) == "character"] <- apply(X = custom_values[,sapply(custom_values, class) == "character"],MARGIN = 2,FUN = function(y){iconv(x = y,from = "latin1",to = "UTF-8")})
  issue_statuses$name <- iconv(x = issue_statuses$name,from = "latin1",to = "UTF-8")
  issues[,sapply(issues, class) == "character"] <- apply(X = issues[,sapply(issues, class) == "character"],MARGIN = 2,FUN = function(y){iconv(x = y,from = "latin1",to = "UTF-8")})
  projects[,sapply(projects, class) == "character"] <- apply(X = projects[,sapply(projects, class) == "character"],MARGIN = 2,FUN = function(y){iconv(x = y,from = "latin1",to = "UTF-8")})
  trackers$name <- iconv(x = trackers$name,from = "latin1",to = "UTF-8")
  users[,sapply(users, class) == "character"] <- apply(X = users[,sapply(users, class) == "character"],MARGIN = 2,FUN = function(y){iconv(x = y,from = "latin1",to = "UTF-8")})
  if(select_custom_values == TRUE){
    custom_values <- merge(x = custom_values,y = custom_fields[,c("id","name")],by.x = "custom_field_id",by.y = "id",all.x = TRUE,sort = FALSE)
    custom_values <- custom_values[custom_values$custom_field_id %in% c(120,121,122,136,145,146,147),c("custom_field_id","customized_id","name","value")]
    custom_values[custom_values$custom_field_id %in% c(120,121,122),]$name <- "setor"
    custom_values[custom_values$custom_field_id %in% c(136),]$name <- "natureza_despesa"
    custom_values[custom_values$custom_field_id %in% c(145),]$name <- "limite_orcamento"
    custom_values[custom_values$custom_field_id %in% c(146),]$name <- "previsao_orcamento"
    custom_values[custom_values$custom_field_id %in% c(147),]$name <- "validacao"
    custom_values <- custom_values[,-c(1)]
    custom_values1 <- custom_values[custom_values$name == "setor",]
    custom_values1 <- spread(data = custom_values1,key = name,value = value,fill = "")
    custom_values2 <- custom_values[custom_values$name == "natureza_despesa",]
    custom_values2 <- spread(data = custom_values2,key = name,value = value,fill = "")
    custom_values3 <- custom_values[custom_values$name == "limite_orcamento",]
    custom_values3 <- spread(data = custom_values3,key = name,value = value,fill = "")
    custom_values3$limite_orcamento <- str_replace_all(string = custom_values3$limite_orcamento,c("[.]"="","[,]"="."))
    custom_values3$limite_orcamento <- as.numeric(custom_values3$limite_orcamento)
    custom_values4 <- custom_values[custom_values$name == "previsao_orcamento",]
    custom_values4 <- spread(data = custom_values4,key = name,value = value,fill = "")
    custom_values4$previsao_orcamento <- str_replace_all(string = custom_values4$previsao_orcamento,c("[.]"="","[,]"="."))
    custom_values4$previsao_orcamento <- as.numeric(custom_values4$previsao_orcamento)
    custom_values5 <- custom_values[custom_values$name == "validacao",]
    custom_values5 <- spread(data = custom_values5,key = name,value = value,fill = "")
    custom_values <- merge(x = custom_values1,y = custom_values2,by.x = "customized_id",by.y = "customized_id",all = TRUE)
    custom_values <- merge(x = custom_values,y = custom_values3,by.x = "customized_id",by.y = "customized_id",all = TRUE)
    custom_values <- merge(x = custom_values,y = custom_values4,by.x = "customized_id",by.y = "customized_id",all = TRUE)
    custom_values <- merge(x = custom_values,y = custom_values5,by.x = "customized_id",by.y = "customized_id",all = TRUE)
  }
  if(!missing(select_project)){
    projects$year <- str_extract(string = projects$name,pattern = "[0-9]{4,4}")
    projects$year <- as.integer(ifelse(nchar(projects$year) == 4,projects$year,""))
    if(length(select_project)==1){
      proj <- projects[grepl(pattern = select_project,projects$name) & projects$year %in% c(year_project),]$id
      issues <- issues[issues$project_id %in% proj,]
    }else if(length(select_project)>1){
      proj <- c()
      for(i in 1:length(select_project)){
        p <- projects[grepl(pattern = select_project[i],projects$name) & projects$year %in% c(year_project),]$id
        proj <- append(proj,p)
      }
      issues <- issues[issues$project_id %in% proj,]
    }
  }
  issues <- merge(x = issues,y = trackers[,c("id","name")],by.x = "tracker_id",by.y = "id",all.x = TRUE,sort = FALSE)
  issues <- merge(x = issues,y = projects[,c("id","name","status")],by.x = "project_id",by.y = "id",suffixes = c("","_project"),all.x = TRUE,sort = FALSE)
  issues <- merge(x = issues,y = projects[,c("id","name")],by.x = "tracker_id",by.y = "id",suffix = c("","_project_parent"),all.x = TRUE,sort = FALSE)
  issues <- merge(x = issues,y = issue_statuses[,c("id","name")],by.x = "status_id",by.y = "id",suffix = c("","_status"),all.x = TRUE,sort = FALSE)
  issues <- merge(x = issues,y = users[,c("id","firstname","lastname")],by.x = "assigned_to_id",by.y = "id",all.x = TRUE,sort = FALSE)
  issues <- merge(x = issues,y = users[,c("id","firstname","lastname")],by.x = "author_id",by.y = "id",suffix = c("","_author"),all.x = TRUE,sort = FALSE)
  if(select_custom_values == TRUE){
    issues <- merge(x = issues,y = custom_values,by.x = "id",by.y = "customized_id",all.x = TRUE,sort = FALSE)
  }
  issues$name <- str_replace_all(issues$name,c("[ç]"="c","[ã]"="a","[õ]"="o","[á]"="a","[é]"="e","[í]"="i","[ó]"="o","[ú]"="u","[:punct:]"=""))
  issues$correct_status <- ifelse(issues$status_id == 5,"Fechada",ifelse(issues$done_ratio == 100,"Finalizada",ifelse(issues$done_ratio == 0,"Nao_Iniciada","Iniciada")))
  issues$deadline_status <- ifelse(is.na(issues$due_date),"Sem_Data",ifelse(issues$correct_status == "Finalizada","Concluida",ifelse(issues$correct_status == "Iniciada"   & class(as.numeric(substr(Sys.Date(),1,4))) <= class(as.numeric(substr(issues$start_date,1,4))) & issues$due_date >= Sys.Date(),"No_prazo",ifelse(issues$correct_status == "Iniciada"   & class(as.numeric(substr(Sys.Date(),1,4))) >  class(as.numeric(substr(issues$start_date,1,4))),"Em_atraso",ifelse(issues$correct_status == "Iniciada"   & class(as.numeric(substr(Sys.Date(),1,4))) <=  class(as.numeric(substr(issues$start_date,1,4))) & issues$due_date < Sys.Date(),"Em_atraso",ifelse(issues$correct_status == "Nao_Iniciada" & class(as.numeric(substr(Sys.Date(),1,4))) <=  class(as.numeric(substr(issues$start_date,1,4))) & issues$due_date >= Sys.Date() & issues$start_date >= Sys.Date(),"No_prazo",ifelse(issues$correct_status == "Nao_Iniciada" & class(as.numeric(substr(Sys.Date(),1,4))) >  class(as.numeric(substr(issues$start_date,1,4))),"Em_atraso",ifelse(issues$correct_status == "Nao_Iniciada" & class(as.numeric(substr(Sys.Date(),1,4))) <=  class(as.numeric(substr(issues$start_date,1,4))) & issues$due_date < Sys.Date(),"Em_atraso",ifelse(issues$correct_status == "Nao_Iniciada" & class(as.numeric(substr(Sys.Date(),1,4))) <=  class(as.numeric(substr(issues$start_date,1,4))) & issues$due_date >= Sys.Date() & issues$start_date <= Sys.Date(),"Em_atraso","Sem_Definicao")))))))))
  issues$year <- str_extract(string = issues$name_project,pattern = "[0-9]{4,4}")
  issues$department <- str_extract(string = issues$name_project,pattern = "[^(]*[)$]")
  issues$department <- substr(x = issues$department,start = 1,nchar(issues$department)-1)
  if(select_custom_values == TRUE){
    issues$department_initials <- ifelse(is.na(issues$setor)|issues$setor == "","Nao_Informado",as.character(substr(x = issues$setor,start = as.numeric(gregexpr(pattern = "[(]",issues$setor))+1,stop = as.numeric(gregexpr(pattern = "[)]",issues$setor))-1)))
  }
  issues$due_date <- as.Date(issues$due_date)
  issues$created_on <- as.POSIXct(strptime(issues$created_on,"%Y-%m-%d %H:%M:%S"))
  issues$updated_on <- as.POSIXct(strptime(issues$updated_on,"%Y-%m-%d %H:%M:%S"))
  issues$start_date <- as.Date(issues$start_date)
  issues$assign <- ifelse(is.na(issues$firstname) & is.na(issues$lastname),"Sem Atribuicao",paste(issues$firstname,issues$lastname))
  issues$author <- paste(issues$firstname_author,issues$lastname_author)
  issues$parent_id0 <- issues$parent_id
  issues <- merge(x = issues,y = issues[,c("id","name","parent_id","subject")],by.x = "parent_id0",by.y = "id",all.x = TRUE,sort = FALSE,suffixes = c("","1"))
  issues <- merge(x = issues,y = issues[,c("id","name","parent_id","subject")],by.x = "parent_id1",by.y = "id",all.x = TRUE,sort = FALSE,suffixes = c("","2"))
  issues <- merge(x = issues,y = issues[,c("id","name","parent_id","subject")],by.x = "parent_id2",by.y = "id",all.x = TRUE,sort = FALSE,suffixes = c("","3"))
  issues <- merge(x = issues,y = issues[,c("id","name","parent_id","subject")],by.x = "parent_id3",by.y = "id",all.x = TRUE,sort = FALSE,suffixes = c("","4"))
  if(missing(col_select)){
    if(select_custom_values == TRUE){
      col_select <- factor(x = c("id","name_project","year","name","subject","description","created_on","updated_on","start_date","due_date","done_ratio","name_status","correct_status","deadline_status","assign","author","setor","department","department_initials","natureza_despesa","limite_orcamento","previsao_orcamento","validacao","parent_id","parent_id0","parent_id1","name1","subject1","parent_id2","name2","subject2","parent_id3","name3","subject3","parent_id4","name4","subject4"),levels = c("id","name_project","year","name","subject","description","created_on","updated_on","start_date","due_date","done_ratio","name_status","correct_status","deadline_status","assign","author","setor","department","department_initials","natureza_despesa","limite_orcamento","previsao_orcamento","validacao","parent_id","parent_id0","parent_id1","name1","subject1","parent_id2","name2","subject2","parent_id3","name3","subject3","parent_id4","name4","subject4"))
    }else{
      col_select <- factor(x = c("id","name_project","year","name","subject","description","created_on","updated_on","start_date","due_date","done_ratio","name_status","correct_status","deadline_status","assign","author","setor","department","parent_id","parent_id0","parent_id1","name1","subject1","parent_id2","name2","subject2","parent_id3","name3","subject3","parent_id4","name4","subject4"),levels = c("id","name_project","year","name","subject","description","created_on","updated_on","start_date","due_date","done_ratio","name_status","correct_status","deadline_status","assign","author","setor","department","parent_id","parent_id0","parent_id1","name1","subject1","parent_id2","name2","subject2","parent_id3","name3","subject3","parent_id4","name4","subject4"))
    }
  }
  issues <- issues[,col_select]
  
  if(!missing(col_names)){
    colnames(issues) <- c(col_names)
  }
  issues$date_extract <- Sys.time()
  issues$date_update <- update_dbi
  poolReturn(conn)
  poolClose(connect)
  return(issues)
}

import_data <- function(pasta = "data/", extensao = "rds"){
  pathit <- function(FUN, path){
    function(file, ...) FUN(file = file.path(path, file), ...)
  }
  readRDS2 <- pathit(readRDS, pasta)
  temp = list.files(path = pasta, pattern= paste0("*.",extensao))
  list2env(lapply(setNames(temp, make.names(gsub(paste0("*.", extensao, "$"), "", temp))), readRDS2), envir = .GlobalEnv)
}

# SHINY -------------------------------------------------------------------

boxnew <- function(inputId, boxtitle, menu_selected, label, choices, selected, status, width_box, plot_highchart, description){
  if(missing(menu_selected)){
    boxnew <- 
      shinydashboard::box(
        solidHeader = T,
        width = width_box,
        collapsible = T,
        collapsed = F,
        title = boxtitle,
        shinycssloaders::withSpinner(highchartOutput(paste0("plot",inputId)),type = 8L,color = "#3c8dbc"))
  } else {
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
        choices = choices[["year"]],
        selected = selected["year"])}
    if(any(menu_selected == "yearmonth")){
      yearmonth <- shiny::selectInput(
        inputId = paste0("yearmonth",inputId), 
        label = if(label["yearmonth"]=="empty"){NULL}else{label["yearmonth"]},
        choices = choices[["yearmonth"]],
        selected = selected["yearmonth"])}
    if(any(menu_selected == "department")){
      department <- shiny::selectInput(
        inputId = paste0("department",inputId), 
        label = if(label["department"]=="empty"){NULL}else{label["department"]},
        choices = choices[["department"]],
        selected = selected["department"])}
    if(any(menu_selected == "programa")){
      programa <- shiny::selectInput(
        inputId = paste0("programa",inputId), 
        label = if(label["programa"]=="empty"){NULL}else{label["programa"]},
        choices = if(any(choices[["programa"]]=="empty")){"X"}else{choices[["programa"]]},
        selected = if(selected["programa"]=="empty"){"X"}else{selected["programa"]})}
    if(any(menu_selected == "selectY")){
      selectY <- shiny::selectInput(
        inputId = paste0("selectY",inputId), 
        label = if(label["selectY"]=="empty"){NULL}else{label["selectY"]},
        choices = if(any(choices[["selectY"]]=="empty")){"Y"}else{choices[["selectY"]]},
        selected = if(selected["selectY"]=="empty"){"Y"}else{selected["selectY"]})}
    if(any(menu_selected == "filterx")){
      filterx <- shiny::selectInput(
        inputId = paste0("filterx",inputId), 
        label = if(label["filterx"]=="empty"){NULL}else{label["filterx"]},
        multiple = TRUE, 
        choices = if(any(choices[["filterx"]]=="empty")){"X"}else{choices[["filterx"]]},
        selected = if(selected["filterx"]=="empty"){choices[["filterx"]]}else{selected["filterx"]})}
    if(any(menu_selected == "filtery")){
      filtery <- shiny::selectInput(
        inputId = paste0("filtery",inputId), 
        label = if(label["filtery"]=="empty"){NULL}else{label["filtery"]},
        multiple = TRUE, 
        choices = if(any(choices[["filtery"]]=="empty")){"Y"}else{choices[["filtery"]]},
        selected = if(selected["filtery"]=="empty"){"Y"}else{selected["filtery"]})}
    boxnew <- 
      shinydashboardPlus::boxPlus(
        title = boxtitle,
        solidHeader = TRUE,
        width = if(missing(width_box)){6}else{width_box},
        collapsible = TRUE, 
        closable = FALSE,
        enable_sidebar = TRUE,
        sidebar_width = 50,
        sidebar_background = "#ffffff",
        sidebar_start_open = FALSE,
        sidebar_content = tagList(
          strong(p(color = "black", "Configuração:")),
          if(any(menu_selected == "typeplot")){typeplot}else{NULL},
          if(any(menu_selected == "groupplot")){groupplot}else{NULL},
          if(any(menu_selected == "dimension")){dimension}else{NULL},
          if(any(menu_selected == "year")){year}else{NULL},
          if(any(menu_selected == "yearmonth")){yearmonth}else{NULL},
          if(any(menu_selected == "department")){department}else{NULL},
          if(any(menu_selected == "programa")){programa}else{NULL},
          if(any(menu_selected == "selectY")){selectY}else{NULL},
          if(any(menu_selected == "filterx")){filterx}else{NULL},
          if(any(menu_selected == "filtery")){filtery}else{NULL}),
        shinycssloaders::withSpinner(highchartOutput(paste0("plot",inputId)),type = 8L,color = "#3c8dbc"))
  }
  return(boxnew)
}

callout <- function(title, message, type, ...){
  if(missing(type)){
    type = "callout callout-info"
    
  } else {
    type = paste0("callout callout-", type)
  }
  if(missing(title)){
    callout <- shiny::tags$div(class = type,
                    shiny::tags$p(message))
  }else{
    callout <- shiny::tags$div(class = type,
                    shiny::tags$h4(..., title),
                    shiny::tags$p(message))
  }
  return(callout)
}

# HIGHCHARTER -------------------------------------------------------------

hcoptslang <- function(){
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
  return(hcoptslang)
}

highchart_new <- function(data, categories, series, subtitle, credits, input_plot, serie_type, origin, colors){
  hc <- highchart() %>%
    hc_title(text = "") %>%
    hc_subtitle(text = subtitle) %>%
    hc_yAxis(title = list(text = "")) %>%
    hc_xAxis(title = list(text = ""), categories = c(list(categories)[[1]], categories)) %>%
    hc_exporting(enabled = TRUE) %>% 
    hc_chart(type = input_plot[["typeplot"]], marginLeft = 70, marginRight = 70)
    if (!missing(origin)){
      if (any(origin == "orcamento")){
        hc <- hc %>% hc_colors(colors)
      }
    }
  if (any(names(series) == "SERIE1")) {hc <- hc %>% hc_add_series(name = series[["SERIE1"]],  data = data$SERIE1)}
  if (any(names(series) == "SERIE2")) {hc <- hc %>% hc_add_series(name = series[["SERIE2"]],  data = data$SERIE2)}
  if (any(names(series) == "SERIE3")) {hc <- hc %>% hc_add_series(name = series[["SERIE3"]],  data = data$SERIE3)}
  if (any(names(series) == "SERIE4")) {hc <- hc %>% hc_add_series(name = series[["SERIE4"]],  data = data$SERIE4)}
  if (any(names(series) == "SERIE5")) {hc <- hc %>% hc_add_series(name = series[["SERIE5"]],  data = data$SERIE5)}
  if (any(names(series) == "SERIE6")) {hc <- hc %>% hc_add_series(name = series[["SERIE6"]],  data = data$SERIE6)}
  if (any(names(series) == "SERIE7")) {hc <- hc %>% hc_add_series(name = series[["SERIE7"]],  data = data$SERIE7)}
  if (any(names(series) == "SERIE8")) {hc <- hc %>% hc_add_series(name = series[["SERIE8"]],  data = data$SERIE8)}
  if (any(names(series) == "SERIE9")) {hc <- hc %>% hc_add_series(name = series[["SERIE9"]],  data = data$SERIE9)}
  if (any(names(series) == "SERIE10")) {hc <- hc %>% hc_add_series(name = series[["SERIE10"]], data = data$SERIE10)}
  if (any(names(series) == "SERIE11")) {hc <- hc %>% hc_add_series(name = series[["SERIE11"]], data = data$SERIE11)}
  if (any(names(series) == "SERIE12")) {hc <- hc %>% hc_add_series(name = series[["SERIE12"]], data = data$SERIE12)}
  if (any(names(series) == "SERIE13")) {hc <- hc %>% hc_add_series(name = series[["SERIE13"]], data = data$SERIE13)}
  if (any(names(series) == "SERIE14")) {hc <- hc %>% hc_add_series(name = series[["SERIE14"]], data = data$SERIE14)}
  if (any(names(series) == "SERIE15")) {hc <- hc %>% hc_add_series(name = series[["SERIE15"]], data = data$SERIE15)}
  if (any(names(series) == "SERIE16")) {hc <- hc %>% hc_add_series(name = series[["SERIE16"]], data = data$SERIE16)}
  if (any(names(series) == "SERIE_TYPE")) {hc <- hc %>% hc_add_series(name = series[["SERIE_TYPE"]], data = data$SERIE_TYPE, type = serie_type)}
    if (any(names(series) == "SERIE_RAP")) {
      if (input_plot[["typeplot"]] == "areaspline"){
        hc <- hc %>% hc_add_series(name = series[["SERIE_RAP"]], data = data$SERIE_RAP, type = "spline")
        } else {
          hc <- hc %>% hc_add_series(name = series[["SERIE_RAP"]], data = data$SERIE_RAP, type = input_plot[["typeplot"]])
        }
    }
  if (input_plot[["dimension"]] == TRUE){
    hc <- hc %>% hc_chart(options3d = list(enabled = input_plot[["dimension"]], beta = 10, alpha = 10))
    }
  if (input_plot[["groupplot"]] != FALSE) {
    hc <- hc %>% hc_plotOptions(series = list(stacking = input_plot[["groupplot"]]))
    }
  if (input_plot[["groupplot"]] == "percent") {
    hc <- hc %>% hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')
    }
  if (!missing(credits)){
    href <- ifelse(credits == "Portal da Transparência", "http://portaltransparencia.gov.br/download-de-dados","")
    hc <- hc %>% hc_credits(enabled = TRUE, text = paste("Fonte:", credits), href = href)
    }
  hc
}

hccolor <- list(exeorc = list(emp = "#3c8dbc", liq = "#00a65a", pag = "#dd4b39", rap = "#f39c12"),
                desexe = list(pag = "#263238", rap = "#4f5b62"),
                pesevo = list(tot = "#455a64", doc = "#3c8dbc", tec = "#00a65a"),
                pessch = list(h20 = "#c6d9ec", h25 = "#8cb3d9", h30 = "#4080bf", h40 = "#264d73", hde = "#0d1a26"),
                palblu = list(blu100 = "#ffffff", blu95 = "#ecf4f9", blu90 = "#d8e9f3", blu85 = "#c5deed", blu80 = "#b1d3e7", blu75 = "#9ec8e0", blu70 = "#8bbdda", blu65 = "#77b2d4", blu60 = "#64a7ce", blu55 = "#519cc8", blu50 = "#3d91c2", blu49 = "#3c8dbc", blu45 = "#3783ae", blu40 = "#31749b", blu35 = "#2b6688", blu30 = "#255774", blu25 = "#1f4961", blu20 = "#183a4e", blu15 = "#122c3a", blu10 = "#0c1d27", blu05 = "#060f13", blu00 = "#000000"),
                palgre = list(gre100 = '#ffffff', gre95 = '#e6fff4', gre90 = '#ccffe8', gre85 = '#b3ffdd', gre80 = '#99ffd1', gre75 = '#80ffc6', gre70 = '#66ffba', gre65 = '#4dffaf', gre60 = '#33ffa3', gre55 = '#1aff98', gre50 = '#00ff8c', gre45 = '#00e67e', gre40 = '#00cc70', gre35 = '#00b362', gre33 = '#00a65a', gre30 = '#009954', gre25 = '#008046', gre20 = '#006638', gre15 = '#004d2a', gre10 = '#00331c', gre05 = '#001a0e', gre0 = '#000000'),
                palred = list(red100 = '#ffffff', red95 = '#fbebe9', red90 = '#f8d8d3', red85 = '#f4c4be', red80 = '#f0b0a8', red75 = '#ed9d92', red70 = '#e9897c', red65 = '#e57566', red60 = '#e16151', red55 = '#dd4b39', red50 = '#da3a25', red45 = '#c43421', red40 = '#ae2e1e', red35 = '#99291a', red30 = '#832316', red25 = '#6d1d12', red20 = '#57170f', red15 = '#41110b', red10 = '#2c0c07', red05 = '#160604', red0 = '#000000'),
                palyel = list(yel100 = '#ffffff', yel95 = '#fef5e7', yel90 = '#fcebcf', yel85 = '#fbe1b6', yel80 = '#fad79e', yel75 = '#f9cd86', yel70 = '#f7c36e', yel65 = '#f6b855', yel60 = '#f5ae3d', yel55 = '#f4a425', yel51 = '#f39c12', yel50 = '#f29a0d', yel45 = '#da8b0b', yel40 = '#c27b0a', yel35 = '#aa6c09', yel30 = '#915d08', yel25 = '#794d06', yel20 = '#613e05', yel15 = '#492e04', yel10 = '#301f03', yel05 = '#180f01', yel00 = '#000000'),
                palgra = list(gra100 = "#ffffff", gra95 = "#f0f3f5", gra90 = "#e1e7ea", gra85 = "#d1dbe0", gra80 = "#c2d0d6", gra75 = "#b3c4cb", gra70 = "#a4b8c1", gra65 = "#95acb7", gra60 = "#86a0ac", gra55 = "#7694a2", gra50 = "#678898", gra45 = "#5d7b89", gra40 = "#536d79", gra35 = "#485f6a", gra30 = "#3e525b", gra25 = "#34444c", gra20 = "#29373d", gra16 = "#222d32", gra15 = "#1f292e", gra10 = "#151b1e", gra5 = "#0a0e0f", gra0 = "#000000"))










