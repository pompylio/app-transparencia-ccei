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

import_multi_files <- function(local,file){
  require(dplyr)
  f <- list.files(path = local,pattern = file,full.names = TRUE)
  db <- readRDS(f[1])
  for(i in 2:length(f)){
    db1 <- readRDS(f[i])
    db <- bind_rows(db,db1)
  }
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

# REDMINE -----------------------------------------------------------------

aca_sit_ger <- function(database, department, year){
  if(missing(database)){
    if(exists("sgi",where = 1)){
      database <- sgi
    }else{
      message("missing 'database' input")
    }
  }
  if(missing(department)){
    d <- unique(database$department)
    if(missing(year)){
      y <- unique(database$year)
    }else{
      y <- year
    }
    db0 <- database %>%
      filter(name == "Acao" & name1 == "Indicador",department %in% d, year %in% y) %>%
      group_by(Grupo = ifelse(substr(department,1,1)=="C","Campus","Reitoria"),Unidade = department,Tipo = correct_status) %>%
      summarise(Valor= n())
    db1 <- database %>%
      filter(name == "Acao" & name1 == "Indicador",department %in% d, year %in% y) %>%
      group_by(Grupo = "IFB",Unidade = "IFB",Tipo = correct_status) %>%
      summarise(Valor= n())
    db <- bind_rows(db0,db1)
  }else{
    d <- department
    if(missing(year)){
      y <- unique(database$year)
    }else{
      y <- year
    }
    db <- database %>%
      filter(name == "Acao" & name1 == "Indicador",department %in% d, year %in% y) %>%
      group_by(Grupo = ifelse(substr(department,1,1)=="C","Campus","Reitoria"),Unidade = department,Tipo = correct_status) %>%
      summarise(Valor= n())
  }
  return(db)
}

aca_sit_res <- function(database, department, type, year){
  if(missing(database)){
    if(exists("sgi",where = 1)){
      database <- sgi
    }else{
      message("missing 'database' input")
    }
  }
  if(missing(department)){
    d <- unique(database$department)
    if(missing(year)){
      y <- unique(database$year)
    }else{
      y <- year
    }
    if(type == "Envolvido"){
      db0 <- database %>%
        filter(name == "Acao" & name1 == "Indicador",department %in% d, year %in% y) %>%
        group_by(Grupo = "Envolvido",Unidade = department,Responsavel = assign,Tipo = correct_status) %>%
        summarise(Valor= n())
      db1 <- database %>%
        filter(name == "Acao" & name1 == "Indicador",department %in% d, year %in% y) %>%
        group_by(Grupo = "Envolvido",Unidade = department,Responsavel = assign,Tipo = correct_status) %>%
        summarise(Valor= n())
      db <- bind_rows(db0,db1)
    }else if(type == "Setor"){
      db0 <- database %>%
        filter(name == "Acao" & name1 == "Indicador",department %in% d, year %in% y) %>%
        group_by(Grupo = "Setor",Unidade = department,Responsavel = department_initials,Tipo = correct_status) %>%
        summarise(Valor= n())
      db1 <- database %>%
        filter(name == "Acao" & name1 == "Indicador",department %in% d, year %in% y) %>%
        group_by(Grupo = "Setor",Unidade = department,Responsavel = department_initials,Tipo = correct_status) %>%
        summarise(Valor= n())
      db <- bind_rows(db0,db1)
    }
  }else{
    d <- department
    if(missing(year)){
      y <- unique(database$year)
    }else{
      y <- year
    }
    if(type == "Envolvido"){
      db <- database %>%
        filter(name == "Acao" & name1 == "Indicador",department %in% d, year %in% y) %>%
        group_by(Grupo = "Envolvido",Unidade = department,Responsavel = assign,Tipo = correct_status) %>%
        summarise(Valor= n())
    }else if(type == "Setor"){
      db <- database %>%
        filter(name == "Acao" & name1 == "Indicador",department %in% d, year %in% y) %>%
        group_by(Grupo = "Setor",Unidade = department,Responsavel = department_initials,Tipo = correct_status) %>%
        summarise(Valor= n())
    }
  }
  return(db)
}

ind_sit_ger <- function(database, department, col_select, year){
  if(missing(database)){
    if(exists("sgi",where = 1)){
      database <- sgi
    }else{
      message("missing 'database' input")
    }
  }
  if(missing(department)){
    d <- unique(database$department)
    if(missing(year)){
      y <- unique(database$year)
    }else{
      y <- year
    }
    db <- database %>%
      filter(name1 == "Indicador",name == "Indicador",year %in% y) %>%
      group_by(Indicador = subject, Cod_Indicador = substr(subject,1,5), Percentual = done_ratio) %>%
      summarise()
  }else{
    d <- department
    if(missing(year)){
      y <- unique(database$year)
    }else{
      y <- year
    }
    db <- database %>%
      filter(name1 == "Indicador",name == "Indicador",department %in% d,year %in% y) %>%
      group_by(Indicador = subject, Cod_Indicador = substr(subject,1,5), Percentual = done_ratio) %>%
      summarise()
  }
  if(!missing(col_select)){
    db <- db[,c(col_select)]
  }
  return(db)
}

orc_cla_uni <- function(database, department, year, reference, melt, ...){
  require(dplyr)
  require(reshape2)
  if(missing(database)){
    if(exists("orc",where = 1)){
      database <- db
    }else{
      message("missing 'database' input")
    }
  }
  if(missing(department)){
    d <- unique(database$UGE_SIGLA)
    if(missing(year)){
      y <- unique(database$ANO)
    }else{
      y <- year
    }
    if(missing(reference)){
      r <- unique(database$REFERENCIA)
    }else{
      r <- reference
    }
    db <- database %>%
      filter(ANO %in% y,REFERENCIA %in% r) %>%
      group_by(ANO,DATA,REFERENCIA,UGE_SIGLA,...,CLASSIFICACAO) %>%
      summarise(DOTACAO_ATUALIZADA = sum(DOTACAO_ATUALIZADA,na.rm = TRUE),
                DESPESAS_EMPENHADAS = sum(DESPESAS_EMPENHADAS,na.rm = TRUE),
                DESPESAS_LIQUIDADAS = sum(DESPESAS_LIQUIDADAS,na.rm = TRUE),
                DESPESAS_PAGAS = sum(DESPESAS_PAGAS,na.rm = TRUE))
  }else{
    d <- department
    if(missing(year)){
      y <- unique(database$ANO)
    }else{
      y <- year
    }
    if(missing(reference)){
      r <- unique(database$REFERENCIA)
    }else{
      r <- reference
    }
    db <- database %>%
      filter(ANO %in% y,REFERENCIA %in% r,UGE_SIGLA %in% d) %>%
      group_by(ANO,REFERENCIA,UGE_SIGLA,...,CLASSIFICACAO) %>%
      summarise(DOTACAO_ATUALIZADA = sum(DOTACAO_ATUALIZADA,na.rm = TRUE),
                DESPESAS_EMPENHADAS = sum(DESPESAS_EMPENHADAS,na.rm = TRUE),
                DESPESAS_LIQUIDADAS = sum(DESPESAS_LIQUIDADAS,na.rm = TRUE),
                DESPESAS_PAGAS = sum(DESPESAS_PAGAS,na.rm = TRUE))
  }
  if(melt == TRUE){
    col <- c("DOTACAO_ATUALIZADA","DESPESAS_EMPENHADAS","DESPESAS_LIQUIDADAS","DESPESAS_PAGAS")
    db <- melt(data = db,id.vars = colnames(db[,!names(db) %in% col]),measure.vars = col,variable.name = "TIPO_VALOR",value.name = "VALOR")
  }
  return(db)
}


# SIAPE -------------------------------------------------------------------

siape_base <- function(data){
  data[, sapply(data, class) == "character"] <-
    apply(
      X = data[, sapply(data, class) == "character"],
      MARGIN = 2,
      FUN = function (y) {
        iconv(x = y, from = "latin1", to = "UTF-8")
      }
    )
  data <- left_join(data, tb_uorg, by = "COD_UORG_LOTACAO")
  data[is.na(data$UORG_LOTACAO_GRUPO), ]$UORG_LOTACAO_GRUPO <- "OUTROS"
  data$FUNCAO_NIVEL <- paste(data$SIGLA_FUNCAO, data$NIVEL_FUNCAO)
  return(data)
}

siape_quadro <- function(data){
  doc <- data %>%
    filter(SIGLA_FUNCAO == "-1", COD_ORG_LOTACAO == "26428", grepl("\\<PROF", DESCRICAO_CARGO)) %>% 
    group_by(REFERENCIA = substr(REFERENCIA, 1, 6), UORG_LOTACAO_GRUPO, TIPO = "DOCENTE") %>%
    summarise(TOTAL = n())
  tec <- data %>%
    filter(SIGLA_FUNCAO == "-1", COD_ORG_LOTACAO == "26428", !grepl("\\<PROF", DESCRICAO_CARGO)) %>% 
    group_by(REFERENCIA = substr(REFERENCIA, 1, 6), UORG_LOTACAO_GRUPO, TIPO = "TECNICO") %>%
    summarise(TOTAL = n())
  db <- bind_rows(doc, tec) %>% spread(key = TIPO, value = TOTAL, fill = 0)
  db$TOTAL <- db$DOCENTE + db$TECNICO
  if(any(is.na(db$DOCENTE))) db[is.na(db$DOCENTE), ]$DOCENTE <- 0
  if(any(is.na(db$TECNICO))) db[is.na(db$TECNICO), ]$TECNICO <- 0
  if(any(is.na(db$TOTAL)))   db[is.na(db$TOTAL), ]$TOTAL <- 0
  return(db)
}

siape_cargo <- function(data){
  doc <- data %>%
    filter(SIGLA_FUNCAO == "-1", COD_ORG_LOTACAO == "26428", grepl("\\<PROF", DESCRICAO_CARGO)) %>% 
    group_by(
      REFERENCIA = substr(REFERENCIA, 1, 6),
      UORG_LOTACAO_GRUPO,
      TIPO_CARGO = "DOCENTE",
      DESCRICAO_CARGO) %>%
    summarise(TOTAL = n())
  tec <- data %>%
    filter(SIGLA_FUNCAO == "-1", COD_ORG_LOTACAO == "26428", !grepl("\\<PROF", DESCRICAO_CARGO)) %>%
    group_by(
      REFERENCIA = substr(REFERENCIA, 1, 6),
      UORG_LOTACAO_GRUPO,
      TIPO_CARGO = "TECNICO",
      DESCRICAO_CARGO) %>%
    summarise(TOTAL = n())
  db <- bind_rows(tec, doc)
  return(db)
}

siape_jornada <- function(data){
  doc <- data %>%
    filter(SIGLA_FUNCAO == "-1", COD_ORG_LOTACAO == "26428", grepl("\\<PROF", DESCRICAO_CARGO)) %>%
    group_by(
      REFERENCIA = substr(REFERENCIA, 1, 6),
      UORG_LOTACAO_GRUPO,
      JORNADA_DE_TRABALHO) %>%
    summarise(DOCENTE = n())
  tec <- data %>%
    filter(SIGLA_FUNCAO == "-1", COD_ORG_LOTACAO == "26428", !grepl("\\<PROF", DESCRICAO_CARGO)) %>%
    group_by(
      REFERENCIA = substr(REFERENCIA, 1, 6),
      UORG_LOTACAO_GRUPO,
      JORNADA_DE_TRABALHO) %>%
    summarise(TECNICO = n())
  db <- left_join(doc, tec, by = c("REFERENCIA", "JORNADA_DE_TRABALHO", "UORG_LOTACAO_GRUPO"))
  db$TOTAL <- db$DOCENTE + db$TECNICO
  if(any(is.na(db$DOCENTE))) db[is.na(db$DOCENTE), ]$DOCENTE <- 0
  if(any(is.na(db$TECNICO))) db[is.na(db$TECNICO), ]$TECNICO <- 0
  if(any(is.na(db$TOTAL)))   db[is.na(db$TOTAL), ]$TOTAL <- 0
  return(db)
}

siape_rotatividade <- function(data, ref_ini, ref_end, group_by, sit_vinculo, sig_funcao){
  if (nchar(ref_ini) < 8) ref_ini <- unique(data[grepl(pattern = ref_ini, x = data$REFERENCIA), ]$REFERENCIA)
  if (nchar(ref_end) < 8) ref_end <- unique(data[grepl(pattern = ref_end, x = data$REFERENCIA), ]$REFERENCIA)
  if (missing(sit_vinculo)) sit_vinculo <- "ATIVO PERMANENTE"
  if (missing(sig_funcao)) sig_funcao <- "-1"
  if (any(sig_funcao == "-1")) db <- data %>% filter(COD_ORG_LOTACAO == "26428", SITUACAO_VINCULO %in% sit_vinculo, SIGLA_FUNCAO %in% sig_funcao)
  else                         db <- data %>% filter(COD_ORG_EXERCICIO == "26428", SITUACAO_VINCULO %in% sit_vinculo, SIGLA_FUNCAO %in% sig_funcao)
  db1 <- db %>%
    filter(REFERENCIA == ref_ini) %>% 
    group_by_at(vars("REFERENCIA", "Id_SERVIDOR_PORTAL", group_by)) %>% 
    summarise()
  db2 <- db %>%
    filter(REFERENCIA == ref_end) %>% 
    group_by_at(vars("REFERENCIA", "Id_SERVIDOR_PORTAL", group_by)) %>%  
    summarise()
  db_saida <- left_join(x = db1, y = db2, by = "Id_SERVIDOR_PORTAL", suffix = c("", "_FIM"))
  db_saida <- db_saida %>% 
    filter(is.na(REFERENCIA_FIM)) %>% 
    group_by_at(.vars = group_by) %>%
    summarise(TOTAL = n()) %>% 
    mutate(INICIO = ref_ini, FIM = ref_end, TIPO = "DESLIGAMENTO")
  db_entrada <- left_join(x = db2, y = db1, by = "Id_SERVIDOR_PORTAL", suffix = c("", "_FIM"))
  db_entrada <- db_entrada %>% 
    filter(is.na(REFERENCIA_FIM)) %>% 
    group_by_at(.vars = group_by) %>%
    summarise(TOTAL = n())%>% 
    mutate(INICIO = ref_ini, FIM = ref_end, TIPO = "ADMISSAO")
  db <- bind_rows(db_saida, db_entrada)
  return(db)
}

siape_rotatividade_acumulada <- function(data, ref_ini, ref_end, group_by, sit_vinculo, sig_funcao){
  if (nchar(ref_ini) < 8) 
    ref_ini <- unique(data[grepl(pattern = ref_ini, x = data$REFERENCIA), ]$REFERENCIA)
  if (nchar(ref_end) < 8) 
    ref_end <- unique(data[grepl(pattern = ref_end, x = data$REFERENCIA), ]$REFERENCIA)
  ref <- unique(data$REFERENCIA)[which(unique(data$REFERENCIA) == ref_ini):which(unique(data$REFERENCIA) == ref_end)]
  db <- siape_rotatividade(data = data, ref_ini = ref[1], ref_end = ref[2], group_by = group_by, sit_vinculo = sit_vinculo, sig_funcao = sig_funcao)  
  i <- 2
  while(i < length(ref)){
    db0 <- siape_rotatividade(data = data, ref_ini = ref[i], ref_end = ref[i+1], group_by = group_by, sit_vinculo = sit_vinculo, sig_funcao = sig_funcao)
    db <- bind_rows(db, db0)
    i = i + 1
  }
  return(db)
}
siape_distribuicao_cargo <- function(data){
  efe <- data %>%
    filter(SIGLA_FUNCAO == "-1", COD_ORG_EXERCICIO == "26428") %>%
    group_by(REFERENCIA, UORG_LOTACAO_GRUPO, TIPO_CARGO = ifelse(grepl("\\<PROF", DESCRICAO_CARGO), "DOCENTE", "TECNICO"),
             DESCRICAO_CARGO, Id_SERVIDOR_PORTAL, ORG_ORIGEM = ifelse(COD_ORG_LOTACAO == "26428", "IFB", "OUTRO")) %>%
    summarise()
  com <- data %>%
    filter(!SIGLA_FUNCAO == "-1", COD_ORG_EXERCICIO == "26428") %>%
    group_by(REFERENCIA, FUNCAO = paste(SIGLA_FUNCAO, NIVEL_FUNCAO), Id_SERVIDOR_PORTAL) %>%
    summarise() %>%
    left_join(efe, by = c("REFERENCIA", "Id_SERVIDOR_PORTAL")) %>% 
    group_by(REFERENCIA, UORG_LOTACAO_GRUPO, ORG_ORIGEM, FUNCAO, TIPO_CARGO) %>% 
    summarise(TOTAL = n())
  if(any(is.na(com$TIPO_CARGO))) com[is.na(com$TIPO_CARGO), ]$TIPO_CARGO <- "EXTERNO"
  if(any(is.na(com$ORG_ORIGEM))) com[is.na(com$ORG_ORIGEM), ]$ORG_ORIGEM <- "EXTERNO"
  return(com)
}

# SIAFI -------------------------------------------------------------------

siafi_classificacao <- function(data, group, sum_by){
  if(missing(group))
    group <- c("LANCAMENTO", "SG_UG", "CLASSIFICACAO")
  else
    group <- c("LANCAMENTO", "SG_UG", "CLASSIFICACAO", group)
  if(missing(sum_by))
    sum_by <- c("DESPESAS_EMPENHADAS", "DESPESAS_LIQUIDADAS", "DESPESAS_PAGAS", "RAP_PAGO")
  else
    sum_by <- c("DESPESAS_EMPENHADAS", "DESPESAS_LIQUIDADAS", "DESPESAS_PAGAS", "RAP_PAGO", summarise)
  db <- db_siafi_tg %>% 
    group_by_at(vars(group)) %>% 
    summarise_at(vars(sum_by), .funs = sum)
  return(db)
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
        sidebar_width = 100,
        sidebar_background = "#ffffff",
        sidebar_start_open = FALSE,
        sidebar_content = tagList(
          column(
            width = 6,
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
            if(any(menu_selected == "filtery")){filtery}else{NULL}
          ),
          column(
            width = 6,
            strong(p("Descrição do gráfico:")),
            p(align = "justify", if(missing(description)){""}else{HTML(paste(description, collapse = "<br/>"))})
          )
          
        ),
        shinycssloaders::withSpinner(highchartOutput(paste0("plot",inputId)),type = 8L,color = "#3c8dbc")
      )
  }
  return(boxnew)
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

HighchartOrc <- function(data, series, subtitle, credits, categories, input_plot){
  hc <- highchart() %>%
    hc_title(text = "") %>%
    hc_subtitle(text = subtitle) %>%
    hc_yAxis(title = list(text = "")) %>%
    hc_xAxis(title = list(text = ""), categories = c(list(categories)[[1]], categories)) %>%
    hc_exporting(enabled = TRUE)
  if (any(series == "EMPENHADO")) hc <- hc %>% hc_add_series(name = "EMPENHADO", data = data$EMPENHADO, type = input_plot[["typeplot"]])
  if (any(series == "LIQUIDADO")) hc <- hc %>% hc_add_series(name = "LIQUIDADO", data = data$LIQUIDADO, type = input_plot[["typeplot"]])
  if (any(series == "PAGO"))      hc <- hc %>% hc_add_series(name = "PAGO", data = data$PAGO, type = input_plot[["typeplot"]]) 
  if (any(series == "RAP_PAGO")){
    if(input_plot[["typeplot"]] == "areaspline"){
        hc <- hc %>% hc_add_series(name = "RAP PAGO", data = data$RAP_PAGO, type = "spline")
      } else {
        hc <- hc %>% hc_add_series(name = "RAP PAGO", data = data$RAP_PAGO, type = input_plot[["typeplot"]])
      }
    }
  if (any(series == "TOTAL"))   hc <- hc %>% hc_add_series(name = "TOTAL", data = data$TOTAL, type = input_plot[["typeplot"]])
  if (any(series == "DOCENTE")) hc <- hc %>% hc_add_series(name = "DOCENTES", data = data$DOCENTE, type = input_plot[["typeplot"]])
  if (any(series == "TECNICO")) hc <- hc %>% hc_add_series(name = "TÉCNICOS", data = data$TECNICO, type =  input_plot[["typeplot"]])
  if(input_plot[["dimension"]] == TRUE){
    hc <- hc %>% 
      hc_chart(options3d = list(enabled = input_plot[["dimension"]], beta = 10, alpha = 10))
    }
  if (input_plot[["groupplot"]] != FALSE) {
    hc <- hc %>% 
      hc_plotOptions(series = list(stacking = input_plot[["groupplot"]]))
  }
  if (input_plot[["groupplot"]] == "percent"){
    hc <- hc %>% hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')
  }
  if (!missing(credits)){
    href <- ifelse(credits == "Portal da Transparência", "http://portaltransparencia.gov.br/download-de-dados","")
    hc <- hc %>% hc_credits(enabled = TRUE, text = paste("Fonte:", credits), href = href)
  }
  hc
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













