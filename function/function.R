download_portal <- function(opendata,item,end_date = Sys.Date(),start_date = "2013-01-01",all = FALSE,unzip = FALSE,mode = "wb",...){
  # library -----------------------------------------------------------------
  require(RCurl)
  data <- c("1"="orcamento-despesa","2"="despesas","3"="despesas-execucao","4"="transferencias","5"="cpgf","6"="cpcc","7"="cpdc","8"="receitas","10"="licitacoes","11"="compras","12"="convenios","13"="bolsa-familia-pagamentos","14"="bolsa-familia-saques","15"="garantia-safra","16"="seguro-defeso","17"="peti","18"="servidores","19"="ceaf","20"="dirigentes","21"="ceis","22"="cepim","23"="cnep","24"="viagens","25"="imoveis-funcionais")
  # missing values ----------------------------------------------------------
  if(missing(opendata)) stop("missing 'opendata'. Valid 'opendata': ", paste(names(data),data,collapse = ", "))
  if(!any(as.character(opendata) == as.character(seq(1:25)))) stop("'opendata' invalid input.")
  item <- ifelse(!opendata %in% c("18","25"),"",ifelse(opendata == "18" & item == "1","_Servidores",ifelse(opendata == "18" & item == "2","_Militares",ifelse(opendata == "25" & item == "1","_MRE",ifelse(opendata == "25" & item == "2","_SPU",ifelse(opendata == "25" & item == "3", "_MD",ifelse(opendata == "25" & item == "4","_PR",NULL)))))))
  if(is.null(item)) stop("'item' input not found.")
  # select data -------------------------------------------------------------
  URL <- paste0("http://www.portaltransparencia.gov.br/download-de-dados/",data[opendata],"/")
  # function for loop -------------------------------------------------------
  fun <- function(by,unzip){
    date <- seq.Date(from = as.Date(start_date),to = as.Date(end_date),by = by)
    date <- unique(paste0(substr(date,1,4),if(by == "year"){""}else{substr(date,6,7)},if(by %in% c("year","month")){""}else{substr(date,9,10)}))
    link <- paste0(URL,date,item)
    n <- length(date)
    while(if(opendata %in% c("12","19","21","22","23")){url.exists(link) == FALSE}else{n > 0}){
      link <- paste0(URL,date[n],item)
      file <- paste0("data/",data[opendata],item,"-",date[n],".zip")
      try(expr = download.file(url = link,destfile = file,mode = mode,...),silent = TRUE)
      Sys.sleep(1)
      if(file.info(file)$size == 0){
        message("URL ",link," not found. Try reference: ",date[n-1])
        unlink(file)
      }else if(unzip == TRUE){
        unzip(zipfile = file,exdir = "data/",overwrite = TRUE)
        unlink(file)
      }
      n = n-1
    }
  }
  # apply function to download a data ---------------------------------------
  if(all == TRUE){
    if(opendata %in% c("1","8","24")){fun(by = "year")}
    if(opendata %in% c("3","4","5","6","7","10","11","13","14","15","16","17","18")){fun(by = "month")}
    if(opendata %in% c("2","25","12","19","21","22","23")){fun(by = "day")}
  }else{
    if(opendata %in% c("1","8","24")){date <- substr(end_date,1,4)}
    if(opendata %in% c("3","4","5","6","7","10","11","13","14","15","16","17","18")){date <- paste0(substr(end_date,1,4),substr(end_date,6,7))}
    if(opendata %in% c("2","25","12","19","21","22","23")){date <- paste0(substr(end_date,1,4),substr(end_date,6,7),substr(end_date,9,10))}
    link <- paste0(URL,date,item)
    file <- paste0("data/",data[opendata],item,"-",date,".zip")
    if(url.exists(link) == TRUE){
      download.file(url = link,destfile = file,mode = "wb",...)
      if(unzip == TRUE){
        unzip(zipfile = file,exdir = "data/",overwrite = TRUE)
        unlink(file)
      }
    }else{
      stop("URL not found.")
    }
  }
}

extract_orc <- function(file,col_names,reference){
  require(openxlsx)
  dbTitle <- read.xlsx(xlsxFile = if(missing(file)){file.choose()}else{file},sheet = 1,startRow = 1,rows = 1,rowNames = FALSE,colNames = TRUE,check.names = TRUE)
  dbTitle <- dbTitle[,c(16:ncol(dbTitle))]
  dbTitle <- gsub(pattern = "[.]",replacement = "_",x = colnames(dbTitle))
  db <- read.xlsx(xlsxFile = if(missing(file)){file.choose()}else{file},sheet = 1,startRow = 3,rowNames = FALSE,colNames = FALSE)
  if(missing(col_names)){
    col_names <- c("DATA","CO_UO","NO_UO","CO_UGE","NO_UGE","CO_UGR","NO_UGR","CO_CATEGORIA_ECONOMICA","NO_CATEGORIA_ECONOMICA","CO_GRUPO_DESPESA","NO_GRUPO_DESPESA","CO_MODALIDADE_APLICACAO","NO_MODALIDADE_APLICACAO","CO_ELEMENTO_DESPESA","NO_ELEMENTO_DESPESA","CO_SUBITEM","NO_SUBITEM","CO_ACAO_GOVERNO","NO_ACAO_GOVERNO","PTRES","CO_NATUREZA_DESPESA","NO_NATUREZA_DESPESA","CO_NATUREZA_DESPESA_DETALHADA","NO_NATUREZA_DESPESA_DETALHADA","CO_PLANO_INTERNO","NO_PLANO_INTERNO",dbTitle)}
  colnames(db) <- col_names
  db$CO_SUBITEM <-         ifelse(db$CO_SUBITEM %in% c("0","1","2","3","4","5","6","7","8","9"),paste0("0",db$CO_SUBITEM),db$CO_SUBITEM)
  db$ANO <-                substr(db$DATA,1,4)
  db$DATA <-               factor(db$DATA)
  db$CO_PLANO_INTERNO <-   gsub(pattern = "[-]",replacement = "",db$CO_PLANO_INTERNO)
  db$CO_UGR <-             ifelse(db$CO_UGR == "-8",db$CO_UGE,db$CO_UGR)
  db$NO_UGR <-             ifelse(db$NO_UGR == "SEM INFORMACAO",db$NO_UGE,db$NO_UGR)
  db$UO_SIGLA  <-          ifelse(db$CO_UO == "26428","IFB",ifelse(db$CO_UGR %in% c("152139","152140","152141","152142","152143","152144","152145","152146","152147","155145","155150","155151","158501","158143") |db$CO_UGE %in% c("152139","152140","152141","152142","152143","152144","152145","152146","152147","155145","155150","155151","158501","158143"),"IFB",db$NO_UO))
  db$UGE_SIGLA <-          ifelse(db$CO_UGE == "152139", "CGAM",ifelse(db$CO_UGE == "152140", "CTAG",ifelse(db$CO_UGE == "152141", "CSAM",ifelse(db$CO_UGE == "152142", "CBRA",ifelse(db$CO_UGE == "152143", "CTGC",ifelse(db$CO_UGE == "152144", "CSSB",ifelse(db$CO_UGE == "152145", "CCEI",ifelse(db$CO_UGE == "152146", "CEST",ifelse(db$CO_UGE == "152147", "CRFI",ifelse(db$CO_UGE == "155145", "CBRZ",ifelse(db$CO_UGE == "155150", "CCAN",ifelse(db$CO_UGE == "155151", "CSOB",ifelse(db$CO_UGE == "158501", "CPLA",ifelse(db$CO_UGE == "158143", "REITORIA",ifelse(db$CO_UGE %in% c("010090","030203"),"EXTERNO","OUTROS")))))))))))))))
  db$UGR_SIGLA <-          ifelse(db$CO_UGR == "152139", "CGAM",ifelse(db$CO_UGR == "152140", "CTAG",ifelse(db$CO_UGR == "152141", "CSAM",ifelse(db$CO_UGR == "152142", "CBRA",ifelse(db$CO_UGR == "152143", "CTGC",ifelse(db$CO_UGR == "152144", "CSSB",ifelse(db$CO_UGR == "152145", "CCEI",ifelse(db$CO_UGR == "152146", "CEST",ifelse(db$CO_UGR == "152147", "CRFI",ifelse(db$CO_UGR == "155145", "CBRZ",ifelse(db$CO_UGR == "155150", "CCAN",ifelse(db$CO_UGR == "155151", "CSOB",ifelse(db$CO_UGR == "158501", "CPLA",ifelse(db$CO_UGR == "158143", "REITORIA","OUTROS"))))))))))))))
  db$CLASSIFICACAO <-
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("14","33"),"DIARIAS E PASSAGENS",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("18","20"),"AUXILIO FINANCEIRO",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("30"),"MATERIAL DE CONSUMO",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("31"),"OUTROS MATERIAIS",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("32"),"OUTROS MATERIAIS",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("35"),"OUTROS SERVICOS",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("36"),"OUTROS SERVICOS",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("37"),"OUTROS SERVICOS",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("39"),"OUTROS SERVICOS",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("47"),"TRIBUTOS E OBRIGACOES",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("51"),"OBRAS INSTALACOES",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("52"),"MATERIAL PERMANENTE",""))))))))))))
  db$CLASSIFICACAO <-
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("30") & db$CO_SUBITEM %in% c("17","47") |
           db$CO_ELEMENTO_DESPESA %in% c("35") & db$CO_SUBITEM %in% c("04") |
           db$CO_ELEMENTO_DESPESA %in% c("36") & db$CO_SUBITEM %in% c("46") & db$CO_GRUPO_DESPESA %in% c("4") |
           db$CO_ELEMENTO_DESPESA %in% c("36") & db$CO_SUBITEM %in% c("54","57") |
           db$CO_ELEMENTO_DESPESA %in% c("37") & db$CO_SUBITEM %in% c("09","27","28") & db$CO_GRUPO_DESPESA %in% c("3") |
           db$CO_ELEMENTO_DESPESA %in% c("37") & db$CO_SUBITEM %in% c("92","93") & db$CO_GRUPO_DESPESA %in% c("4") |
           db$CO_ELEMENTO_DESPESA %in% c("39") & db$CO_SUBITEM %in% c("08","11","26","27","28","30","31","56") & db$CO_GRUPO_DESPESA %in% c("3") |
           db$CO_ELEMENTO_DESPESA %in% c("39") & db$CO_SUBITEM %in% c("57","97") |
           db$CO_ELEMENTO_DESPESA %in% c("39") & db$CO_SUBITEM %in% c("92","93","95") & db$CO_GRUPO_DESPESA %in% c("4") |
           db$CO_ELEMENTO_DESPESA %in% c("52") & db$CO_SUBITEM %in% c("35")& db$CO_GRUPO_DESPESA %in% c("4"),
           "TECNOLOGIA DA INFORMACAO",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("36") & db$CO_SUBITEM %in% c("07","26","35") |
           db$CO_ELEMENTO_DESPESA %in% c("37") & db$CO_SUBITEM %in% c("01","19","08") |
           db$CO_ELEMENTO_DESPESA %in% c("39") & db$CO_SUBITEM %in% c("79"),
           "APOIO ADMINISTRATIVO",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("36") & db$CO_SUBITEM %in% c("15") |
           db$CO_ELEMENTO_DESPESA %in% c("36") & db$CO_SUBITEM %in% c("16","17") & db$CO_GRUPO_DESPESA %in% c("3") |
           db$CO_ELEMENTO_DESPESA %in% c("39") & db$CO_SUBITEM %in% c("10","12","14") |
           db$CO_ELEMENTO_DESPESA %in% c("39") & db$CO_SUBITEM %in% c("13") & db$CO_GRUPO_DESPESA %in% c("4"),
           "LOCACAO DE BENS",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("36") & db$CO_SUBITEM %in% c("18","20","21","22") |
           db$CO_ELEMENTO_DESPESA %in% c("36") & db$CO_SUBITEM %in% c("19") & db$CO_GRUPO_DESPESA %in% c("4") |
           db$CO_ELEMENTO_DESPESA %in% c("37") & db$CO_SUBITEM %in% c("04","06") & db$CO_GRUPO_DESPESA %in% c("3") |
           db$CO_ELEMENTO_DESPESA %in% c("39") & db$CO_SUBITEM %in% c("18") & db$CO_GRUPO_DESPESA %in% c("4") |
           db$CO_ELEMENTO_DESPESA %in% c("39") & db$CO_SUBITEM %in% c("19") & db$CO_GRUPO_DESPESA %in% c("3") |
           db$CO_ELEMENTO_DESPESA %in% c("39") & db$CO_SUBITEM %in% c("16","17","20","21"),
           "MANUTENCAO E CONSERVACAO",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("36") & db$CO_SUBITEM %in% c("28") |
           db$CO_ELEMENTO_DESPESA %in% c("39") & db$CO_SUBITEM %in% c("48"),
           "SELECAO E TREINAMENTO",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("36") & db$CO_SUBITEM %in% c("19") & db$CO_GRUPO_DESPESA %in% c("3") |
           db$CO_ELEMENTO_DESPESA %in% c("37") & db$CO_SUBITEM %in% c("03")|
           db$CO_ELEMENTO_DESPESA %in% c("39") & db$CO_SUBITEM %in% c("77"),
           "VIGILANCIA OSTENSIVA",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("36") & db$CO_SUBITEM %in% c("25") |
           db$CO_ELEMENTO_DESPESA %in% c("37") & db$CO_SUBITEM %in% c("02") |
           db$CO_ELEMENTO_DESPESA %in% c("39") & db$CO_SUBITEM %in% c("78"),
           "LIMPEZA E CONSERVACAO",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("36") & db$CO_SUBITEM %in% c("27") |
           db$CO_ELEMENTO_DESPESA %in% c("36") & db$CO_SUBITEM %in% c("59","63") & db$CO_GRUPO_DESPESA %in% c("3")|
           db$CO_ELEMENTO_DESPESA %in% c("39") & db$CO_SUBITEM %in% c("47","58","59") |
           db$CO_ELEMENTO_DESPESA %in% c("39") & db$CO_SUBITEM %in% c("49","91","92","93") & db$CO_GRUPO_DESPESA %in% c("3"),
           "COMUNICACAO",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("39") & db$CO_SUBITEM %in% c("44"),
           "AGUA E ESGOTO",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("39") & db$CO_SUBITEM %in% c("90"),
           "COMUNICACAO OFICIAL",
    ifelse(db$CO_ELEMENTO_DESPESA %in% c("39") & db$CO_SUBITEM %in% c("43") |
           db$CO_ELEMENTO_DESPESA %in% c("47") & db$CO_SUBITEM %in% c("22"),
           "ENERGIA ELETRICA",
           db$CLASSIFICACAO)))))))))))
  if(missing(reference)){
    reference <- substr(x = file,nchar(file)-14,nchar(file)-5)}
  db$REFERENCIA <- reference
  return(db)
}

extract_redmine <- function(pool_connect,select_project,year_project,col_names,col_select,select_custom_values){
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

aca_sit_ger <- function(database,department,year){
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

aca_sit_res <- function(database,department,type,year){
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

ind_sit_ger <- function(database,department,col_select,year){
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

orc_cla_uni <- function(database,department,year,reference,melt,...){
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
