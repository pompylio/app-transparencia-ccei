require(dplyr)
require(tidyverse)
require(reshape2)

# create data base --------------------------------------------------------
files <- list.files(path = "data",pattern = "Despesas.csv",full.names = TRUE)
col_names <- c("LANCAMENTO","CO_ORG_SUP","NO_ORC_SUP","CO_ORG_SUB","NO_ORG_SUB","CO_UG","NO_UG","CO_GE","NO_GE",
               "CO_UO","NO_UO","CO_FUNCAO","NO_FUNCAO","CO_SUBFUNCAO","NO_SUBFUNCAO","CO_PROGRAMA","NO_PROGRAMA",
               "CO_ACAO","NO_ACAO","CO_PROGRAMA_GOV","NO_PROGRAMA_GOV","CO_GD","NO_GD","CO_ED","NO_ED","CO_MD","NO_MD",
               "EMPENHADO","LIQUIDADO","PAGO","RAP_INSCRITO","RAP_CANCELADO","RAP_PAGO")
col_class <- c(rep("character",27),rep("numeric",6))
for(i in 1:length(files)){
  db <- read.csv2(files[i],
                  header = TRUE,
                  stringsAsFactors = FALSE, 
                  fileEncoding = "Latin1",
                  col.names = col_names,nrows = 100000,
                  colClasses = "character",
                  comment.char = "")
  assign(paste0("Despesas",substr(files[i],6,11)),db)
  assign(paste0("class",substr(files[i],6,11)),mapply(db,FUN = function(x){class(x)}))
}
colclass <- cbind(class201401,class201402,class201403,class201404,class201405,class201406,class201407,class201408,class201409,class201410,class201411,class201412,
                  class201501,class201502,class201503,class201504,class201505,class201506,class201507,class201508,class201509,class201510,class201511,class201512,
                  class201601,class201602,class201603,class201604,class201605,class201606,class201607,class201608,class201609,class201610,class201611,class201612,
                  class201701,class201702,class201703,class201704,class201705,class201706,class201707,class201708,class201709,class201710,class201711,class201712,
                  class201801,class201802,class201803,class201804,class201805,class201806,class201807)
db <- bind_rows(Despesas201401,Despesas201402,Despesas201403,Despesas201404,Despesas201405,Despesas201406,Despesas201407,Despesas201408,Despesas201409,Despesas201410,Despesas201411,Despesas201412,
                Despesas201501,Despesas201502,Despesas201503,Despesas201504,Despesas201505,Despesas201506,Despesas201507,Despesas201508,Despesas201509,Despesas201510,Despesas201511,Despesas201512,
                Despesas201601,Despesas201602,Despesas201603,Despesas201604,Despesas201605,Despesas201606,Despesas201607,Despesas201608,Despesas201609,Despesas201610,Despesas201611,Despesas201612,
                Despesas201701,Despesas201702,Despesas201703,Despesas201704,Despesas201705,Despesas201706,Despesas201707,Despesas201708,Despesas201709,Despesas201710,Despesas201711,Despesas201712,
                Despesas201801,Despesas201802,Despesas201803,Despesas201804,Despesas201805,Despesas201806,Despesas201807)
rm(list = c(ls(pattern = "class"),ls(pattern = "Despesas")))
ano <- c("2014","2015","2016","2017","2018")
for(i in 1:length(ano)){
  db1 <- db[substr(db$LANCAMENTO,1,4) == ano[i] & db$CO_ORG_SUB == "26428",]
  saveRDS(db1,paste0("data/despesas_",ano[i],".rds"))
  assign(paste0("des_",ano[i]),db1)
  rm(db1)
}
# import data -------------------------------------------------------------
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
db <- import_multi_files(local = "data",file = "despesas")
db$SG_UG <- ifelse(grepl(pattern = "REI",x = db$NO_UG),"REITORIA",
            ifelse(grepl(pattern = "BRA",x = db$NO_UG),"CBRA",
            ifelse(grepl(pattern = "CEI",x = db$NO_UG),"CCEI",
            ifelse(grepl(pattern = "CEN",x = db$NO_UG),"CTGC",
            ifelse(grepl(pattern = "EST",x = db$NO_UG),"CEST",
            ifelse(grepl(pattern = "GAM",x = db$NO_UG),"CGAM",
            ifelse(grepl(pattern = "NOR",x = db$NO_UG),"CTAG",
            ifelse(grepl(pattern = "PLA",x = db$NO_UG),"CPLA",
            ifelse(grepl(pattern = "SAM",x = db$NO_UG),"CSAM",
            ifelse(grepl(pattern = "RIA",x = db$NO_UG),"CRFI",
            ifelse(grepl(pattern = "SEB",x = db$NO_UG),"CSSB",
            ifelse(grepl(pattern = "REC",x = db$NO_UG),"CREM",""))))))))))))
db[,c("EMPENHADO","LIQUIDADO","PAGO","RAP_INSCRITO","RAP_CANCELADO","RAP_PAGO")] <- 
  apply(db[,c("EMPENHADO","LIQUIDADO","PAGO","RAP_INSCRITO","RAP_CANCELADO","RAP_PAGO")], 2, FUN = function(z){as.numeric(as.character(gsub(pattern = ",",replacement = ".",x = z)))})





