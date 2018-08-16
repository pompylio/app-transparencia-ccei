source("function/function.R",encoding = "UTF-8")

list_files <- data.frame(files = list.files(path = "data/orcamento",full.names = TRUE),stringsAsFactors = FALSE)
list_files$date = as.numeric(gsub("[A-z]|\\/|\\-|\\.","",list.files(path = "data/orcamento",full.names = TRUE)))
list_files$month <- substr(list_files$date,5,6)
list_files <- aggregate(files ~ month, data = list_files, max)

for(i in 1:nrow(list_files)){
  db <- extract_orc(file = list_files$files[i])
  assign(paste0("dbOrc_",gsub(pattern = "-",replacement = "",unique(db$REFERENCIA))),db)
}
dbOrc <-do.call("rbind", lapply(ls(pattern = "dbOrc"),get))
saveRDS(dbOrc, "data/orcamento.rds")

orc_2016 <- dbOrc[dbOrc$ANO == 2016 & dbOrc$REFERENCIA == max(unique(dbOrc$REFERENCIA)),]
saveRDS(orc_2016,"data/orc2016.rds")
