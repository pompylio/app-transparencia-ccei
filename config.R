if(!dir.exists("security")){
  dir.create("security")
}
if(!file.exists("security/googledrive_token.rds")){
  require(googledrive)
  googledrive_token <- drive_auth(reset = TRUE)
  saveRDS(googledrive_token, "security/googledrive_token.rds")
}
if(!file.exists("security/googlesheets_token.rds")){
  require(googlesheets)
  options(httr_oob_default=TRUE)
  googlesheets_token <- gs_auth(new_user = TRUE)
  saveRDS(googlesheets_token, "security/googlesheets_token.rds")
}
if(!dir.exists("data")){
  dir.create("data")
}
drive_auth("security/googledrive_token.rds")
RData <- drive_find(pattern =  "RData", type = "folder")
RData <- drive_ls(as_id(RData$id))
RData <- RData[RData$name == "portransp-panel", ]
RData <- drive_ls(as_id(RData$id))
for(i in 1:nrow(RData)){
  path <- paste0("data/", RData$name[i])
  if(!file.exists(path)){
    drive_download(file = as_id(RData$id[i]), path = path)
  } else {
    next()
  }
}
