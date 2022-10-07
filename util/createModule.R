#################################################### only for developing: create module


createModule <- function(modulename){
  if(dir.exists("module") == F) dir.create('module') else print("Module exist")
  filename <- paste0("module/module_", modulename, ".R")
  file.create(filename)
  text <- '########## Legende
# mf_ui   module for ui
  # m_name  namespacefor module
  # mf_s_    module for server
  # m_id_ module id


  # UI:     mf_ui_modulename("m_name_modulename")
  # Server: callModule(mf_s_modulename, "m_name_modulename")
  # Global: source("module/module_modulename.R")

  mf_ui_modulename <- function(id) {
  ns <- NS(id)
  tagList(
  "Module modulename"

  )
  }



  mf_s_modulename <-
  function(input,
  output,
  session) {


  }'
  text <- gsub("modulename", modulename, text)
  
  
  
  fileConn<-file(filename)
  writeLines(text, fileConn)
  close(fileConn)
  
}
