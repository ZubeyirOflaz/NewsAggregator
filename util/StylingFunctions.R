########## Legende
# mf_ui   module for ui
  # m_name  namespacefor module
  # mf_s_    module for server
  # m_id_ module id


  # UI:     mf_ui_StylingFunctions("m_name_StylingFunctions")
  # Server: callModule(mf_s_StylingFunctions, "m_name_StylingFunctions")
  # Global: source("module/module_StylingFunctions.R")
###Function that  is used to put UI elements into stylized ShinyDashboard boxes
boxer <- function(boxoutput,header, collapd=F,coloring='success'){
#if (PAGE_THEME=='flatly'){coloring <- 'success'}
  boxedtable <-  box(boxoutput, solidHeader = T, collapsible = T, width = 12, collapsed = collapd,
                     title = header,status = coloring)
  return(boxedtable)
}

boxThemeSetter <- function(currentTheme){
  defaults <- formals(boxer)
  colorDefault <- 'success'
  if (currentTheme=='flatly') {colorDefault <- 'primary'}
  else if (currentTheme=='cosmo') {colorDefault <- 'success'}
  defaults$coloring <- colorDefault
  formals(boxer) <- defaults
}