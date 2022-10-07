########## Legende
# mf_ui   module for ui
  # m_name  namespacefor module
  # mf_s_    module for server
  # m_id_ module id


  # UI:     mf_ui_Boilerplate("m_name_Boilerplate")
  # Server: callModule(mf_s_Boilerplate, "m_name_Boilerplate")
  # Global: source("module/module_Boilerplate.R")

  mf_ui_Boilerplate <- function(id) {
  ns <- NS(id)
  tagList(
    
    mf_ui_Sidebar(ns("#######")),
    
    mainPanel(
      tabsetPanel(
        tabPanel("#####",
                 h2("The Economist"),
                 mf_ui_rsstable(ns("#####")),
                 h2("Google News"),
                 mf_ui_rsstable(ns("#####"))
        ),
        tabPanel("Overview", "This panel is intentionally left blank")
      ), width = 10
      
    )
    
    
  )
  }



  mf_s_Boilerplate <-
  function(input,
  output,
  session) {
    pagnum <- callModule(mf_s_Sidebar,"######")
    Economist <- callModule(mf_s_rsstable,
                      "#####",
                      datTab='########',
                      sideInfo=pagnum
    )
    GNews <- callModule(mf_s_rsstable,
                        "#####",
                        datTab="#####",
                        sideInfo=pagnum
    )
    
    
  }
