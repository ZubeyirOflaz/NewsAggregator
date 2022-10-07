########## Legende
# mf_ui   module for ui
  # m_name  namespacefor module
  # mf_s_    module for server
  # m_id_ module id


  # UI:     mf_ui_FandE("m_name_FandE")
  # Server: callModule(mf_s_FandE, "m_name_FandE")
  # Global: source("module/module_FandE.R")

  mf_ui_FandE <- function(id) {
  ns <- NS(id)
  tagList(
    
    mf_ui_Sidebar(ns("sbarFandE")),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Finance and Economics",
                 h2(""),
                 mf_ui_ReviewCell(ns("FandE")),
                 boxer(mf_ui_rsstable(ns("FandE1")),'The Economist', collapd = TRUE),
                 boxer(mf_ui_rsstable(ns("FandE2")),'Google News')
        ),
        tabPanel("Overview", "This panel is intentionally left blank")
      ), width = 10
      
    )
    
    
  )
  }



  mf_s_FandE <-
  function(input,
  output,
  session,
  trickleVal) {
    pagnum <- callModule(mf_s_Sidebar,"sbarFandE", trickleValue=trickleVal)
    rowsFandE <- reactive({
      a <- Economist()
      b <- GNews()
      selectedR <- list(Economist=as.data.frame(a),
                        GNews=as.data.frame(b))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    
    reviewFandE <- callModule(mf_s_ReviewCell,
                                 "FandE",
                                 reactives=trickleVal,
                                 domains=rowsFandE)
    
    Economist <- callModule(mf_s_rsstable,
                            "FandE1",
                            datTab=EconomistFinanceandEconomics,
                            sideInfo=pagnum
    )
    GNews <- callModule(mf_s_rsstable,
                        "FandE2",
                        datTab=GNewsFinanceandEconomics,
                        sideInfo=pagnum
    )
    
    
  }
