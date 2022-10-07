########## Legende
# mf_ui   module for ui
  # m_name  namespacefor module
  # mf_s_    module for server
  # m_id_ module id


  # UI:     mf_ui_Business("m_name_Business")
  # Server: callModule(mf_s_Business, "m_name_Business")
  # Global: source("module/module_Business.R")

  mf_ui_Business <- function(id) {
  ns <- NS(id)
  tagList(
    
    mf_ui_Sidebar(ns("Busines")),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Business",
                 h2(""),
                 mf_ui_ReviewCell(ns("Business")),
                 boxer(mf_ui_rsstable(ns("Busines1")),'The Economist', collapd = TRUE),
                 boxer(mf_ui_rsstable(ns("Busines3")),'Reuters'),
                 boxer(mf_ui_rsstable(ns("Busines2")),'Google News')
                 
        )
      ), width = 10
      
    )
    
    
  )
  }



  mf_s_Business <-
  function(input,
  output,
  session,
  trickleVal) {
    pagnum <- callModule(mf_s_Sidebar,"Busines", trickleValue=trickleVal)
    rowsBusiness <- reactive({
      a <- Economist()
      b <- GNews()
      c <- Reuters()
      selectedR <- list(Economist=as.data.frame(a),
                        GNews=as.data.frame(b),
                        Reuters=as.data.frame(c))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewBusiness <- callModule(mf_s_ReviewCell,
                              "Business",
                              reactives=trickleVal,
                              domains=rowsBusiness)
    
    Economist <- callModule(mf_s_rsstable,
                            "Busines1",
                            datTab=EconomistBusiness,
                            sideInfo=pagnum
    )
    GNews <- callModule(mf_s_rsstable,
                        "Busines2",
                        datTab=GNewsBusiness,
                        sideInfo=pagnum
    )
    Reuters <- callModule(mf_s_rsstable,
                          "Busines3",
                          datTab=ReutersBusiness,
                          sideInfo=pagnum
    )
    
  }
