########## Legende
# mf_ui   module for ui
  # m_name  namespacefor module
  # mf_s_    module for server
  # m_id_ module id


  # UI:     mf_ui_Europe("m_name_Europe")
  # Server: callModule(mf_s_Europe, "m_name_Europe")
  # Global: source("module/module_Europe.R")

  mf_ui_Europe <- function(id) {
  ns <- NS(id)
  tagList(
    
    mf_ui_Sidebar(ns("Europe")),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Europe",
                 h1(""),
                 mf_ui_ReviewCell(ns("Europer")),
                 boxer(mf_ui_rsstable(ns("Europe1")),'The Economist', collapd = TRUE),
                 boxer(mf_ui_rsstable(ns("Europe2")),'Google News')
        ),
        tabPanel("Germany",
                 h1(""),
                 mf_ui_ReviewCell(ns("Germany")),
                 boxer(mf_ui_rsstable(ns("Germany1")),'The Economist', collapd = TRUE),
                 boxer(mf_ui_rsstable(ns("Germany2")),'Google News')
        )
      ), width = 10
      
    )
    
    
  )
  }



  mf_s_Europe <-
  function(input,
  output,
  session,
  trickleVal) {
    pagnum <- callModule(mf_s_Sidebar,"Europe", trickleValue=trickleVal)
    
    rowsEuroper <- reactive({
      a <- Economist()
      b <- GNews()
      selectedR <- list(Economist=as.data.frame(a),
                        GNews=as.data.frame(b))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewEuroper <- callModule(mf_s_ReviewCell,
                              "Europer",
                              reactives=trickleVal,
                              domains=rowsEuroper)
    
    Economist <- callModule(mf_s_rsstable,
                            "Europe1",
                            datTab=EconomistEurope,
                            sideInfo=pagnum
    )
    GNews <- callModule(mf_s_rsstable,
                        "Europe2",
                        datTab=GNewsEurope,
                        sideInfo=pagnum
    )
    
    
    rowsGermany <- reactive({
      a <- German1()
      b <- German2()
      selectedR <- list(Economist=as.data.frame(a),
                        GNews=as.data.frame(b))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewGermany <- callModule(mf_s_ReviewCell,
                                "Germany",
                                reactives=trickleVal,
                                domains=rowsGermany)
    
    German1 <- callModule(mf_s_rsstable,
                          "Germany1",
                          datTab=EconomistGermany,
                          sideInfo=pagnum
    )
    German2 <- callModule(mf_s_rsstable,
                          "Germany2",
                          datTab=GNewsGermany,
                          sideInfo=pagnum
    )
    
  }
