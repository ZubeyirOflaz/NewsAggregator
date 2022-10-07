########## Legende
# mf_ui   module for ui
  # m_name  namespacefor module
  # mf_s_    module for server
  # m_id_ module id


  # UI:     mf_ui_TheAmericas("m_name_TheAmericas")
  # Server: callModule(mf_s_TheAmericas, "m_name_TheAmericas")
  # Global: source("module/module_TheAmericas.R")

  mf_ui_TheAmericas <- function(id) {
  ns <- NS(id)
  tagList(
    
    mf_ui_Sidebar(ns("SBarAmericas")),
    
    mainPanel(
      tabsetPanel(
        tabPanel("The Americas",
                 h1(""),
                 mf_ui_ReviewCell(ns("Americas")),
                 boxer(mf_ui_rsstable(ns("Americas1")),'The Economist', collapd = TRUE),
                 boxer(mf_ui_rsstable(ns("Americas2")),'Google News')
                ),
        tabPanel("United States",
                 h1(""),
                 mf_ui_ReviewCell(ns("US")),
                 boxer(mf_ui_rsstable(ns("US1")),'The Economist', collapd = TRUE),
                 boxer(mf_ui_rsstable(ns("US2")),'Google News')
        ),
        tabPanel("Brazil",
                 h1(""),
                 mf_ui_ReviewCell(ns("BR")),
                 boxer(mf_ui_rsstable(ns("BR1")),'The Economist', collapd = TRUE),
                 boxer(mf_ui_rsstable(ns("BR2")),'Google News')
        )
      ), width = 10
      
    )
    
    
  )
  }



  mf_s_TheAmericas <-
  function(input,
  output,
  session,
  trickleVal) {
    pagnum <- callModule(mf_s_Sidebar,"SBarAmericas", trickleValue=trickleVal)
    
    rowsAmericas <- reactive({
      a <- Economist()
      b <- GNews()
      selectedR <- list(Economist=as.data.frame(a),
                        GNews=as.data.frame(b))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewAmericas <- callModule(mf_s_ReviewCell,
                                "Americas",
                                reactives=trickleVal,
                                domains=rowsAmericas)
    
    Economist <- callModule(mf_s_rsstable,
                            "Americas1",
                            datTab=EconomistTheAmericas,
                            sideInfo=pagnum
    )
    GNews <- callModule(mf_s_rsstable,
                        "Americas2",
                        datTab=GNewsTheAmericas,
                        sideInfo=pagnum
    )
    
    rowsUS <- reactive({
      a <- EconomistUS()
      b <- GNewsUS()
      selectedR <- list(Economist=as.data.frame(a),
                        GNews=as.data.frame(b))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewUS <- callModule(mf_s_ReviewCell,
                                 "US",
                                 reactives=trickleVal,
                                 domains=rowsUS)
    
    EconomistUS <- callModule(mf_s_rsstable,
                        "US1",
                        datTab=EconomistUnitedStates,
                        sideInfo=pagnum
    )
    GNewsUS <- callModule(mf_s_rsstable,
                        "US2",
                        datTab=GNewsUnitedStates,
                        sideInfo=pagnum
    )
    rowsBR <- reactive({
      a <- EconomistBR()
      b <- GNewsBR()
      selectedR <- list(Economist=as.data.frame(a),
                        GNews=as.data.frame(b))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewBR <- callModule(mf_s_ReviewCell,
                           "BR",
                           reactives=trickleVal,
                           domains=rowsBR)
    
    EconomistBR <- callModule(mf_s_rsstable,
                              "BR1",
                              datTab=EconomistBrazil,
                              sideInfo=pagnum
    )
    GNewsBR <- callModule(mf_s_rsstable,
                          "BR2",
                          datTab=GNewsBrazil,
                          sideInfo=pagnum
    )
    
  }
