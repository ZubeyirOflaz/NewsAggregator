########## Legende
# mf_ui   module for ui
  # m_name  namespacefor module
  # mf_s_    module for server
  # m_id_ module id


  # UI:     mf_ui_MEA("m_name_MEA")
  # Server: callModule(mf_s_MEA, "m_name_MEA")
  # Global: source("module/module_MEA.R")

  mf_ui_MEA <- function(id) {
  ns <- NS(id)
  tagList(
    
    mf_ui_Sidebar(ns("MeA")),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Middle East and Africa",
                 h1(""),
                 mf_ui_ReviewCell(ns("MeAr")),
                 boxer(mf_ui_rsstable(ns("MeA1")),'The Economist', collapd = TRUE),
                 boxer(mf_ui_rsstable(ns("MeA2")),'Google News')

        ),
        tabPanel("Yemen",
                 h1(""),
                 mf_ui_ReviewCell(ns("Yemen")),
                 boxer(mf_ui_rsstable(ns("Yemen1")),'The Economist', collapd = TRUE),
                 boxer(mf_ui_rsstable(ns("Yemen2")),'Google News')
        ),
        tabPanel("Congo",
                 h1(""),
                 mf_ui_ReviewCell(ns("Congo")),
                 boxer(mf_ui_rsstable(ns("Congo1")),'The Economist', collapd = TRUE),
                 boxer(mf_ui_rsstable(ns("Congo2")),'Google News')
        ),
        tabPanel("Nigeria",
                 h1(""),
                 mf_ui_ReviewCell(ns("Nigeria")),
                 boxer(mf_ui_rsstable(ns("Nigeria1")),'The Economist', collapd = TRUE),
                 boxer(mf_ui_rsstable(ns("Nigeria2")),'Google News')
        )
      ), width = 10
      
    )
    
    
  )
  }



  mf_s_MEA <-
  function(input,
  output,
  session,
  trickleVal) {
    pagnum <- callModule(mf_s_Sidebar,"MeA", trickleValue=trickleVal)
    rowsMeAr <- reactive({
      a <- Economist()
      b <- GNews()
      selectedR <- list(Economist=as.data.frame(a),
                        GNews=as.data.frame(b),
                        )
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewMeAr <- callModule(mf_s_ReviewCell,
                             "MeAr",
                             reactives=trickleVal,
                             domains=rowsMeAr)
    
    Economist <- callModule(mf_s_rsstable,
                            "MeA1",
                            datTab=EconomistMiddleEastandAfrica,
                            sideInfo=pagnum
    )
    GNews <- callModule(mf_s_rsstable,
                        "MeA2",
                        datTab=GNewsMiddleEastandAfrica,
                        sideInfo=pagnum
    )
    
    rowsYemen <- reactive({
      a <- EconomistYeme()
      b <- GNewsYeme()
      selectedR <- list(Economist=as.data.frame(a),
                        GNews=as.data.frame(b))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewYemen <- callModule(mf_s_ReviewCell,
                             "Yemen",
                             reactives=trickleVal,
                             domains=rowsYemen)
    
    EconomistYeme <- callModule(mf_s_rsstable,
                            "Yemen1",
                            datTab=EconomistYemen,
                            sideInfo=pagnum
    )
    GNewsYeme <- callModule(mf_s_rsstable,
                        "Yemen2",
                        datTab=GNewsYemen,
                        sideInfo=pagnum
    )
    
    rowsCongo <- reactive({
      a <- EconomistCong()
      b <- GNewsCong()
      selectedR <- list(Economist=as.data.frame(a),
                        GNews=as.data.frame(b))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewCongo <- callModule(mf_s_ReviewCell,
                              "Congo",
                              reactives=trickleVal,
                              domains=rowsCongo)
    
    
    EconomistCong <- callModule(mf_s_rsstable,
                            "Congo1",
                            datTab=EconomistCongo,
                            sideInfo=pagnum
    )
    GNewsCong <- callModule(mf_s_rsstable,
                        "Congo2",
                        datTab=GNewsCongo,
                        sideInfo=pagnum
    )
    
    rowsNigeria <- reactive({
      a <- EconomistNigeri()
      b <- GNewsNigeri()
      selectedR <- list(Economist=as.data.frame(a),
                        GNews=as.data.frame(b))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewNigeria <- callModule(mf_s_ReviewCell,
                              "Nigeria",
                              reactives=trickleVal,
                              domains=rowsNigeria)
    
    EconomistNigeri <- callModule(mf_s_rsstable,
                            "Nigeria1",
                            datTab=EconomistNigeria,
                            sideInfo=pagnum
    )
    GNewsNigeri <- callModule(mf_s_rsstable,
                        "Nigeria2",
                        datTab=GNewsNigeria,
                        sideInfo=pagnum
    )
    return(pagnum)
    
  }
