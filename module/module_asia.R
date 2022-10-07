########## Legende
# mf_ui   module for ui
  # m_name  namespacefor module
  # mf_s_    module for server
  # m_id_ module id


  # UI:     mf_ui_asia("m_name_asia")
  # Server: callModule(mf_s_asia, "m_name_asia")
  # Global: source("module/module_asia.R")

  mf_ui_asia <- function(id) {
  ns <- NS(id)
  tagList(
    
    mf_ui_Sidebar(ns("sbarAsia")),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Asia",
                 h1(""),
                 mf_ui_ReviewCell(ns("Asia")),
                 boxer(mf_ui_rsstable(ns("Asia1")),'The Economist', collapd = TRUE),
                 boxer(mf_ui_rsstable(ns("Asia2")),'Google News')
        ),
        tabPanel("China",
                 h1(""),
                 mf_ui_ReviewCell(ns("China")),
                 boxer(mf_ui_rsstable(ns("China1")),'The Economist', collapd = TRUE),
                 boxer(mf_ui_rsstable(ns("China2")),'Google News')
        )
      ), width = 10
      
    )
    
    
  )
  }



  mf_s_asia <-
  function(input,
  output,
  session,
  trickleVal) {
    pagnum <- callModule(mf_s_Sidebar,"sbarAsia", trickleValue=trickleVal)
    
    rowsAsia <- reactive({
      a <- Economist()
      b <- GNews()
      selectedR <- list(Economist=as.data.frame(a),
                        GNews=as.data.frame(b))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewAsia <- callModule(mf_s_ReviewCell,
                           "Asia",
                           reactives=trickleVal,
                           domains=rowsAsia)
    
    Economist <- callModule(mf_s_rsstable,
                            "Asia1",
                            datTab=EconomistAsia,
                            sideInfo=pagnum
    )
    GNews <- callModule(mf_s_rsstable,
                        "Asia2",
                        datTab=GNewsAsia,
                        sideInfo=pagnum
    )
    
    rowsChina <- reactive({
      a <- EconomistChin()
      b <- GNewsChin()
      selectedR <- list(Economist=as.data.frame(a),
                        GNews=as.data.frame(b))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewChina <- callModule(mf_s_ReviewCell,
                             "China",
                             reactives=trickleVal,
                             domains=rowsChina)
    
    EconomistChin <- callModule(mf_s_rsstable,
                        "China1",
                        datTab=EconomistChina,
                        sideInfo=pagnum
    )
    GNewsChin <- callModule(mf_s_rsstable,
                        "China2",
                        datTab=GNewsChina,
                        sideInfo=pagnum
    )
  }
