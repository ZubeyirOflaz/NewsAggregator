########## Legende
# mf_ui   module for ui
  # m_name  namespacefor module
  # mf_s_    module for server
  # m_id_ module id


  # UI:     mf_ui_BandA("m_name_BandA")
  # Server: callModule(mf_s_BandA, "m_name_BandA")
  # Global: source("module/module_BandA.R")

  mf_ui_BandA <- function(id) {
  ns <- NS(id)
  tagList(
    
    mf_ui_Sidebar(ns("BandA4")),
    
    mainPanel(
      tabsetPanel(
        tabPanel("BooksandArts",
                 h1(""),
                 mf_ui_ReviewCell(ns("BandA")),
                 boxer(mf_ui_rsstable(ns("BandA1")),'The Economist', collapd = TRUE),
                 boxer(mf_ui_rsstable(ns("BandA2")),'Reuters'),
                 boxer(mf_ui_rsstable(ns("BandA3")),'Google News')
                ),
        tabPanel("Overview", "This panel is intentionally left blank")
      ), width = 10
      
    )
    
    
  )
  }



  mf_s_BandA <-
  function(input,
  output,
  session,
  trickleVal) {
    pagnum <- callModule(mf_s_Sidebar,"BandA4", trickleValue=trickleVal)
    
    rowsBandA <- reactive({
      a <- Economist()
      b <- GNews()
      c <- Reuters()
      selectedR <- list(Economist=as.data.frame(a),
                        GNews=as.data.frame(b),
                        Reuters=as.data.frame(c))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewBandA <- callModule(mf_s_ReviewCell,
                             "BandA",
                             reactives=trickleVal,
                             domains=rowsBandA)
    
    Economist <- callModule(mf_s_rsstable,
                            "BandA1",
                            datTab=EconomistBooksandArts,
                            sideInfo=pagnum
    )
    GNews <- callModule(mf_s_rsstable,
                        "BandA3",
                        datTab=GNewsBooksandArts,
                        sideInfo=pagnum
    )
    Reuters <- callModule(mf_s_rsstable,
                        "BandA2",
                        datTab=ReutersBooksandArts,
                        sideInfo=pagnum
    )

  }
