########## Legende
# mf_ui   module for ui
  # m_name  namespacefor module
  # mf_s_    module for server
  # m_id_ module id


  # UI:     mf_ui_Int("m_name_Int")
  # Server: callModule(mf_s_Int, "m_name_Int")
  # Global: source("module/module_Int.R")

  mf_ui_Int <- function(id) {
  ns <- NS(id)
  a <- id
  tagList(
mf_ui_Sidebar(ns("SidebarInt")),
  mainPanel(
    tabsetPanel(
      tabPanel("International",
               h1(""),
               mf_ui_ReviewCell(ns("IntReview")),
               boxer(mf_ui_rsstable(ns("Int1")),'The Economist', collapd = TRUE),
               boxer(mf_ui_rsstable(ns("Int2")),'Google News'),
               boxer(mf_ui_rsstable(ns("Int3")),'Reuters')
              ),
      tabPanel("Overview",
               h4("Google News International Domain Distribution"),
               highchartOutput(ns("GNInternational")),
               h4("Keywords"),
               wordcloud2Output(ns("GNewsWC"))
               
              
               )
    ), width = 10
    
  )
  )
  }



  mf_s_Int <-
  function(input,
  output,
  session,
  trickleVal) {
    pagnum <- callModule(mf_s_Sidebar,"SidebarInt", trickleValue=trickleVal)
    selectedRows <- reactive({
      a <- res()
      b <- GNews()
      selectedR <- list(economist=as.data.frame(a),
                        GNews=as.data.frame(b))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewInt <-  callModule(mf_s_ReviewCell,
               "IntReview",
               reactives = trickleVal,
               domains=selectedRows)
    
    res <- callModule(mf_s_rsstable,
                      "Int1",
                      datTab=EconomistInternational,
                      sideInfo=pagnum
                      )
   GNews <- callModule(mf_s_rsstable,
                       "Int2",
                       datTab=GNewsInternational,
                       sideInfo=pagnum
                       )
   if (exists('ReutersInternational')){
   reuters <- callModule(mf_s_rsstable,
                         "Int3",
                         datTab=ReutersInternational,
                         sideInfo=pagnum
   )}
   output$GNInternational <- renderHighchart2(sourceToTMap(GNewsInternational))
   output$GNTotal <- renderHighchart(mapMasterIndex(trickleVal$masterArchive))
   output$GNewsWC <- renderWordcloud2(wordcloud2(FormWordCloud(GNewsInternational),ellipticity = ".65"))
   
  }
