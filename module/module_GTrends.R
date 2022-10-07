########## Legende
# mf_ui   module for ui
  # m_name  namespacefor module
  # mf_s_    module for server
  # m_id_ module id


  # UI:     mf_ui_GTrends("m_name_GTrends")
  # Server: callModule(mf_s_GTrends, "m_name_GTrends")
  # Global: source("module/module_GTrends.R")

  mf_ui_GTrends <- function(id) {
  ns <- NS(id)
  tagList(
    mf_ui_Sidebar(ns("leaf")),
    mainPanel(
              fluidRow(
                column(width = 4),
                column(width = 4,h3(textOutput(ns("Searched")))),
                column(width = 4)
                
              ),
              fluidRow(
                highchartOutput(ns("World"),height = "720",width = 'auto'),
                br(),
                column(width = 6,mf_ui_rsstable(ns("RT"))),
                column(width = 6,mf_ui_rsstable(ns("RQ")))),
                #plotOutput(ns("IOT")),
                plotOutput(ns("IOTI")),
              width = 10
                
              
              
              )

  )
  }



  mf_s_GTrends <-
  function(input,
  output,
  session,
  trickleVal) {
    ns <- session$ns
    pagnum <- callModule(mf_s_Sidebar,"leaf", trickleValue=trickleVal)
    
    GTR <- reactiveValues(
      IntOT =TRUE,
      IntW = TRUE,
      IntRT = TRUE,
      IntRQ = TRUE
    )
    
    
    
    
    
    
    
    observeEvent(trickleVal$RenderTrends,{
      if ("related_topics" %in% names(trickleVal$GtrendsList)) {
        RT <- callModule(mf_s_rsstable,
                       "RT",
                       datTab=RelTop(trickleVal$GtrendsList),
                       sideInfo=pagnum
        )
        }
      RQ <- callModule(mf_s_rsstable,
                       "RQ",
                       datTab=RelQuer(trickleVal$GtrendsList),
                       sideInfo=pagnum
      )
      #output$Searched <- renderText(paste(trickleVal$GTrendsSearch,'in',trickleVal$location))
      #output$IOT <- renderPlot({
      #    trickleVal$interestPlotted
      #}) 
      
      output$IOTI <- renderPlot({
        trickleVal$interestInterpolated
      }) 
      {GTR$IntRT <- !GTR$IntRT}
      delay(50,if ("interest_by_country" %in% names(trickleVal$GtrendsList)){GTR$IntW <- !GTR$IntW})
    },ignoreInit = TRUE)
    
    observeEvent(GTR$IntW,{
      output$World <- renderHighchart({
        trickleVal$interestGeolocation 
      }) 
    },ignoreInit = TRUE)


  }
