########## Legende
# mf_ui   module for ui
  # m_name  namespacefor module
  # mf_s_    module for server
  # m_id_ module id


  # UI:     mf_ui_NewsSearch("m_name_NewsSearch")
  # Server: callModule(mf_s_NewsSearch, "m_name_NewsSearch")
  # Global: source("module/module_NewsSearch.R")

  mf_ui_NewsSearch <- function(id) {
  ns <- NS(id)
  tagList(
    
    mf_ui_Sidebar(ns("seeker")),
    
    mainPanel(
                 mf_ui_ReviewCell(ns("GNewsSearch")),
                 uiOutput(ns("myboxes")),
                 boxer(mf_ui_rsstable(ns("GNN")),'Google News', collapd = F),
                 boxer(mf_ui_rsstable(ns("GNN2")),'Secondary Search', collapd = F),
                 width = 10
      
    )
    
    
  )
  }



  mf_s_NewsSearch <-
  function(input,
  output,
  session,
  trickleVal) {
    ns <- session$ns
    pagnum <- callModule(mf_s_Sidebar,"seeker", trickleValue=trickleVal)
    
    switches <- reactiveValues(buttons=F,
                               RisingSearches = list(),
                               AddSearch=F,
                               SndTable=cars,
                               reviewModule=F,
                               table1=list(),
                               table2=list())
    
    
    
    
    
    observeEvent(trickleVal$GNewsSearch,{
      link <- GNLinkCreator(trickleVal$GNewsSearch)
      GNTable2 <- try(tidyfeed(link))
      if(class(GNTable2)=="try-error"){
        sendSweetAlert(
          session = session,
          title = "Connection Error",
          text = paste0("NewsDock is currently unable to conduct the search. Please check",
                        "    your internet connection and configuration"),
          type = "error")
      }else{
      trickleVal$GNN <- CombinedFeedEdit(GNTable2)
      print(isolate(link))}

    },
    ignoreInit = T
    )

    observeEvent(trickleVal$GNN,{
      switches$table1 <- callModule(mf_s_rsstable,
                        "GNN",
                        datTab=isolate(trickleVal$GNN),
                        sideInfo=pagnum)
      delay(300, trickleVal$GNQueried <- !trickleVal$GNQueried)
      },
      ignoreInit = T
      )
    ###Retreival of relevant rising Google searches and creation of input list from them
    observeEvent(trickleVal$GNQueried,{
      pgt2 <- shiny::Progress$new()
      pgt2$set(message="Requesting Related Topics",value = 0.2)
      GTR <- gtrends(keyword = trickleVal$GNewsSearch, geo = "", time = "now 7-d",
                               gprop = "news",
                               category = 0, hl = "en-US", low_search_volume = FALSE,
                               cookie_url = "http://trends.google.com/Cookies/NID", tz = 0,
                               onlyInterest = FALSE)
      RelatedTopics <- GTR[["related_topics"]]
      RelatedTopics <- filter(RelatedTopics,RelatedTopics$related_topics=="rising")
      TopicNames <- RelatedTopics$value
      switches$RisingSearches <- selectInput(ns('selected'),
                                             'Rising Related Searches',
                                             TopicNames)
     
      switches$buttons <- !switches$buttons
      pgt2$close()
    },
    ignoreInit = T
    )
    
    #Renders the list of related topics retrieved from Google Trends
    observeEvent(switches$buttons,{
      print(switches$buttons)
      output$myboxes <- renderUI(tagList(switches$RisingSearches))
      
    },
    ignoreInit = T
    ###Creates a data table from the combination of main and secondary search topics
    )
    observeEvent(input$selected,{
      searchTerm <- paste(c(trickleVal$GNewsSearch,input$selected),collapse = "+")
      print(input$selected)
      SLink <- GNLinkCreator(searchTerm)
      print(SLink)
      GNTable2 <- tidyfeed(SLink)
      switches$SndTable <- CombinedFeedEdit(GNTable2)
      delay(10, switches$AddSearch <- !switches$AddSearch)
    },
    ignoreInit = TRUE
    )
    observeEvent(switches$AddSearch,{
      #print(switches$SndTable)
      switches$table2 <- callModule(mf_s_rsstable,
                          "GNN2",
                          datTab=switches$SndTable,
                          sideInfo=pagnum)
      delay(50, switches$reviewModule <- !switches$reviewModule)
    },
    ignoreInit = T
    )
    
    observeEvent(switches$reviewModule, {
    GNewsRows <- reactive({
      a <- switches$table2()
      b <- switches$table1()
      selectedR <- list(secondSearch=as.data.frame(a),
                        PrimarySearch=as.data.frame(b))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    }
    )
    reviewNews <-  callModule(mf_s_ReviewCell,
                             "GNewsSearch",
                             reactives = trickleVal,
                             domains=GNewsRows)
    },
    ignoreInit = T)
    
    }
