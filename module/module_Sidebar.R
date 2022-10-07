########## Legende
# mf_ui   module for ui
  # m_name  namespacefor module
  # mf_s_    module for server
  # m_id_ module id


  # UI:     mf_ui_Sidebar("m_name_Sidebar")
  # Server: callModule(mf_s_Sidebar, "m_name_Sidebar")
  # Global: source("module/module_Sidebar.R")

  mf_ui_Sidebar <- function(id) {
  ns <- NS(id)
  tagList(
      sidebarPanel(
      ##Slider that Controls the pagelength
      tags$style(
        ".irs-bar {",
        "  border-color: transparent;",
        "  background-color: #02064a;",
        "}",
        ".irs-bar-edge {",
        "  border-color: transparent;",
        "  background-color: #02064a;",
        "}",
        ".irs-single {",
        "  background: #3b444b;",
        "}"
      ),
      sliderInput(ns("slider"), "Table Length", 0, 100, 10,step = 5),
      ##Custom GNews input and pageset controls
      textInput(ns("txt"), "Google News Search:", "Search Terms"),
                actionButton(ns("action"), "Search", style="color: #fff; background-color: #333e48; border-color: #2C3E50"),
      ##Google Trends controls
      textInput(ns("gtxt"), "Google Trends:", "Search Terms"),
      splitLayout(
      div(style="display:inline-block",
          selectInput(ns('selectDate'), 'Date Selections', c(Choose='',
                                                 'last Hour'='now 1-H',
                                                 'last Four Hours'='now 4-H',
                                                 'last Day'='now 1-d',
                                                 'last Week'='now 7-d',
                                                 'last Month'='today 1-m',
                                                 'last Year'='today 12-m',
                                                 'Since 2004'='all'),
                  selectize=FALSE)),
      div(style="display:inline-block",
          selectInput(ns('gprod'), 'Scope', c('Web'='web',
                                          'GNews'='news',
                                          'Youtube'='youtube'),
                  selectize=FALSE))),
      dateRangeInput(ns('rangeDate'),label = NULL,
                     start = "2009-06-26",
                     end = Sys.Date(),
                     min = "2005-01-01",
                     max = Sys.Date(),
                     format = "yy-mm-dd",
                     weekstart = 1),
      uiOutput(ns('geolocation')),
    # selectInput(ns('geolocation'),
    #             'GeolocationSelect',
    #             CCodes(),
    #             selectize=TRUE),
    # 
      div(style="display:inline-block;width:100%;text-align: center;float::center;",
      actionButton(ns("action2"), "Trends Search", style="color: #fff; background-color: #333e48; border-color: #2C3E50")),
    ####File input that creates dynamic feed retrieval
    fileInput(ns("file"), 
              "File input:", 
              accept = '.xlsx',
              placeholder = "Upload an xlsx file"),
    ##Requesting and updating all RSS feeds, reloading session
      tags$h4("Reload feeds, Reset session"),
      div(style="display:inline-block;width:100%;text-align: center;float::center;",
          actionButton(ns("actionDelta"),
                       label = "Reset", class="btn btn-warning", icon = icon("refresh"))),
      width = 2
      )
      )
  }



  mf_s_Sidebar <-
  function(input,
  output,
  session,
  trickleValue=NULL) {
    #Button enter/click javascript code
    ns <- session$ns
    shinyjs::runjs(paste0("EnterClick('", ns(""), "');"))
###Input reactive declarations####
    s <- reactive({
      a <- input$slider
      return(a)
    })
    v <- reactive({
      a <- input$action
      return(a)
    })
    textI <- reactive({
      a <- input$txt
      return(a)
    })
    x <- reactive({
      a <- input$file
      a <- a$datapath
      return(a)
    })
    observeEvent(input$file,{
      a <- input$file
      a <- a$datapath
      trickleValue$xlsxPath <- a 
      a <- getTables(a)
      trickleValue$fileSelected <- !trickleValue$fileSelected
    },
    ignoreInit = T)
    
    output$geolocation <- renderUI(selectInput(ns('geolocation2'),
                                               'GeolocationSelect',
                                               trickleValue$CountryISO[,1],
                                               selectize=TRUE),)
###Event Observers####    
    ###Functions to be executed when GNews action button is pressed
    observeEvent(input$action,{
      trickleValue$aButton <- !trickleValue$aButton
      #print(input$txt)
      trickleValue$GNewsSearch <- input$txt
      #print(trickleValue$aButton)
    },
    ignoreInit = T
    )
    
    ###GTrends Button Press
    observeEvent(input$action2,{
      trickleValue$aButton2 <- !trickleValue$aButton2
      trickleValue$GTrendsSearch <- input$gtxt
    },
    ignoreInit = T
    )
    ###Date selection observers for GTrends
    observeEvent(input$selectDate,{
      trickleValue$DateSelect <- input$selectDate
    },
    ignoreInit = T
    )
    observeEvent(input$rangeDate,{
      trickleValue$DateSelect <- paste(input$rangeDate,collapse = ' ')
    },
    ignoreInit = T
    )
    ### Location and Scope observers for GTrends
   observeEvent(input$geolocation2,{
     cTable <- trickleValue$CountryISO
     selectionnumber <- which(cTable$UNTERM.English.Short==input$geolocation2)
     trickleValue$geolocation <-paste0(cTable[[selectionnumber,2]])
     if(trickleValue$geolocation==''){
       trickleValue$location <- 'The World'
       } else{trickleValue$location <- cTable[[selectionnumber,1]]}
   },
   ignoreInit= T)
   observeEvent(input$gprod,{
     trickleValue$gscope <- input$gprod
   },ignoreInit= T)
    
    ###Reload of the system is initiated
    observeEvent(input$actionDelta,{
      trickleValue$resetProtocol <- !trickleValue$resetProtocol
      print("trickleValue$resetProtocol switched, reload procedure activated")
    },ignoreInit = T)

###Sidebar Returns####
    ###The returned values from the input elements
    return(
      list(
        slider = s,
        actionB = v,
        sheetPath = x
        )
    )
  }
