########## Legende
# mf_ui   module for ui
  # m_name  namespacefor module
  # mf_s_    module for server
  # m_id_ module id


  # UI:     mf_ui_FileSearch("m_name_FileSearch")
  # Server: callModule(mf_s_FileSearch, "m_name_FileSearch")
  # Global: source("module/module_FileSearch.R")

  mf_ui_FileSearch <- function(id) {
  ns <- NS(id)
  tagList(
  mf_ui_Sidebar(ns('Kaleidescope')),

  mainPanel(
   # tabsetPanel(
   #   #tabPanel("International",
   #   #         br(),
              uiOutput(ns("tabsets"))
   #   #),
   #   #tabPanel("Overview", "This panel is intentionally left blank")
   # ), width = 10
    
  )
  
  
  
  )
  }



  mf_s_FileSearch <-
  function(input,
  output,
  session,
  trickleVal) {
    ns <- session$ns
    pagnum <- callModule(mf_s_Sidebar,"Kaleidescope", trickleValue=trickleVal)
    fs <- reactiveValues(tabs=list(),
                         databases=list(),
                         customEnv=environment())
    TabNam = list()
  observeEvent(trickleVal$fileSelected,{
    
    #print(trickleVal$xlsxPath)
    ##Get the xlsx sheet names to object called f
    f <- getTables(trickleVal$xlsxPath)
    TabNam <- getSInfo(trickleVal$xlsxPath)
    lapply(1:length(f), function(g){
      ##Read the indexed spreadsheet
     xll <- read.xlsx(trickleVal$xlsxPath,g)
     ##Acquire the Source and Category names of all variables to TabNam list
     fs$databases[[f[[g]]]] <- feedFetch_v1(xll,addNamespace = 'custom')
     list2env(fs$databases[[f[[g]]]], caller_env(2))
            lapply(1:length(xll[,1]),function(l){
              nam <- TabNam[[f[[g]]]]
       callModule(mf_s_rsstable,
                  nam[[l]],
                  datTab=get(paste0('custom',nam[l])),
                  sideInfo=pagnum
       )
       
     }
    )

     #write.xlsx(fs$databases[[f[[g]]]],file = "test",sheetName = f[[g]],append=T)
    }
    )
    ####Renders the tabs and boxes of the custom feeds
    for (i in 1:length(f)){
      nam <- TabNam[[f[[i]]]]
      #View(TabNam)
      xlp <- read.xlsx(trickleVal$xlsxPath,i)
     fs$tabs[i] <-   tagList(tabPanel(f[i],
                                      br(),
                                      lapply(1:length(xlp[,1]), function(a){
                                        boxer(mf_ui_rsstable(ns(nam[[a]])),nam[[a]])}),
                                      br())
                             
                             )
}
    output$tabsets <- renderUI(do.call(tabsetPanel,fs$tabs))
  },
  ignoreInit = T)

  }
