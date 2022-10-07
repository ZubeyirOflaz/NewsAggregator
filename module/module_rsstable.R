########## Legende
# mf_ui   module for ui
  # m_name  namespacefor module
  # mf_s_    module for server
  # m_id_ module id


  # UI:     mf_ui_rsstable("m_name_rsstable")
  # Server: callModule(mf_s_rsstable, "m_name_rsstable")
  # Global: source("module/module_rsstable.R")

  mf_ui_rsstable <- function(id) {
  ns <- NS(id)
  tagList(
  DT::dataTableOutput(ns("mytable"))
  )
  }



  mf_s_rsstable <-
  function(input,
  output,
  session,
  datTab,
  sideInfo) 
    {
###Gets the slider information as an input and outputs a reactive that controls pagelength via the slider
 pageLeng <- reactive({
   b <- sideInfo$slider()
   return(b)
 })   
    
 ###Creates the reactive datatable that is supplied as an output from datatable and slidervalue inputs
    tableType1 <- reactive(
      {
      a <- datatable(datTab,
                     escape=FALSE,
                     extensions = 'Buttons',
                     options = list(dom = 'Bftp',
                               buttons = I('colvis'), 
                               columnDefs = list(list(className = 'dt-center', targets = 1:2)),
                               pageLength = pageLeng(),
                               lengthMenu = c(5, 10, 15, 20)
      ))
      return(a)
    }
    )
    
   #
    output$mytable <- renderDataTable(tableType1()
                                      #datatable(datTab,
                                      #          escape=FALSE,
                                      #          extensions = 'Buttons',
                                      #          options = list(dom = 'Bftp',
                                      #                         buttons = I('colvis'), 
                                      #                         columnDefs = list(list(className = 'dt-center', targets = 1:2)),
                                      #                         pageLength = pageLeng(),
                                      #                         lengthMenu = c(5, 10, 15, 20)))
    )
    myrows <- reactive({
      theRows <- datTab[input$mytable_rows_selected,]
      return(theRows)
    })
return(myrows)
  }
