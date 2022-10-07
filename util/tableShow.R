tableShow2 <- reactive({
  tableShow(tabName,
            pLength = input$slider)
})
tableShow <- function(tabName, pLength=10){
  renderDataTable({
    cat(pLength)
    datatable(tabName, escape=FALSE, extensions = 'Buttons',
              options = list(dom = 'Bftp',buttons = I('colvis'), columnDefs = list(list(className = 'dt-center', targets = 1:2)),
                             pageLength = (pLength),
                             #pageLength = 5,
                             lengthMenu = c(5, 10, 15, 20)
              ))
  }
  )
}