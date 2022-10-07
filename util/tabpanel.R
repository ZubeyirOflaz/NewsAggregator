tabPanel_UI <- function(name,id){
  #ns = NS(id)
  tabPanel(name,
           sidebarPanel(
             fileInput("file", "File input:"),
             textInput("txt", "Text input:", "general"),
             sliderInput("slider", "Slider input:", 1, 100, 30),
             tags$h5("Deafult actionButton:"),
             actionButton("action", "Search"),
             
             tags$h5("actionButton with CSS class:"),
             actionButton("action2", "Action button", class = "btn-primary")
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Tab 1",
#                        h4("Table"),
#                        tableOutput("table"),
#                        h4("Verbatim text output"),
                        DT::dataTableOutput("Gnews"),
#                        h1("Header 1"),
#                        h2("Header 2"),
                        h3("Header 3"),
                        h4("Header 4"),
                        h5("Header 5")
               ),
               tabPanel("Overview", "This panel is intentionally left blank"),
               tabPanel("International", "This panel is intentionally left blank")
             )))
}
#display <- function(input, output, session) {
#  numshown <- input$slider
#  observeEvent(input$slider, {numShown <- input$slider})
#  output$out <- renderText({
#    count()
#  })
#  count
#}
server = function(input, output,session) {
  output$GNews <- renderDataTable(NewGNewsInternational, escape=FALSE, extensions = 'Buttons',
                                  options = list(dom = 'Bftp',buttons = I('colvis'), columnDefs = list(list(className = 'dt-center', targets = 1:2)),
                                                 pageLength = 5,
                                                 lengthMenu = c(5, 10, 15, 20)
                                  ))
}
