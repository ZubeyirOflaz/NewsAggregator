sidebar_UI <- function(id){
  ns = NS(id)
  list(
    fileInput("file", "File input:"),
    textInput("txt", "Text input:", "general"),
    sliderInput("slider", "Slider input:", 1, 100, 30),
    tags$h5("Deafult actionButton:"),
    actionButton("action", "Search"),
    
    tags$h5("actionButton with CSS class:"),
    actionButton("action2", "Action button", class = "btn-primary")
  )
}