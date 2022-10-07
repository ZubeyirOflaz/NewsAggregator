#
# This web application aims to provide a centralized way of tracking the news and developments
# number of subjects, and give a way to compare, classify and archive news articles along with
# the news sources published the article. In order to achieve this the app has 4 functionalities
#
# 1. Web pages that present the news feeds from selected sources  according to subject and source category
# 2. Custom searches in Google News and Trends, and provision of related searhes for quick and broad search of topics
# 3. Presenting the distribution of news companies and current news topics to provide analysis
# 4. Providing a toolbar through which the user can classify a certain news article along with the news
#  source according to how the news is relayed
# 
# Created and being maintained by Zubeyir Oflaz; 
# e-mail: zubeyir.oflaz(at)gmail.com
#
# 
# libraries ---------------------------------------------------------------
library(DT)
library(tidyverse)
library(rlang)
library(rvest)
library(xml2)
library(urltools)
library(xlsx)
library(tidyRSS)
library(feedeR)
library(gtrendsR)
library(ggpmisc)
library(ggplot2)
library(ggthemes)
library(highcharter)
library(waiter)
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(tm)
library(wordcloud2)
# Modules, CSS and Scripts -----------------------------------------------------------------
source("util/tabpanel.R")
source("util/StylingFunctions.R")
source("util/GTrendFunctions.R")
source("util/NewsSourceObserve.R")
source("util/module_apprehensionFunctions.R")
source("util/tableShow.R")
source("util/editingStaff.R")
source("util/feedFest.R")
source("util/KeywordExtract.R")
source("module/module_createModule.R")
source("module/module_Sidebar.R")
source("module/module_rsstable.R")
source("module/module_FiletoTab.R")
source("module/module_Int.R")
source("module/module_SandT.R")
source("module/module_Europe.R")
source("module/module_TheAmericas.R")
source("module/module_asia.R")
source("module/module_FandE.R")
source("module/module_BandA.R")
source("module/module_Business.R")
source("module/module_MEA.R")
source("module/module_NewsSearch.R")
source("module/module_FileSearch.R")
source("module/module_GTrends.R")
source("module/module_ReviewCell.R")
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"
includeCSS("css/styles.css")

# Global Variables and UI -------------------------------------------------
PAGE_TITLE <- "NewsDock"
PAGE_THEME <- "flatly"


SourceBook <- read.csv2('data/SourceBook.csv')
CountryCodes <- read.csv('data/country-codes.csv')
ui = ( 
  dashboardPage
       (
    dashboardHeader(disable = TRUE),
    
    dashboardSidebar(disable = TRUE),
    dashboardBody
           (
             ####Javascript Calls and style sources####
             use_waiter(),
             useShinyjs(),
             tags$head(includeScript("js/enter_button.js")),
             singleton(tags$style(includeCSS("css/customized.css"))),

    navbarPage
               (windowTitle= PAGE_TITLE,
                title=div(img(src="Soul_of_a_Seasoned_Warrior.png",height = 100,width = 100,style = "margin:-100px -20px"),'News Dock'),
                id="GitGud",
        ####Theme Select####
        theme=shinytheme(PAGE_THEME),
                 
        ####Ancient Code####
#        tabPanel("International",
#sidebarPanel(
#        fileInput("file", "File input:"),
#        textInput("txt", "Text input:", "general"),
#        sliderInput("slider", "Slider input:", 1, 100, 10),
#        tags$h5("Deafult actionButton:"),
#        actionButton("action3", "Search"),
#        
#        tags$h5("actionButton with CSS class:"),
#        actionButton("action2", "Action button", class = "btn-primary"),
#        width = 2
#),
#mainPanel(useShinyjs(),
#              h4("Table"),
#              tableOutput("table"),
#              verbatimTextOutput("txtout"),
#              h2("The Economist"),
#              DT::dataTableOutput("mytable"),
#              h2("Google News"),
#              dataTableOutput("mytable2"),
#      width = 10
# 
#         )
#),
###Tabs of the Application####

tabPanel('International',mf_ui_Int(id = 'test')),
tabPanel('Science and Technology',mf_ui_SandT(id = 'ST')),
tabPanel("MidEast&Africa", mf_ui_MEA(id='MeAfrica')),
tabPanel('Europe',mf_ui_Europe(id = 'Europer')),
tabPanel("The Americas", mf_ui_TheAmericas(id = 'TheAmericas1')),
tabPanel('Asia',mf_ui_asia(id = 'Asia')),
tabPanel("Books and Arts",  mf_ui_BandA(id='BandArts')),
navbarMenu("More",
           tabPanel("Finance and Economics", mf_ui_FandE(id = 'FandE1')),
           tabPanel("Business", mf_ui_Business(id='Business')),
           tabPanel("GNews Search",value = "GNewsSearch",mf_ui_NewsSearch(id='pathfinder')),
           tabPanel("File Cascade",value = "cascade", mf_ui_FileSearch(id='fs')),
           tabPanel("Google Trends",value = "lens", mf_ui_GTrends(id='aperture'))
)
)
)
)
)

# Server ------------------------------------------------------------------
server = function(input, output,session) {
####Customization of elements according to the theme selection made######
o <- observe({ html = show_waiter(tagList(
  spin_double_bounce(),
  span("Hello Zubeyir", style = "color:white;"),
  br(),
  span("Query, Queue, Repeat", style = "color:white;")),logo = "Soul_of_a_Seasoned_Warrior.png")
  
  feedRefresh()
  #featured <- list()
  #res <- NULL
  #try(featured$article <- getFeaturedArticle())
  #if(!is.null(featured$article)) {
  #featured$image <- getFeaturedImage()
  #rv$featuredContent <- featured
  #rv$redditPoetry <- redditFeedEdit('https://www.reddit.com/r/Poetry/.rss?format=xml')
  #rv$redditRandom1 <- redditFeedEdit('https://www.reddit.com/r/Random/.rss?format=xml')
  #rv$redditRandom2 <- redditFeedEdit('https://www.reddit.com/r/Random/.rss?format=xml')}
  hide_waiter()
  delay(10,o$destroy())
  },priority = 100)
  

####The reactive values that are exchanged with modules to be modified#####
  rv <- reactiveValues(  ####Dynamic Tabs reactives
                         fileSelected=FALSE,##File selection check
                         xlsxPath='', #file selection
                         ####Google News reactives
                         aButton=TRUE, #First action Button
                         GNewsSearch=' ',
                         GNN=data.frame(),#GNews custom dataframe
                         GNQueried=FALSE,#Check if GNews query is requested
                         #### Google Trends reactives
                         aButton2=TRUE,
                         RenderTrends=TRUE,
                         DateSelect='today+5-y',
                         GTrendsSearch='Trends',
                         CountryISO=CCodes(),
                         Geolocation='',
                         location ='',
                         gsopce='web',
                         GtrendsList=list(),
                         interestPlotted=list(),
                         interestInterpolated=list(),
                         interestGeolocation=list(),
                         
                         ####Reset protocol reactive
                         resetProtocol=TRUE, #reset the system
                         
                         ####Data fetched during the initialization
                         featuredContent=list(),
                         redditPoetry=data.frame(),
                         redditRandom1=data.frame(),
                         redditRandom2=data.frame(),
                         malevolent=readRDS('archive/badFaith.RData'),
                         evidence=readRDS('archive/evidenceArchive.RData'),
                         masterArchive=readRDS('archive/masterDomainFrame.RData')
                       )
 LM <- reactive({
     ltm <- rv
     return(ltm)
 })
 ####Sidebar observers####
    observeEvent(rv$fileSelected,{
      updateNavbarPage(session, inputId ="GitGud",selected = "cascade")
    },ignoreInit = TRUE)
    observeEvent(rv$aButton,{
      pgn <- shiny::Progress$new()
      pgn$set(message="Fetching feed",value = 0.2)
      link2 <- GNLinkCreator(rv$GNewsSearch)
      GNTable2 <- try(tidyfeed(link2))
      if(class(GNTable2)=="try-error"){
        pgn$close()
        sendSweetAlert(
          session = session,
          title = "Connection Error",
          text = paste0("NewsDock is currently unable to conduct the search. Please check",
                        "    your internet connection and configuration"),
          type = "error")
      }else{
        rv$GNN <- CombinedFeedEdit(GNTable2)
        pgn$close()
        updateNavbarPage(session, inputId ="GitGud",selected = "GNewsSearch")
      }
        
    },ignoreInit = TRUE)
    observeEvent(rv$aButton2,{
      pgt <- shiny::Progress$new()
      pgt$set(message="Obtaining Google Trends Data",value = 0.2)
      rv$GtrendsList <- try(gtrends(keyword = rv$GTrendsSearch,
                                geo = rv$Geolocation,
                                time = rv$DateSelect,
                                gprop = rv$gsopce))
      if(class(rv$GtrendsList) == "try-error"){
        pgt$close()
        sendSweetAlert(
          session = session,
          title = "Connection Error",
          text = paste0("NewsDock is currently unable to conduct the search. Please check",
                        "    your internet connection and configuration"),
          type = "error")
      }else{
      rv$RenderTrends <- !rv$RenderTrends
      pgt$set(message="Plotting the data",value = 0.8)
      rv$interestPlotted <- plotInterest(rv$GtrendsList)
      rv$interestInterpolated <- interpolated(rv$GtrendsList)
      if ("interest_by_country" %in% names(rv$GtrendsList)){
        rv$interestGeolocation <- highMapper(CCodeAppend(rv$GtrendsList))
        }
      pgt$close()
      updateNavbarPage(session, inputId ="GitGud",selected = "lens")}
    },ignoreInit = TRUE)
    observeEvent(rv$resetProtocol,{
     feedCheck <-  feedReset()
     if(feedCheck==FALSE){
       sendSweetAlert(
         session = session,
         title = "Connection Error",
         text = paste0("NewsDock is currently unable to fetch RSS feeds. Please check",
                 "    your internet connection and configuration"),
         type = "error")
     }else{
      print('Reload completed')
      session$reload()}
    },ignoreInit = TRUE)
    



# Module Calls ------------------------------------------------------------
        callModule(mf_s_Int,'test', trickleVal=rv)
        callModule(mf_s_SandT,'ST', trickleVal=rv)
        callModule(mf_s_Europe,'Europer', trickleVal=rv)
        callModule(mf_s_TheAmericas,'TheAmericas1', trickleVal=rv)

        callModule(mf_s_FandE,'FandE1', trickleVal=rv)
        callModule(mf_s_BandA,'BandArts', trickleVal=rv)
        callModule(mf_s_Business,'Business', trickleVal=rv)
        callModule(mf_s_MEA,'MeAfrica', trickleVal=rv)
        callModule(mf_s_asia,'Asia',trickleVal=rv)
        callModule(mf_s_NewsSearch,'pathfinder',trickleVal=LM())
        callModule(mf_s_FileSearch,'fs',trickleVal=rv)
        callModule(mf_s_GTrends,'aperture',trickleVal=rv)
        
        }

shinyApp(ui, server)
