########## Legende
# mf_ui   module for ui
  # m_name  namespacefor module
  # mf_s_    module for server
  # m_id_ module id


  # UI:     mf_ui_ReviewCell("m_name_ReviewCell")
  # Server: callModule(mf_s_ReviewCell, "m_name_ReviewCell")
  # Global: source("module/module_ReviewCell.R")

  mf_ui_ReviewCell <- function(id) {
  ns <- NS(id)
  tagList(
uiOutput(ns("sources"))
  )
  }



  mf_s_ReviewCell <-
  function(input,
  output,
  session,
  reactives,
  domains) {
    ns <- session$ns
    
    uniqueDomains <- reactive({
      domains <- domains()
      domains2 <- domains
      req(if(length(domains)==0)isTruthy(FALSE) else isTruthy(TRUE))
      domainVector <- uniqueDList(findDomains(domains2))
      Sys.sleep(0.1)
      return(domainVector)
      
    })
    confirmationNotice <- function(sourceName,ListName){
      notice <- paste("Are you sure you want to add",sourceName,"to the",ListName,"list?")
      return(notice)
    }
    output$sources <-  renderUI(
      
      tagList(
        
        
        tags$head(
          tags$style(HTML('#examplary{background-color:#333e48}')),
          tags$style(HTML('#clickbait{background-color:#333e48}')),
          tags$style(HTML('#sloppy{background-color:#333e48}')),
          tags$style(HTML('#malicious{background-color:#333e48}'))
        ),
        br(),
        fluidRow(
          column(width = 8,
                 radioGroupButtons(ns("domains"),
                                   label = "domains selected",
                                   choices = uniqueDomains(),
                                   individual = TRUE,
                                   checkIcon = list(
                                     yes = tags$i(class = "fa fa-circle", 
                                                  style = "color: steelblue"),
                                     no = tags$i(class = "fa fa-circle-o", 
                                                 style = "color: steelblue"))
                 )
                 
                 ),
          #column(width = 1,
          #br(),
          #br(),
          #actionButton(ns("Confirmation"), "Confirm",style="color: #fff; 
          #                    background-color: #95a5a6; border-color: #95a5a6")),
          column(width = 4,
                 
                 
                 actionGroupButtons(inputIds = c(ns("exemplary"), ns("clickbait"), ns("sloppy"), ns("malicious")),
                                    labels = c("Examplary","Clickbait", "Sloppy", "Malicious"),
                                    status = "primary",fullwidth = TRUE),
                 br(),
                 br()
                 
          )
        )
      
    ))
    
####Confirmations of users decision    
    observeEvent(input$exemplary,{
      confirmSweetAlert(session,inputId = ns("conf1"),
                        title = "Confirm Decision",
                        type = "info",
                        closeOnClickOutside = TRUE,
                        text = confirmationNotice(input$domains,"examplary sources"))
    },ignoreInit = TRUE)
    observeEvent(input$clickbait,{
      confirmSweetAlert(session,inputId = ns("conf2"),
                        title = "Confirm Decision",
                        type = "info",
                        closeOnClickOutside = TRUE,
                        text = confirmationNotice(input$domains,"clickbait"))
    },ignoreInit = TRUE)
    observeEvent(input$sloppy,{
      confirmSweetAlert(session,inputId = ns("conf3"),
                        title = "Confirm Decision",
                        type = "warning",
                        closeOnClickOutside = TRUE,
                        text = confirmationNotice(input$domains,"bad reporting"))
    },ignoreInit = TRUE)
    observeEvent(input$malicious,{
      confirmSweetAlert(session,inputId = ns("conf4"),
                        title = "Confirm Decision",
                        danger_mode = TRUE,
                        type = "warning",
                        closeOnClickOutside = TRUE,
                        text = confirmationNotice(input$domains,"malevolent sources"))
    },ignoreInit = TRUE)
    ####Recording of the observation according to user's choices
    observeEvent(input$conf1, {
      if (input$conf1==TRUE){
        updateArchives(input$domains,'examplary')
        write_rds(domains(),"test.RDS")
        addEvidencetoArchive_v2(domains(),"examplary",input$domains)
        #saveRDS(addEvidencetoArchive(domains(),"examplary",input$domains),
        #        file = sprintf("archive/evidenceArchive/%s_%s_%s_%s.RData",
        #                      input$domains,
        #                      format(Sys.Date()),
        #                      "examplary",runif(1)))
        sendSweetAlert(session, title = "Success",
                       text = sprintf("%s's examplary reporting has been archived",input$domains),
                       type = "success",
                       html = FALSE)}}
      ,ignoreInit = TRUE)
    observeEvent(input$conf2, {
      if (input$conf2==TRUE){
        updateArchives(input$domains,'clickbait')
        addEvidencetoArchive_v2(domains(),"clickbait",input$domains)
       # saveRDS(addEvidencetoArchive(domains(),"clickbait",input$domains),
       #         file = sprintf("archive/evidenceArchive/%s_%s_%s_%s.RData",
       #                        input$domains,
       #                        format(Sys.Date()),
       #                        "clickbait",runif(1)))
        sendSweetAlert(session, title = "Success",
                       text = sprintf("%s's clickbaiting has been recorded",input$domains),
                       type = "success",
                       html = FALSE)}}
      ,ignoreInit = TRUE)
    observeEvent(input$conf3, {
      if (input$conf3==TRUE){
        updateArchives(input$domains,'badReporting')
        addEvidencetoArchive_v2(domains(),"badReporting",input$domains)
       # saveRDS(addEvidencetoArchive(domains(),"badReporting",input$domains),
       #         file = sprintf("archive/evidenceArchive/%s_%s_%s_%s.RData",
       #                        input$domains,
       #                        format(Sys.Date()),
       #                        "badReporting",runif(1)))
        sendSweetAlert(session, title = "Success",
                       text = sprintf("%s's bad reporting has been archived",input$domains),
                       type = "success",
                       html = FALSE)}}
      ,ignoreInit = TRUE)
    observeEvent(input$conf4, {
      if (input$conf4==TRUE){
        updateArchives(input$domains,'badFaith')
        addEvidencetoArchive_v2(domains(),"badFaith",input$domains)
        #saveRDS(addEvidencetoArchive(domains(),"badFaith",input$domains),
        #        file = sprintf("archive/evidenceArchive/%s_%s_%s_%s.RData",
        #                       input$domains,
        #                       format(Sys.Date()),
        #                       "badFaith",runif(1)))
        sendSweetAlert(session, title = "Success",
                       text = sprintf("%s's infraction has been recorded",input$domains),
                       type = "success",
                       html = FALSE)}}
      ,ignoreInit = TRUE)

  }
