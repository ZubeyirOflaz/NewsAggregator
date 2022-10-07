########## Legende
# mf_ui   module for ui
  # m_name  namespacefor module
  # mf_s_    module for server
  # m_id_ module id


  # UI:     mf_ui_SandT("m_name_SandT")
  # Server: callModule(mf_s_SandT, "m_name_SandT")
  # Global: source("module/module_SandT.R")

  mf_ui_SandT <- function(id) {
  ns <- NS(id)
  tagList(
    
    mf_ui_Sidebar(ns("SidebarSandT")),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Science and Technology",
                 h1(""),
                 mf_ui_ReviewCell(ns("SciTech")),
                 boxer(mf_ui_rsstable(ns("NAT")),'Nature'),
                 boxer(mf_ui_rsstable(ns("SandT1")),'The Economist',collapd = T),
                 boxer(mf_ui_rsstable(ns("SandT2")),'Google News'),
                 boxer(mf_ui_rsstable(ns("SandT4")),'Nautilus'),
                 boxer(mf_ui_rsstable(ns("SandT5")),'MIT')
                 ),
        tabPanel("Machine Learning /AI",
                 h1(""),
                 mf_ui_ReviewCell(ns("AI")),
                 boxer(mf_ui_rsstable(ns("DL1")),'Google News'),
                 boxer(mf_ui_rsstable(ns("DL2")),'MIT'),
                 boxer(mf_ui_rsstable(ns("DL3")),'Cortana'),
                 boxer(mf_ui_rsstable(ns("DL4")),'Medium'),
                 img(src="BoardingPass_MyNameOnMars2020.png",height = 295,width = 720,style ="display: block; margin-left: auto; margin-right: auto;")
        ),
        tabPanel("Neurology",
                 h1(""),
                 mf_ui_ReviewCell(ns("Neuro")),
                 boxer(mf_ui_rsstable(ns("Neu1")),'Google News'),
                 boxer(mf_ui_rsstable(ns("Neu2")),'Neurology Journal'),
                 boxer(mf_ui_rsstable(ns("Neu3")),'Neurology Journal: Neuroimmunology & Neuroinflammation')
                 ),
        tabPanel("Nanotechnology",
                 h1(""),
                 mf_ui_ReviewCell(ns("Nano")),
                 boxer(mf_ui_rsstable(ns("Nan4")),'Google News'),
                 boxer(mf_ui_rsstable(ns("Nan1")),'Eureka Alert'),
                 boxer(mf_ui_rsstable(ns("Nan2")),'Nanowerk'),
                 boxer(mf_ui_rsstable(ns("Nan3")),'Nanowerk Spotlight')
                 ),
        tabPanel("Medworm",
                 h1(""),
                 mf_ui_ReviewCell(ns("Medworm")),
                 boxer(mf_ui_rsstable(ns("Med1")),'Material Science'),
                 boxer(mf_ui_rsstable(ns("Med4")),'Biomedical Engineering'),
                 boxer(mf_ui_rsstable(ns("Med2")),'Neurology'),
                 boxer(mf_ui_rsstable(ns("Med3")),'Neuroscience')
                 ),
        tabPanel("DIY",
                 h1(""),
                 mf_ui_ReviewCell(ns("make")),
                 boxer(mf_ui_rsstable(ns("DIY1")),'Makezine'),
                 boxer(mf_ui_rsstable(ns("DIY2")),'Maker Faire'),
                 boxer(mf_ui_rsstable(ns("DIY3")),'Hackaday')
                 ),
        tabPanel("MIT",
                 h1(""),
                 mf_ui_ReviewCell(ns("MIT")),
                 boxer(mf_ui_rsstable(ns("MIT1")),'Aeronautical and astronautical engineering'),
                 boxer(mf_ui_rsstable(ns("MIT2")),'Neuroscience, neurology, and cognitive sciences'),
                 boxer(mf_ui_rsstable(ns("MIT3")),'Making and the maker movement'),
                 boxer(mf_ui_rsstable(ns("MIT4")),'Materials science and engineering'),
                 boxer(mf_ui_rsstable(ns("MIT5")),'Mechanical engineering'),
                 boxer(mf_ui_rsstable(ns("MIT6")),'Nanoscience and nanotechnology')
                 ),
        tabPanel("Overview",
                 h4("GNews Science and Technology Domain Distribution"),
                 highchartOutput2(ns("DomainSandT")),
                 h4("GNews Machine Learning Domain Distribution"),
                 highchartOutput2(ns("DomainAI")),
                 h4("GNews Neurology Domain Distribution"),
                 highchartOutput2(ns("DomainNeuro")),
                 h4("GNews Nanotechnology Domain Distribution"),
                 highchartOutput2(ns("DomainNano")),
                 h4("Keywords"),
                 wordcloud2Output(ns("GNewsWC"))
                 
                 )
        
      
        ), width = 10
      
    )
    

  )
  }



  mf_s_SandT <-
  function(input,
  output,
  session,
  trickleVal) {
    pagnum <- callModule(mf_s_Sidebar,"SidebarSandT", trickleValue=trickleVal)
    rowsSciTech <- reactive({
      a <- res()
      b <- GNews()
      c <- Nature1()
      d <- Nautilus()
      e <- MIT23()
      selectedR <- list(economist=as.data.frame(a),
                        GNews=as.data.frame(b),
                        Nature=as.data.frame(c),
                        Nautilus=as.data.frame(d),
                        MIT=as.data.frame(e))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewSciTech <- callModule(mf_s_ReviewCell,
                                "SciTech",
                                reactives=trickleVal,
                                domains=rowsSciTech)
    res <- callModule(mf_s_rsstable,
                      "SandT1",
                      datTab=EconomistScienceandTechnology,
                      sideInfo=pagnum
    )
    GNews <- callModule(mf_s_rsstable,
                        "SandT2",
                        datTab=GNewsScienceandTechnology,
                        sideInfo=pagnum
    )
    Nature1 <- callModule(mf_s_rsstable,
                         "NAT",
                         datTab=NatureScienceandTechnology,
                         sideInfo=pagnum
    )
    Nautilus <- callModule(mf_s_rsstable,
                           "SandT4",
                           datTab=NautilusScienceandTechnology,
                           sideInfo=pagnum
    )
    MIT23 <- callModule(mf_s_rsstable,
                              "SandT5",
                              datTab=MITScienceandTechnology,
                              sideInfo=pagnum
    )
    rowsAI <- reactive({
      a <- DeepL1()
      b <- DeepL2()
      c <- DeepL3()
      d <- DeepL4()
      selectedR <- list(GNews=as.data.frame(a),
                        MIT=as.data.frame(b),
                        Cortana=as.data.frame(c),
                        Medium=as.data.frame(d))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewAI <- callModule(mf_s_ReviewCell,
                                "AI",
                                reactives=trickleVal,
                                domains=rowsAI)
    DeepL1  <- callModule(mf_s_rsstable,
                        "DL1",
                        datTab=GNewsDeepLearning,
                        sideInfo=pagnum
    )
    DeepL2  <- callModule(mf_s_rsstable,
                          "DL2",
                          datTab=MITDeepLearning,
                          sideInfo=pagnum
    )
    DeepL3  <- callModule(mf_s_rsstable,
                          "DL3",
                          datTab=CortanaDeepLearning,
                          sideInfo=pagnum
    )
    DeepL4  <- callModule(mf_s_rsstable,
                          "DL4",
                          datTab=MediumDeepLearning,
                          sideInfo=pagnum
    )
    rowsNano <- reactive({
      a <- Eureka()
      b <- Nanowerk1()
      c <- Nanowerk2()
      d <- GNNano()
      selectedR <- list(Eureka=as.data.frame(a),
                        Nanowerk1=as.data.frame(b),
                        Nanowerk2=as.data.frame(c),
                        GNews=as.data.frame(d))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewNano <- callModule(mf_s_ReviewCell,
                           "Nano",
                           reactives=trickleVal,
                           domains=rowsNano)
    
    Eureka <- callModule(mf_s_rsstable,
                         "Nan1",
                         datTab=EurekaalertNanotechnology,
                         sideInfo=pagnum
    )
    Nanowerk1 <- callModule(mf_s_rsstable,
                            "Nan2",
                            datTab=NanowerkNanotechnology,
                            sideInfo=pagnum
    )
    
    Nanowerk2 <- callModule(mf_s_rsstable,
                            "Nan3",
                            datTab=NanowerkTNanotechnology,
                            sideInfo=pagnum
    )
    
    GNNano  <-  callModule(mf_s_rsstable,
                            "Nan4",
                            datTab=GnewsNanotechnology,
                            sideInfo=pagnum
    )
    
    rowsNeuro <- reactive({
      a <- GNNeu()
      b <- Neu1()
      c <- Neu2()
      selectedR <- list(GNews=as.data.frame(a),
                        JNeuro=as.data.frame(b),
                        JNeuro2=as.data.frame(c)
                        )
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewNeuro <- callModule(mf_s_ReviewCell,
                             "Neuro",
                             reactives=trickleVal,
                             domains=rowsNeuro)
    GNNeu  <-  callModule(mf_s_rsstable,
                         "Neu1",
                         datTab=GNewsNeurology,
                         sideInfo=pagnum
    )
    Neu1  <-  callModule(mf_s_rsstable,
                       "Neu2",
                       datTab=NeurologyJNeurology,
                       sideInfo=pagnum
    )
    Neu2  <-  callModule(mf_s_rsstable,
                   "Neu3",
                   datTab=NeurologyJNNNeurology,
                   sideInfo=pagnum
    )
    rowsMedworm <- reactive({
      a <- Med1()
      b <- Med2()
      c <- Med3()
      d <- Med4()
      selectedR <- list(Medworm1=as.data.frame(a),
                        Medworm2=as.data.frame(b),
                        Medworm3=as.data.frame(c),
                        Medworm4=as.data.frame(d))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewMedworm <- callModule(mf_s_ReviewCell,
                             "Medworm",
                             reactives=trickleVal,
                             domains=rowsMedworm)
    
    Med1  <-  callModule(mf_s_rsstable,
                         "Med1",
                         datTab=MaterialScienceMedworm,
                         sideInfo=pagnum
    )
    Med2  <-  callModule(mf_s_rsstable,
                       "Med2",
                       datTab=NeurologyMedworm,
                       sideInfo=pagnum
    )
    Med3  <-  callModule(mf_s_rsstable,
                         "Med3",
                         datTab=NeuroscienceMedworm,
                         sideInfo=pagnum
    )
    Med4  <-  callModule(mf_s_rsstable,
                         "Med4",
                         datTab=BiomedicalEngineeringMedworm,
                         sideInfo=pagnum
    )
    rowsmake <- reactive({
      a <- Makezine()
      b <- MakerFaire()
      c <- Hackaday()
      selectedR <- list(Makezine=as.data.frame(a),
                        MakerFaire=as.data.frame(b),
                        Hackaday=as.data.frame(c))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewmake <- callModule(mf_s_ReviewCell,
                                "make",
                                reactives=trickleVal,
                                domains=rowsmake)
    
    Makezine  <-  callModule(mf_s_rsstable,
                             "DIY1",
                             datTab=MakezineDIY,
                             sideInfo=pagnum
    )
    
    MakerFaire  <-  callModule(mf_s_rsstable,
                               "DIY2",
                               datTab=MakerFaireDIY,
                               sideInfo=pagnum
    )
    Hackaday <-  callModule(mf_s_rsstable,
                                "DIY3",
                                datTab=HackadayDIY,
                                sideInfo=pagnum
    )
    
    rowsMITU <- reactive({
      a <- Aeronauticalandastronauticalengineering()
      b <- Neuroscienceneurologyandcognitivesciences()
      c <- Makingandthemakermovement()
      d <- Materialsscienceandengineering()
      e <- Mechanicalengineering()
      f <- Nanoscienceandnanotechnology()
      selectedR <- list(Aeronauticalandastronauticalengineering=as.data.frame(a),
                        Neuroscienceneurologyandcognitivesciences=as.data.frame(b),
                        Makingandthemakermovement=as.data.frame(c),
                        Materialsscienceandengineering=as.data.frame(d),
                        Mechanicalengineering=as.data.frame(e),
                        Nanoscienceandnanotechnology=as.data.frame(f))
      selectedR <- emptyRemove(selectedR)
      return(selectedR)
    })
    reviewMIT <- callModule(mf_s_ReviewCell,
                                "MIT",
                                reactives=trickleVal,
                                domains=rowsMITU)
    Aeronauticalandastronauticalengineering <- callModule(mf_s_rsstable,
                      "MIT1",
                      datTab=AeronauticalandastronauticalengineeringMIT,
                      sideInfo=pagnum
    )
    Neuroscienceneurologyandcognitivesciences <- callModule(mf_s_rsstable,
                        "MIT2",
                        datTab=NeuroscienceneurologyandcognitivesciencesMIT,
                        sideInfo=pagnum
    )
    Makingandthemakermovement <- callModule(mf_s_rsstable,
                          "MIT3",
                          datTab=MakingandthemakermovementMIT,
                          sideInfo=pagnum
    )
    Materialsscienceandengineering <- callModule(mf_s_rsstable,
                           "MIT4",
                           datTab=MaterialsscienceandengineeringMIT,
                           sideInfo=pagnum
    )
    Mechanicalengineering <- callModule(mf_s_rsstable,
                      "MIT5",
                      datTab=MechanicalengineeringMIT,
                      sideInfo=pagnum
    )
    Nanoscienceandnanotechnology <- callModule(mf_s_rsstable,
                      "MIT6",
                      datTab=NanoscienceandnanotechnologyMIT,
                      sideInfo=pagnum
    )
    
    output$DomainSandT <- renderHighchart2(sourceToTMap(GNewsScienceandTechnology))
    output$DomainAI <- renderHighchart2(sourceToTMap(GNewsDeepLearning))
    output$DomainNeuro <- renderHighchart2(sourceToTMap(GNewsNeurology))
    output$DomainNano <- renderHighchart2(sourceToTMap(GnewsNanotechnology))
    output$GNewsWC <- renderWordcloud2(wordcloud2(
      FormWordCloud(
        bind_rows(GNewsScienceandTechnology,
                  GNewsDeepLearning,
                  GNewsNeurology,
                  GnewsNanotechnology)
        ),ellipticity = ".65"))
    
    
    
    
  }
  
  
