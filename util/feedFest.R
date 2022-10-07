###########--------------Contains the funtions related to the retrieving RSS feeds and adding them to the enviroment----------------###########



feedFetch <- function(feedlinks,addNamespace=''){
  ###Editing dataset before data processing
  rssId <- feedlinks
  rssId <- transmute(feedlinks, rssID= paste0(feedlinks[,1],feedlinks[,2]))
  transmute(feedlinks, rssID=paste0(feedlinks[,1]),feedlinks[,2])
  ###Links of the RSS feeds
  t <- as.vector(feedlinks[,3])
  fList <- list()
  ###For loop that 1 gets the RSS tables from supplied links, 2 Saves the dataframes to a list according to their names
  for (i in sample(c(1:length(t)))) {
    print(t[i])
    a <- try(suppressWarnings(tidyfeed(as.character(t[i]))))
    if ("data.frame" %in% class(a)){
    ###Edit the data frames with a conditional dataset
    a <- CombinedFeedEdit(a)
    #if (any(names(a)=='item_date_published')&any(names(a)=='item_description')&any(names(a)=='item_title')){
    #  a <- GeneralFeedEdit(a)
    #} else if(any(names(a)=='item_description')&any(names(a)=='item_title')){
    #  a <- DatelessFeedEdit(a)
    #}else if(any(names(a)=='item_title')&any(names(a)=='item_link')){
    #  a <- TitleFeedEdit(a)
    #} else {a <- a}
    
    ##Naming the lists using source and category
    b <- as.character( rssId[i,])
    b <- str_remove_all(b," ")
    b <- paste0(addNamespace,b)
    #a <- list(a)
    fList[[b]] <- a} else {warning(paste(t[i],"feed cannot be received"))}
  }
  saveRDS(fList,file = "data/LatestFeeds.RData")
  saveRDS(fList,file = paste0("archive/PreviousRssFeeds/arc",format(Sys.Date()),".RData"))
  saveRDS(Sys.Date(),file = 'data/UpdateData.RData')
  return(fList)
}

###First version of feedFetch for specific uses
feedFetch_v1 <- function(feedlinks,addNamespace=''){
  ###Editing dataset before data processing
  rssId <- feedlinks
  rssId <- transmute(feedlinks, rssID= paste0(feedlinks[,1],feedlinks[,2]))
  transmute(feedlinks, rssID=paste0(feedlinks[,1]),feedlinks[,2])
  ###Links of the RSS feeds
  t <- as.vector(feedlinks[,3])
  fList <- list()
  ###For loop that 1 gets the RSS tables from supplied links, 2 Saves the dataframes to a list according to their names
  for (i in sample(c(1:length(t)))) {
    print(t[i])
    a <- suppressWarnings(tidyfeed(t[i]))
    ###Edit the data frames with a conditional dataset
    if (any(names(a)=='item_date_published')&any(names(a)=='item_description')&any(names(a)=='item_title')){
      a <- GeneralFeedEdit(a)
    } else if(any(names(a)=='item_description')&any(names(a)=='item_title')){
      a <- DatelessFeedEdit(a)
    }else if(any(names(a)=='item_title')&any(names(a)=='item_link')){
      a <- TitleFeedEdit(a)
    } else {a <- a}
    
    ##Naming the lists using source and category
    b <- as.character( rssId[i,])
    b <- str_remove_all(b," ")
    b <- paste0(addNamespace,b)
    #a <- list(a)
    fList[[b]] <- a
  }
  return(fList)
}



###Function for the  retrieval and edition of Google RSS feed from a given search string
lantern <- function(rva){
  searchtext <- character()
  searchtext<- rva
  searchtext <-  str_replace_all(searchtext," ","+")
  searchtext <-  str_remove_all(searchtext,"/")
  if (searchtext==''){
    link <- 'https://news.google.com/rss/topics/CAAqBwgKMNzD8wow49fTAg?hl=en-US&gl=US&ceid=US%3Aen'
  }else link <- paste0('https://news.google.com/rss/search?q=',searchtext)
  tab <- tidyfeed(link)
  tab <- CombinedFeedEdit(tab)
  return(tab)
}
    feedReset <- function(){
      SourceBook <- read.csv2('data/SourceBook.csv')
      testFetch <- try(tidyfeed('https://www.economist.com/europe/rss.xml'))
      if(class(testFetch)[1] == 'tbl_df'){
      a <- feedFetch(SourceBook)
      #list2env(a, .GlobalEnv)
      #rm(a)
      return(TRUE)} else{
        return(FALSE)
      }
    }
#### Check if the feeds saved are recent and if the network refuses to connect, if not refresh the feeds
feedRefresh <- function(){
  refreshDate <- readRDS("data/UpdateData.RData")
  if (as.integer(Sys.Date()-refreshDate)>0) {
    testFetch <- try(tidyfeed('https://www.economist.com/europe/rss.xml'))
    if(class(testFetch)[1] == 'tbl_df') {
      SourceBook <- read.csv2('data/SourceBook.csv')
      LatestFeeds <-  feedFetch(SourceBook)}else {
        LatestFeeds <- readRDS("data/LatestFeeds.RData")
      }
    } else {
  LatestFeeds <- readRDS("data/LatestFeeds.RData")
  }
  list2env(LatestFeeds, .GlobalEnv)
  rm(LatestFeeds)
}


