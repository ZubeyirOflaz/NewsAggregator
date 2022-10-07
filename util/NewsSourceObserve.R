#### Functions and helper functions to analyze the content of news sources
sourceToTMap <- function(sTable){
  theTable <- dnameExtract(sTable) %>% 
    group_by(domain) %>% 
    summarise(n = n()) %>% 
    arrange(-n)
  return(hctreemap2(theTable, group_vars = "domain", size_var = "n"))
}
mapMasterIndex <- function(mTable){
  xAxis <- colnames(mTable)
  yRange <- mTable[1,]
  theTable <- highchart() %>% 
    hc_xAxis(categories = colnames(mTable))
  for (i in 1:15) {
    domainName <- as.character(row.names(mTable[i,]))
    theTable %>% 
      hc_add_series(name = domainName, data = mTable[i,])
    
  }
  return(theTable)
}

CompleteFeedScan <- function(){
  savedFeedVector <- dir("archive/PreviousRssFeeds")
  domainList <- list()
  for (n in 1:length(savedFeedVector)) {
    tempFeed <- readRDS(paste0("archive/PreviousRssFeeds/",savedFeedVector[[n]]))
    domains <- FeedList2Domain(tempFeed)
    domainList[[n]] <- domains
    print(length(domainList))
  }
  savedFeedVector <- str_remove_all(savedFeedVector,"arc")
  names(domainList) <- savedFeedVector
  return(domainList)
}


FeedList2Domain <- function(feedList){
  #### The feeds that are explicitly tracked####
  trackVector <- c("GNewsInternational","GNewsEurope","GNewsAsia", "GNewsGermany",
                   "GNewsBooksandArts","GNewsMiddleEastandAfrica","GNewsUnitedStates",
                   "GNewsTheAmericas","GNewsScienceandTechnology","GNewsBusiness")
completeDataFrame <- feedList[[match(trackVector[[1]],names(feedList))]]
  for(i in 2:length(trackVector)){
  addendumDF <- feedList[[match(trackVector[[i]],names(feedList))]]
  completeDataFrame <- rbind2(completeDataFrame, addendumDF)
  }
domainFrame <- domainCount(completeDataFrame)
return(domainFrame)
}
domainCount <- function(dataFrame){
  domainFrame <- dnameExtract(dataFrame) %>% 
    group_by(domain) %>% 
    summarise(n = n()) %>% 
    filter(domain != "google") %>% 
    arrange(-n)
  return(domainFrame)
}