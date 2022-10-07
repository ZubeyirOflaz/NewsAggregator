#editor <- function(dataf) {
#  if (str_detect(deparse(substitute(dataf)),"GNews")) {dataf <- GNewsFeedEdit(dataf)}
#  if (str_detect(deparse(substitute(dataf)),"Economist")) {dataf <- EconomistFeedEdit(dataf)}
#  return(dataf)
#}
#tFeedEdit <- function(rssTable){
#  newTable <- select(rssTable, c('item_date_published','item_title','item_description'))
#  newTable <- mutate(newTable, item_title=paste0('<a href="',rssTable$item_link,'">',newTable$item_title,'</a>'))
#  return(newTable)
#}
#GNewsFeedEdit <- function(rssTable){
#  newTable <- select(rssTable, c('item_date_published','item_title','item_description'),)
#  newTable <- mutate(newTable, item_title=paste0('<a href="',rssTable$item_link,'">',newTable$item_title,'</a>'))
#  return(newTable)}
#ReutersFeedEdit <- function(rssTable){
#  newTable <- select(rssTable, c('item_date_published','item_title','item_description'),)
#  newTable <- mutate(newTable, item_title=paste0('<a href="',rssTable$item_link,'">',newTable$item_title,'</a>'))
#  return(newTable)}
#NeurologyJNFeedEdit <- function(rssTable){
#  newTable <- select(rssTable, c('item_title','item_description'),)
#  newTable <- mutate(newTable, item_title=paste0('<a href="',rssTable$item_link,'">',newTable$item_title,'</a>'))
#  return(newTable)}

###Feed edit for expected RSS feeds
GeneralFeedEdit <- function(rssTable){
  newTable <- select(rssTable, c('item_date_published','item_title','item_description'),)
  newTable <- mutate(newTable, item_date_published=format(item_date_published, "%d/%m/%y %H:%M"))
  newTable <- mutate(newTable, item_title=paste0('<a href="',rssTable$item_link,'">',newTable$item_title,'</a>'))
  colnames(newTable) <- c("Publication Date","Title","ItemDetails")
  return(newTable)
}

###Feed edit for RSS that doesn't include dates
DatelessFeedEdit <- function(rssTable){
  newTable <- select(rssTable, c('item_title','item_description'),)
  newTable <- mutate(newTable, item_title=paste0('<a href="',rssTable$item_link,'">',newTable$item_title,'</a>'))
  colnames(newTable) <- c("Title","ItemDetails")
  return(newTable)
}

###Feed edit for RSS that doesn't include dates or description
TitleFeedEdit <- function(rssTable){
  newTable <- select(rssTable, c('item_title','item_link'),)
  newTable <- mutate(newTable, item_title=paste0('<a href="',rssTable$item_link,'">',newTable$item_title,'</a>'))
  colnames(newTable) <- c("Title","ItemLink")
  return(newTable)
}
NondescriptiveFeedEdit <- function(rssTable){
  newTable <- select(rssTable, c('item_date_published','item_title'),)
  newTable <- mutate(newTable, item_date_published=format(item_date_published, "%d/%m/%y %H:%M"))
  newTable <- mutate(newTable, item_title=paste0('<a href="',rssTable$item_link,'">',newTable$item_title,'</a>'))
  colnames(newTable) <- c("Publication Date","Title")
  return(newTable)
}

###Function that inputs the search term(s) from GNews text input and outputs a Google News RSS feed link 
GNLinkCreator <- function(st){
  searchtext <- character()
  searchtext <- st
  searchtext <-  str_replace_all(searchtext," ","+")
  searchtext <-  str_remove_all(searchtext,"/")
  
  
  if (searchtext=='' | searchtext=='Search+Terms'|searchtext=='GNewsSearch'){
    link <- 'https://news.google.com/rss/topics/CAAqBwgKMNzD8wow49fTAg?hl=en-US&gl=US&ceid=US%3Aen'
  }else link <- paste0('https://news.google.com/rss/search?q=',searchtext)
  return(link)
}
redditFeedEdit <- function(feedeRlink){
  feedList <- feedeR::feed.extract(feedeRlink)
  feedTable <- feedList$items
  feedTable %>% select("date","title") %>% 
    mutate(date=format(feedTable$date,"%d/%m/%y %H:%M"))%>% 
    mutate(title=paste0('<a href="',feedTable$link,'">',feedTable$title,'</a>')) -> feedTable
    return(feedTable)
}
###Combination of all feed edits into a conditional function set
CombinedFeedEdit <- function(a){
  if (any(names(a)=='item_date_published')&any(names(a)=='item_description')&any(names(a)=='item_title')){
    a <- GeneralFeedEdit(a)
  } else if(any(names(a)=='item_date_published')&any(names(a)=='item_title')){
    a <- NondescriptiveFeedEdit(a)
  } else if(any(names(a)=='item_description')&any(names(a)=='item_title')){
    a <- DatelessFeedEdit(a)
  }else if(any(names(a)=='item_title')&any(names(a)=='item_link')){
    a <- TitleFeedEdit(a)
  } else if(!is.data.frame(a)){
    a <- data.frame(matrix(ncol = 3, nrow = 0))
    x <- c("No", "Feed", "Found")
    colnames(a) <- x
  }else  {a <- a}
}