##########--------------------------Functions that are written for the custom Rss table from file functionality

getTables <- function(xlsx){
  a <- loadWorkbook(xlsx)
  a <- names(getSheets(a))
  return(a)
}

getSInfo <- function(tabPath){
  TabSet <- list()
  named <- getTables(tabPath)
  for (i in 1:length(named)){
    sourceTab <- read.xlsx(tabPath,i)
    TabSet[[named[i]]] <- paste0(sourceTab[,1],sourceTab[,2])
    TabSet[[named[i]]] <- str_remove_all(TabSet[[named[i]]],' ')
  }
  return(TabSet)
}

getFeaturedArticle <- function(){
  wiki <- read_html('https://en.wikipedia.org/wiki/Main_Page')
  feAr <- html_node(wiki,xpath = '//*[@id="mp-tfa"]')
  return(feAr)
}
getFeaturedImage <- function(){
  wiki <- read_html('https://en.wikipedia.org/wiki/Main_Page')
  feIm <- html_node(wiki,xpath = '//*[@id="mp-tfp"]')
  return(feIm)
}
