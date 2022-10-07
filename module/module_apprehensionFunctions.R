###Extracts the url from the vector it is given than uses urltools package to return domain list
###It doesn't works with sources that provide tracking links
dnameExtract <- function(sourceTable){
  SGoogle <- FALSE
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  links <- vector(mode = "character",length = 0)
  if(isTruthy(grep("ItemDetails",colnames(sourceTable)))) {
    links <- str_extract_all(sourceTable$ItemDetails,url_pattern)}
  if(is.list(links))  
    {
    if (!isTruthy(links[[1]])) links <- vector(mode = "character",length = 0)
    }
  if(length(links) == 0) links <- str_extract_all(sourceTable$Title,url_pattern)
  if(is.list(links)) {if(startsWith(links[[1]][[1]],"https://news.google.com")){
    sourceTable2 <- Reduce(c,sourceTable$ItemDetails)
    links2 <- str_extract_all(sourceTable2,"<font color=\"#[[:alnum:]]*\">([[:alnum:]]|[[:blank:]]|[$-_@.&+])*</font>")
    links2 <- Reduce(c,links2)
    links2 <- str_replace_all(links2, "<font color=\"#[[:alnum:]]*\">", "")
    links2 <- str_replace_all(links2, "</font>", "")
    SGoogle <- TRUE
  }
    {links <- Reduce(c,links)}}
  domains <-   suffix_extract(domain(links))
  if(SGoogle) {domains <- domains[1:length(links2),]
  domains$domain <- links2
  }
  return(domains)
}

###This domain extract function is the modified version of first domain extract that is written
###with the purpose of handling the issue of tracking links from Google News by getting the source name 
###from Item description  HTML text that is provided rather thank from the links alone
###if the source is GoogleNews
dnameExtractGoogle <- function(sourceTable){
  
  
}
####Insert new rows in a shorter amount of time

insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

###Applies dnameExtract over multiple data frames

findDomains <- function(tableList){
  domainTable <- data.frame()
  for (i in 1:length(tableList)){
    tTable <- dnameExtract(tableList[[i]])
    domainTable <-  rbind(domainTable,tTable)
  }
  return(domainTable)
}

###Returns a vector of unique domain names from the domain datatable provided
uniqueDList <- function(domainTable){
  domainV <- domainTable$domain
  domainV <- unique(domainV)
  return(domainV)
}

###Applies grabEvidence over multiple data frames
addEvidencetoArchive <- function(tableList,instanceType,domainName){
  domainTable <- list()
  
  for (i in 1:length(tableList)){
    tTable <- grabEvidence(tableList[[i]],domainName)
    domainTable[[i]] <-   as.data.frame(tTable)
  }
  domainTable[[length(domainTable)+1]] <- domainName
  domainTable[[length(domainTable)+1]] <- instanceType
  return(domainTable)
}
addEvidencetoArchive_v2 <- function(tableList,instanceType,domainName){
  evidenceList <- readRDS("archive/evidenceArchive/masterArchiveIndex.RDS")
  columnNames <- colnames(evidenceList)
  for (i in 1:length(tableList)) {
    tTable <- grabEvidence(tableList[[i]],domainName)
    if (nrow(tTable)>0){
      for (j in 1:nrow(tTable)) {
        vectorRow <- character(5)
       for (k in 1:length(colnames(tTable))) {
         vectorRow[match(colnames(tTable)[k],colnames(evidenceList))] <- tTable[j,k]
         
       } 
        #insertRow(evidenceList,vectorRow,nrow(evidenceList)+1)
        evidenceList <-  rbind(evidenceList,vectorRow)
        evidenceList[nrow(evidenceList),5] <- domainName
        evidenceList[nrow(evidenceList),6] <- instanceType
        }
    }
  }
  colnames(evidenceList) <- columnNames
  write_rds(evidenceList,"archive/evidenceArchive/masterArchiveIndex.RDS")
  #return(evidenceList)
}

###Finds the rows in which the selected domain name occurs. Returns those rows
grabEvidence <- function(newsTable,domainName){
  evidenceIndices <- NA
  if("ItemDetails" %in% colnames(newsTable)) {
    evidenceIndices <- match(domainName,newsTable$ItemDetails)
    }
  if(!isTruthy(evidenceIndices)) evidenceIndices <-  grep(domainName,newsTable$Title)
  evidences <- newsTable[evidenceIndices,]
  if(!isTruthy(evidences[[1]])) return(data.frame()) else  return(evidences)
}

###Enhanced strsplit funtion with which the splitting direction can be selected
strsplit <- function(x,
                     split,
                     type = "remove",
                     perl = FALSE,
                     ...) {
  if (type == "remove") {
    # use base::strsplit
    out <- base::strsplit(x = x, split = split, perl = perl, ...)
  } else if (type == "before") {
    # split before the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=.)(?=", split, ")"),
                          perl = TRUE,
                          ...)
  } else if (type == "after") {
    # split after the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=", split, ")"),
                          perl = TRUE,
                          ...)
  } else {
    # wrong type input
    stop("type must be remove, after or before!")
  }
  return(out)
}

###Function that adds the occurence of an event to the past records of that event type
addIncidence <- function(domainName, iTable){
  cTable <- iTable
  if(!isTruthy(grep(domainName,cTable[,1]))) {
    cTable <- rbind(cTable,c(domainName,1,format(Sys.Date())),stringsAsFactors=FALSE)
  }else{
  domainIndice <- grep(domainName,cTable[,1])
  cTable[[domainIndice,2]] <- as.integer(cTable[[domainIndice,2]])+1
  cTable[[domainIndice,3]] <- paste(cTable[[domainIndice,3]],format(Sys.Date()),sep = "_=_",collapse = "")
  }
  colnames(cTable) <- colnames(iTable)
  return(cTable)
}

updateArchives <- function(domainName,archiveDir){
  directory <- paste0("archive/",archiveDir,".RData")
  arc <- readRDS(directory)
  arc <- addIncidence(domainName,arc)
  saveRDS(arc , file = directory)
  saveRDS(arc, file = paste0("archive/categoryArchive/",archiveDir,"_",format(Sys.Date()),".RData"))
}
###Append list of the rows reactive
appendDataFrames <- function(sList,source1,source2=NULL){
  nList <- sList
  bList <- list()
  cList <- list()
  if (isTruthy(source1)){
    sName <- deparse(substitute(source1))
    bList[[1]] <- source1
    names(bList) <- sName
    nList <- c(nList,bList)
  }
  if (isTruthy(source2)){
    sName <- deparse(substitute(source2))
    cList[[1]] <- source2
    names(cList) <- sName
    nList <- c(nList,cList)
  }
}
####Remove empty data frames from the list
emptyRemove <- function(sList){
  for (i in length(sList):1) {
  if(!isTruthy(sList[[i]][[1]])){sList[[i]] <- NULL}
  }
  return(sList)
}
####Combines all the archive types into a one data frame 
####in which each sources scores in different categories are gathered
MergeArchives <- function(){
  Examplary <- readRDS("archive/examplary.RData")
  Clickbait <- readRDS("archive/clickbait.RData")
  badReporting <- readRDS("archive/badReporting.RData")
  badFaith <- readRDS("archive/badFaith.RData")
  sourceV <- as.vector(rbind(Examplary[,1],Clickbait[,1],badReporting[,1],badFaith[,1]))
  sourceV <- unique(sourceV)
  mergedTable <- data.frame(examplary= integer(length = length(sourceV)),
                            clickbait= integer(length = length(sourceV)),
                            BadlyReported= integer(length = length(sourceV)),
                            MaliciousIntent = integer(length = length(sourceV)))
  mergedTable <- cbind(sourceV,mergedTable)
  for (i in 1:length(Examplary[,1])){
    matchedRow <-  match(Examplary[[i,1]],mergedTable[,1])
    mergedTable[matchedRow,2] <- Examplary[i,2]
  }
  for (i in 1:length(Clickbait[,1])){
    matchedRow <-  match(Clickbait[[i,1]],mergedTable[,1])
    mergedTable[matchedRow,3] <- Clickbait[i,2]
  }
  for (i in 1:length(badReporting[,1])){
    matchedRow <-  match(badReporting[[i,1]],mergedTable[,1])
    mergedTable[matchedRow,4] <- badReporting[i,2]
  }
  for (i in 1:length(badFaith[,1])){
    matchedRow <-  match(badFaith[[i,1]],mergedTable[,1])
    mergedTable[matchedRow,5] <- badFaith[i,2]
  }
  return(mergedTable)
}






