### This group of fucntions extract keywords from the news titles and provide the Word Clouds
removeDomain <- function(x){
  #Removes the news article source names from the given text
  y <- reduce(x,paste)
  cleanedText <- str_remove_all(y,
                                "<font color=\"#[[:alnum:]]*\">([[:alnum:]]|[[:blank:]]|[$-_@.&+])*</font>")
  
  return(cleanedText)
  
}

bindDataTables <- function(DT1,DT2,DT3=data.frame(),DT4=data.frame(),DT5=data.frame()){
  y <- bind_rows(DT1,DT2,DT3,DT4,DT5)
  return(y)
}

RemoveHtml <- function(x){
  #Combines the vector of Item Descriptions and returns a string removed of Html tags
  
  y <- html_text(read_html(x))
  y <- cleanedText <- str_remove_all(y,
                                "(View Full Coverage on Google News)")
  return(y)
}

KeywordPreprocess <- function(DTable){
  x <- DTable$ItemDetails
  tt <- class(try(read_html(x)))
  #if(tt[[1]]=="try-error") return(x)
  y <- RemoveHtml(removeDomain(x))
  return(y)
}

CorpusProcess <- function(x){
  #Transforms a character string to a Corpus object and  prepares the text for keyword extraction
  y <- Corpus(VectorSource(x))
  y %>% tm_map(content_transformer(tolower)) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, c("news","says","news","deep","learning","nanotechnology", "thehill","[-]","[-]"))   %>% 
  tm_map(removePunctuation) ->y
  return(y)
}
frequencyMatrix <- function(x){
  ##From the processed document, returns a frequency word matrix
  termsfreq <- TermDocumentMatrix(x)
  termsfreq <- as.matrix(termsfreq)
  termsfreq <- sort(rowSums(termsfreq),decreasing=TRUE)
  termsfreq <- data.frame(word = names(termsfreq),freq=termsfreq)
  return(termsfreq)
  
}

FormWordCloud <- function(NewsTable){
  #Combines the helper functions for the processing of news source
  #performs keyword extraction and returns wordcloud
  myDT <- KeywordPreprocess(NewsTable)
  myDT <- CorpusProcess(myDT)
  myDT <- frequencyMatrix(myDT)
  myDT <- filter(myDT, freq>2)

  return(myDT)
}



