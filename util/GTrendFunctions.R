###Contains functions that are written for the processing and presentation of information that is taken from Google Trends


CCodes <- function(){
  CountryCodes <- read.csv('data/country-codes.csv')
  CodePairs <- CountryCodes %>% select(c('UNTERM.English.Short','ISO3166.1.Alpha.2')) %>% 
    filter(!is.na(UNTERM.English.Short) & UNTERM.English.Short != '') %>% 
    mutate(UNTERM.English.Short=str_remove_all(UNTERM.English.Short," \\(.*\\)"))%>% 
    mutate_if(is.character, ~gsub('[^ -~]', '', .))

  #CodePairs <- str_remove_all(CodePairs$UNTERM.English.Short,'(the)')
  Choice <- data.frame('Choose','')
  names(Choice) <- names(CodePairs)
  CodePairs <- rbind(Choice,CodePairs)
  return(CodePairs)                     
}





###Functions to create data frames of related topics and related queries####
RelQuer <- function(GTData){
  my_df <- GTData$related_queries
  my_df %>%  select(subject,related_queries,value) -> my_df
  my_df %>% 
    filter(related_queries=='top') %>% 
    select(value) ->my_df1
  my_df %>% 
    filter(related_queries=='rising') %>%
    select(value,subject)->my_df2
  my_df3 <- merge(data.frame(my_df1, row.names=NULL), 
                  data.frame(my_df2, row.names=NULL), 
                  by = 0, all = TRUE)[-1]
  colnames(my_df3) <- c('Top Searches','Rising Searches','Increase Amount for Rising Searches')
  my_df3 %>% arrange(my_df3[,3]) -> my_df3
  my_df3 <- sapply(my_df3, as.character)
  my_df3[is.na(my_df3)] <- " "
  return(my_df3)
}

RelTop <- function(GTData){
  my_df <- GTData$related_topics
  my_df %>%  select(subject,related_topics,value) -> my_df
  my_df %>% 
    filter(related_topics=='top') %>% 
    select(value) ->my_df1
  my_df %>% 
    filter(related_topics=='rising') %>%
    select(value,subject) ->my_df2
  my_df3 <- merge(data.frame(my_df1, row.names=NULL), 
                  data.frame(my_df2, row.names=NULL), 
                  by = 0, all = TRUE)[-1]
  colnames(my_df3) <- c('Related Topics','Rising Related Topics','Increase in Relevancy')
  my_df3 %>% arrange(my_df3[,3]) -> my_df3
  my_df3 <- sapply(my_df3, as.character)
  my_df3[is.na(my_df3)] <- " "
  return(my_df3)
}



###Functions used to plot interest by region when the search is done for Germany
NameChange <- function(TList){
  for (i in 1:length(TList[,1])) {
    if(TList[i,1]=="Baden-WÃ¼rttemberg"){TList[i,1] <- "Baden-Württemberg"}
    if(TList[i,1]=="Bavaria"){TList[i,1] <- "Bayern"}
  }
  AList <- arrange(TList,location)
    
    return(AList)

}





###Functions used to plot interest by country on a World map####
worldMap <- function(){
  world <- map_data("world")
  
  world %>%
    mutate(region = replace(region, region=="USA", "United States")) %>%
    mutate(region = replace(region, region=="UK", "United Kingdom")) -> world
  return(world)
}

WorldDataFrame <- function(data,WMap){
  data$interest_by_country %>%
    filter(location %in% WMap$region, hits > 0) %>%
    mutate(region = location, hits = as.numeric(hits)) %>%
    select(region, hits) -> my_df
  return(my_df)
}
plotWorld <- function(GTData){
  world <- worldMap()
  my_df <-  worldDF <- WorldDataFrame(GTData,world)
  
  my_plot <- ggplot() +
    geom_map(data = world,
             map = world,
             aes(x = long, y = lat, map_id = region),
             fill="#ffffff", color="#ffffff", size=0.15) +
    geom_map(data = my_df,
             map = world,
             aes(fill = hits, map_id = region),
             color="#ffffff", size=0.15) +
    scale_fill_continuous(type="gradient")+
    theme_map()+
    theme(plot.background = element_rect(fill = "#ecf0f5", colour = "#ecf0f5"))
  return(my_plot)
}



### Function(s) for the plotting of interest over time
plotInterest <- function(GTData){
  my_df <- GTData$interest_over_time
  my_plot <- ggplot(data = my_df,
                    aes(x=date,
                        y=hits))+
    stat_peaks(span = 31,size=3, geom = "label")+
    geom_smooth(colour="slategray4",
                method = "lm", 
                formula = y ~ poly(x, 3), 
                se = FALSE)+
    geom_line(colour="steelblue3",size=0.7)+ 
    geom_point(shape=4)+
    ggtitle("Interest Over Time")+
    xlab("Time Scale") + ylab("Hits")+
    scale_color_fivethirtyeight()+
    theme_fivethirtyeight()+
    theme(plot.title = element_text(hjust = 0.5))
  return(my_plot)
}
interpolated <- function(GTData){
  my_data <- GTData$interest_over_time
  my_df <- as.data.frame(spline(x=my_data$date,
                                y=my_data$hits,
                                n=10*length(my_data$hits)))
  my_df <- as.data.frame(spline(x=my_df$x,
                                y=my_df$y,
                                n=10*length(my_df$x)))
  my_df$x <-as.POSIXct(my_df$x,origin="1970-01-01") 
  my_plot <- ggplot(data = my_df,
                    aes(x=x,
                        y=y))+
    geom_line(colour="steelblue3",size=0.7)+
    stat_peaks(span = length(my_df[,2])/10,
               size=3, 
               geom = "label")+
    labs(caption = "Cubic spline interpolation to n*100")+
    ggtitle("Interest Over Time Interpolated")+
    xlab("Time Scale") + ylab("Hits")+
    scale_color_fivethirtyeight()+
    theme_fivethirtyeight()+
    theme(plot.title = element_text(hjust = 0.5))
  return(my_plot)
}
####Adds the country codes to Google Trends searches and returns interest by country dataframe####
CCodeAppend <- function(GTrendsData){
  codeList <- read.csv("data/GTrendsCountryCodes.csv")
  CountryInterest <- GTrendsData$interest_by_country
  CountryInterest <- CountryInterest[order(CountryInterest$location),]
  columnNames <- c("country_code",colnames(CountryInterest))
  #CountryInterest[which(is.na(CountryInterest$hits)),2] <- 0
  CountryInterest <-  cbind(codeList$country_code,CountryInterest)
  colnames(CountryInterest) <- columnNames
  return(CountryInterest)
}
### Plots highchart map
highMapper <- function(dataToMap){
  theMap <- hcmap(map = "custom/world-palestine",
                  data = dataToMap,
                  value = "hits",
                  joinBy = c("hc-key","country_code"))
  return(theMap)
}
