###############################################
# Text mining monetary policy communication   #
#           using AWS and R                   #
#       Created by Adam Jozsef Kovacs         #
#       CEU MS Business Analytics             #
#               2021-12-07                    #
###############################################


# Preparations ------------------------------------------------------------

# clean enviromnemt
rm(list = ls())

# load packages
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(rvest, tidyverse, dplyr, data.table, reshape2,  
               aws.translate, aws.comprehend, stringr, gridExtra)


# Web scraping ------------------------------------------------------------

# scrape links from page with all press releases
base_url <- 'https://www.mnb.hu/monetaris-politika/a-monetaris-tanacs/kozlemenyek'
t <- read_html( base_url )

# extracting full links to all the press releases after monetary policy meeting
full_links <- t %>% html_nodes('.news-list-item-title a') %>% html_attr( 'href' )

full_links <- paste0("https:",full_links[str_detect(full_links,pattern="kozlemeny-a-monetaris-tanacs")])

full_links <- full_links[seq(0, length(full_links), by = 2)]

# create function which scrapes the date of the meeting and the text of the press release
# and saves it into a list

get_one_release <- function( url ) {
  tlist <- list()
  
  page <- read_html( url )
  
  date <- 
    page %>% 
    html_nodes('.hero-header h1')%>%
    html_text()
  tlist[[ 'date' ]] <- gsub("Közlemény a Monetáris Tanács|-i üléséről", "", date)
    
  text_list <- 
    page %>% 
    html_nodes('.block p')%>%
    html_text()
  
  text <- NULL
  for ( i in text_list ){
    text <- paste( text, i )
  }
  
  tlist[[ 'text' ]] <- trimws(text)
  
  return( tlist )
}

# scraping the pages of every press release with the function
content <- lapply( full_links, get_one_release )

# writing all the scraped content into a data frame
df <- rbindlist( content, fill = T )

#get the texts
text <- df[['text']]


# Set up AWS --------------------------------------------------------------

# set AWS credentials
keyfile = list.files(path=".", pattern="accessKeys.csv", full.names=TRUE)
if (identical(keyfile, character(0))){
  stop("ERROR: AWS key file not found")
} 
# setting up the R - AWS connection
keyTable <- read.csv(keyfile, header = T) 
AWS_ACCESS_KEY_ID <- as.character(keyTable$Access.key.ID)
AWS_SECRET_ACCESS_KEY <- as.character(keyTable$Secret.access.key)

# activate key
Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = "eu-west-1") 

char.segments <- function(x, segm.length){
  byte.counter <- nchar(x, type = 'bytes')
  f <- c(1, rep(0, segm.length - 1))
  f <- cumsum(rep(f, length.out = byte.counter))
  s <- split(unlist(strsplit(x,'')), f)
  unname(sapply(s, paste, collapse = ''))
}


# Build dataframes --------------------------------------------------------

df_phrases <- data.frame( 'Phrase' = character(), 'Count' = integer() )

df_entities <- data.frame( 'Entity' = character(), 'Count' = integer() )

for (i in 1:length(text)) {
  #translate dates to English
  df[i, 'date'] <- translate(df[i, 'date']$date, from = "hu", to = "en")
  
  release = text[i]
  #get character count of each press release
  df[i, 'nchar'] <- nchar(release)
  
  #translate, sentiment analysis, key phrase and key entity recognition
  translated_text <- ""
  keyphrases <- vector(mode = "list", length = 2)
  names(keyphrases) <- c("Text","Score")
  keyentities <- vector(mode = "list", length = 2)
  names(keyentities) <- c("Text","Score")
  sentiment_mixed <- 0
  sentiment_negative <- 0
  sentiment_neutral <- 0
  sentiment_positive <- 0
  chunks <- trimws(char.segments(text[i], 2000))
  for (j in 1:length(chunks)) {
    if (chunks[j] > "") {
      translated_chunk <- translate(chunks[j], from = "hu", to = "en") %>%
        tolower() %>% 
        removeWords(c("a", "an","the")) %>% 
        str_squish() 
      
      sentiment_chunk <- detect_sentiment(translated_chunk)
    }
    translated_text <- paste0(translated_text, translated_chunk[1])
    sentiment_mixed <- sentiment_mixed + sentiment_chunk$Mixed/length(chunks)
    sentiment_negative <- sentiment_negative + sentiment_chunk$Negative/length(chunks)
    sentiment_neutral <- sentiment_neutral + sentiment_chunk$Neutral/length(chunks)
    sentiment_positive <- sentiment_positive + sentiment_chunk$Positive/length(chunks)
    
    keyphrases <- mapply(c, keyphrases, detect_phrases(translated_chunk) %>% select(Text, Score), SIMPLIFY=FALSE)
    keyentities <-  mapply(c, keyentities,detect_entities(translated_chunk) %>% select(Text, Score), SIMPLIFY=FALSE)
  }
  
  df[i, 'translated'] <- translated_text
  df[i, 'mixed'] <- sentiment_mixed 
  df[i, 'negative'] <- sentiment_negative
  df[i, 'neutral'] <- sentiment_neutral
  df[i, 'positive'] <- sentiment_positive
  
  df_phrase <- data.frame(score = keyphrases$Score, text = keyphrases$Text) %>% 
    group_by( text ) %>%
    summarise( n = n() )
  
  colnames(df_phrase) <- c("Phrase","Count")
  
  df_phrases <- rbind(df_phrases, df_phrase)
  
  df_entity <- data.frame(score = keyentities$Score, text = keyentities$Text) %>% 
    group_by( text ) %>%
    summarise( n = n() )
  
  colnames(df_entity) <- c("Entity","Count")
  
  df_entities <- rbind(df_entities,df_entity)
}

df <- df %>% mutate(base_rate = rev(c(0.6,0.6,0.6,0.6,0.6,0.9,1.2, 1.5, 1.65, 1.8,2.1)), corr_lower = rev(c(-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,0.25,0.55,0.7,0.85,1.15)), corr_higher = rev(c(1.85,1.85,1.85,1.85,1.85,1.85,2.15,2.45,2.6,2.75,3.05)))

df$date[7] <- "May 25, 2021"

df$date <- as.Date(df$date, format='%B %d, %Y')


# Vizzes ------------------------------------------------------------------

# number of characters 

ggplot(df, aes(date,nchar)) +
  geom_col(width = 15, fill = "darkblue" ) +
  labs(x = "Month",
       y = "Number of characters",
       title = "Length of Monetary Council policy meeting press releases",
       subtitle = "In the year 2021",
       caption = "Created by Adam Jozsef Kovacs") +
  scale_x_date(date_labels="%b",breaks = unique(df$date)) +
  geom_line(color = "orange", size = 1)


# sentiment over time

df2 <- melt(df[,c("date","mixed", "negative", "neutral", "positive")] ,  id.vars = 'date', variable.name = 'sentiment')

group.colors <- c(mixed = "#56B4E9", negative = "#D55E00", neutral ="#CC79A7", positive = "#009E73")

ggplot(df2, aes(date, value)) +
  geom_line(aes(colour = sentiment), size = 2) + 
  labs(x = "Month",
       y = "Sentiment score",
       title = "Sentiment analysis of Monetary Council policy meeting press releases",
       subtitle = "In the year 2021",
       caption = "Created by Adam Jozsef Kovacs") +
  scale_x_date(date_labels="%b",breaks = unique(df2$date)) +
  scale_y_continuous(limits = c(0, 1),breaks = seq(0,1,0.2)) +
  scale_colour_manual(values=group.colors)

# without neutral parts
s <- ggplot(df2 %>% filter(sentiment != "neutral"), aes(x=date, y = value, fill=sentiment)) + 
  geom_bar(position='fill', stat="identity") +
  labs(x = "Month",
       y = "Sentiment score",
       title = 'Sentiment analysis without neutral parts',
       subtitle = "Monetary Council policy meeting press releases in 2021") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_labels="%b",breaks = unique(df2$date))+
  scale_fill_manual(values=group.colors)

# interest rate corridor

df3 <- melt(df[,c("date","base_rate", "corr_lower", "corr_higher")] ,  id.vars = 'date', variable.name = 'rates')

r <- ggplot(df3, aes(date, value)) +
  geom_line(aes(colour = rates), size = 1) + 
  geom_point(aes(colour = rates), size = 2) +
  labs(x = "Month",
       y = "Interest rate",
       title = "Changes in interest rates set by the Monetary Council",
       subtitle = "In the year 2021",
       caption = "Created by Adam Jozsef Kovacs") +
  scale_x_date(date_labels="%b",breaks = unique(df2$date)) +
  scale_y_continuous(limits = c(-0.5, 3.5),breaks = seq(-0.5,3.5,1)) 

grid.arrange(s, r, ncol=2)

# key phrases

df_phrases %>% 
  group_by( Phrase ) %>% 
  summarise( Count = sum(Count) )%>%
  arrange(desc(Count)) %>% 
  top_n(10) %>% 
  ggplot( mapping = aes( x = reorder(Phrase, Count), y = Count ) ) +
  geom_col( fill = 'darkgreen' ) +
  coord_flip() +
  labs( y = 'Times appeared', x = 'Phrases', title = 'Most Frequent Phrases')

# key entities

df_entities %>% 
  group_by( Entity ) %>% 
  summarise( Count = sum(Count) )%>%
  arrange(desc(Count)) %>% 
  top_n(10) %>% 
  ggplot( mapping = aes( x = reorder(Entity, Count), y = Count ) ) +
  geom_col( fill = 'darkblue' ) +
  coord_flip() +
  labs( y = 'Times appeared', x = 'Entities', title = 'Most Frequent Entities')
