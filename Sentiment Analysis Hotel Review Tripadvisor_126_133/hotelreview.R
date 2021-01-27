library(dplyr)
library(tidyverse)
library(tm)
library(e1071)
library(caret)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(syuzhet)
library(tidytext)
library(shiny)

# Read file
hotel <- read_csv("~/Documents/Praktikum Data Science - B/projek/reviews.csv")

#Build corpus
hotelcorpus <- VCorpus(VectorSource(hotel$Review))
hotelcorpus <- tm_map(hotelcorpus, content_transformer(tolower))
hotelcorpus <- tm_map(hotelcorpus, content_transformer(removePunctuation))
hotelcorpus <- tm_map(hotelcorpus, content_transformer(removeNumbers))
hotelcorpus <- tm_map(hotelcorpus, content_transformer(removeWords), stopwords('english'))
cleaned_hotelcorpus <- tm_map(hotelcorpus, content_transformer(stripWhitespace))

#Create csv dari data yang clean
dataframe<-data.frame(review=unlist(sapply(cleaned_hotelcorpus, `[`)), stringsAsFactors=F)
View(dataframe)
write.csv(dataframe,file = 'commentclean.csv', col.names = FALSE)

#DTM
dtm <- TermDocumentMatrix(cleaned_hotelcorpus) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

#
hotelfreq<-findFreqTerms(hotelDTM,1)
hotelDTMfreq<-hotelDTM[,hotelfreq]

dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(cleaned_hotelcorpus,min.freq = 4,max.words=100,random.order=F,colors=brewer.pal(8,"Dark2"))

convert_counts<-function(x){x<-ifelse(x>0,"yes","no")}
hoteltrain<-apply(hotelDTMfreq,MARGIN=2,convert_counts)

hotels <- unnest_tokens(hotel, words, Review)
hotels <- anti_join(hotels, stop_words, by = c('words' = 'word'))

word_count <- dplyr::count(hotels, words, sort = TRUE)

sentiments <-get_sentiments("nrc")
sentiments <- dplyr::select(sentiments, word, sentiment)

hotels_sentiments <- merge(word_count, sentiments, by.x = c('words'), by.y = c('word'))

sentiments_count <- dplyr::count(hotels_sentiments, sentiment, sort = TRUE)

sentiments_sum <- sum(sentiments_count$'n')
sentiment_count <- rbind(sentiments_count)
sentiment_count <- sentiment_count[order(sentiment_count$sentiment), ]
sentiment_count

par(mar=rep(3,4))
barplot(sentiment_count$'n', names.arg = sentiment_count$'sentiment', col=rainbow(10), cex.names = .9)

# Shiny
hotels <- vroom(here("proyek","commentclean.csv"))
hotels <- hotels$review
ui <- fluidPage(
  titlePanel("Analisa"),
  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Bagan", plotOutput("scatterplot")), 
                # Plot
                tabPanel("Data", DT::dataTableOutput('tbl')), 
                # Output Data Dalam Tabel
                tabPanel("Wordcloud", plotOutput("Wordcloud"))
    )
  )
)

# Server

server <- function(input, output) {
  
  # Output Data
  output$tbl = DT::renderDataTable({
    DT::datatable(hotel, options = list(lengthChange = FALSE))
  })
  
  output$scatterplot <- renderPlot({produk_dataset<-read.csv("commentclean.csv",stringsAsFactors = FALSE)
  par(mar=rep(3,4))
  barplot(sentiment_count$'n', names.arg = sentiment_count$'sentiment', col=rainbow(10), cex.names = .7)
  }, height=400)
  
  output$Wordcloud <- renderPlot({
    library(wordcloud)
   # dev.new(width = 1000, height = 1000, unit = "px")
    wordcloud(cleaned_hotelcorpus, min.freq = 30, max.words = 200, random.order=F,colors=brewer.pal(8,"Dark2"))
  })
}
shinyApp(ui = ui, server = server)
