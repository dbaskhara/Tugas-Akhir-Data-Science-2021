library(shiny)
library(datasets)
library(stringr)
library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(syuzhet)
library(tidytext)
library(tidyr)
library(igraph)
library(ggraph)
library(readr)
library(circlize)
library(reshape2)
library(tm)
library(wordcloud)

# We tweak the "am" field to have nicer factor labels. Since this doesn't
# rely on any user inputs we can do this once at startup and then use the
# value throughout the lifetime of the application


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
    lyrics <- read.csv(file.choose())
    lyrics$length <- str_count(lyrics$lyric)
    length_df <- lyrics %>%
    group_by(track_title) %>%
    summarise(length = sum(length))
    test <- reactive({
        if(input$Pilihan == "1"){
            test <- "Halaman 1";
        }
        else if(input$Pilihan == "2"){
            test <- "Halaman 2";
        }
        else if(input$Pilihan == "3"){
            test <- "Halaman 3";
        }
        else if(input$Pilihan == "4"){
            test <- "Halaman 4";
        }
        return (test);
    })
    
    test1 <- reactive({
        if(input$Pilihan == "1"){
            lyrics_text <- lyrics$lyric
            #creating a text corpus
            docs <- Corpus(VectorSource(lyrics_text))
            # Converting the text to lower case
            docs <- tm_map(docs, content_transformer(tolower))
            # creating term document matrix
            tdm <- TermDocumentMatrix(docs)
            # defining tdm as matrix
            m <- as.matrix(tdm)
            # getting word counts in decreasing order
            word_freqs = sort(rowSums(m), decreasing=TRUE)
            # creating a data frame with words and their frequencies
            lyrics_wc_df <- data.frame(word=names(word_freqs), freq=word_freqs)
            
            lyrics_wc_df <- lyrics_wc_df[1:300,]
            
            # plotting wordcloud
            
            set.seed(1234)
            test1 <- wordcloud(words = lyrics_wc_df$word, freq = lyrics_wc_df$freq,
                      min.freq = 1,scale=c(1.8,.5),
                      max.words=200, random.order=FALSE, rot.per=0.15,colors=brewer.pal(8, 'Dark2'))
        }
        else if(input$Pilihan == "2"){
            # Getting the sentiment value for the lyrics
            ty_sentiment <- get_nrc_sentiment((lyrics_text))
            
            # Dataframe with cumulative value of the sentiments
            sentimentscores<-data.frame(colSums(ty_sentiment[,]))
            names(sentimentscores) <- 'Score'
            sentimentscores <- cbind('sentiment'=rownames(sentimentscores),sentimentscores)
            rownames(sentimentscores) <- NULL
            
            # Plot for the cumulative sentiments
            test1 <- ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
                geom_bar(aes(fill=sentiment),stat = 'identity')+
                theme(legend.position='none')+
                xlab('sentiments')+ylab('Scores')+
                theme_minimal()
        }
        else if(input$Pilihan == "3"){
            
            lyrics$lyric <- as.character(lyrics$lyric)
            
            tidy_lyrics <- lyrics %>%
                unnest_tokens(word,lyric)
            
            song_wrd_count <- tidy_lyrics %>% count(track_title)
            
            lyric_counts <- tidy_lyrics %>%
                left_join(song_wrd_count, by = "track_title") %>%
                rename(total_words=n)
            
            lyric_sentiment <- tidy_lyrics %>%
                inner_join(get_sentiments('nrc'),by='word')
            
            test1 <- lyric_sentiment %>%
                count(word,sentiment,sort=TRUE) %>%
                group_by(sentiment)%>%top_n(n=10) %>%
                ungroup() %>%
                ggplot(aes(x=reorder(word,n),y=n,fill=sentiment)) +
                geom_col(show.legend = FALSE) +
                facet_wrap(~sentiment,scales='free') +
                xlab('Sentiments') + ylab('Scores')+
                ggtitle('Top words used to express emotions and sentiments') +
                coord_flip()
            
        }
        else if(input$Pilihan == "4"){
            lyrics$lyric <- as.character(lyrics$lyric)
            
            tidy_lyrics <- lyrics %>%
                unnest_tokens(word,lyric)
            bng <- get_sentiments("bing")
            
            set.seed(1234)
            
            test1 <- tidy_lyrics %>%
                inner_join(get_sentiments('bing')) %>%
                count(word, sentiment, sort = TRUE) %>%
                acast(word ~ sentiment, value.var = 'n', fill = 0) %>%
                comparison.cloud(colors = c('#F8766D', '#00BFC4'),
                                 max.words = 250)
        }
        return (test1);
    })
    
    # Return the formula text for printing as a caption
    output$caption <- renderText({
        test()
    })
    
    output$plot <- renderPlot({
        test1()
    })
})