library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(wordcloud)
library(sentimentr)
library(tidytext)
library(textdata)
library(janeaustenr)
library(dplyr)
library(stringr)
library(reshape2)

rm(list=ls())

setwd("C:/data 332/archive")

#Renaming different columns
complaints_table<-readRDS("Consumer_Complaints.rds")
colnames(complaints_table)[1]<-"Date"
colnames(complaints_table)[6]<-"Consumer_response"
colnames(complaints_table)[15]<-"Company_response"
colnames(complaints_table)[18]<-"Complaint_ID"

#selecting a table
selected<-complaints_table%>%
  dplyr::select(Company, Consumer_response)


get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

#Breaking down the sentence into words by row
Data_by_word <- selected%>%
  group_by(Company) %>%
  mutate(linenumber = row_number())%>%
  ungroup() %>%
  unnest_tokens(word, Consumer_response)

#removing unwanted words
Data_by_word <- Data_by_word %>%
  anti_join(stop_words)
Data_by_word<-filter(Data_by_word, word != "xxxx" )
Data_by_word<-filter(Data_by_word, word != "xx" )

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")
  
#starting to inner join words to create sentiment value
Data_by_word %>%
  filter(Company == "Bank of America") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

#Positive, negative and sentiment values for each companies
clean_Data <- Data_by_word %>%
  inner_join(get_sentiments("bing")) %>%
  count(Company, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

#creating pivot tables to compare highest and lowest companies with sentiments
df <- clean_Data %>% group_by(Company) %>% 
  summarise(sum_sentiment=sum(sentiment),
            .groups = 'drop') %>%
  as.data.frame()
#put the data above in descending order
df2<-df[order(df$sum_sentiment, decreasing = TRUE),]

top_5<-df2[1:5,]

#top 5 companies plot
ggplot(data=top_5,aes(Company,sum_sentiment))+
  geom_histogram(stat="identity",aes(color=Company))+
  labs( title = "Top 5 companies with good sentiments",x="Company", y="Sentiment value")+
  scale_color_manual(values=c('Red','Orange','Green','Yellow', 'Violet'))

low_5<-df2[2679:2683,]
#low 5 companies plot
ggplot(data=low_5,aes(Company,sum_sentiment))+
  geom_histogram(stat="identity",aes(color=Company))+
  labs( title = "Top 5 companies with bad sentiments",x="Company", y="Sentiment value")+
  scale_color_manual(values=c('Red','Orange','Green','Yellow', 'Violet'))


#counting sentiments in the dataset above, nrc and bing 
get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)

get_sentiments("bing") %>% 
  count(sentiment)


bing_word_counts <- Data_by_word %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# graph of positive and negative words comparison
bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)


#wordcloud of 100 words
bing_word_counts %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


#ShinnyApp

new_data<-Data_by_word
column_names<-colnames(Data_by_word)
ui<-fluidPage( 
  
  titlePanel(title = "Top 5 companies with better and worse sentiments"),
  
  
  fluidRow(
    column(2,
           selectInput('X', 'choose company',column_names,column_names[1]),
           selectInput('Y', 'Choose Y',column_names,column_names[2]),
           #selectInput('Splitby', 'Split By', column_names,column_names[3])
    ),
    column(4,plotOutput('plot_01')),
    column(6,DT::dataTableOutput("table_01", width = "100%"))
  )
)
    server<-function(input,output){
      
      output$plot_01 <- renderPlot({
        ggplot(Data_by_word,aes_string(x=input$X,y=input$Y))+
          
          geom_smooth()
        
      })
      
      output$table_01<-DT::renderDataTable(Data_by_word[,c(input$X,input$Y,input$Splitby)],options = list(pageLength = 4))
    }
    
    shinyApp(ui=ui, server=server)
    





    
  
  

