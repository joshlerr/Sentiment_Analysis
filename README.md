# Sentiment_Analysis
# Introduction  
I will do a sentiment analysis of the consumer complaint file to know which companies had the most sever complaints and which companies had good compliments  
![image](https://user-images.githubusercontent.com/118494139/222984814-6e63a002-6123-4d7b-95c8-7b3e6461b999.png)  
# Dictionary  
From the 18 columns present in the data, the ones we used for our sentiment analysis are the following:  

1. Company  
2. consumer response  
3. Issue  
# Cleaning the data  
Cleaning the data took most of the time during the analysis. The first thing that i did was renaming different columns into the names that we want so that it could be easier for us to read.  
```r
colnames(complaints_table)[1]<-"Date"
colnames(complaints_table)[6]<-"Consumer_response"
colnames(complaints_table)[15]<-"Company_response"
colnames(complaints_table)[18]<-"Complaint_ID"
```  
after that, we started breaking down the sentences under the consumer response column into individual words. this multiplied the dataset into millions which becomes hard to make a sentiment analysis. thus, we start using the stop_word function which cutts down unwanted words like "the", "is"..etc. then, we also cut of some words that are not important for our analysis.  
'''r
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
'''   







