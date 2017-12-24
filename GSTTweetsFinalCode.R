


#Load the libraries 
library(magrittr)
library(dplyr)
library(stringr)
library(tidyverse)
library(tidytext)
library(date)

#Apply Lemmatization
library(koRpus)
library(ggplot2)
library(data.table)
library(text2vec)

library(reshape2)
library(wordcloud)

#Set the working directory
setwd("D:\\Harsha\\BA-BI\\Capstone\\data_path")
getwd()

#Variables Used
#Filename
File_To_Read<-"tweets_10nov_08dec.csv"
Text_To_Log<-"tweets_all"

#Read the data
tweets_read1<-read.csv("tweets_jan_mar.csv",stringsAsFactors = FALSE)
tweets_read2<-read.csv("tweets_apr_jun.csv",stringsAsFactors = FALSE)
tweets_read3<-read.csv("tweets_1jul_9nov.csv",stringsAsFactors = FALSE)
tweets_read4<-read.csv("tweets_10nov_08dec.csv",stringsAsFactors = FALSE)

str(tweets_read3)
str(tweets_read4)

tweets_read <- rbind(tweets_read1,tweets_read2,tweets_read3,tweets_read4)
#Head of Data
head(tweets_read)

str(tweets_read)

#Get the column names of data
colnames(tweets_read)

#Give the dimensions of the data
dim(tweets_read)

#Check the structure of data
str(tweets_read)

# Define a function to find the type of variable in the data frame
findClass <- function (x) {
  for (i in 1:8)
  {
    l1  <- names(x[i])
    l2  <- class(x[[i]])
    cat("\n ",l1,": ",l2)
  } 
}

#Checking the type of data
cat("\n Variables according to the class \n")
findClass(tweets_read)

datatype<-findClass(tweets_read)

#Checking the missing values
cat("\n Variables with number of missing values \n")
sapply(tweets_read, function(x) sum(is.na(x))) 

#Perform EDA using the summary function
summary(tweets_read)

#Viewing the data
View(tweets_read)


#Select only the columns required for further processing
tweets <- tweets_read %>%
  mutate(linenumber = row_number()) %>%
  mutate(date = as.Date(timestamp))

View(tweets)
write.csv(tweets,file=paste("Data",Text_To_Log,"tweets.csv"))

#Cleaning the data
tweets_clean <- tweets %>%
  filter(!is.na(text)) %>%
  filter(!str_detect(text, "^RT")) %>%   #anything starting with RT as it indicates retweet and duplicate
  mutate(text_clean = str_replace_all(text,"(ht)tp(s?)://(.*)[.][a-z]+.*", "")) %>% #Remove url start with http
  mutate(text_clean = str_replace_all(text_clean,"pic.twitter.com(.*)", "")) %>% #Remove pic.twiiter start with http
  mutate(text_clean = str_replace_all(text_clean,"\u2019s|'s",""))  %>% #Remove possessive words
  mutate(text_clean = str_replace_all(text_clean,"@\\w+"," ")) %>% #Remove UserHandles
  mutate(text_clean = str_replace_all(text_clean,"#\\w+"," "))  %>% #Remove Hashtags
  mutate(text_clean = str_replace_all(text_clean,"[[:digit:]]"," "))  %>% #Remove Numbers
  mutate(text_clean = str_replace_all(text_clean,"[[:punct:]]"," "))  %>% #Remove Punctuation characters
  mutate(text_clean = str_replace_all(text_clean,"[^\x20-\x7e]","")) #Remove non-ascii characters

write.csv(tweets_clean,file=paste("Data",Text_To_Log,"tweets_clean.csv"))

View(tweets_clean)

str(tweets_clean)

#Tokenize the data and remove extra characters
tweets_token <- tweets_clean %>%
  unnest_tokens(word, text_clean) %>%
  filter(str_detect(word, "[a-z']$"))

View(tweets_token)

dim(tweets_token)

#Remove stopwords
tweets_rm_stopwords <- tweets_token %>%
  anti_join(stop_words, by = c("word" = "word"))

View(tweets_rm_stopwords)

dim(tweets_rm_stopwords)

#Displaying the words with minimum frequency of 150 in descending order
tweets_rm_stopwords %>%
  count(word, sort=TRUE) %>%
  filter(n > 500) %>%
  mutate(word = reorder(word, n))

dim(tweets_rm_stopwords)
write.csv(tweets_rm_stopwords,file=paste("Data",Text_To_Log,"tweets_rm_stopwords.csv"))


#Lemmatization, conversion of diff parts of speech into common usage, like replied converted to reply
#Install treetagger
#copy english file in lib
#install perl
#set path as in install file
#restart the system
tweets_lemma <- treetag(tweets_rm_stopwords$word, treetagger="manual", 
                        format="obj", TT.tknz=FALSE , lang="en",
                        TT.options=list(
                          path="C:/TreeTagger", preset="en")
)

tweets_lemma_tagged_tbl <- tbl_df(tweets_lemma@TT.res)

dim(tweets_lemma_tagged_tbl)

write.csv(tweets_lemma_tagged_tbl,file=paste("Data",Text_To_Log,"tweets_lemma_tagged_tbl.csv"))

#counts of tokens which are not in english and not lemmatized
#as these words do not carry any positive or negative weightage, no further processing is considered 
tweets_lemma_tagged_tbl %>%
  filter(str_detect(lemma, "<unknown>"))   %>%
  count(token, sort=TRUE) %>%
  filter(n > 150) %>%
  mutate(token = reorder(token, n))

tweets_lemma_unique <- tweets_lemma_tagged_tbl %>% 
  mutate(word_clean = ifelse(lemma != "<unknown>",lemma,token))

dim(tweets_lemma_unique)
View(tweets_lemma_unique)

#df with the final set of words considered for analysis
tweets_final <- tweets_lemma_unique %>% 
  select(word_clean)


dim(tweets_final)


#final data on which algorithms would be applied
tweets_unique<-cbind(tweets_rm_stopwords,tweets_final)

View(tweets_unique)

tweets_unique %>% 
  count(word_clean,sort=TRUE) %>%
  filter(n >= 2500)  %>%
  mutate(word = reorder(word_clean, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle(paste('Most common words in tweets')) +
  theme(legend.position="none") +
  coord_flip()

#Based on the above word frequency
#removing certains words which do not add meaning fule sense
custom_stop_words <- data_frame(word_clean=c("twitter",
                                                 "gst",
                                                 "pic",
                                                 "tax",
                                                 "news",
                                                 "india",
                                                 "gl",
                                                 "goo",
                                                 "html",
                                                 "bills",
                                                 "fm",
                                                 "jaitley",
                                                 "lok",
                                                 "arun",
                                                 "www",
                                                 "indiatimes",
                                                 "gstnashik",
                                                 "e0",
                                                 "pm",
                                                 "cms",
                                                 "parliament",
                                                 "a4",
                                                 "#gst",
                                                 "ly",
                                                 "council",
                                                 "july"
                                                 ,"clears"
                                                 ,"bjp"
                                                 ,"modi"
                                                 ,"#tax")
                                          ,lexicon = c("custom"))

tweets_final <- tweets_unique %>%
  anti_join(custom_stop_words)

View(tweets_final)

tweets_word_frequency <-tweets_final %>% 
  count(word_clean,sort=TRUE)

write.csv(tweets_word_frequency,file=paste("Data",Text_To_Log,"tweets_word_frequency.csv"))

tweets_final %>% 
  count(word_clean,sort=TRUE) %>%
  filter(n >=1500 )  %>%
  mutate(word = reorder(word_clean, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle(paste('Most common words in tweets')) +
  theme(legend.position="none") +
  coord_flip()

#we can consider adding logic for replacing synonyms with common words


#logic to determine the words which are having frequency greater than 10
it = itoken(tweets_final$word_clean, progressbar = FALSE)

str(it)

class(it)

vocab <- create_vocabulary(it)

head (vocab)

write.csv(vocab,file=paste("Data",Text_To_Log,"vocab.csv"))

vocab <- prune_vocabulary(vocab, term_count_min = 10L)

View(vocab)

dim(vocab)

library(kableExtra)

vocab %>%
  sample_n(10) %>%
  arrange(term_count) %>%
  knitr::kable(format="markdown") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),full_width = F)


#Remove stopwords
tweets_final_data <- tweets_final %>%
  left_join(vocab, by = c("word_clean" = "term"))

View(tweets_final_data)

tweets_final_data <- tweets_final_data %>% select(date,linenumber,word=word_clean)

View(tweets_final_data)

sentiment_afinn <- tweets_final_data %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(date) %>% 
  summarise(score_afinn = sum(score)) %>%
  mutate(method = "AFINN")

View(sentiment_afinn)

sentiment_afinn %>%
  ggplot(aes(date, score_afinn, fill = "AFFIN")) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

sentiment_bing <- tweets_final_data %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(date, sentiment) %>%
  spread(sentiment,n, fill=0) %>%
  mutate(score_bing = positive - negative) %>%
  select(-positive, -negative) %>%
  ungroup()


View(sentiment_bing)

dim(sentiment_bing)

sentiment_nrc <- tweets_final_data %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(date, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  setNames(c(names(.)[1],paste0('nrc_', names(.)[-1]))) %>%
  mutate(score_nrc = nrc_positive - nrc_negative) %>%
  ungroup()

View(sentiment_nrc)


afinn <- tweets_final_data %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = date) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN")

View(afinn)

afinn %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")


bing_and_nrc <- bind_rows(tweets_final_data %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          tweets_final_data %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = date, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

tweets_sentiments <- Reduce(full_join,list(sentiment_nrc, sentiment_bing, sentiment_afinn)) %>% 
  mutate_if(is.numeric,funs(replace(., which(is.na(.)), 0)))

View(tweets_sentiments)


h_cors <-tweets_sentiments %>%
  select(starts_with("score")) %>%
  cor() %>%
  round(digits=2)

upper<-h_cors
upper[upper.tri(h_cors)]<-""
knitr::kable(upper, format="markdown", booktabs = TRUE)

#install.packages('hexbin')
library(hexbin)

tweets_sentiments %>%
  gather(emotion, intensity,starts_with("nrc_")) %>%
  filter(intensity > 0) %>%
  mutate(emotion = substring(emotion,5)) %>%
  ggplot(aes(x = score_nrc, y = score_bing)) +
  geom_hex(bins=5) +
  facet_wrap(~emotion, nrow = 2)


tweets_full <- full_join(tweets_clean, tweets_sentiments)  %>% 
  mutate_if(is.numeric,funs(replace(., which(is.na(.)), 0)))

#tweets_full <- tweets_full %>% mutate (nrc_score_nrc=score_nrc,nrc_score_bing=score_bing,nrc_score_afinn=score_afinn)

View(tweets_full)
#max_date<-max(tweets_full)

tweets_full %>%
  gather(emotion, intensity,starts_with("nrc_")) %>%
  mutate(emotion = substring(emotion,5)) %>%
  filter(!emotion %in% c("positive", "negative")) %>%
  ggplot(aes(x=date, y=intensity, color=emotion, fill=emotion)) +
  xlab("Date") +
  ylab(paste('Tweets','Text')) +
  geom_smooth(se = FALSE) +
  scale_color_brewer(palette="Spectral") 

tweets_full %>%
  gather(emotion, intensity,starts_with("score_")) %>%
  mutate(emotion = substring(emotion,7)) %>%
  filter(!emotion %in% c("positive", "negative")) %>%
  ggplot(aes(x=date, y=intensity, color=emotion, fill=emotion)) +
  xlab("Date") +
  ylab(paste('Tweets','Text')) +
  geom_smooth(se = FALSE) +
  scale_color_brewer(palette="Spectral") 



View(tweets_final_data)

#affin sentiment word counts and graph

afinn_words <- tweets_final_data %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(word = word) %>%
  summarise(n = n(),sentiment = sum(score)) %>%
  mutate(method = "AFINN")

View(afinn_words)

afinn_words %>%
  top_n(25, abs(sentiment)) %>%
  mutate(word = reorder(word, sentiment)) %>%
  ggplot(aes(word, sentiment, fill = sentiment > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip()
##

#bing sentiment word counts and graph
bing_word_counts <- tweets_final_data %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

View(bing_word_counts)


bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##

##nrc sentiment word counts and graph
nrc_word_counts <- tweets_final_data %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

View(nrc_word_counts)

nrc_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
##


dim (tweets_final_data)

#word comparision cloud
tweets_final_data %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 500)


tweets_final_data %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors=brewer.pal(10,"Dark2"), random.order = FALSE, title.size = 1.5,
                   max.words = 500)


