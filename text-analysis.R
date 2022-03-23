# Creating bag of words and removal of upper case, white space, punctuation, and word stemming.

#Pull in the libraries
stopifnot(require(wordcloud))
stopifnot(require(tm))
stopifnot(require(dplyr))
stopifnot(require(data.table))
stopifnot(require(SnowballC))
stopifnot(require(lsa))
stopifnot(require(broom))
stopifnot(require(scales))
library(tidyverse)
library(tidytext)
library(stringr)
library(wordcloud)
library(tm)

#set working directory to pull in each dataset
setwd("fill-in")
dem <- read.csv('/democrats_merged.csv', header=TRUE)
rep <- read.csv('republicans_merged.csv', header=TRUE)

#Create bag of words and remove odd words that I was able to find/punctuation etc
dem$X0 <- gsub("&amp", " ", dem$X0)
dem$X0 <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", dem$X0)
dem$X0 <- gsub("@\\w+", " ", dem$X0)
dem$X0 <- gsub("[[:punct:]]", " ", dem$X0)
dem$X0 <- gsub("[[:digit:]]", " ", dem$X0)
dem$X0 <- gsub("http\\w+", " ", dem$X0)
dem$X0 <- gsub("[ \t]{2,}", " ", dem$X0)
dem$X0 <- gsub("^\\s+|\\s+$", " ", dem$X0) 
dem$X0 <- gsub("state", " ", dem$X0,ignore.case = TRUE) 
dem$X0 <- gsub("governor|will|also|address|www|delivers", " ", dem$X0,ignore.case = TRUE) 
dem_text <- sapply(dem$X0, function(row) iconv(row, "latin1", "ASCII", sub=""))
dem_2 <- paste(unlist(dem_text), collapse =" ")
dem_2 <- Corpus(VectorSource(dem_2))
dem_2 <- tm_map(dem_2, PlainTextDocument)
dem_2 <- tm_map(dem_2, removePunctuation)
dem_2 <- tm_map(dem_2, content_transformer(tolower))
dem_2 <- tm_map(dem_2, removeWords, stopwords("english"))

rep$X0 <- gsub("&amp", " ", rep$X0)
rep$X0 <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", rep$X0)
rep$X0 <- gsub("@\\w+", " ", rep$X0)
rep$X0 <- gsub("[[:punct:]]", " ", rep$X0)
rep$X0 <- gsub("[[:digit:]]", " ", rep$X0)
rep$X0 <- gsub("http\\w+", " ", rep$X0)
rep$X0 <- gsub("[ \t]{2,}", " ", rep$X0)
rep$X0 <- gsub("^\\s+|\\s+$", " ", rep$X0) 
rep$X0 <- gsub("state", " ", rep$X0,,ignore.case = TRUE) 
rep$X0 <- gsub("governor|will|also|address|www|delivers", " ", rep$X0,ignore.case = TRUE) 
rep_text <- sapply(rep$X0, function(row) iconv(row, "latin1", "ASCII", sub=""))
rep_2 <- paste(unlist(rep_text), collapse =" ")
rep_2 <- Corpus(VectorSource(rep_2))
rep_2 <- tm_map(rep_2, PlainTextDocument)
rep_2 <- tm_map(rep_2, removePunctuation)
rep_2 <- tm_map(rep_2, content_transformer(tolower))
rep_2 <- tm_map(rep_2, removeWords, stopwords("english"))
```

# Find relative word frequencies for each bag of words & compare top 15
dem.dtm <- TermDocumentMatrix(dem_2)
dem.m <- as.matrix(dem.dtm)
dem.v <- sort(rowSums(dem.m),decreasing=TRUE)
dem.d <- data.frame(word = names(dem.v),freq=dem.v)
head(dem.d, 15) #top 15

rep.dtm <- TermDocumentMatrix(rep_2)
rep.m <- as.matrix(rep.dtm)
rep.v <- sort(rowSums(rep.m),decreasing=TRUE)
rep.d <- data.frame(word = names(rep.v),freq=rep.v)
head(rep.d, 15) #top 15

#merging both together based on the word
all.corpus <- c(dem_2, rep_2)
all.corpus <- Corpus(VectorSource(all.corpus))
all.tdm <- TermDocumentMatrix(all.corpus)
all.m <- as.matrix(all.tdm)
all.df = as.data.frame(all.m)
all.df = all.df[,c(1,4)]
colnames(all.df) <- c("dem", "rep")
df <- cbind(names = rownames(all.df), all.df)
rownames(df) <- 1:nrow(df)

#Creating a wordcloud for each based on the column name
wordcloud(df$names, df$dem, min.freq=50, random.color=T, 
ordered.colors=T) #democrats

wordcloud(df$names, df$rep, min.freq=50, random.color=T, 
ordered.colors=T) #republicans

#looking at specific words of interest 
df %>% filter(names=='climate')
df %>% filter(names=='nation')

# Further differentiation using distinctive words of each by taking the difference in frequencies to identify most frequent words in each over the other
summing = function(x) x/sum(x, na.rm=T)
df.2 = apply(all.df, 2, summing)
df.2 <- cbind(names = rownames(df.2), df.2)
rownames(df.2) <- 1:nrow(df.2)
total <- merge(df,df.2,by="names")
i <- c('dem.y', 'rep.y')                                  
total[ , i] <- apply(total[ , i], 2,           
                     function(x) as.numeric(as.character(x)))
total$dem.over.rep = (total$dem.y) - (total$rep.y)
sort.OT <- total[order(total$dem.over.rep) , ]
(sort.OT[1:15, ])
rev.sort.OT <- total[rev(order(total$dem.over.rep) ), ]
rev.sort.OT[1:15, ]

#Looking at the graph of this difference
to_graph = sort.OT[1:50, ]
q = qplot(dem.x, rep.x, data = to_graph)
q<- q + geom_text(aes(label=names), size = 4.5) 

# Statistical tests of association between the bags of words 
library(lsa)
cosine <- as.data.frame(cosine(all.m))[c(1,4),c(1,4)] #cosine
colnames(cosine) <- c("dem", "rep")
cosine
ctable <- table(all.m) #chisquared test
chisq.test(ctable)

# Sentiment analysis of the bags of words
rep.words = as.data.frame(rep.m)
rep.words <- cbind(names = rownames(rep.words), rep.words)
rownames(rep.words) <- 1:nrow(rep.words)

dem.words = as.data.frame(dem.m)
dem.words <- cbind(names = rownames(dem.words), dem.words)
rownames(dem.words) <- 1:nrow(dem.words)

#restarting the directory due to changes to the files thus far
setwd("fill-in")
dem <- read.csv('/democrats_merged.csv', header=TRUE)
rep <- read.csv('republicans_merged.csv', header=TRUE)

dem$X0 <- gsub("&amp", " ", dem$X0)
dem$X0 <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", dem$X0)
dem$X0 <- gsub("@\\w+", " ", dem$X0)
dem$X0 <- gsub("[[:punct:]]", " ", dem$X0)
dem$X0 <- gsub("[[:digit:]]", " ", dem$X0)
dem$X0 <- gsub("http\\w+", " ", dem$X0)
dem$X0 <- gsub("[ \t]{2,}", " ", dem$X0)
dem$X0 <- gsub("^\\s+|\\s+$", " ", dem$X0) 
dem$X0 <- gsub("state", " ", dem$X0) 
dem$X0 <- gsub("governor", " ", dem$X0) 

rep$X0 <- gsub("&amp", " ", rep$X0)
rep$X0 <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", rep$X0)
rep$X0 <- gsub("@\\w+", " ", rep$X0)
rep$X0 <- gsub("[[:punct:]]", " ", rep$X0)
rep$X0 <- gsub("[[:digit:]]", " ", rep$X0)
rep$X0 <- gsub("http\\w+", " ", rep$X0)
rep$X0 <- gsub("[ \t]{2,}", " ", rep$X0)
rep$X0 <- gsub("^\\s+|\\s+$", " ", rep$X0) 
rep$X0 <- gsub("state", " ", rep$X0) 
rep$X0 <- gsub("governor", " ", rep$X0) 

#tokenizing and removing stop words
tokenize_d <- tibble(line=1:4530,text=dem$X0) #the end row# is the dataframe length
data(stop_words)
to_sent_d <- tokenize_d %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tokenize_r <- tibble(line=1:5358,text=rep$X0)
to_sent_r <- tokenize_r %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#Code from here: https://www.tidytextmining.com/sentiment.html

#getting sentiments for each and plotting them yo identify negative and positive word frequencies in each group by counting words based on sentiment
dem_sentiment <- to_sent_d %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = line %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

rep_sentiment <- to_sent_r %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = line %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(dem_sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE) 
ggplot(rep_sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE) 


#identifying top negative and positive words in each bucket
d_word_counts <- to_sent_d %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

d_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)
r_word_counts <- to_sent_r %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

r_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

# Topic Modelling
library(topicmodels)
dem.dtm <- DocumentTermMatrix(dem_2)
d_lda <- LDA(dem.dtm, k = 3, control = list(seed = 1234))
d_topics <- tidy(d_lda, matrix = "beta")

#id top democrat terms to do lda
d_top_terms <- d_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>% 
  ungroup() %>%
  arrange(topic, -beta)

d_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() + ggtitle('Three Democrat Topics')

rep.dtm <- DocumentTermMatrix(rep_2)
r_lda <- LDA(rep.dtm, k = 3, control = list(seed = 1234))
r_topics <- tidy(r_lda, matrix = "beta")

#id top republican terms to do lda

r_top_terms <- r_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>% 
  ungroup() %>%
  arrange(topic, -beta)

r_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() + ggtitle('Three Republican Topics')

# Bigram Analysis
#Code is from here: https://bookdown.org/Maxine/tidy-text-mining/tokenizing-by-n-gram.html

#creating bigrams for each group (remove odd words that I was able to find/punctuation etc)
tokenize_r$text <- gsub("&amp", " ", tokenize_r$text)
tokenize_r$text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tokenize_r$text)
tokenize_r$text <- gsub("@\\w+", " ", tokenize_r$text)
tokenize_r$text <- gsub("[[:punct:]]", " ", tokenize_r$text)
tokenize_r$text <- gsub("[[:digit:]]", " ", tokenize_r$text)
tokenize_r$text <- gsub("http\\w+", " ",tokenize_r$text)
tokenize_r$text <- gsub("[ \t]{2,}", " ", tokenize_r$text)
tokenize_r$text <- gsub("^\\s+|\\s+$", " ", tokenize_r$text) 
tokenize_r$text <- gsub("NA}state|watch|address", " ", tokenize_r$text,ignore.case = TRUE)
tokenize_r$text <- gsub("governor", " ", tokenize_r$text,ignore.case = TRUE)
tokenize_r$text <- gsub("delivers|subscribe|version|lady|id|kim reynolds", " ", tokenize_r$text,ignore.case = TRUE)
tokenize_r$text <- gsub("press|release|news|speech|releases|lieute|phil|gov|www|pm|site|sites|delivery|lly|fi|se|tor|ent", " ", tokenize_r$text,ignore.case = TRUE)


r_bigrams <- tokenize_r %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
r_bigrams <- r_bigrams %>%
  count(bigram, sort = TRUE) %>%  
  separate(bigram, into = c("word1", "word2"), sep = " ")%>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  unite(bigram, c(word1, word2), sep = " ")

tokenize_d$text <- gsub("&amp", " ", tokenize_d$text)
tokenize_d$text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tokenize_d$text)
tokenize_d$text <- gsub("@\\w+", " ", tokenize_d$text)
tokenize_d$text <- gsub("[[:punct:]]", " ", tokenize_d$text)
tokenize_d$text <- gsub("[[:digit:]]", " ", tokenize_d$text)
tokenize_d$text <- gsub("http\\w+", " ",tokenize_d$text)
tokenize_d$text <- gsub("[ \t]{2,}", " ", tokenize_d$text)
tokenize_d$text <- gsub("^\\s+|\\s+$", " ", tokenize_d$text) 
tokenize_d$text <- gsub("state", " ", tokenize_d$text,ignore.case = TRUE)
tokenize_d$text <- gsub("watch", " ", tokenize_d$text,ignore.case = TRUE)
tokenize_d$text <- gsub("address", " ", tokenize_d$text,ignore.case = TRUE)
tokenize_d$text <- gsub("NA", " ", tokenize_d$text,ignore.case = TRUE)
tokenize_d$text <- gsub("governor", " ", tokenize_d$text,ignore.case = TRUE)
tokenize_d$text <- gsub("delivers", " ", tokenize_d$text,ignore.case = TRUE)
tokenize_d$text <- gsub("press|release|news|speech|releases|lieute|phil|gov|www|pm|site|sites|delivery|lly|fi|se|tor|ent", " ", tokenize_d$text,ignore.case = TRUE)

d_bigrams <- tokenize_d %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
d_bigrams <- d_bigrams %>%
  count(bigram, sort = TRUE) %>%  
  separate(bigram, into = c("word1", "word2"), sep = " ")%>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  unite(bigram, c(word1, word2), sep = " ")


#word cloud for democrat bigrams
d<-data.frame(d_bigrams)
d<-d[2:8613,]
wordcloud(d$bigram, d$n, min.freq=7, random.color=T, 
          ordered.colors=T)

#word cloud for republican bigrams
r<-data.frame(r_bigrams)
r<-r[2:9336,]
wordcloud(r$bigram, r$n, min.freq=13, random.color=T, 
          ordered.colors=T)

#merging them together to see differences between the two
bigramdf <- merge(d,r,by='bigram')
colnames(bigramdf) <- c('bigram','dem','rep')
bigramdf$dem.over.rep = (bigramdf$dem) - (bigramdf$rep)
sort.OT <- bigramdf[order(bigramdf$dem.over.rep) , ]
(sort.OT[1:25, ])
rev.sort.OT <- bigramdf[rev(order(bigramdf$dem.over.rep) ), ]
rev.sort.OT[1:25, ]

#filtering to bigrams that contain energy
filter(bigramdf, grepl("energy", bigram, ignore.case = TRUE))
