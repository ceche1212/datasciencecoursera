setwd("~/Data Science/Jhon Hopkins/Capstone/Week2")

list.files()

library(ggplot2)
library(quanteda)
library(data.table)
library(dplyr)

#file information and size

file.info("en_US.blogs.txt")$size
file.info("en_US.news.txt")$size
file.info("en_US.twitter.txt")$size

#read data

blogs<-readLines("en_US.blogs.txt",skipNul = TRUE, warn = TRUE)
news<-readLines("en_US.news.txt",skipNul = TRUE, warn = TRUE)
twitter<-readLines("en_US.twitter.txt",skipNul = TRUE,warn = TRUE)

#number of lines

length(blogs)
length(news)
length(twitter)

#max number of characters per line

max(nchar(blogs))
max(nchar(news)) 
max(nchar(twitter))

#mean number of characters per line

mean(nchar(blogs))
mean(nchar(news))
mean(nchar(twitter))

#summary of nchar

summary(nchar(blogs))
summary(nchar(news))
summary(nchar(twitter))

#data sample unification and saving the file

set.seed(3007)
sam_blogs<-sample(blogs,size = 3000,replace = TRUE)
sam_news<-sample(news,size = 3000,replace = TRUE)
sam_twitter<-sample(twitter,size = 3000,replace = TRUE)
muestra<-c(sam_blogs,sam_news,sam_twitter)

writeLines(muestra,"muestra.txt")

#removing original data from memory

rm(blogs,twitter,news)
rm(sam_blogs,sam_news,sam_twitter)

# transformir the text into a data.frame

muestra<-as.data.frame(muestra)
names(muestra)<-c("text")
muestra$text<-as.character(muestra$text)

#determining if after unification there is missing values

length(which(!complete.cases(muestra)))

#Unigram

#tokenization using quanteda
#the quanteda package offer multiple options for cleaning the data

train.tokens<-tokens(muestra$text,what="word",remove_numbers = TRUE, remove_punct = TRUE,
                     remove_symbols = TRUE, remove_separators = TRUE,
                     remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)

#lowercase the tokens

train.tokens<-tokens_tolower(train.tokens)

#Create a Unigram DFM matrix using quanteda

train.tokens.dfm <- dfm(train.tokens,tolower = FALSE)


#Calculating the term frequency of each word and arranging them by the most frequent

unigram_freq<-colSums(train.tokens.dfm)
termFreq <- data.frame(unigram=names(unigram_freq), frequency=unigram_freq)

termFreq <-arrange(termFreq,desc(termFreq$frequency))
top10<-head(termFreq,10)

#plotting the most frequent words

g1<-ggplot(data = top10,aes(x=reorder(unigram,-frequency),y=top10$frequency))
g1<-g1 + geom_bar(stat = "identity",fill = "steelblue")+geom_text(data = top10,aes(x=top10$unigram,y=top10$frequency,label = top10$frequency),hjust=0.5,vjust=2, position = "identity",size = 5,color = "White")
g1<-g1 + ggtitle("Unigram Top 10 Frequently Words") + xlab("Unigrams") + ylab("Frequency")
g1

#bigram creation

bigram.token <-tokens_ngrams(train.tokens,n=2)
bigram.token.dfm <- dfm(bigram.token,tolower = FALSE)

bigram_freq<-colSums(bigram.token.dfm)
bigram_termfreq<-data.frame(bigram=names(bigram_freq),frequency=bigram_freq)

bigram_termfreq<-arrange(bigram_termfreq,desc(bigram_termfreq$frequency))
top10bigram<-head(bigram_termfreq,10)

#plotting bigrams

g2<-ggplot(data = top10bigram,aes(x=reorder(bigram,-frequency),y=top10bigram$frequency))
g2<-g2 + geom_bar(stat = "identity",fill = "seagreen4")+geom_text(data = top10bigram,aes(x=top10bigram$bigram,y=top10bigram$frequency,label = top10bigram$frequency),hjust=0.5,vjust=2, position = "identity",size = 5,color = "White")
g2<-g2 + ggtitle("Bigram Top 10 Frequently Words") + xlab("Bigrams") + ylab("Frequency")
g2

#trigrams creation

trigram.token <-tokens_ngrams(train.tokens,n=3)
trigram.token.dfm<-dfm(trigram.token,tolower = FALSE)

trigram_freq<-colSums(trigram.token.dfm)
trigram_termfreq<-data.frame(trigram=names(trigram_freq),frequency=trigram_freq)

trigram_termfreq<-arrange(trigram_termfreq,desc(trigram_termfreq$frequency))
top10trigram<-head(trigram_termfreq,10)

#plotting trigrams

g3<-ggplot(data = top10trigram,aes(x=reorder(trigram,-frequency),y=top10trigram$frequency))
g3<-g3 + geom_bar(stat = "identity",fill = "red4")+geom_text(data = top10trigram,aes(x=top10trigram$trigram,y=top10trigram$frequency,label = top10trigram$frequency),hjust=0.5,vjust=2, position = "identity",size = 5,color = "White")
g3<-g3 + ggtitle("Trigram Top 10 Frequently Words") + xlab("Trigrams") + ylab("Frequency")
g3

#quadgrams creation

quadgram.token <-tokens_ngrams(train.tokens,n=4)
quadgram.token.dfm<-dfm(quadgram.token,tolower=FALSE)

quadgram_freq<-colSums(quadgram.token.dfm)
quadgram_termfreq<-data.frame(quadgram=names(quadgram_freq),frequency=quadgram_freq)
quadgram_termfreq<-arrange(quadgram_termfreq,desc(quadgram_termfreq$frequency))

top10quadgram<-head(quadgram_termfreq,10)

#plotting quadgrams

g4<-ggplot(data = top10quadgram,aes(x=reorder(quadgram,-frequency),y=top10quadgram$frequency))
g4<-g4 + geom_bar(stat = "identity",fill = "purple4")+geom_text(data = top10quadgram,aes(x=top10quadgram$quadgram,y=top10quadgram$frequency,label = top10quadgram$frequency),hjust=0.5,vjust=2, position = "identity",size = 5,color = "White")
g4<-g4 + ggtitle("Quadgram Top 10 Frequently Words") + xlab("Quadgrams") + ylab("Frequency")
g4<-g4 + theme(axis.text.x=element_text(angle=45,hjust=1))
g4

#Pentagram Creation

pentagram.token <-tokens_ngrams(train.tokens,n=5)
pentagram.token.dfm<-dfm(pentagram.token,tolower = FALSE)

pentagram_freq<-colSums(pentagram.token.dfm)
pentagram_termfreq<-data.frame(pentagram=names(pentagram_freq),frequency=pentagram_freq)
pentagram_termfreq<-arrange(pentagram_termfreq,desc(pentagram_termfreq$frequency))


top10pentagram<-head(pentagram_termfreq,10)

#plotting pentagrams

g5<-ggplot(data = top10pentagram,aes(x=reorder(pentagram,-frequency),y=top10pentagram$frequency))
g5<-g5 + geom_bar(stat = "identity",fill = "darkorange4")+geom_text(data = top10pentagram,aes(x=top10pentagram$pentagram,y=top10pentagram$frequency,label = top10pentagram$frequency),hjust=0.5,vjust=2, position = "identity",size = 5,color = "White")
g5<-g5 + ggtitle("Pentagram Top 10 Frequently Words") + xlab("Pentagrams") + ylab("Frequency")
g5<-g5 + theme(axis.text.x=element_text(angle=45,hjust=1))
g5

# grepl("^the",pentagram_termfreq$pentagram)), perhaps for the model

seqs<-textstat_collocations(train.tokens)
seqs2<-textstat_collocations(train.tokens,size = 3,min_count = 2)
intermedia<-seqs2[grepl("one of",seqs2$collocation),]
intermedia<-arrange(intermedia,desc(intermedia$count))
strsplit(intermedia[1,1]," ")

#to handel the input of the function

prueba3<-"yo amo a stephania noemi kossman farias"
prueba3<-strsplit(prueba3," ")[[1]] 
prueba3<-as.data.frame(prueba3)
names(prueba3)<-c("words")


