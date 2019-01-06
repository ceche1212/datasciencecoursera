setwd("~/Data Science/Jhon Hopkins/Capstone/Week3")

list.files()

library(ggplot2)
library(quanteda)
library(data.table)
library(dplyr)

blogs<-readLines("en_US.blogs.txt",skipNul = TRUE, warn = TRUE)
news<-readLines("en_US.news.txt",skipNul = TRUE, warn = TRUE)
twitter<-readLines("en_US.twitter.txt",skipNul = TRUE,warn = TRUE)
longblogs<-length(blogs)
longnews<-length(news)
longtwitter<-length(twitter)

set.seed(3007)
sam_blogs<-sample(blogs,size = (longblogs/10),replace = TRUE)
sam_news<-sample(news,size = (longnews/10),replace = TRUE)
sam_twitter<-sample(twitter,size = (longtwitter/10),replace = TRUE)
muestra<-c(sam_blogs,sam_news,sam_twitter)

writeLines(muestra,"muestragrande.txt")

muestra<-as.data.frame(muestra)
names(muestra)<-c("text")
muestra$text<-as.character(muestra$text)


rm(blogs,twitter,news)
rm(sam_blogs,sam_news,sam_twitter)

length(which(!complete.cases(muestra)))

train.tokens<-tokens(muestra$text,what="word",remove_numbers = TRUE, remove_punct = TRUE,
                     remove_symbols = TRUE, remove_separators = TRUE,
                     remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)

#lowercase the tokens

#creating the unigram

train.tokens<-tokens_tolower(train.tokens)
train.tokens.dfm <- dfm(train.tokens,tolower = FALSE)
unigram_freq<-colSums(train.tokens.dfm)


Unigram <- data.frame(words=names(unigram_freq), count=unigram_freq)

#creating the bigram

bigram.token <-tokens_ngrams(train.tokens,n=2,concatenator = " ")
bigram.token.dfm <- dfm(bigram.token,tolower = FALSE)

bigram_freq<-colSums(bigram.token.dfm)
Bigram<-data.frame(word=names(bigram_freq),count=bigram_freq)
Bigram$word<-as.character(Bigram$word)
Bigram<-filter(Bigram,Bigram$count>5)

#creating the trigram

trigram.token <-tokens_ngrams(train.tokens,n=3, concatenator = " ")
trigram.token.dfm<-dfm(trigram.token,tolower = FALSE)

trigram_freq<-colSums(trigram.token.dfm)
Trigram<-data.frame(word=names(trigram_freq),count=trigram_freq)
Trigram$word<-as.character(Trigram$word)
Trigram<-filter(Trigram,Trigram$count>5)

#Creating a quadgram

quadgram.token <-tokens_ngrams(train.tokens,n=4, concatenator = " ")
quadgram.token.dfm<-dfm(quadgram.token,tolower=FALSE)

quadgram_freq<-colSums(quadgram.token.dfm)
Quadgram<-data.frame(word=names(quadgram_freq),count=quadgram_freq)
Quadgram$word<-as.character(Quadgram$word)
Quadgram<-filter(Quadgram,Quadgram$count>5)


saveRDS(Unigram,file="unigram.RData")
saveRDS(Bigram,file="bigram.RData")
saveRDS(Trigram,file = "trigram.RData")
saveRDS(Quadgram,file = "quadgram.RData")

topworduni<-arrange(Unigram,desc(Unigram$count))
topworduni<-topworduni[1,1]
topworduni<-as.character(topworduni)
saveRDS(topworduni,file = "topworduni.Rdata")


