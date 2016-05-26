library(igraph)
library(tm)
library(RCurl)
library(plyr)
library(pacman)
library(ggplot2)
library(twitteR)
library(stringr)
library(Rstem)
library(devtools)
library(sentiment)
library(wordcloud)
catch.error = function(x)
{
  y=NA
  catch_error = tryCatch(tolower(x),error=function(e) e)
  if(!inherits(catch_error,'error'))
    y=tolower(x)
  return(y)
}
cleanTweets<-function(tweet){
  #removing links
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  tweet <-  gsub("http[[:alnum:]]*", "", tweet)
  #retweet
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  # removing hashtags
  tweet = gsub("#\\w+", " ", tweet)
  # removing @people
  tweet = gsub("@\\w+", " ", tweet)
  #removing punctuations
  tweet = gsub("[[:punct:]]", " ", tweet)
  #removing numbers
  tweet = gsub("[[:digit:]]", " ", tweet)
  #removing emojis
  tweet<-str_replace_all(tweet,"[^[:graph:]]"," ")
  tweet<- str_replace_all(tweet,',',' ')
  #removing spaces
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  # covert to lower
  tweet = catch.error(tweet)
  tweet
}
cleanTweetsAndRemoveNAs<- function(tweet)
{
  tweetscleaned<- sapply(tweet,cleanTweets)
  # remove NAs tweets from this tweet
  tweetscleaned <- tweetscleaned[!is.na(tweetscleaned)]
  #remove the repetitive
  tweetscleaned = unique(tweetscleaned)
  tweetscleaned
}
tm_convert<- function(text)
{
  prep <- read.table('preposition.txt')
  mystopwords <- c('will','am','how','can','about','why','who','whom','could','which','would','should',"wouldn't","shouldn't",'got','get')
  mycorpus <- Corpus(VectorSource(text))
  mycorpus <- tm_map(mycorpus,content_transformer(tolower))
  mycorpus <- tm_map(mycorpus,removePunctuation)
  mycorpus <- tm_map(mycorpus,removeNumbers)
  mycorpus <- tm_map(mycorpus,removeWords,stopwords('english'))
  mycorpus <- tm_map(mycorpus,removeWords,mystopwords)
  mycorpus <- tm_map(mycorpus,removeWords,prep$V1)
  #mycorpus <- tm_map(mycorpus,stemDocument)
  mycorpus
}
tdm_plot <- function(corp)
{
  tdm<-TermDocumentMatrix(corp)
  termfreq <- rowSums(as.matrix(tdm))
  #cat(max(termfreq)/10)
  termfreq <- subset(termfreq,termfreq>=max(termfreq)/10)
  #qplot(names(termfreq), termfreq, geom ="bar", xlab="Words Used")+coord_flip()+geom_bar(stat="identity")
  df <- data.frame(term = names(termfreq), freq = termfreq)
  ggplot(df ,aes(x =term,y = freq))+ geom_bar(stat ='identity')+ xlab("Terms")+ylab("Count")+coord_flip()
}
############################################################################################################
tweets<-read.csv('Tweets.csv',stringsAsFactors = F)
tw <- read.csv('Tweets.csv')
tweets_array=c()
levels(tw$airline)
#[1] "American"       "Delta"          "Southwest"      "United"         "US Airways"     "Virgin America"

tweets_american <- tweets[tweets$airline=='American',]
tweets_delta <- tweets [tweets$airline=='Delta',]
tweets_southwest <- tweets[tweets$airline=='Southwest',]
tweets_united <- tweets[tweets$airline =='United',]
tweets_usa <- tweets[tweets$airline=='US Airways',]
tweets_va <- tweets[tweets$airline=='Virgin America',]

american <- cleanTweetsAndRemoveNAs(tweets_american$text)
va <- cleanTweetsAndRemoveNAs(tweets_va$text)
usa <- cleanTweetsAndRemoveNAs(tweets_usa$text)
delta <- cleanTweetsAndRemoveNAs(tweets_delta$text)
southwest <- cleanTweetsAndRemoveNAs(tweets_southwest$text)
united <- cleanTweetsAndRemoveNAs(tweets_united$text)

american_c<-tm_convert(american)
va_c <- tm_convert(va)
usa_c <- tm_convert(usa)
delta_c <- tm_convert(delta)
southwest_c <- tm_convert(southwest)
united_c <- tm_convert(united)
# Data cleaning done.
# Term Document Matrix

american_tdm<-tdm_plot(american_c)
va_tdm <- tdm_plot(va_c)
usa_tdm <- tdm_plot(usa_c)
delta_tdm <- tdm_plot(delta_c)
southwest_tdm <- tdm_plot(southwest_c)
united_tdm <- tdm_plot(united_c)

# Cluster
tdm <- removeSparseTerms(TermDocumentMatrix(american_c),sparse=0.95)
m2 <- as.matrix(tdm)
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix,method='ward.D')
plot(fit)
rect.hclust(fit,k=10)
m3 <- t(m2)
set.seed(122)
k <- 8
kmeansResult <- kmeans(m3,k)
round(kmeansResult$centers,digits = 3)
for ( i in 1:k){
  cat(paste("cluster",i,": ",sep=" "))
  s <- sort(kmeansResult$centers[i,],decreasing=T)
  cat(names(s)[1:3],"\n")
}

#clustering using the K-Mediod Algorithm
library(fpc)
pamResult <- pamk(m3,metric="manhattan")
k <- pamResult$nc
#pamResult<- pamResult$pamobject
layout(matrix(c(1,2),2,1))
plot(pamResult$pamobject,color=F,labels=4,lines=0,cex=0.8,col.clus=1,col.p=pamResult$clustering)
plot(pamResult$pamobject)

layout(matrix(1))

#Sentiment Polarity

american_emo<-classify_emotion(american,algorithm='bayes',prior=1.0)
va_emo<-classify_emotion(va,algorithm='bayes',prior=1.0)
usa_emo<-classify_emotion(usa,algorithm='bayes',prior=1.0)
delta_emo<-classify_emotion(delta,algorithm='bayes',prior=1.0)
southwest_emo<-classify_emotion(southwest,algorithm='bayes',prior=1.0)
united_emo <- classify_emotion(united,algorithm='bayes',prior=1.0)
#Sentiment
americanEmotion<-american_emo[,7]
vaEmotion<-va_emo[,7]
usaEmotion<-usa_emo[,7]
deltaEmotion<-delta_emo[,7]
southwestEmotion<-southwest_emo[,7]
unitedEmotion<-united_emo[,7]
#Removing NA's
americanEmotion[is.na(americanEmotion)] ='unknown'
vaEmotion[is.na(vaEmotion)] ='unknown'
usaEmotion[is.na(usaEmotion)]='unknown'
deltaEmotion[is.na(deltaEmotion)]='unknown'
southwestEmotion[is.na(southwestEmotion)]='unknown'
unitedEmotion[is.na(unitedEmotion)]='unknown'
#Polarity Classification
americanClassPol<-classify_polarity(american,algorithm='bayes')
vaClassPol<-classify_polarity(va,algorithm='bayes')
usaClassPol<-classify_polarity(usa,algorithm='bayes')
deltaClassPol<-classify_polarity(delta,algorithm='bayes')
southwestClassPol<-classify_polarity(southwest,algorithm='bayes')
unitedClassPol<- classify_polarity(united,algorithm='bayes')
#polarity category
americanPol <- americanClassPol[,4]
vaPol <- vaClassPol[,4]
usaPol <- usaClassPol[,4]
deltaPol <- deltaClassPol[,4]
southwestPol <- southwestClassPol[,4]
unitedPol <- unitedClassPol[,4]
#data frame
americanSentimentDataFrame = data.frame(text=american,emotion=americanEmotion,polarity=americanPol,stringsAsFactors = F)
vaSentimentDataFrame = data.frame(text=va,emotion=vaEmotion,polarity=vaPol,stringsAsFactors = F)
usaSentimentDataFrame = data.frame(text=usa,emotion=usaEmotion,polarity=usaPol,stringsAsFactors = F)
deltaSentimentDataFrame = data.frame(text=delta,emotion=deltaEmotion,polarity=deltaPol,stringsAsFactors = F)
southwestSentimentDataFrame = data.frame(text=southwest,emotion=southwestEmotion,polarity=southwestPol,stringsAsFactors = F)
unitedSentimentDataFrame = data.frame(text=united,emotion=unitedEmotion,polarity=unitedPol,stringsAsFactors = F)
#Removing emotion classified as unknown
americanSentimentDataFrame<- americanSentimentDataFrame[americanSentimentDataFrame$emotion!='unknown',]
vaSentimentDataFrame <- vaSentimentDataFrame[vaSentimentDataFrame$emotion!='unknown',]
usaSentimentDataFrame <- usaSentimentDataFrame[usaSentimentDataFrame$emotion!='unknown',]
deltaSentimentDataFrame <- deltaSentimentDataFrame[deltaSentimentDataFrame$emotion!='unknown',]
southwestSentimentDataFrame <- southwestSentimentDataFrame[southwestSentimentDataFrame$emotion!='unknown',]
unitedSentimentDataFrame <- unitedSentimentDataFrame[unitedSentimentDataFrame$emotion!='unknown',]
#arranging
americanSentimentDataFrame = within(americanSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
vaSentimentDataFrame = within(vaSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
usaSentimentDataFrame = within(usaSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
deltaSentimentDataFrame = within(deltaSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
southwestSentimentDataFrame = within(southwestSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
unitedSentimentDataFrame = within(unitedSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

plotSentiments1<- function (sentiment_dataframe,title) {
  library(ggplot2)
  ggplot(sentiment_dataframe, aes(x=emotion)) + 
    geom_bar(aes(y=..count.., fill=emotion)) +
    scale_fill_brewer(palette="Dark2") +
    ggtitle(title) +
    theme(legend.position='right') + ylab('Number of Tweets') + 
    xlab('Emotion Categories')
}
#plotting
layout(matrix(c(3,3,3,3), 3, 3, byrow = TRUE))
#par(mfrow=c(3,2))
p1<-plotSentiments1(americanSentimentDataFrame, 'American Airlines')
p2<-plotSentiments1(vaSentimentDataFrame, 'Virgin Atlantic')
p3<-plotSentiments1(usaSentimentDataFrame, 'US Airlines')
p4<-plotSentiments1(deltaSentimentDataFrame, 'Delta Airlines')
p5<-plotSentiments1(southwestSentimentDataFrame, 'South West Airlines')
p6<-plotSentiments1(unitedSentimentDataFrame, 'United Airlines')
#Multiple Plots
library(Rmisc)
png('plot1.png')
multiplot(p1,p2,p3,cols=1)
dev.off()
png('plot2.png')
multiplot(p4,p5,p6,cols=1)
dev.off()