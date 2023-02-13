#Import dataset
library(readxl)
Airbnb <- read_excel("D:/Krunch/Athitya_Open_Data.xls")

#Sentiment Analysis. Polarity is the measure of positive or negative intent in a writer's tone.
negation.words
amplification.words
##read Zipf's law and principle of least efforts.

#basic subjectivity lexicon is held in an object loaded within qdap called key.pol
#The key.pol object has both positive and negative terms.
View(key.pol)

#subset( ) function is the easiest way to select variables and observations which meet conditions.
#Using the subset function, you are able to retain only the terms in the original key.pol lexicon that have a polarity value equal to one.

#as.data.frame function converts a list to a data frame.
#== exactly equal to
new.pos<-c('rofl', 'lol')
old.pos<-subset(as.data.frame(key.pol),key.pol$y==1)
all.pos<-c(new.pos,old.pos[,1])


#kappa is a term used among gamers to denote some amount of negative sarcasm.
#meh, is more broadly used in the context of being unenthusiastic or apathetic. 
new.neg<-c('kappa','meh')
old.neg<-subset(as.data.frame(key.pol),key.pol$y==-1)
all.neg<-c(new.neg,old.neg[,1])

#create a new object called all.polarity using the sentment_frame function.
#sentiment_frame generate a table for use of various sentiment functions. 4 arguments.
all.polarity<-sentiment_frame(all.pos, all.neg, 1, -1)
View(all.polarity)

#polarity function approximate the sentiment of text by grouping variable(s).
#polarity.frame argument: a dataframe of positive/negative words and weights.

#ROFL, look at that!
polarity('ROFL, look at that!', polarity.frame = all.polarity)


#-----------------
#October 20, 2022

polarity('The acting was good, but the movie could have been better.', polarity.frame = key.pol)

##The polarity score is dependent upon the polarity dictionary used.
#group.var - the grouping variable
#total.sentences - Total sentences spoken.
#total.words - Total words used.
#ave.polarity - The sum of all polarity scores for that group divided by number of sentences spoken.
#sd.polarity - The standard deviation of that group's sentence level polarity scores.
#stan.mean.polarity - A standardized polarity score calculated by taking the average polarity score for a group divided by the standard deviation.


#Practice dataset: Airbnb
library(tm)
library(qdap)
library(wordcloud)
library(ggplot2)
library(ggthemes)

#stringsAsfactors indicates whether strings in a data frame should be treated as factor or plain strings.
#options(stringsAsFactors = FALSE)

#1. Calculate polarity.The overall sentiment is often inferred as positive, 
#neutral or negative from the sign of the polarity score.

#The range of polarity is from -1 to 1 (negative to positive) and 
#will tell us if the text contains positive or negative tone. 
Airbnb.polarity<-polarity(Airbnb$house_rules)
Airbnb.polarity

View(Airbnb.polarity)
AB<-as.data.frame(Airbnb.polarity)
View(AB)  #polarity for each comment and list of positive and negative words in each comment.

#2.Plot the distribution of the polarity scores.
#ggplot() is used to construct the initial plot object, and is almost always followed by + to add component to the plot.
#aes: aesthetic mappings. Each aesthetic is a mapping between a visual cue and a variable. 
#Examples include: position (i.e., on the x and y axes)
#Histograms (geom_histogram()) display the counts with bars.
#binwidth: the width of the bins. 
#geom_density computes and draws kernel density estimate, which is a smoothed version of the histogram. 

ggplot(Airbnb.polarity$all, aes(x=polarity, y= ..density..)) +  geom_histogram(binwidth=.25,
                                                                               fill="darkred",colour="grey60", size=.2) +   geom_density(size=.75)


#3. Wordcloud
#You may notice that the polarity scores are not centered at 0. 
#As shown in plot, the mean of the original polarity scores is 0.90. 
#This means that on average each comment has at least a single positive word in it. 
#Scaling the polarity score vector moves the average to zero.
Airbnb$polarity<-scale(Airbnb.polarity$all$polarity)
head(Airbnb$polarity)

#You need to create a subset of the original data. 
#This will give you only the documents that are positive or negative. Assuming cut-off threshold as 0.
pos.comments<-subset(Airbnb$comments, Airbnb$polarity>0)
neg.comments<-subset(Airbnb$comments, Airbnb$polarity<0)
pos.comments
neg.comments

#Polarity is not centered at 0 and this is because we have text that have positive sentiments alongside 
#negative within them. Hence we will take subset of comments that are negative or positive and not both.
#You need to collapse the pos.comments and neg.comments into two distinct documents. 
#collapse: an argument to separate the results. 
pos.terms<-paste(pos.comments, collapse = " ")
neg.terms<-paste(neg.comments, collapse = " ")
all.terms<-c(pos.terms, neg.terms)
all.terms
convert_list <- function(x){
  return (list(x))}

Airbnb$positive=Airbnb.polarity$all$pos.words
Airbnb$negative=Airbnb.polarity$all$neg.words

Airbnb$positive=as.list(Airbnb$positive)
Airbnb$negative=as.list(Airbnb$negative)

library("writexl")
write_xlsx(Airbnb,"D:/Krunch/Athitya_Open_Data_updated.xlsx")
all.corpus<-VCorpus(VectorSource(all.terms))
all.tdm<-TermDocumentMatrix(all.corpus, control=list(weighting=weightTfIdf, 
                                                     removePunctuation =TRUE, removeNumbers = TRUE, stopwords=stopwords('english')))

#TFIDF is the product of the term frequency (TF) and the inverse document frequency (IDF).
#TF = (term occurrences in a document) / (total unique terms in the document).
#IDF = log(total document in corpus / number of documents with term t in it).

#switching to simple matrix using as.matrix
all.tdm.m<-as.matrix(all.tdm)

#naming the columns
colnames(all.tdm.m)<-c('positive','negative')
View(all.tdm.m)

#Plot a cloud comparing the frequencies of words across documents.
comparison.cloud(all.tdm.m, max.words=500, colors=c('darkgreen','darkred'))

#Change margin of plot
par(mar = c(0.01, 0.01, 0.01, 0.01))


#----
library(tidytext) 
library(textdata)


#The three general-purpose lexicons are:
#1. AFINN from Finn Årup Nielsen
#2. Bing from Bing Liu and collaborators
#3. NRC from Saif Mohammad and Peter Turney
#All three lexicons are based on unigrams, i.e., single words.

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")
?get_sentiments

#The AFINN lexicon assigns words with a score that runs between -5 and 5, with negative scores indicating negative sentiment and positive scores indicating positive sentiment.
#The Bing lexicon categorizes words in a binary fashion into positive and negative categories.
#The NRC lexicon categorizes words in a binary fashion ("yes"/"no") into categories of positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.
