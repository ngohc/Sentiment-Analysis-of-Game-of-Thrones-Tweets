options(download.file.method = "curl")
options(repos='http://cran.rstudio.com/')

#ip <- installed.packages()
#pkgs.to.remove <- ip[!(ip[,"Priority"] %in% c("base", "recommended")), 1] 
#sapply(pkgs.to.remove, remove.packages)

#install.packages('twitteR',dependencies=TRUE)
#install.packages('tm',dependencies=TRUE)
#install.packages('wordcloud',dependencies=TRUE)
#install.packages('ggplot2',dependencies=TRUE)
#install.packages('syuzhet')
#install.packages('wordcloud2')

source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

library(twitteR)
library(tm)
library(wordcloud)
library(ggplot2)
library(Rgraphviz)
library(syuzhet)

consumerKey <- "GjSFF7Qn7LfZxAEU6vmZ9lp8i"
consumerSecret <- "QGkxPldWC2eZOTpVTqwPsIqzVqqu4HQQpv0C5mGZSYatywOJjL"
accessToken <- "156185352-paLYiCRsfN2uXPaoLiHjzOfMXgfrjHTW0m0D8SOI"
accessTokenSecret <- "la2VxRr1k0R1JDVQIHVCd4gcvYoQVvuEmw78tMeEXhshF"

setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)

# step 7: Crawl for tweets 
tweets <- searchTwitter('#gameofthrones', n=1000, lang="en")
tweets.df <- twListToDF(tweets)
write.csv(tweets.df,"gameofthronestweets")

onetweet <- tweets[[10]]
onetweet$getScreenName()
onetweet$getText()

tweets.text <- sapply(tweets, function(x) x$getText())

tweets.text <- iconv(tweets.text,to="ASCII",sub="")

# You can also use the following if you prefer to work with 
# UTF-8 encoding. 
tweets.text <- iconv(tweets.text, to="utf-8-mac", sub="")

# step 8: clean the data
removeTags <- function(input){gsub("[@#]\\S+ *","",input)}
removeURL <- function(input){gsub("http\\S+ *","",input)}
removeEnter <- function(input){gsub("\\n","",input)}
removeSpecialChar <- function(input) {gsub("[^[:alpha:][:space:]]*","",input)}

tweets.text <- removeTags(tweets.text)
tweets.text <- removeURL(tweets.text)
tweets.text <- removeEnter(tweets.text)
tweets.text <- removeSpecialChar(tweets.text)

# load all the data
tweets.text

# so that we can use text mining functions
tweets.text.corpus <- Corpus(VectorSource(tweets.text))
inspect(tweets.text.corpus[1:5])


# convert all to lowercase
tweets.text.corpus <- tm_map(tweets.text.corpus, content_transformer(tolower))
tweets.text.corpus <- tm_map(tweets.text.corpus, removeWords, stopwords("english"))
tweets.text.corpus <- tm_map(tweets.text.corpus, stripWhitespace)


# inspects on first 5 tweets
inspect(tweets.text.corpus[1:5])

save.image(file = "TSALGOT.RData")

par(mar=c(1,1,1,1))

# TERM DOCUMENT MATRIX <<find those with frequency>> 
# greater than 10, these are commonly used terms in the tweets
tweets.tdm <- TermDocumentMatrix(tweets.text.corpus)
tdm <- as.matrix(tweets.tdm)
findFreqTerms(tweets.tdm,lowfreq = 50)

# Inspect the TDM 
inspect(tweets.tdm)

# WORD CLOUD 
# There are many options for word cloud, please explore using # the help function ?wordcloud
w <- sort(rowSums(tdm), decreasing = TRUE)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'RdGy'),
          scale = c(3,0.4))

w <- data.frame(names(w), w)
colnames(w) <- c('word','freq')
wordcloud2(w, size=0.5,shape='triangle',minSize = 1)


#FINDING THE CORRELATION

# Find associated terms with a given search term, in this case # we use the term "gameofthrones"
findAssocs(tweets.tdm,"gameofthrones",corlimit=0.5)

# We will produce a correlation plot to see how words are 
# linked in the corpus
dev.off()
myattrs <- list(node=list(shape="ellipse", fixedsize=FALSE, fontsize=15))
par(mar=c(1,1,1,1))
plot(tweets.tdm,
     terms=findFreqTerms(tweets.tdm,lowfreq=35),
     corThreshold = 0.25,
     attrs=myattrs)

# Remove terms that are sparse, keeping 5% of top appearing 
# terms. Inspect the resulting TDM
tweets.tdm <- removeSparseTerms(tweets.tdm,sparse=0.95)
inspect(tweets.tdm)

# compute the distance between terms
tdm.scale <- scale(tweets.tdm)

# perform hierarchical clustering on the terms see which terms # appear with which other terms in tweets.
d <- dist(tdm.scale,method='euclidean')
fit <- hclust(d,method='ward.D')
plot(fit)

# We cut the tree into 4 main clusters
groups <- cutree(fit, k=4)
rect.hclust(fit,k=4,border='red')


library(syuzhet)
score <- get_sentiment(tweets.text, method="syuzhet")
emotions <- get_nrc_sentiment(tweets.text)

#view some
head(emotions)
tweets[4]


combine <- data.frame(tweets.text,emotions[1:8],score)
write.csv(combine,"senti_GOT.csv")

count <- colSums(emotions[,1:8])
labels <- colnames(emotions[1:8])

senti_df <- data.frame(labels,count)

joy_items <- which(emotions$joy > 0)
tweets.text[joy_items]

ggplot(senti_df, aes(x=labels,y=count,fill=labels))+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette='Dark2')+
  labs(x='Emotions',y='No. of words')

plot(combine$score)
boxplot(combine$score)

ggplot(data=combine,
       aes(x = as.numeric(rownames(combine)),y = score))+
  geom_point()


