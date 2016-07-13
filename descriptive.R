##* ****************************************************************
##  Programer[s]: Leandro Fernandes
##  Company/Institution:
##  email: leandroohf@gmail.com
##  Date: June 18, 2016
##
##  The author believes that share code and knowledge is awesome.
##  Feel free to share and modify this piece of code. But don't be
##  impolite and remember to cite the author and give him his credits.
##* ****************************************************************

library(feather, quietly = TRUE )
library(ggplot2, quietly = TRUE )
library(wordcloud, quietly = TRUE )
library(gridExtra, quietly = TRUE )
library(dplyr, quietly = TRUE )
library(jsonlite, quietly = TRUE )

source("./utils/utils.R")
source("./utils/features_selection.R")

settings   <- fromJSON( "SETTINGS.json", flatten=TRUE)

raop.target <- read_feather( settings$data_target_path)

## =================================

cols.cat <- c("in_test_set","request_id",
              "requester_received_pizza",
              "requester_username", "has.link",
              "request.date", "first.half.of.month",
              "posted.raop.before")

cols.num <- c("requester_received_pizza",
              "requester_account_age_in_days_at_request",
              "requester_days_since_first_post_on_raop_at_request",
              "requester_number_of_posts_at_request",
              "requester_number_of_posts_on_raop_at_request",
              "requester_upvotes_minus_downvotes_at_request",
              "nword",
              "post.sent",
              "desire.score","family.score",
              "job.score", "money.score","student.score")

cat("Summaring vars...\n")
cat(" numeric: \n")
print(summary(raop.target[,cols.num]))

cat(" cat: \n")
print(summary(raop.target[,cols.cat]))

data.view <- raop.target[,cols.num]
names(data.view) <- paste0("x",as.character(seq(1:ncol(data.view))))
data.exp <- DataExplorer(data.view ,"x1")
data.exp$GetCorrDashBoard()
data.exp$GetHistogramDashBoard()

## Text analysis
pizza.df <- raop.target %>% dplyr::filter(requester_received_pizza == TRUE)

nopizza.df <- raop.target %>% dplyr::filter(requester_received_pizza == FALSE)

pizza.corpus   <- GetCleanedCorpus(pizza.df$request_text)
nopizza.corpus <- GetCleanedCorpus(nopizza.df$request_text)

pizza.term.freq <- GetDocTermFreq(pizza.corpus)
nopizza.term.freq <- GetDocTermFreq(nopizza.corpus)

set.seed(142)
opar <- par() ## copy defaults
par(mfrow=c(1,2))

## 33% most often words
wordcloud(names(pizza.term.freq), pizza.term.freq, min.freq=100,
          scale=c(5, .1), colors=brewer.pal(6, "Dark2"))
text(x=0.5, y=1.1, "pizza")

wordcloud(names(nopizza.term.freq), nopizza.term.freq, min.freq=250,
          scale=c(5, .1), colors=brewer.pal(6, "Dark2"))
text(x=0.5, y=1.1, "no pizza")
par(opar) ## reset defaults

nw <- 21
pizza.data.view <- data.frame(word=names(pizza.term.freq[1:nw]),
                        freq=pizza.term.freq[1:nw],
                        row.names = NULL)

pizza.data.view$word <-
    factor( pizza.data.view$word,
           levels=pizza.data.view[order(pizza.data.view$freq), "word"])


p.left <- ggplot(pizza.data.view, aes(x=word, y=freq)) + 
     geom_bar(stat="identity") + 
     coord_flip() + ggtitle("pizza")

nopizza.data.view <- data.frame(word=names(nopizza.term.freq[1:nw]),
                        freq=nopizza.term.freq[1:nw],
                        row.names = NULL)

nopizza.data.view$word <-
    factor( nopizza.data.view$word,
           levels=nopizza.data.view[order(nopizza.data.view$freq), "word"])

p.right <- ggplot(nopizza.data.view, aes(x=word, y=freq)) + 
    geom_bar(stat="identity") + 
    coord_flip() + ggtitle("no pizza")

grid.arrange (p.left, p.right, ncol=2)
