library(feather, quietly = TRUE )
library(ggplot2, quietly = TRUE )
library(wordcloud, quietly = TRUE )
library(gridExtra, quietly = TRUE )

source("./utils/utils.R")

raop.target <- read_feather("./data/stage/raop_target.feather" )

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
              "requester_username","nword",
              "post.sent")

cat("Summaring vars...\n")
cat(" numeric: \n")
print(summary(raop.target[,cols.num]))

cat(" cat: \n")
print(summary(raop.target[,cols.cat]))

## Text analysis
pizza.df   <- raop.df[raop.df["requester_received_pizza"] == TRUE,]
nopizza.df <- raop.df[raop.df["requester_received_pizza"] == FALSE,]

pizza.corpus   <- GetCleanedCorpus(pizza.df)
nopizza.corpus <- GetCleanedCorpus(nopizza.df)

pizza.term.freq <- GetDocTermFreq(pizza.corpus)
nopizza.term.freq <- GetDocTermFreq(nopizza.corpus)

set.seed(142)
par(mfrow=c(1,2))

## 33% most often words
wordcloud(names(pizza.term.freq), pizza.term.freq, min.freq=100,
          scale=c(5, .1), colors=brewer.pal(6, "Dark2"))
text(x=0.5, y=1.1, "pizza")

wordcloud(names(nopizza.term.freq), nopizza.term.freq, min.freq=250,
          scale=c(5, .1), colors=brewer.pal(6, "Dark2"))
text(x=0.5, y=1.1, "no pizza")

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

## =================================
## requester des
