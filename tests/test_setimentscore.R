library(testthat)

test_GetSentimentScoreFromCorpus <- function(){

    pos <- readLines("./data/positive-words.txt")
    neg <- readLines("./data/negative-words.txt")
    
    ## 3 docs
    x <- c('hug celebrate humor party ', 'abnormal alarm idol anger', 'husband family parents')
    
    x.corpus = Corpus(VectorSource(x))

    s <- GetSentimentScoreFromCorpus(x.corpus,pos,neg)
    
    expect_equal(s, c(3,-2,0))
}

test <- GetSentimentScoreFromCorpus()
