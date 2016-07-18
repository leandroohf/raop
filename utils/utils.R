##* ****************************************************************
##  Programmer[s]: Leandro Fernandes
##  Company/Institution: 
##  email: leandroohf@gmail.com
##  Date: June 18, 2016
##  
##  The author believes that share code and knowledge is awesome.
##  Feel free to share and modify this piece of code. But don't be
##  impolite and remember to cite the author and give him his credits.
##* ****************************************************************

library(tm, quietly = TRUE )
library(assertthat, quietly = TRUE )

## Text tools
GetCleanedCorpus <- function(posts.text){

    raop.corpus <- Corpus(VectorSource(posts.text))
    raop.corpus <- tm_map( raop.corpus, removeNumbers)
    raop.corpus <- tm_map(raop.corpus, tolower)
    ## First remove stopword and then punctuation
    ## Keep this oredr because of I'm -> becomes Im and that is not stopword
    raop.corpus <- tm_map(raop.corpus, removeWords, stopwords("english"))

    raop.corpus <- tm_map(raop.corpus, removeWords, c("will", "pizza",
                                                      "food", "can",
                                                      "get", "just",
                                                      "now", "last",
                                                      "really", "raop"))
    
    raop.corpus <- tm_map( raop.corpus, removePunctuation)
    raop.corpus <- tm_map(raop.corpus, stemDocument, language = "english")

    ## Fixing stemDocument is not working for thank n thanks
    for (j in seq(raop.corpus))
    {
        raop.corpus[[j]] <- gsub("thanks", "thank", raop.corpus[[j]])
    }
    
    raop.corpus <- tm_map(raop.corpus, stripWhitespace)

    raop.corpus <- tm_map( raop.corpus, PlainTextDocument)

    return(raop.corpus)
}

GetDocTermFreq <- function(corpus){

    dtm <- DocumentTermMatrix(corpus)
    term.freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
    ## wf <- data.frame(word=names(term.freq), freq=term.freq, row.names = NULL)   

    return(term.freq)
}

TokenizeCorpusElement <- function(x){

    return(unlist(stri_extract_all_words(as.character(x))))
}

GetPostSentimentScore <- function(post.terms, pos, neg){

    stopifnot( typeof(post.terms) == "character")

    pos.score <- sum(!is.na(match(post.terms,pos)))/length(pos)
    neg.score <- sum(!is.na(match(post.terms,neg)))/length(neg)
    sent.score <- pos.score - neg.score

    return(sent.score)
}

GetSentimentScoreFromCorpus <- function(raop.corpus, pos, neg){

    number.of.posts <- length(raop.corpus)
    scores <- numeric(number.of.posts)

    for ( k in (1:number.of.posts)){
        ## print(k)                        
        post.terms <- TokenizeCorpusElement(raop.corpus[[k]])
        scores[k]  <- GetPostSentimentScore(post.terms, pos, neg)
    }

    return(scores)
}

GetNarrativesScoreFromCorpus <- function(raop.corpus, narrative.words){
    
    number.of.posts  <- length(raop.corpus)
    narrative.score  <- numeric(number.of.posts)
    nwords <- length(narrative.words)
    
    for( k in (1:number.of.posts)){
        post.terms <- TokenizeCorpusElement(raop.corpus[[k]])
        ## post score = count the number of shared  words
        narrative.score[k]  <- sum(!is.na(match(post.terms,narrative.words)))/nwords
    }
    
    return(narrative.score)
}

## Data Tranformations tools
TransformVariable <- function(x, max_x, min_x){    

    return( x/(max_x - min_x))
    ##return( (x - median(x))/(max_x - min_x) )
}

ConvertToDecile <- function(x, decile_x = NULL){

    assert_that(is.numeric(x))
    
    if( exists("decile_x") == FALSE){
        decile_x <- quantile(x, seq(0, .9, .1))
    }
    xdc <- findInterval(x, decile_x)
    
    return(xdc)
}
