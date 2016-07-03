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


library(tm)

GetCleanedCorpus <- function(raop.df){

    raop.corpus <- Corpus(VectorSource(raop.df[,8]))
    raop.corpus <- tm_map( raop.corpus, removeNumbers)
    raop.corpus <- tm_map(raop.corpus, tolower)
    ## First remove stopword and then punctuation
    ## Keep this oredr because of I'm -> becomes Im and that is not stopword
    raop.corpus <- tm_map(raop.corpus, removeWords, stopwords("english"))

    raop.corpus <- tm_map(raop.corpus, removeWords, c("will", "pizza",
                                                      "food", "can",
                                                      "get", "just",
                                                      "now", "last",
                                                      "really","raop"))
    
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

    pos.score <- sum(!is.na(match(post.terms,pos)))
    neg.score <- sum(!is.na(match(post.terms,neg)))
    sent.score <- pos.score - neg.score

    return(sent.score)
}

GetSentimentScoreFromCorpus <- function(raop.corpus, pos, neg){

    number.of.posts <- length(raop.corpus)
    scores <- numeric(number.of.posts)

    for( k in (1:number.of.posts)){
                                        # print(k)                        
        post.terms <- TokenizeCorpusElement(raop.corpus[[k]])
        scores[k]  <- GetPostSentimentScore(post.terms, pos, neg)
    }

    return(scores)
}

## GetNarrativesScoreFromCorpus <- function(raop.corpus,desire.words,family.words,
##                                          job.words,money.words, student.words){

GetNarrativesScoreFromCorpus <- function(raop.corpus,narrative.words){
    
    ##cat("NOT implemented YET\n")
    
    number.of.posts <- length(raop.corpus)
    narrative.score  <- numeric(number.of.posts)
    nwords <- length(narrative.words)
    
    ## desire.score  <- numeric(number.of.posts)
    ## family.score  <- numeric(number.of.posts)
    ## job.score     <- numeric(number.of.posts)
    ## money.score   <- numeric(number.of.posts)
    ## student.cores <- numeric(number.of.posts)

    for( k in (1:number.of.posts)){
        ##print(k)                        
        post.terms <- TokenizeCorpusElement(raop.corpus[[k]])
        ##print(post.terms)

        narrative.score[k]  <- sum(!is.na(match(post.terms,narrative.words)))/nwords
        
        ## desire.score  <- sum(!is.na(match(post.terms,desire.words)))
        ## family.score  <- sum(!is.na(match(post.terms,family.words)))
        ## job.score     <- sum(!is.na(match(post.terms,job.words)))
        ## money.score   <- sum(!is.na(match(post.terms,money.words)))
        ## student.score <- sum(!is.na(match(post.terms,student.words)))
        
    }
    
    ##return(list(desire.score,family.words,job.words,money.words,student.words))
    return(narrative.score)
}


ConvertToDecile <- function(x,max_x, min_x){

    delta <- (max_x - min_x)/10.00
    xdc <- round(x/delta)
    
    return(xdc)
}


TranformVarToMedianStandatVar <- function(x){

    return(x - median(x))
}
