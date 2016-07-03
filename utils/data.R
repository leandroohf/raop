
BuildDataTarget <- function(raop.df, pos.words, neg.words){

    ## TODO Refactor this code
    desire.words  <- readLines("./dict/desire.txt")
    family.words  <- readLines("./dict/family.txt")
    job.words     <- readLines("./dict/job.txt")
    money.words   <- readLines("./dict/money.txt")
    student.words <- readLines("./dict/student.txt")

    cat("Features engineering...\n")
    raop.df$nword          <- str_count(raop.df[,"request_text"], "\\S+")
    raop.df$has.link       <- str_detect( raop.df[,"request_text"], "https?://")

    raop.df$request.date   <- as.POSIXct(raop.df[,"unix_timestamp_of_request"],
                                         origin="1970-01-01", tz = "UTC")
    raop.df$first.half.of.month <- (day(raop.df$request.date) < 16)
    raop.df$is.weekend          <- (wday(raop.df$request.date) %in%  c(1,6,7))

    raop.df$posted.raop.before  <- raop.df[,"requester_number_of_posts_on_raop_at_request"] > 0

    cat("Text engineering...\n")
    raop.corpus <- GetCleanedCorpus(raop.df)
    raop.df$post.sent <- GetSentimentScoreFromCorpus(raop.corpus, pos.words, neg.words)

    raop.df$desire.score  <- GetNarrativesScoreFromCorpus(raop.corpus, desire.words)
    raop.df$family.score  <- GetNarrativesScoreFromCorpus(raop.corpus, family.words)
    raop.df$job.score     <- GetNarrativesScoreFromCorpus(raop.corpus, job.words)
    raop.df$money.score   <- GetNarrativesScoreFromCorpus(raop.corpus, money.words)
    raop.df$student.score <- GetNarrativesScoreFromCorpus(raop.corpus, student.words)
    
    cat("Selecting target vars... \n")
    cols.target <- c("in_test_set","request_id","request_text",
                     "requester_account_age_in_days_at_request",
                     "requester_days_since_first_post_on_raop_at_request",
                     "requester_number_of_posts_at_request",
                     "requester_number_of_posts_on_raop_at_request",
                     "requester_received_pizza",
                     "requester_upvotes_minus_downvotes_at_request",
                     "requester_username","nword", "has.link",
                     "request.date", "first.half.of.month",
                     "posted.raop.before", "post.sent", "is.weekend",
                     "desire.score","family.score","job.score","money.score",
                     "student.score")

    raop.target <- raop.df[,cols.target]

    return(raop.target)
}
