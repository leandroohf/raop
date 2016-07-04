##* ****************************************************************
##  Programmer[s]: Leandro Fernandes
##  Company/Institution: Cargill
##  email: leandro_h_fernandes@cargill.com
##  Date: June 20, 2016
##  
##  The author believes that share code and knowledge is awesome.
##  Feel free to share and modify this piece of code. But don't be
##  impolite and remember to cite the author and give him his credits.
##* ****************************************************************

library(jsonlite, quietly = TRUE )
library(feather, quietly = TRUE )

source("./utils/data.R")
source("./utils/utils.R")

cat('Loading new data...\n')
newdata.file <- "data/raw/new_request_data.json"
newdata.df   <- fromJSON( newdata.file, flatten=TRUE)

pos.words <- readLines("./dict/positive-words.txt")
neg.words <- readLines("./dict/negative-words.txt")


cat('Loading model...\n')
load(file='./models/m14.rda')
train.data <- read_feather("./data/stage/train_data.feather" )

newdata.df <- BuildNewFeatures(newdata.df, pos.words, neg.words)

## TODO Read from settings.json file
cols.pred <- c("requester_account_age_in_days_at_request",
               "requester_number_of_posts_at_request",
               "requester_upvotes_minus_downvotes_at_request",
               "nword", "has.link",
               "first.half.of.month",
               "posted.raop.before", "post.sent","is.weekend",
               "desire.score","family.score","money.score",
               "job.score", "student.score")
    
new.data <- newdata.df[,cols.pred]
new.data <- TransformNumericalVars(new.data,train.data)

pred <- predict(m14,new.data, type = 'response')

pred.df <- cbind(new.data,pred)

write_feather(pred.df, paste0("data/stage/pred_",lubridate::today(),".feather"))
