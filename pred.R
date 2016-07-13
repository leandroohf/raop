##* ****************************************************************
##  Programmer[s]: Leandro Fernandes
##  Company/Institution:
##  email: leandroohf@gmail.com
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

cat('Loading model...\n')
load(file='./models/m14.rda')
split.list <- m14$GetData()
train.data <- split.list[[1]]

cat('Loading new data...\n')
newdata.file <- "data/raw/new_request_data.json"
newdata.df   <- fromJSON( newdata.file, flatten=TRUE)

cat('Loading dictinaries...\n')
dict.list <- LoadDcitionariesFromSetings(settings)
sent.dict      <- dict.list[[1]]
narrative.dict <- dict.list[[2]]

newdata.df <- BuildNewFeatures(newdata.df, sent.dict, narrative.dict)
new.data   <- newdata.df[,m14$GetPredictorsName()]
new.data   <- TransformNumericalVars(new.data,train.data)

cat('Make predition...\n')
pred <- m14$GetPrediction(new.data)
print(pred)

cat('Saving predition...\n')
pred.df <- cbind(new.data,pred)
write_feather(pred.df, paste0("data/stage/pred_",lubridate::today(),".feather"))
