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

settings   <- fromJSON( "SETTINGS.json", flatten=TRUE)

cat('Loading model...\n')
raop.model <- readRDS(settings$raop_model_path)

cat('Loading data engineer...\n')
raop.engineer <- readRDS(settings$data_engineer_path)

cat('Loading new data...\n')
newdata.df   <- fromJSON( settings$new_data_raw_path, flatten=TRUE)

new.data <- raop.engineer$PreProcessNewData(newdata.df)

cat('Make predition...\n')
pred <- raop.model$GetPrediction(new.data)
print(pred)

cat('Saving predition...\n')
pred.df <- cbind(new.data,pred)
pred.file <- paste0("data/stage/pred_",lubridate::today(),".feather"
write_feather(pred.df, pred.file))
