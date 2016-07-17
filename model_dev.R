#* ****************************************************************
#  Programmer[s]: Leandro Fernandes
#  Company/Institution: 
#  email: leandroohf@gmail.com
#  Date: June 20, 2016
#  
#  The author believes that share code and knowledge is awesome.
#  Feel free to share and modify this piece of code. But don't be
#  impolite and remember to cite the author and give him his credits.
#* ****************************************************************

library(feather, quietly = TRUE )
library(jsonlite, quietly = TRUE )

source("./utils/data.R")
source("./utils/utils.R")
source("./utils/report.R")
source("./utils/model.R")

settings   <- fromJSON( "SETTINGS.json", flatten=TRUE)

cat('Loading data engineer...\n')
raop.engineer <- readRDS(settings$data_engineer_path)

data.split.list <- raop.engineer$GetDesignData()
train.data      <- data.split.list[[1]]
val.data        <- data.split.list[[2]]

## There is 1 Nan in each narrative score 
train.data <- na.omit(train.data)
val.data   <- na.omit(val.data)

cat("train size:\n")
print(dim(train.data))
cat("val size:\n")
print(dim(val.data))

resp.var <- 'requester_received_pizza'

cat('Model building...\n')
glm.formula <- formula("requester_received_pizza ~
                           requester_upvotes_minus_downvotes_at_request +
                           nword +
                           requester_account_age_in_days_at_request +
                           money.score +
                           post.sent +
                           has.link +
                           first.half.of.month+
                           posted.raop.before")

m14 <- RAoPModel(glm.formula,train.data, val.data)

cat('Report model summary...\n')
BuildModelReport(m14$GetGlmObject(),resp.var,train.data,val.data)

cat('Saving model...\n')
saveRDS(m14, settings$raop_model_path)
