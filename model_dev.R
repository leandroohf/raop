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

source("./utils/data.R")
source("./utils/utils.R")
source("./utils/features_selection.R")
source("./utils/report.R")
source("./utils/model.R")

raop.target <- read_feather("./data/stage/raop_target.feather" )

data.split.list <- DesignData(raop.target)
train.data <- data.split.list[[1]]
val.data   <- data.split.list[[2]]

## There are 1 Nan in each narrative score 
train.data <- na.omit(train.data)
val.data   <- na.omit(val.data)

cat("train size:\n")
print(dim(train.data))
cat("val size:\n")
print(dim(val.data))

resp.var <- 'requester_received_pizza'

cols.num <- c("requester_received_pizza",
              "requester_account_age_in_days_at_request", 
              "requester_number_of_posts_at_request",
              "requester_upvotes_minus_downvotes_at_request", 
              "nword",
              "desire.score", "family.score", "money.score", 
              "job.score", "student.score")

data.exp <- DataExplorer(train.data[,cols.num],resp.var)

cat("Featuring Selection... \n")
param.list <- list("objective" = "binary:logistic",
                   "eta" = 0.01,
                   "min_child_weight" = 3,
                   "subsample" = 0.80,
                   "colsample_bytree" = 0.80,
                   "scale_pos_weight" = 1.00,
                   "silent" = 1,
                   "booster" = "gbtree",
                   "max_depth" = 9,
                   "eval_metric" = "error")


number.of.models <- 15
xgb.exp <- XGBoostExplorer(train.data,
                           val.data,
                           resp.var,
                           number.of.models,
                           param.list)

xgb.exp$PlotRelativeImportance()

## FIXME: PLot is considering RMSE instead of error: #wrong/(total)
xgb.exp$GetXGBoostDashBoard()

cat('Model building...\n')
m14 <- RAoPModel(train.data, val.data, resp.var)

cat('Report model summary...\n')
BuildModelReport(m14$GetGlmObject(),resp.var,train.data,val.data)

cat('Saving model...\n')
save(m14,file='./models/m14.rda')

write_feather(train.data, "data/stage/train_data.feather")
write_feather(val.data, "data/stage/train_data.feather")
