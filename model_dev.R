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

raop.target <- read_feather("./data/stage/raop_target.feather" )

data.split.list <- DesignData(raop.target)
train.data <- data.split.list[[1]]
val.data   <- data.split.list[[2]]

cat("train size:\n")
print(dim(train.data))
cat("val size:\n")
print(dim(val.data))
