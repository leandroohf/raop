##* ****************************************************************
##  Programer[s]: Leandro Fernandes
##  Company/Institution:
##  email: leandroohf@gmail.com
##  Date: June 18, 2016
##
##  The author believes that share code and knowledge is awesome.
##  Feel free to share and modify this piece of code. But don't be
##  impolite and remember to cite the author and give him his credits.
##* ****************************************************************


library(jsonlite, quietly = TRUE )
library(feather, quietly = TRUE )

source("./utils/data.R")
source("./utils/utils.R")

raop.file <- "data/raw/pizza_request_dataset.json"
raop.df   <- fromJSON( raop.file, flatten=TRUE)

pos.words <- readLines("./dict/positive-words.txt")
neg.words <- readLines("./dict/negative-words.txt")

raop.target <- BuildDataTarget(raop.df, pos.words, neg.words)

write_feather(raop.target, "data/stage/raop_target.feather")
