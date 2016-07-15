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

settings   <- fromJSON( "SETTINGS.json", flatten=TRUE)

dict.list <- LoadDcitionariesFromSetings(settings)
sent.dict      <- dict.list[[1]]
narrative.dict <- dict.list[[2]]

raop.df     <- fromJSON( settings$data_raw_path, flatten=TRUE)
raop.target <- BuildDataTarget(raop.df, sent.dict, narrative.dict,
                               settings$cols_target)

write_feather(raop.target, settings$data_target_path)
