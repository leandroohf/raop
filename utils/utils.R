#* ****************************************************************
#  Programmer[s]: Leandro Fernandes
#  Company/Institution: 
#  email: leandroohf@gmail.com
#  Date: February 6, 2017
#  
#  The author believes that share code and knowledge is awesome.
#  Feel free to share and modify this piece of code. But don't be
#  impolite and remember to cite the author and give him his credits.
#* ****************************************************************

library(tools, quietly = TRUE )
library(lubridate, quietly = TRUE )
library(assertthat, quietly = TRUE )

PrepareFileName <- function(desc, folder = 'scratch', author = 'lhof'){
    
    ## YYY-MM-DD-author-description.ext
    fname <- paste0(today(),"-", author,"-", desc)

    fname <- file.path(folder,fname)
    
    return(fname)
}
