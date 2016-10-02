##* ****************************************************************
##  Programmer[s]: Leandro Fernandes
##  Company/Institution: 
##  email: leandroohf@gmail.com
##  Program: etl                                                      
##  Date: September 18, 2016
##  
##  The author believes that share code and knowledge is awesome.
##  Feel free to share and modify this piece of code. But don't be
##  impolite and remember to cite the author and give him his credits.
##* ****************************************************************

library(jsonlite, quietly = TRUE )


settings   <- fromJSON( "SETTINGS.json", flatten=TRUE)


temp <- tempfile()
download.file(settings$data_url,temp)

untar(temp,files=settings$data_compressed_path, exdir = "data/raw/", extras = "--strip-components=1")

file.remove(temp)




            
