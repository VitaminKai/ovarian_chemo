# --------------
# Author: Kaiwen Wang
# Date: 2021-07-20
# Purpose: Initialiation file to load required packages
# Requires:
# Output: 
# --------------

sapply(c('data.table','here','ggplot2','RColorBrewer','glue','viridis','stringr'),function(x){library(x,quietly = T,character.only = TRUE)})


###===============================================================###
# Text formatting
###===============================================================###

rid_of_underscore <- function(x,wrap=F,width=12){
  out<-gsub(pattern='_',replacement=' ',x=x)
  if(wrap==T){
    out <- stringr::str_wrap(out,width)
  }
  return(out)
}


rid_of_quotation <- function(x,width=12){
  out<-gsub(pattern='`',replacement='',x=x)
  return(out)
}