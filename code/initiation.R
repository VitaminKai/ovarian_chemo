# --------------
# Author: Kaiwen Wang
# Date: 2021-07-20
# Purpose: Initialiation file to load required packages
# Requires:
# Output: 
# --------------

sapply(c('data.table','here','ggplot2','RColorBrewer','glue','viridis','stringr'),function(x){library(x,quietly = T,character.only = TRUE)})
