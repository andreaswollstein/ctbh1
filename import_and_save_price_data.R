
rm(list=ls())
library(tidyverse)
library(dplyr)
source("filter_price_data.R")

pricedir = "ORIGINAL_DATA_PATH/Price data/"
csvfiles = list.files(pricedir,pattern="*.csv")

# get all files into one list
pd=lapply(csvfiles,function(x) invisible(read_csv(paste0(pricedir,x))))
pd=lapply(pd,del_no_vol)

# provide names to list
names(pd) = gsub("*.csv$", "", csvfiles)
print(names(pd))

save(file="data/price_data.Rdata",pd)
