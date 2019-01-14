
rm(list=ls())
library(tidyverse)
library(dplyr)

pricedir = "~/Dropbox/crypto_traders_berlin_hs1/Price data/"
csvfiles = list.files(pricedir,pattern="*.csv")

# get all files into one list
pd=lapply(csvfiles,function(x) invisible(read_csv(paste0(pricedir,x))))

# provide names to list
names(pd) = gsub("*.csv$", "", csvfiles)
print(names(pd))

save(file="data/price_data.Rdata",pd)
