rm(list=ls())
library(tidyverse)
library(dplyr)
library(tseries)
library(forecast)
library(reshape2)

load("data/price_data.Rdata")
load("data/bitcoin_1H_pct.Rdata")

# initial cutoff (remove starting interval)
first_date = as.POSIXct("2016-11-07 00:00:00")

# define seasonality in ts 
frequency = 7*24

# forecast interval
fc_interval = 7*24

# forecast plot xlim
fc_xlim = c(80,110)

# define currency
currency = "BTC"

# define arima model
arima_order = c(7,0,2)
arima_sorder = c(0,0,0)

# get currency
df = pd[[currency]];

smct$time=smct$date
joined = inner_join(smct,df, by="time") %>% drop_na()

idx = which(joined$time==first_date)+1
joined=joined[idx:nrow(joined),]

# create ts object
bts = ts(joined$close,frequency=7*24)

# sliding window definition
wstart = length(bts)-11*frequency;
wend = length(bts)
wsize = 10*frequency
wseq = seq(wstart,wend,10)

FChw = matrix(0,nrow = length(wseq),ncol = fc_interval)
FCarima = matrix(0,nrow = length(wseq),ncol = fc_interval)
FCarimax = matrix(0,nrow = length(wseq),ncol = fc_interval)
FCtrue = matrix(0,nrow = length(wseq),ncol = fc_interval)

k=0
for (i in wseq) {
  k=k+1
  print(k)

  # get window
  tswin = ts(bts[(i-wsize):i], frequency = frequency)
  covar = joined$PC1[(i-wsize):i]

  # build model forecasts for arima, arimax, hw
  bts_arima = arima(bts,order=arima_order, seasonal = list(order = arima_sorder), method="CSS")
  fc_arima = forecast(bts_arima,h=fc_interval)
  bts_arimax = arima(tswin,order=arima_order, seasonal = list(order = arima_sorder),method="CSS",xreg=covar)
  pc1fc = forecast(covar,h=fc_interval)
  fc_arimax = forecast(bts_arimax,xreg = pc1fc$mean, h=fc_interval)
  fc_hw = forecast(HoltWinters(tswin),h=fc_interval)

  FChw[k,]=fc_hw$mean
  FCarima[k,]=fc_arima$mean
  FCarimax[k,]=fc_arimax$mean
  FCtrue[k,]=bts[(i+1):(i+fc_interval)]
}

fcasts = list(hw=FChw,arima=FCarima,arimax=FCarimax,real=FCtrue)
save(fcasts,file="data/TSA_compute_fcasts_v1.Rdata")
