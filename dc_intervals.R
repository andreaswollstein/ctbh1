dc_intervals <- function(dat, treshold=0.025, open="open", hi="high", lo="low", close="close", time="time", vol="volumeto") {
  # candidates for confirmed directional change
  dcc_up_cand <- c(FALSE, dat[2:nrow(dat), "lo"]<dat[1:(nrow(dat)-1), "close"]*(1-treshold))
  dcc_down_cand <- c(FALSE, dat[2:nrow(dat), "lo"]>dat[1:(nrow(dat)-1), "close"]*(1+treshold))
  
  dat[,"interval"] <- NA  # 4 types of intervals: down, down_os, up, up_os (os=overshoot, NA means unknown)
  dat[,"point"] <- NA     # 3 types of points: dcc_up, dcc_down,  ep (NA means ignore)
  
  browser()
  # second half of the calculation
  # dat[,"down"]
  # down <- 0
  # up <- 0
  # for (i in seq(1, nrow(dat))) {
  #   if(up+down==0 && dat[i, "dc_up_confirmed"]+dat[i,"dc_down_confirmed"]==0) dat[i,"interval"] <- NA # definitely the case for i=1
  #   if()
  #   
  # }
  
}