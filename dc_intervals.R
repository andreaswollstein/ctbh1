dc_intervals <- function(dat, threshold=0.007, price="close", time="time", vol="volumeto") {
  # candidates for confirmed directional change
  dcc_up_cand <- c(FALSE, c(dat[2:nrow(dat), price]<dat[1:(nrow(dat)-1), price]*(1-threshold)))
  dcc_down_cand <- c(FALSE, c(dat[2:nrow(dat), price]>dat[1:(nrow(dat)-1), price]*(1+threshold)))
  
  # initialize new columns
  dat[,"interval"] <- NA  # 4 types of intervals: down, down_os, up, up_os (os=overshoot, NA means unknown)
  dat[,"point"] <- NA     # 3 types of points: dcc_up, dcc_down,  ep (NA means ignore)
  
  # figure out if the first change is up or down
  first_up <- which.max(dcc_up_cand) 
  first_down <- which.max(dcc_down_cand)
  
  if(first_up<first_down) {
    dat[first_up,"point"] <- "dcc_up"
    mode <- "uoi/dei"
    i <- first_up
  } else {
    dat[first_down, "point"] <- "dcc_down"
    mode <- "doi/uei"
    i <- first_down
  }
  
  # iterate till the end of the data
  while (i<nrow(dat)) {
    if(mode=="dcc_up") {
      j <- which.max(dcc_down_cand[i+1:length(dcc_down_cand)])+i
      if(!dcc_down_cand[j]) break # end of dataset
      dat[i,"point"] <- "dcc_up"
      dat[i,"interval"] <- "uoi"
      if((j-1)-(i+1)>0) { # if not changing direction immediately
        ep_idx <- which.min(dat[(i+1):(j-1), price])+i
        dat[ep_idx, "point"] <- "ep"
        dat[i:ep_idx-1, "interval"] <- "uoi"
        dat[ep_idx:j,"interval"] <- "dei"
      } 
      i <- j+1
      mode <- "dcc_down"
    } else { # mode=="dcc_down"
      j <- which.max(dcc_up_cand[i+1:length(dcc_up_cand)])+i
      if(!dcc_up_cand[j]) break # end of dataset
      dat[i,"point"] <- "dcc_down"
      dat[i,"interval"] <- "doi"
      if((j-1)-(i+1)>0) { # if not changing direction immediately
        ep_idx <- which.max(dat[(i+1):(j-1), price])+i
        dat[ep_idx, "point"] <- "ep"
        dat[i:ep_idx-1, "interval"] <- "doi"
        dat[ep_idx:j,"interval"] <- "uei"
      }
      i <- j+1
      mode <- "dcc_up"      
    }
  }
  
  dat <- dat[!is.na(dat[,"point"]),] # drop all points that are not dcc points
  return(dat)
}

test_dc_intervals <- function() {
  if(!exists("pd")) load(file="data/price_data.Rdata", envir=.GlobalEnv) 
  dat <- pd[["BTC"]]
  dc <- dc_intervals(dat=dat)
  browser()
}
