# begin, end ... "YYYY-MM-DD"
plot_dc <- function(dat, dc_dat=NULL, begin=NULL, end=NULL, lwd=3, ...) {
  if(is.null(end)) end <- Sys.time()
  if(is.character(end)) end <- as.POSIXct(strptime(end, "%Y-%m-%d"))
  if(is.null(begin)) begin <- end - 180*24*3600 # 180 d from end as default
  if(is.character(begin)) begin <- as.POSIXct(strptime(begin, "%Y-%m-%d"))
  if(is.null(dc_dat)) dc_dat <- dc_intervals(dat=dat)

  plot(close ~ time, data=dat, xlim=c(begin, end), type="l", col="darkgrey", lwd=lwd, tcl=0, ...)
  
  dcc_idx <- which(dc_dat$point=="dcc_up"|dc_dat$point=="dcc_down")
  for (i in setdiff(dcc_idx,1)) {
    segments(
      x0=dc_dat[i-1,"time"], y0=dc_dat[i-1,"close"],
      x1=dc_dat[i,"time"], y1=dc_dat[i,"close"],
      col="red", lwd=lwd
    )
  }
  for (i in setdiff(dcc_idx,nrow(dc_dat))) {
    segments(
      x0=dc_dat[i,"time"], y0=dc_dat[i,"close"],
      x1=dc_dat[i+1,"time"], y1=dc_dat[i+1,"close"],
      col="darkgreen", lwd=lwd
    )
  }
  axis(1,at=dc_dat[dcc_idx, "time"],labels=FALSE)
}

test_plot_dc <- function(threshold=NULL) {
  if(!exists("pd")) load(file="data/price_data.Rdata", envir=.GlobalEnv) 
  dat <- pd[["BTC"]]
  if(!exists("dc")||!is.null(threshold)) {
    if(!exists("dc_intervals")) source("dc_intervals.R")
    if(is.null(threshold)) threshold <- 0.017
    assign("dc", dc_intervals(dat=dat, threshold=threshold), envir=.GlobalEnv)
  }  
  if(is.null(threshold)) threshold <- 0.017
  par(mar=c(2,2,2,0)+0.1)
  plot_dc(
    dat=dat, dc_dat=dc, begin="2018-07-01", end="2018-09-01", 
    ylim=c(5000,9000), xlab="",
    main=sprintf("%.1f%% Direction Changes for BTC, Jul-Sep. 2018", threshold*100),
    cex.main=1
  )
  legend("bottom", legend=c("Directional Changes", "Overshot"), col=c("red", "darkgreen"), lwd=3, bty="n")
}

test_plot_dc(threshold=NULL) # 0.017