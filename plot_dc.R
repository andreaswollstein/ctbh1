plot_dc <- function(dat, dc_dat) {
  wndw <- c(-180*24*60*60,0)+Sys.time()-21*24*60*60
  plot(dat$time, dat$close, xlim=wndw, type="l")
  lines(dc_dat$time, dc_dat$close, col=2)
}