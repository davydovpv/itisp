######## Config ########

config <- list()
config$dir.cfg <- "/home/mub/ShinyApps/board01/config"
config$dir.csv <- "/home/mub/ShinyApps/board01/vol"
config$file.log <- "/home/mub/ShinyApps/board01/log/board.log"

config$db.name <- "//172.16.11.225/odasdb"
config$db.username <- "spvol"
config$db.password <- "spvolwork"
config$db.securities <- "moff.securities"
config$db.prices <- "moff.prices"


#config$fut.table <- "mw_futures" #delete
#config$opt.table <- "mw_options" #delete
#config$spec.fut <- "spec_futures" #delete
#config$spec.opt <- "spec_options" #delete
#config$vol.param.table <- "hist_vol_param" #delete

config$exp.time <- "14:00:00" #change
config$date.format <- "%Y-%m-%d"
config$opt.table.nrow.min <- 10
config$vol.tol <- 1e-4
config$vol.range.ratio <- 0.2
config$auto.update.sec <- 60
config$sys.fut.dtime <- 5 #sec
config$fut.opt.dtime <- 120 #sec
config$plotly.dir <- "qboard"

#pref <- NULL

Sys.setenv("plotly_username"="olegmub")
Sys.setenv("plotly_api_key"="4154qr6dc8")