library(ROracle)
drv <- dbDriver("Oracle")
con <- dbConnect(drv, dbname="//172.16.11.225/odasdb", username="spvol", password="spvolwork")
rs <- dbSendQuery(con, "select * from moff.prices p where p.seccode = 'Si-6.16_FT'")
moff.prices <- fetch(rs)
head(moff.prices)
rs <- dbSendQuery(con, "select * from moff.securities p where p.description = 'Si' and matdate = to_date('15.06.2016', 'dd.mm.yyyy')")
rs <- dbSendQuery(con, "select * from moff.securities")

moff.securities <- fetch(rs)
head(moff.securities)
colnames(moff.securities)

unique(moff.securities$DESCRIPTION)

moff.securities[moff.securities$SECBOARD=="RTS_FUT" & moff.securities$ID_SEC_TYPE=="OPTM" & moff.securities$DESCRIPTION == "Si" & moff.securities$MATDATE == "2016-06-15", "EXCODE"]

moff.securities[moff.securities$ID_SEC_TYPE=="OPTM" & moff.securities$DESCRIPTION == "Si" & moff.securities$MATDATE == "2016-06-15", "SHORT_NAME"]



db.query <- "select prices.*  from moff.prices ,  moff.securities where securities.tiker = prices.seccode and securities.description = 'Si' and id_sec_type = 'OPTM'"
db.query <- "select prices.* from moff.prices, moff.securities s where s.tiker = prices.seccode and s.description = 'Si' and s.id_sec_type = 'OPTM'"
send.query <- dbSendQuery(con, db.query)
fetch.query <- fetch(send.query, -1)
clear.res <- dbClearResult(send.query)
head(fetch.query)

db.col <- paste("securities.TIKER", "securities.FACEVALUE", "securities.MATDATE", "securities.FS_INST_TERM", "securities.EXCODE", sep=",")
db.col <- paste(db.col, "prices.DATE_TIME", "prices.BID", "prices.BIDDEPTH", "prices.ASK", "prices.ASKDEPTH", "prices.LAST_VAL", "prices.VOLATILITY", "prices.T_PRICE", sep=",")
db.where <- paste("securities.TIKER = prices.SECCODE AND securities.SECBOARD = 'RTS_FUT' AND securities.ID_SEC_TYPE = 'OPTM' AND securities.DESCRIPTION = '", fut.dsc, "' AND MATDATE = to_date('", format(as.Date(p$opt.expiry), config$date.format), "', 'yyyy-mm-dd')", sep="")
db.query <- paste("SELECT", db.col, "FROM", config$db.prices, "prices,", config$db.securities, "securities", "WHERE", db.where)
send.query <- dbSendQuery(con, db.query)
fetch.query <- fetch(send.query, -1)
clear.res <- dbClearResult(send.query)
tail(fetch.query)



if(class(try({
  #db.col <- paste("SECBOARD", "ID_SEC_TYPE", "TIKER", "DESCRIPTION", "FACEVALUE", "MATDATE", "FS_INST_TERM", "EXCODE", sep=",")
  db.col <- paste("TIKER", "FACEVALUE", "MATDATE", "FS_INST_TERM", "EXCODE", sep=",")
  db.where <- paste("SECBOARD = 'RTS_FUT' AND ID_SEC_TYPE = 'OPTM' AND DESCRIPTION = '", fut.dsc, "' AND MATDATE = to_date('", format(as.Date(p$opt.expiry), config$date.format), "', 'yyyy-mm-dd')", sep="")
  db.query <- paste("SELECT", db.col, "FROM", config$db.securities, "WHERE", db.where)
  send.query <- dbSendQuery(con, db.query)
  fetch.query <- fetch(send.query, -1)
  clear.res <- dbClearResult(send.query)
}, silent=TRUE))[1] == "try-error")
{
  return(list(error=paste("ERROR in DB opt.table get tickers", dbGetException(con)$errorMsg)))
} else {
  if(is.null(fetch.query))
    return(list(error=paste("fetch.query is null", db.query, sep="; ")))
  if(nrow(fetch.query) < config$opt.table.nrow.min)
    return(list(error=paste("nrow fetch.query < ", config$opt.table.nrow.min, db.query, sep="; ")))
}





data <- fetch(rs)
dim(data)
head(data)
tail(data)

fetchResult
dbWriteTable(con,"TEST_TABLE", test_table)
dbGetQuery(con, "select count(*) from TEST_TABLE")
d <- dbReadTable(con, "TEST_TABLE")
dbDisconnect(con)



library(ROracle)
config <- list()
config$securities.table <- "moff.securities"
config$prices.table <- "moff.prices"
p <- list()
p$fut.code <- "SiM6"

db.drv <- dbDriver("Oracle")

db.col <- paste("EXCODE", "TIKER", "DESCRIPTION ", sep=", ")
db.where <- paste("EXCODE = ", "'", p$fut.code, "'", sep="")
db.query <- paste("SELECT", db.col, "FROM", config$securities.table, "WHERE", db.where)
db.con <- dbConnect(db.drv, dbname="//172.16.11.225/odasdb", username="spvol", password="spvolwork")
db.rs <- dbSendQuery(db.con, db.query)
db.table <- fetch(db.rs)
fut.param <- db.table
dbDisconnect(db.con)

dim(data)
head(data)


rs <- dbSendQuery(db.con, "select * from moff.prices p where p.seccode = 'Si-6.16_FT'")
rs <- dbSendQuery(db.con, "select * from moff.prices p where p.description = 'Si' and p.matdate = to_date('15.06.2016', 'dd.mm.yyyy')")
rs <- dbSendQuery(db.con, "select * from moff.prices p where p.description = 'Si'")
rs <- dbSendQuery(db.con, "select * from moff.securities p where p.description = 'MXI' and matdate = to_date('16.06.2016', 'dd.mm.yyyy')")

data <- fetch(rs)
dim(data)
head(data)


SECCODE, DATE_TIME, BID, ASK, LAST_VAL

db.col <- paste("DESCRIPTION", "SEC_NAME", "ID_SEC_TYPE", "TIKER", "MATDATE", "FACEVALUE", "FS_INST_TERM", "STATUS", "MIN_STEP", "FS_STEP_PRICE", sep=",")
db.where <- paste("SEC_NAME = ", "'", p$fut.code, "'", sep="")
db.query <- paste("SELECT", db.col, "FROM", config$securities.table, "WHERE", db.where)


load(file=file.path(config$dir, load.config.name))

load(file.path("/home/mub/ShinyApps/qboard/config", "SiM6_06"))

pref
p <- pref

data <- list(board=board, vol=vol, date.time=date.time, date.time.opt=date.time.opt, fut.price=fut.price,
             t.day=round(t.day, 2), vv.strike=vv.strike, error=0)

