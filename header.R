######## Config ########


######## Libraries ########

library(package="fOptions", lib.loc="/home/mub/R/x86_64-pc-linux-gnu-library/3.2")
library(package="ROracle", lib.loc="/home/mub/R/x86_64-pc-linux-gnu-library/3.2")



######## Functions ########

StrikeDeltaConv <- function(delta=0, S=0, T=0, r=0, b=0, vola=0)
{
  q <- r - b
  if(delta > 0)
    d1 <- qnorm(delta * exp(q * T)) else d1 <- -qnorm(-delta * exp(q * T))
  return(S * exp(-d1 * vola * sqrt(T) + (r - q + vola * vola/2) * T))
}


GreekCalc <- function(name=c("premium", "delta", "vega", "theta", "rho", "gamma", "vanna", "volga", "charm")[1],
                      type=c("c", "p")[1], S=0, K=0, T=0, r=0, b=0, vol=0, RN.par=NULL)
{
  
  c.type <- c("premium", "delta", "vega", "theta", "rho", "gamma", "vanna", "volga", "charm")
  
  if(type == "c" | type == "C" | type == "call" | type == "Call")
  {
    i <- 1
  } else {
    if(type == "p" | type == "P" | type == "put" | type == "Put")
    {
      i <- -1
    } else {
      err <- "type out of (c, p, call, put, Call, Put)"
      return(list(err=err, value=NA))
    }
  }
  
  if(!(name %in% c.type))
  {
    err <- "name out of (premium, delta, vega, theta, rho, gamma, vanna, volga, charm)"
    return(list(err=err, value=NA))
  }
  
  if(S <= 0)
  {
    err <- "S must have a positive value"
    return(list(err=err, value=NA))
  }
  
  if(K <= 0)
  {
    err <- "K must have a positive value"
    return(list(err=err, value=NA))
  }
  
  if(T <= 0)
  {
    out <- 0
    if(name == "premium") out <- max(0, i * (S - K))
    if(name == "delta") out <- ifelse(i * (S - K) > 0, i, 0)
    return(list(err=0, value=out))
  }
  
  if(vol <= 0)
  {
    err <- "vol must have a positive value"
    return(list(err=err, value=NA))
  }
  
  if(!is.null(RN.par))
  {
    if(!(is.vector(RN.par, mode="numeric") & length(RN.par) == 3))
    {
      err <- "RN.par must be NULL or a vector of 3 numeric values: sigma, skew, kurt"
      return(list(err=err, value=NA))
    } else {
      if(name == "delta")
      {
        sqrtT <- sqrt(T)
        a0 <- RN.par[1]/100
        a1 <- RN.par[2]/6
        a2 <- RN.par[3]/24
        ksi <- log(K/S)/(a0 * sqrtT)
        sigma <- a0 * (1 + a1 * ksi + a2 * ksi * ksi)
        d1 <- (log(S/K) + (b + sigma * sigma/2) * T)/(sigma * sqrtT)
        d2 <- d1 - sigma * sqrtT
        phi.d1 <- exp(-d1 * d1/2)/sqrt(2 * pi)
        phi.d2 <- exp(-d2 * d2/2)/sqrt(2 * pi)
        Phi.id1 <- pnorm(i * d1)
        dksi.ds <- -1/(a0 * sqrtT * S)
        dsigma.ds <- a0 * dksi.ds * (a1  + 2 * a2 * ksi)
        dd1.ds <- ((1/S + T * sigma * dsigma.ds) * sigma * sqrtT - (log(S/K) + (b + sigma * sigma/2) * T) * sqrtT * dsigma.ds)/(sigma * sigma * T)
        dd2.ds <- dd1.ds - sqrtT * dsigma.ds
        out <- exp(-r * T) * (exp(b * T) * (i * Phi.id1 + S * phi.d1 * dd1.ds) - K * phi.d2 * dd2.ds)
        return(list(err=0, value=out))
      }
    }
  }
  
  q <- r - b
  d1 <- (log(S/K) + (r - q + vol * vol/2) * T)/(vol * sqrt(T))
  d2 <- d1 - vol * sqrt(T)
  
  if(name == "premium")
  {
    out <- i * (S * exp(-q*T) * pnorm(i*d1) - K * exp(-r*T) * pnorm(i*d2))
    return(list(err=0, value=out))
  }
  
  if(name == "delta")
  {
    out <- i*exp(-q*T) * pnorm(i*d1)
    return(list(err=0, value=out))
  }
  
  if(name == "vega")
  {
    out <- S * exp(-q*T) * dnorm(d1) * sqrt(T)
    return(list(err=0, value=out))
  }
  
  if(name == "theta")
  {
    out <- -exp(-q*T) * S*dnorm(d1)*vol/(2*sqrt(T)) - i*r*K*exp(-r*T)*pnorm(i*d2) + i*q*S*exp(-q*T)*pnorm(i*d1)
    return(list(err=0, value=out))
  }
  
  if(name == "rho")
  {
    out <- i*K * T * exp(-r*T) * pnorm(i*d2)
    return(list(err=0, value=out))
  }
  
  if(name == "gamma")
  {
    out <- exp(-q*T) * dnorm(d1)/(S*vol*sqrt(T))
    return(list(err=0, value=out))
  }
  
  if(name == "vanna")
  {
    out <- -exp(-q*T) * dnorm(d1) * d2/vol
    return(list(err=0, value=out))
  }
  
  if(name == "volga")
  {
    out <- S * exp(-q*T) * dnorm(d1) * sqrt(T) * (d1*d2)/vol
    return(list(err=0, value=out))
  }
  
  if(name == "charm")
  {
    out <- i*q*exp(-q*T)*pnorm(i*d1) - exp(-q*T)*dnorm(d1)*(2*(r-q)*T - d2*vol*sqrt(T))/(2*T*vol*sqrt(T))
    return(list(err=0, value=out))
  }
  
}


VannaVolgaVol <- function(method = c("continuous", "discrete"), S=0, K=0, T=0, vol0=0,
                          vol=data.frame(K=c(0, 0, 0), v=c(0, 0, 0)), r=0, b=0, tol=1e-4)
{
  
  if(method == "continuous")
  {
    C <- c(function(v=vol0, spot=S) GreekCalc(name="premium", type="c", S=spot, K=vol$K[1], T=T, r=r, b=b, vol=v)$value,
           function(v=vol0, spot=S) GreekCalc(name="premium", type="c", S=spot, K=vol$K[2], T=T, r=r, b=b, vol=v)$value,
           function(v=vol0, spot=S) GreekCalc(name="premium", type="c", S=spot, K=vol$K[3], T=T, r=r, b=b, vol=v)$value)
    TV.BS <- GreekCalc(name="premium", type="c", S=S, K=K, T=T, r=r, b=b, vol=vol0)$value
    B.vega <- sapply(1:3, function(i) GreekCalc(name="vega", type="c", S=S, K=vol$K[i], T=T, r=r, b=b, vol=vol0)$value)
    B.vanna <- sapply(1:3, function(i) GreekCalc(name="vanna", type="c", S=S, K=vol$K[i], T=T, r=r, b=b, vol=vol0)$value)
    B.volga <- sapply(1:3, function(i) GreekCalc(name="volga", type="c", S=S, K=vol$K[i], T=T, r=r, b=b, vol=vol0)$value)
    O.vega <- GreekCalc(name="vega", type="c", S=S, K=K, T=T, r=r, b=b, vol=vol0)$value
    O.vanna <- GreekCalc(name="vanna", type="c", S=S, K=K, T=T, r=r, b=b, vol=vol0)$value
    O.volga <- GreekCalc(name="volga", type="c", S=S, K=K, T=T, r=r, b=b, vol=vol0)$value
    B.cost <- sapply(1:3, function(i) C[[i]](v=vol$v[i]) - C[[i]](v=vol0))
    A <- t(matrix(c(B.vega, B.vanna, B.volga), nrow = 3))
    x <- matrix(c(O.vega, O.vanna, O.volga), nrow = 3)
    w <- solve(A, x)
    CF <- t(w) %*% matrix(B.cost, nrow = 3)
    return(GBSVolatility(price=TV.BS+CF, TypeFlag="c", S=S, X=K, Time=T, r=r, b=b, tol=tol))
  }
  
  if(method == "discrete")
  {
    dS <- S * 0.001
    dv <- vol0 * 0.01
    C <- c(function(v=vol0, spot=S) GBSOption(TypeFlag="c", S=spot, X=vol$K[1], Time=T, r=r, b=b, sigma=v)@price,
           function(v=vol0, spot=S) GBSOption(TypeFlag="c", S=spot, X=vol$K[2], Time=T, r=r, b=b, sigma=v)@price,
           function(v=vol0, spot=S) GBSOption(TypeFlag="c", S=spot, X=vol$K[3], Time=T, r=r, b=b, sigma=v)@price)
    Vega <- function(f, v, spot=S) (f(v+dv, spot) - f(v-dv, spot))/(2*dv)
    Vanna <- function(f, v, spot=S) (Vega(f, v, spot+dS) - Vega(f, v, spot-dS))/(2*dS)
    Volga <- function(f, v) (Vega(f, v+dv) - Vega(f, v-dv))/(2*dv)
    O <- function(v=vol0, spot=S) GBSOption(TypeFlag="c",  S=spot, X=K, Time=T, r=r, b=b, sigma=v)@price
    TV.BS <- O()
    B.vega <- sapply(1:3, function(i) Vega(C[[i]], vol0))
    B.vanna <- sapply(1:3, function(i) Vanna(C[[i]], vol0))
    B.volga <- sapply(1:3, function(i) Volga(C[[i]], vol0))
    O.vega <- Vega(O, vol0)
    O.vanna <- Vanna(O, vol0)
    O.volga <- Volga(O, vol0)
    B.cost <- sapply(1:3, function(i) C[[i]](vol$v[i]) - C[[i]](vol0))
    A <- t(matrix(c(B.vega, B.vanna, B.volga), nrow = 3))
    x <- matrix(c(O.vega, O.vanna, O.volga), nrow = 3)
    w <- solve(A, x)
    CF <- t(w) %*% matrix(B.cost, nrow = 3)
    return(GBSVolatility(price=TV.BS+CF, TypeFlag="c", S=S, X=K, Time=T, r=r, b=b, tol=tol))
  }
  
  return(NA)
  
}


BoardCalc <- function(p)
{
  
  # DB connect
  if(class(try({
    drv <- dbDriver("Oracle")
    con <- dbConnect(drv, dbname=config$db.name, username=config$db.username, password=config$db.password)
  }, silent=TRUE))[1] == "try-error")
  {
    return(list(error=paste("ERROR in DB connect", dbGetException(con)$errorMsg)))
  } else {
    if(is.null(con))
      return(list(error=paste("DB con is null", config$db.name, sep="; ")))
  }
  
  # DB fut.table get ticker
  if(class(try({
    fut.ticker <- fut.dsc <- NULL
    db.col <- paste("TIKER", "EXCODE", "DESCRIPTION", sep=",")
    db.where <- paste("EXCODE = ", "'", p$fut.code, "'", sep="")
    db.query <- paste("SELECT", db.col, "FROM", config$db.securities, "WHERE", db.where)
    send.query <- dbSendQuery(con, db.query)
    fetch.query <- fetch(send.query, -1)
    clear.res <- dbClearResult(send.query)
  }, silent=TRUE))[1] == "try-error")
  {
    discon <- dbDisconnect(con)
    return(list(error=paste("ERROR in DB fut.table get ticker", dbGetException(con)$errorMsg)))
  } else {
    if(is.null(fetch.query))
    {
      discon <- dbDisconnect(con)
      return(list(error=paste("fetch.query is null", db.query, sep="; ")))
    }
    if(nrow(fetch.query) != 1)
    {
      discon <- dbDisconnect(con)
      return(list(error=paste("nrow fetch.query != 1", db.query, sep="; ")))
    }
    fut.ticker <- fetch.query$TIKER
    fut.dsc <- fetch.query$DESCRIPTION
  }
  
  # DB fut.table get price
  if(class(try({
    db.col <- paste("SECCODE", "DATE_TIME", "BID", "ASK", "LAST_VAL", sep=",")
    db.where <- paste("SECCODE = ", "'",fut.ticker, "'", sep="")
    db.query <- paste("SELECT", db.col, "FROM", config$db.prices, "WHERE", db.where)
    send.query <- dbSendQuery(con, db.query)
    fetch.query <- fetch(send.query, -1)
    clear.res <- dbClearResult(send.query)
  }, silent=TRUE))[1] == "try-error")
  {
    discon <- dbDisconnect(con)
    return(list(error=paste("ERROR in DB fut.table get price", dbGetException(con)$errorMsg)))
  } else {
    if(is.null(fetch.query))
    {
      discon <- dbDisconnect(con)
      return(list(error=paste("fetch.query is null", db.query, sep="; ")))
    }
    if(nrow(fetch.query) != 1)
    {
      discon <- dbDisconnect(con)
      return(list(error=paste("nrow fetch.query != 1", db.query, sep="; ")))
    }
    fut.table <- fetch.query
  }
  
  # DB opt.table get prices
  if(class(try({
    db.col <- paste("securities.EXCODE", "securities.FACEVALUE", "securities.MATDATE", "securities.FS_INST_TERM", sep=",")
    db.col <- paste(db.col, "prices.DATE_TIME", "prices.BID", "prices.BIDDEPTH", "prices.ASK", "prices.ASKDEPTH", "prices.LAST_VAL", "prices.VOLATILITY", "prices.T_PRICE", sep=",")
    db.where <- paste("securities.TIKER = prices.SECCODE AND securities.SECBOARD = 'RTS_FUT' AND securities.ID_SEC_TYPE = 'OPTM' AND securities.DESCRIPTION = '", fut.dsc, "' AND securities.MATDATE = to_date('", format(as.Date(p$opt.expiry), config$date.format), "', 'yyyy-mm-dd')", sep="")
    db.query <- paste("SELECT", db.col, "FROM", config$db.prices, "prices,", config$db.securities, "securities", "WHERE", db.where)
    send.query <- dbSendQuery(con, db.query)
    fetch.query <- fetch(send.query, -1)
    clear.res <- dbClearResult(send.query)
  }, silent=TRUE))[1] == "try-error")
  {
    discon <- dbDisconnect(con)
    return(list(error=paste("ERROR in DB opt.table get tickers", dbGetException(con)$errorMsg)))
  } else {
    discon <- dbDisconnect(con)
    if(is.null(fetch.query))
      return(list(error=paste("fetch.query is null", db.query, sep="; ")))
    if(nrow(fetch.query) < config$opt.table.nrow.min)
      return(list(error=paste("nrow fetch.query < ", config$opt.table.nrow.min, db.query, sep="; ")))
    opt.table <- fetch.query
    colnames(opt.table) <- c("sec.code", "strike", "expiry", "type", "date.time", "bid", "bid.size", "ask", "ask.size", "last", "volatility", "theor.price")
    opt.table$type[opt.table$type == 0] <- "Call"
    opt.table$type[opt.table$type == 1] <- "Put"
  }
  
  # fut.table manipulations
  if(class(try({
    date.time <- as.POSIXct(fut.table$DATE_TIME, tz="Europe/Moscow")
    trade.date <- as.Date(date.time)
    change.time <- strftime(date.time, format="%H:%M:%S")
    fut.price <- (fut.table$BID + fut.table$ASK)/2
    exp.date.time <- as.POSIXct(paste(p$opt.expiry, config$exp.time), tz="Europe/Moscow")
    t.day <- as.numeric(difftime(exp.date.time, date.time, units="days"))
    t.day <- t.day - p$bsm.dt
    t.year <- t.day/365
  }, silent=TRUE))[1] == "try-error") return(list(error="fut.table manipulations"))
  
  # opt.table manipulations
  if(class(try({
    if(!is.na(p$strike.low))
      opt.table <- opt.table[opt.table$strike >= p$strike.low, ] 
    if(!is.na(p$strike.high))
      opt.table <- opt.table[opt.table$strike <= p$strike.high, ]
    if(!is.na(p$strike.delta) & p$strike.delta > 0)
    {
      vol.max <- min(max(opt.table$volatility, config$vol.min * 100), config$vol.max * 100)
      strike.low <- StrikeDeltaConv(delta=-p$strike.delta/100, S=fut.price, T=t.year, r=p$bsm.r/100, b=p$bsm.b/100, v=vol.max/100)
      strike.high <- StrikeDeltaConv(delta=p$strike.delta/100, S=fut.price, T=t.year, r=p$bsm.r/100, b=p$bsm.b/100, v=vol.max/100)
      strike.low <- max(strike.low, p$strike.low, na.rm=TRUE)
      strike.high <- min(strike.high, p$strike.high, na.rm=TRUE)
    } else {
      strike.low <- max(0, p$strike.low, na.rm=TRUE)
      strike.high <- min(1e8, p$strike.high, na.rm=TRUE)
    }
    opt.table <- opt.table[opt.table$strike >= strike.low & opt.table$strike <= strike.high, ]
    date.time.opt <- as.POSIXct(max(opt.table$date.time, na.rm=TRUE), tz="Europe/Moscow")
    if(nrow(opt.table) < config$opt.table.nrow.min)
      return(list(error=paste("nrow(opt.table) < config$opt.table.nrow.min", nrow(opt.table), sep="; ")))
    opt.strike <- sort(unique(opt.table$strike))
  }, silent=TRUE))[1] == "try-error") return(list(error="ERROR in opt.table manipulations"))
  
  # vanna-volga calculations
  if(p$vv.plot | p$greek.vol == "vv")
  {
    if(class(try({
      atm.vol <- p$vv.atm
      call.vol <- p$vv.rr/2 + p$vv.bf + atm.vol
      put.vol <- call.vol - p$vv.rr
      atm.k <- StrikeDeltaConv(delta=0.5, S=fut.price, T=t.year, r=p$bsm.r/100, b=p$bsm.b/100, v=atm.vol/100)
      call.k <- StrikeDeltaConv(delta=p$vv.delta/100, S=fut.price, T=t.year, r=p$bsm.r/100, b=p$bsm.b/100, v=call.vol/100)
      put.k <- StrikeDeltaConv(delta=-p$vv.delta/100, S=fut.price, T=t.year, r=p$bsm.r/100, b=p$bsm.b/100, v=put.vol/100)
      opt.table$vol.vv <- NA
      for(k in opt.strike)
      {
        vv.vol <- VannaVolgaVol(method="continuous", S=fut.price, K=k, T=t.year, vol0=atm.vol/100,
                                vol=data.frame(K=c(put.k, atm.k, call.k), v=c(put.vol/100, atm.vol/100, call.vol/100)),
                                r=p$bsm.r/100, b=p$bsm.b/100, tol=config$vol.tol) * 100
        opt.table$vol.vv[opt.table$strike == k] <- rep(vv.vol, length(opt.table$vol.vv[opt.table$strike == k]))
      }
      vv.strike <- c(put25=put.k, call25=call.k)
    }, silent=TRUE))[1] == "try-error") return(list(error="ERROR in vanna-volga calculations"))
  } else {vv.strike <- NULL}
  
  # risk-neutral calculations
  if(p$rn.plot | p$greek.vol == "rn")
  {
    if(class(try({
      a1 <- p$rn.sigma/100
      a2 <- p$rn.skew/6
      a3 <- p$rn.kurt/24
      opt.table$vol.rn <- NA
      for(k in opt.strike)
      {
        x <- log(k/fut.price)/sqrt(t.year)/a1
        rn.vol <- a1 * (1 + a2 * x + a3 * x * x) * 100
        opt.table$vol.rn[opt.table$strike == k] <- rep(rn.vol, length(opt.table$vol.rn[opt.table$strike == k]))
      }
    }, silent=TRUE))[1] == "try-error") return(list(error="ERROR in risk-neutral calculations"))
  }
  
  # risk-neutral bid calculations
  if(p$rnb.plot)
  {
    if(class(try({
      a1 <- (p$rn.sigma + p$rnb.sigma)/100
      a2 <- (p$rn.skew + p$rnb.skew)/6
      a3 <- (p$rn.kurt + p$rnb.kurt)/24
      opt.table$vol.rnb <- NA
      for(k in opt.strike)
      {
        x <- log(k/fut.price)/sqrt(t.year)/a1
        rn.vol <- a1 * (1 + a2 * x + a3 * x * x) * 100
        opt.table$vol.rnb[opt.table$strike == k] <- rep(rn.vol, length(opt.table$vol.rnb[opt.table$strike == k]))
      }
    }, silent=TRUE))[1] == "try-error") return(list(error="ERROR in risk-neutral bid calculations"))
  }
  
  # risk-neutral ask calculations
  if(p$rna.plot)
  {
    if(class(try({
      a1 <- (p$rn.sigma + p$rna.sigma)/100
      a2 <- (p$rn.skew + p$rna.skew)/6
      a3 <- (p$rn.kurt + p$rna.kurt)/24
      opt.table$vol.rna <- NA
      for(k in opt.strike)
      {
        x <- log(k/fut.price)/sqrt(t.year)/a1
        rn.vol <- a1 * (1 + a2 * x + a3 * x * x) * 100
        opt.table$vol.rna[opt.table$strike == k] <- rep(rn.vol, length(opt.table$vol.rna[opt.table$strike == k]))
      }
    }, silent=TRUE))[1] == "try-error") return(list(error="ERROR in risk-neutral ask calculations"))
  }
  
  # greeks calculations
  if(class(try({
    opt.table$vol.bid <- NA
    opt.table$vol.ask <- NA
    opt.table$premium <- 0
    opt.table$delta <- 0
    opt.table$vega <- 0
    opt.table$theta <- 0
    opt.table$gamma <- 0
    if(p$greek.vol == "ex") opt.table$greek.vol <- opt.table$volatility
    if(p$greek.vol == "vv") opt.table$greek.vol <- opt.table$vol.vv
    if(p$greek.vol == "rn") opt.table$greek.vol <- opt.table$vol.rn
    opt.table$short.type <- tolower(substr(opt.table$type, 1, 1))
    for(i in 1:nrow(opt.table)) # i <- 1
    {
      opt.table$premium[i] <- GreekCalc(name="premium", type=opt.table$type[i], S=fut.price, K=opt.table$strike[i], T=t.year, r=p$bsm.r/100, b=p$bsm.b/100, v=opt.table$greek.vol[i]/100)$value
      opt.table$delta[i] <- GreekCalc(name="delta", type=opt.table$type[i], S=fut.price, K=opt.table$strike[i], T=t.year, r=p$bsm.r/100, b=p$bsm.b/100, v=opt.table$greek.vol[i]/100)$value
      opt.table$vega[i] <- GreekCalc(name="vega", type=opt.table$type[i], S=fut.price, K=opt.table$strike[i], T=t.year, r=p$bsm.r/100, b=p$bsm.b/100, v=opt.table$greek.vol[i]/100)$value/100
      opt.table$theta[i] <- GreekCalc(name="theta", type=opt.table$type[i], S=fut.price, K=opt.table$strike[i], T=t.year, r=p$bsm.r/100, b=p$bsm.b/100, v=opt.table$greek.vol[i]/100)$value/365
      opt.table$gamma[i] <- GreekCalc(name="gamma", type=opt.table$type[i], S=fut.price, K=opt.table$strike[i], T=t.year, r=p$bsm.r/100, b=p$bsm.b/100, v=opt.table$greek.vol[i]/100)$value
      price.min <- max(0.01, opt.table$premium[i] - opt.table$vega[i] * opt.table$greek.vol[i] * config$vol.range, na.rm=TRUE)
      price.max <- max(0.01, opt.table$premium[i] + opt.table$vega[i] * opt.table$greek.vol[i] * config$vol.range, na.rm=TRUE)
      if(opt.table$bid[i] > price.min & opt.table$bid[i] < price.max)
        opt.table$vol.bid[i] <- max(0, GBSVolatility(price=opt.table$bid[i], TypeFlag=opt.table$short.type[i], S=fut.price, X=opt.table$strike[i], Time=t.year, r=p$bsm.r/100, b=p$bsm.b/100, tol=config$vol.tol) * 100)
      if(opt.table$ask[i] > price.min & opt.table$ask[i] < price.max)
        opt.table$vol.ask[i] <- max(0, GBSVolatility(price=opt.table$ask[i], TypeFlag=opt.table$short.type[i], S=fut.price, X=opt.table$strike[i], Time=t.year, r=p$bsm.r/100, b=p$bsm.b/100, tol=config$vol.tol) * 100)
    }
    if(!is.na(p$strike.delta) & p$strike.delta > 0)
      opt.table <- opt.table[abs(opt.table$delta) >= p$strike.delta/100 & abs(opt.table$delta) <= (1 - p$strike.delta/100), ]
    opt.strike <- sort(unique(opt.table$strike))
  }, silent=TRUE))[1] == "try-error") return(list(error="ERROR in greeks calculations"))
  
  # board calculations
  if(class(try({
    board <- NULL
    for(k in opt.strike)
    {
      l.put <- opt.table[opt.table$type == "Put" & opt.table$strike == k, c("sec.code", "gamma", "theta", "vega", "delta", "bid", "bid.size", "ask", "ask.size", "premium", "vol.bid", "vol.ask")]
      l.call <- opt.table[opt.table$type == "Call" & opt.table$strike == k, c("greek.vol", "vol.bid", "vol.ask", "premium", "bid", "bid.size", "ask", "ask.size", "delta", "vega", "theta", "gamma", "sec.code")]
      if(nrow(l.put) == 1 & nrow(l.call) == 1)
        board <- rbind(board, cbind(l.put, data.frame(Strike = k), l.call))
    }
    colnames(board) <- c("PCode", "PGamma", "PTheta", "PVega", "PDelta", "PBid", "PSBid", "PAsk", "PSAsk", "PPrem", "PVBid", "PVAsk", "Strike", 
                         "Vol", "CVBid", "CVAsk", "CPrem", "CBid", "CSBid", "CAsk", "CSAsk", "CDelta", "CVega", "CTheta", "CGamma", "CCode")
    board$PVBid <- round(board$PVBid, 2)
    board$PVAsk <- round(board$PVAsk, 2)
    board$CVBid <- round(board$CVBid, 2)
    board$CVAsk <- round(board$CVAsk, 2)
    board$Vol <- round(board$Vol, 2)
    board$PDelta <- round(board$PDelta, 2)
    board$PVega <- round(board$PVega, 2)
    board$PTheta <- round(board$PTheta, 2)
    board$PGamma <- round(board$PGamma * p$gamma.mult, 2)
    board$CDelta <- round(board$CDelta, 2)
    board$CVega <- round(board$CVega, 2)
    board$CTheta <- round(board$CTheta, 2)
    board$CGamma <- round(board$CGamma * p$gamma.mult, 2)
  }, silent=TRUE))[1] == "try-error") return(list(error="ERROR in board calculations"))
  
  # vol calculations
  if(class(try({
    #vol <- data.frame(strike=board$Strike, ex=NA, vv=NA, rn=NA)
    vol <- data.frame(strike=board$Strike)
    tmp <- opt.table[opt.table$strike %in% vol$strike, c("strike", "volatility")]
    tmp <- tmp[!duplicated(tmp$strike), ]
    tmp <- tmp[order(tmp$strike), 2]
    vol$ex <- tmp
    if(!is.null(opt.table$vol.vv))
    {
      tmp <- opt.table[opt.table$strike %in% vol$strike, c("strike", "vol.vv")]
      tmp <- tmp[!duplicated(tmp$strike), ]
      tmp <- tmp[order(tmp$strike), 2]
      vol$vv <- tmp
    }
    if(!is.null(opt.table$vol.rn))
    {
      tmp <- opt.table[opt.table$strike %in% vol$strike, c("strike", "vol.rn")]
      tmp <- tmp[!duplicated(tmp$strike), ]
      tmp <- tmp[order(tmp$strike), 2]
      vol$rn <- tmp
    }
    if(!is.null(opt.table$vol.rnb))
    {
      tmp <- opt.table[opt.table$strike %in% vol$strike, c("strike", "vol.rnb")]
      tmp <- tmp[!duplicated(tmp$strike), ]
      tmp <- tmp[order(tmp$strike), 2]
      vol$rnb <- tmp
    }
    if(!is.null(opt.table$vol.rna))
    {
      tmp <- opt.table[opt.table$strike %in% vol$strike, c("strike", "vol.rna")]
      tmp <- tmp[!duplicated(tmp$strike), ]
      tmp <- tmp[order(tmp$strike), 2]
      vol$rna <- tmp
    }
  }, silent=TRUE))[1] == "try-error") return(list(error="ERROR in vol calculations"))
  
  return(list(board=board, vol=vol, date.time=date.time, date.time.opt=date.time.opt, fut.price=fut.price,
              t.day=round(t.day, 2), vv.strike=vv.strike, error=0))
  
}

