
# Description
#
#
#
#

library(package="shiny", lib.loc="/home/mub/R/x86_64-pc-linux-gnu-library/3.2")
library(package="ggplot2", lib.loc="/home/mub/R/x86_64-pc-linux-gnu-library/3.2")
library(package="plotly", lib.loc="/home/mub/R/x86_64-pc-linux-gnu-library/3.2")



shinyServer(function(input, output, session) {
  
  source(file="/home/mub/ShinyApps/board01/global.R", local=TRUE)
  source(file="/home/mub/ShinyApps/board01/header.R", local=TRUE)
  
  pref <- NULL
  #gboard <- NULL
  log.sess <- NULL
  log.react <- reactiveValues()
  board.react <- reactiveValues()
  smile.plot.click.react <- reactiveValues()
  plotly.link.react <- reactiveValues()
  
  # board load config
  observe({
    isolate(load.config.name <- input$load.config.name)
    if(input$load.config.button > 0)
    {
      if(class(try({
        load(file=file.path(config$dir.cfg, load.config.name))
      }, silent=TRUE))[1] != "try-error")
      {
        log.line <- paste(Sys.time(), "OK", "load config file", file.path(config$dir.cfg, load.config.name), sep=", ")
        #cat(log.line, file=config$file.log, sep="\n", append=TRUE)
        updateTextInput(session, "fut.code", value=pref$fut.code)
        updateTextInput(session, "opt.expiry", value=pref$opt.expiry)
        updateNumericInput(session, "bsm.r", value=pref$bsm.r)
        updateNumericInput(session, "bsm.b", value=pref$bsm.b)
        updateNumericInput(session, "bsm.dt", value=pref$bsm.dt)
        updateCheckboxInput(session, "rn.plot", value=pref$rn.plot)
        updateNumericInput(session, "rn.sigma", value=pref$rn.sigma)
        updateNumericInput(session, "rn.skew", value=pref$rn.skew)
        updateNumericInput(session, "rn.kurt", value=pref$rn.kurt)
        updateCheckboxInput(session, "rnb.plot", value=pref$rnb.plot)
        updateNumericInput(session, "rnb.sigma", value=pref$rnb.sigma)
        updateNumericInput(session, "rnb.skew", value=pref$rnb.skew)
        updateNumericInput(session, "rnb.kurt", value=pref$rnb.kurt)
        updateCheckboxInput(session, "rna.plot", value=pref$rna.plot)
        updateNumericInput(session, "rna.sigma", value=pref$rna.sigma)
        updateNumericInput(session, "rna.skew", value=pref$rna.skew)
        updateNumericInput(session, "rna.kurt", value=pref$rna.kurt)
        updateRadioButtons(session, "vv.delta", selected=as.character(pref$vv.delta))
        updateCheckboxInput(session, "vv.plot", value=pref$vv.plot)
        updateNumericInput(session, "vv.atm", value=pref$vv.atm)
        updateNumericInput(session, "vv.rr25", value=pref$vv.rr25)
        updateNumericInput(session, "vv.bf25", value=pref$vv.bf25)
        updateRadioButtons(session, "greek.vol", selected=pref$greek.vol)
        updateNumericInput(session, "strike.low", value=pref$strike.low)
        updateNumericInput(session, "strike.high", value=pref$strike.high)
        updateNumericInput(session, "strike.delta", value=pref$strike.delta)
        updateNumericInput(session, "vol.low", value=pref$vol.low)
        updateNumericInput(session, "vol.high", value=pref$vol.high)
        updateNumericInput(session, "vol.auto", value=pref$vol.auto)
        updateTextInput(session, "save.config.name", value=load.config.name)
        updateNumericInput(session, "gamma.mult", value=pref$gamma.mult)
      } else {
        log.line <- paste(Sys.time(), "ERROR", "load config file", file.path(config$dir.cfg, load.config.name), sep=", ")
        #cat(log.line, file=config$file.log, sep="\n", append=TRUE)
      }
      log.react$rec <- log.line
    }
  })
  
  # save pref to config file and DB
  observe({
    isolate({
      save.config.name <- input$save.config.name
      pref$fut.code <- input$fut.code
      pref$opt.expiry <- input$opt.expiry
      pref$bsm.r <- input$bsm.r
      pref$bsm.b <- input$bsm.b
      pref$bsm.dt <- input$bsm.dt
      pref$rn.plot <- input$rn.plot
      pref$rn.sigma <- input$rn.sigma
      pref$rn.skew <- input$rn.skew
      pref$rn.kurt <- input$rn.kurt
      pref$rnb.plot <- input$rnb.plot
      pref$rnb.sigma <- input$rnb.sigma
      pref$rnb.skew <- input$rnb.skew
      pref$rnb.kurt <- input$rnb.kurt
      pref$rna.plot <- input$rna.plot
      pref$rna.sigma <- input$rna.sigma
      pref$rna.skew <- input$rna.skew
      pref$rna.kurt <- input$rna.kurt
      pref$vv.delta <- as.numeric(input$vv.delta)
      pref$vv.plot <- input$vv.plot
      pref$vv.atm <- input$vv.atm
      pref$vv.rr25 <- input$vv.rr25
      pref$vv.bf25 <- input$vv.bf25
      pref$greek.vol <- input$greek.vol
      pref$strike.low <- input$strike.low
      pref$strike.high <- input$strike.high
      pref$strike.delta <- input$strike.delta
      pref$vol.low <- input$vol.low
      pref$vol.high <- input$vol.high
      pref$vol.auto <- input$vol.auto
      pref$gamma.mult <- input$gamma.mult
    })
    if(input$save.config.button > 0 | input$save.db.button > 0)
    {
      if(class(try({
        file.save <- file.path(config$dir.cfg, save.config.name)
        save(pref, file=file.save)
      }, silent=TRUE))[1] != "try-error")
      {
        log.line <- paste(Sys.time(), "OK", "save config file", file.save, sep=", ")
        #cat(log.line, file=config$file.log, sep="\n", append=TRUE)
      } else {
        log.line <- paste(Sys.time(), "ERROR", "save config file", file.save, sep=", ")
        #cat(log.line, file=config$file.log, sep="\n", append=TRUE)
      }
      
      # log.line1 <- log.line
      # if(class(try({
      #   sys.date <- Sys.time()
      #   sys.time <- format(sys.date, "%H:%M:%S")
      #   sys.date <- format(sys.date, "%Y-%m-%d")
      #   con <- do.call(dbConnect, pg.config)
      #   if(pref$rn.plot)
      #   {
      #     db.query <- paste("INSERT INTO ", config$vol.param.table, " VALUES(",
      #                       "'", sys.date, "','", sys.time, "','", pref$fut.code, "','", pref$opt.expiry, "','", "RN", "','",
      #                       pref$bsm.r, "','", pref$bsm.b, "','", pref$rn.sigma, "','", pref$rn.skew, "','", pref$rn.kurt, "')", sep="")
      #     send.query <- dbSendQuery(con, db.query)
      #     clear.res <- dbClearResult(send.query)
      #   }
      #   if(pref$rn.plot & pref$rnb.plot)
      #   {
      #     db.query <- paste("INSERT INTO ", config$vol.param.table, " VALUES(",
      #                       "'", sys.date, "','", sys.time, "','", pref$fut.code, "','", pref$opt.expiry, "','", "RN_bid", "','",
      #                       pref$bsm.r, "','", pref$bsm.b, "','", pref$rnb.sigma, "','", pref$rnb.skew, "','", pref$rnb.kurt, "')", sep="")
      #     send.query <- dbSendQuery(con, db.query)
      #     clear.res <- dbClearResult(send.query)
      #   }
      #   if(pref$rn.plot & pref$rna.plot)
      #   {
      #     db.query <- paste("INSERT INTO ", config$vol.param.table, " VALUES(",
      #                       "'", sys.date, "','", sys.time, "','", pref$fut.code, "','", pref$opt.expiry, "','", "RN_ask", "','",
      #                       pref$bsm.r, "','", pref$bsm.b, "','", pref$rna.sigma, "','", pref$rna.skew, "','", pref$rna.kurt, "')", sep="")
      #     send.query <- dbSendQuery(con, db.query)
      #     clear.res <- dbClearResult(send.query)
      #   }
      #   if(pref$vv.plot)
      #   {
      #     db.query <- paste("INSERT INTO ", config$vol.param.table, " VALUES(",
      #                       "'", sys.date, "','", sys.time, "','", pref$fut.code, "','", pref$opt.expiry, "','", paste0("VV", pref$vv.delta), "','",
      #                       pref$bsm.r, "','", pref$bsm.b, "','", pref$vv.atm, "','", pref$vv.rr25, "','", pref$vv.bf25, "')", sep="")
      #     send.query <- dbSendQuery(con, db.query)
      #     clear.res <- dbClearResult(send.query)
      #   }
      #   discon <- dbDisconnect(con)
      # }, silent=TRUE))[1] != "try-error")
      # {
      #   log.line <- paste(Sys.time(), "OK", "save to DB", config$vol.param.table, sep=", ")
      #   #cat(log.line, file=config$file.log, sep="\n", append=TRUE)
      # } else {
      #   log.line <- paste(Sys.time(), "ERROR", "save to DB", config$vol.param.table, sep=", ")
      #   #cat(log.line, file=config$file.log, sep="\n", append=TRUE)
      # }
      # log.react$rec <- c(log.line, log.line1)
      
      log.react$rec <- log.line
    }
  })
  
  # board calc
  observe({
    
    isolate({
      pref$fut.code <- input$fut.code
      pref$opt.expiry <- input$opt.expiry
      pref$bsm.r <- input$bsm.r
      pref$bsm.b <- input$bsm.b
      pref$bsm.dt <- input$bsm.dt
      pref$rn.plot <- input$rn.plot
      pref$rn.sigma <- input$rn.sigma
      pref$rn.skew <- input$rn.skew
      pref$rn.kurt <- input$rn.kurt
      pref$rnb.plot <- input$rnb.plot
      pref$rnb.sigma <- input$rnb.sigma
      pref$rnb.skew <- input$rnb.skew
      pref$rnb.kurt <- input$rnb.kurt
      pref$rna.plot <- input$rna.plot
      pref$rna.sigma <- input$rna.sigma
      pref$rna.skew <- input$rna.skew
      pref$rna.kurt <- input$rna.kurt
      pref$vv.delta <- as.numeric(input$vv.delta)
      pref$vv.plot <- input$vv.plot
      pref$vv.atm <- input$vv.atm
      pref$vv.rr25 <- input$vv.rr25
      pref$vv.bf25 <- input$vv.bf25
      pref$greek.vol <- input$greek.vol
      pref$strike.low <- input$strike.low
      pref$strike.high <- input$strike.high
      pref$strike.delta <- input$strike.delta
      pref$vol.low <- input$vol.low
      pref$vol.high <- input$vol.high
      pref$vol.auto <- input$vol.auto
      pref$gamma.mult <- input$gamma.mult
    })
    
    if(input$update.button > 0 | input$update.slider > 0)
    {
      board.react$data <- BoardCalc(p=pref)
      #gboard <<- board.react$data
    }
    
  })
  
  # save vol csv
  observe({
    isolate({
      file.name <- input$save.csv.name
      board <- board.react$data$board
      #board <- gboard$board
    })
    if(input$save.csv.button > 0)
    {
      if(class(try({
        vol <- board[, c("Strike", "Vol")]
        file.save <- file.path(config$dir.csv, file.name)
        write.table(vol, file=file.save, row.names=FALSE, quote=TRUE, sep=";")
      }, silent=TRUE))[1] != "try-error")
      {
        log.line <- paste(Sys.time(), "OK", "save csv file", file.save, "Strike Vol:", paste(vol$Strike, vol$Vol, sep=" ", collapse=", "), sep=", ")
        #cat(log.line, file=config$file.log, sep="\n", append=TRUE)
      } else {
        log.line <- paste(Sys.time(), "ERROR", "save csv file", file.save, sep=", ")
        #cat(log.line, file=config$file.log, sep="\n", append=TRUE)
      }
      log.react$rec <- log.line
    }
  })
  
  # click on smile chart
  observe({
    if(!is.null(input$smile.plot.click))
    {
      smile.plot.click.react$x <- input$smile.plot.click$x
      smile.plot.click.react$y <- input$smile.plot.click$y
    }
  })
  
  # plot smile chart
  output$smile.plot <- renderPlot({
    
    isolate({
      pref$fut.code <- input$fut.code
      pref$opt.expiry <- input$opt.expiry
      pref$vv.plot <- input$vv.plot
      pref$rn.plot <- input$rn.plot
      pref$rnb.plot <- input$rnb.plot
      pref$rna.plot <- input$rna.plot
      pref$vol.low <- input$vol.low
      pref$vol.high <- input$vol.high
      pref$vol.auto <- input$vol.auto
    })
    
    #if(input$update.button == 0) return(NULL)
    
    data <- board.react$data
    #data <- gboard
    
    if(is.null(data)) return(NULL)
    
    if(data$error != 0) return(NULL)
    
    board <- data$board
    vol <- data$vol
    
    board$PVBid[board$PBid == 0] <- NA
    board$PVAsk[board$PAsk == 0] <- NA
    board$CVBid[board$CBid == 0] <- NA
    board$CVAsk[board$CAsk == 0] <- NA
    
    plot.ylim <- range(board[, c("PVBid", "PVAsk", "CVBid", "CVAsk", "Vol")], vol[, colnames(vol) != "strike"], na.rm=TRUE)
    plot.ylim <- c(max(plot.ylim[1], pref$vol.low, na.rm=TRUE), min(plot.ylim[2], pref$vol.high, na.rm=TRUE))
    if(!is.null(pref$vol.auto) & !is.na(pref$vol.auto) & pref$vol.auto > 0)
      plot.ylim <- c(max(plot.ylim[1], (min(board$Vol) - pref$vol.auto), na.rm=TRUE), min(plot.ylim[2], (max(board$Vol) + pref$vol.auto), na.rm=TRUE))
    
    sys.time <- Sys.time()
    dtime1 <- round(as.numeric(difftime(sys.time, data$date.time, units="secs")), 0)
    dtime2 <- round(abs(as.numeric(difftime(data$date.time, data$date.time.opt, units="secs"))), 0)
    warning.msg <- NULL
    if(dtime1 > config$sys.fut.dtime)
      warning.msg <- c(warning.msg, paste("dt(sys-fut) =", dtime1, "sec"))
    if(dtime2 > config$fut.opt.dtime)
      warning.msg <- c(warning.msg, paste("dt(fut-opt) =", dtime2, "sec"))
    main.title <- paste("Options: ", pref$fut.code, "/", pref$opt.expiry, ", update: ", data$date.time.opt, sep="")
    sub.title <- paste("Fut: ", data$fut.price, ", update: ", data$date.time, sep="")
    if(!is.null(warning.msg))
    {
      sub.title <- paste(sub.title, "WARNING:")
      for(i in 1:length(warning.msg))
        sub.title <- paste(sub.title, warning.msg[i])
    }
    
    g <- ggplot() + theme_bw()
    g <- g + ggtitle(bquote(atop(.(main.title), atop(italic(.(sub.title)), ""))))
    if(!is.null(warning.msg))
      g <- g + theme(plot.title=element_text(color="red"), plot.background=element_rect(fill="yellow"))
    g <- g + xlab("Strike") + ylab("Volatility, %")
    g <- g + ylim(plot.ylim)
    g <- g + geom_vline(xintercept=data$fut.price, color="#808080")
    if(pref$vv.plot)
      g <- g + geom_vline(xintercept=c(data$vv.strike["put25"], data$vv.strike["call25"]), color="#808080", linetype="dotted")
    g <- g + geom_line(data=vol, mapping=aes(x=strike, y=ex), color="#999999")
    if(pref$vv.plot)
      g <- g + geom_line(data=vol, mapping=aes(x=strike, y=vv), color="#3399ff", size=1, alpha=0.5) + geom_point(data=vol, mapping=aes(x=strike, y=vv), color="#3399ff", size=2)
    if(pref$rn.plot)
      g <- g + geom_line(data=vol, mapping=aes(x=strike, y=rn), color="#fd482f", size=1, alpha=0.5) + geom_point(data=vol, mapping=aes(x=strike, y=rn), color="#fd482f", size=2)
    if(pref$rnb.plot)
      g <- g + geom_line(data=vol, mapping=aes(x=strike, y=rnb), color="#55bf3b", size=0.5, alpha=1, linetype="dotted")# + geom_point(data=vol, mapping=aes(x=strike, y=rnb), color="#55bf3b", size=1)
    if(pref$rna.plot)
      g <- g + geom_line(data=vol, mapping=aes(x=strike, y=rna), color="#d63b3b", size=0.5, alpha=1, linetype="dotted")# + geom_point(data=vol, mapping=aes(x=strike, y=rna), color="#fd482f", size=1)
    g <- g + geom_point(data=board, mapping=aes(x=Strike, y=PVBid), shape=24, color="#ff0000", fill="#ff0000", size=4, alpha=0.50)
    g <- g + geom_point(data=board, mapping=aes(x=Strike, y=PVAsk), shape=25, color="#ff0000", fill="#ff0000", size=4, alpha=0.50)
    g <- g + geom_point(data=board, mapping=aes(x=Strike, y=CVBid), shape=24, color="#008000", fill="#008000", size=4, alpha=0.50)
    g <- g + geom_point(data=board, mapping=aes(x=Strike, y=CVAsk), shape=25, color="#008000", fill="#008000", size=4, alpha=0.50)
    
    return(g)
    
  }, res=90)
  
  # click on smile plot
  output$smile.plot.click <- renderPrint({
    strike <- smile.plot.click.react$x
    vol <- smile.plot.click.react$y
    if(is.null(strike) | is.null(vol))
    {
      return(cat(NULL))
    } else {
      isolate({
        pref$bsm.r <- input$bsm.r
        pref$bsm.b <- input$bsm.b
        pref$gamma.mult <- input$gamma.mult
        data <- board.react$data
        #data <- gboard
      })
      board <- data$board
      fut.price <- data$fut.price
      t.day <- data$t.day
      strike.seq <- board$Strike
      strike <- strike.seq[abs(strike.seq - strike) == min(abs(strike.seq - strike))][1]
      vol <- round(vol, 2)
      board <- board[board$Strike == strike, ]
      opt.type <- ifelse(strike > fut.price, "c", "p")
      call.premium <- round(GreekCalc(name="premium", type="c", S=fut.price, K=strike, T=t.day/365, r=pref$bsm.r/100, b=pref$bsm.b/100, v=vol/100)$value, 2)
      put.premium <- round(GreekCalc(name="premium", type="p", S=fut.price, K=strike, T=t.day/365, r=pref$bsm.r/100, b=pref$bsm.b/100, v=vol/100)$value, 2)
      delta.call <- round(GreekCalc(name="delta", type="c", S=fut.price, K=strike, T=t.day/365, r=pref$bsm.r/100, b=pref$bsm.b/100, v=vol/100)$value, 2)
      delta.put <- round(GreekCalc(name="delta", type="p", S=fut.price, K=strike, T=t.day/365, r=pref$bsm.r/100, b=pref$bsm.b/100, v=vol/100)$value, 2)
      vega <- round(GreekCalc(name="vega", type=opt.type, S=fut.price, K=strike, T=t.day/365, r=pref$bsm.r/100, b=pref$bsm.b/100, v=vol/100)$value/100, 2)
      theta <- round(GreekCalc(name="theta", type=opt.type, S=fut.price, K=strike, T=t.day/365, r=pref$bsm.r/100, b=pref$bsm.b/100, v=vol/100)$value/365, 2)
      gamma <- round(pref$gamma.mult * GreekCalc(name="gamma", type=opt.type, S=fut.price, K=strike, T=t.day/365, r=pref$bsm.r/100, b=pref$bsm.b/100, v=vol/100)$value, 3)
      return(cat(paste("Strike: ", strike, ", Vol: ", vol, "\n",
                       "Call: ", call.premium, ", BBBO: ", "(", board$CSBid, ")", board$CBid, " - ", board$CAsk, "(", board$CSAsk, "), VS: ", board$CVBid, " - ", board$CVAsk, "\n",
                       "Put: ", put.premium, ", BBBO: ", "(", board$PSBid, ")", board$PBid, " - ", board$PAsk, "(", board$PSAsk, "), VS: ", board$PVBid, " - ", board$PVAsk, "\n",
                       "Delta: ", delta.call, "/", delta.put, ", Vega: ", vega, ", Theta: ", theta, ", Gamma: ", gamma, sep="")))
    }
  })
  
  # board table
  output$board.table <- renderTable({
    
    #if(input$update.button == 0) return(NULL)
    if(input$board.table.pres == "hide") return(NULL)
    
    data <- board.react$data
    #data <- gboard
    if(is.null(data)) return(NULL)
    if(data$error != 0) return(NULL)
    
    if(input$board.table.pres == "prem")
      board <- data$board[, c("PCode", "PTheta", "PVega", "PDelta", "PBid", "PAsk", "PPrem", "Strike", "Vol", "CPrem", "CBid", "CAsk", "CDelta", "CVega", "CTheta", "CCode")]
    
    if(input$board.table.pres == "vol")
    {
      board <- data$board[, c("PCode", "PGamma", "PTheta", "PVega", "PDelta", "PVBid", "PVAsk", "Strike", "Vol", "CVBid", "CVAsk", "CDelta", "CVega", "CTheta", "CGamma", "CCode")]
      board$PVBid[board$PBid == 0] <- NA
      board$PVAsk[board$PAsk == 0] <- NA
      board$CVBid[board$CBid == 0] <- NA
      board$CVAsk[board$CAsk == 0] <- NA
    }
    
    return(board)
    
  }, include.rownames=FALSE)
  
  # print log
  output$log.text <- renderPrint({
    log.sess <<- c(log.react$rec, log.sess)
    if(length(log.sess) > 5)
      log.sess <<- log.sess[1:5]
    cat(paste(log.sess, collapse="\n"))
  })
  
  # post plotly
  observe({
    isolate({
      pref$fut.code <- input$fut.code
      pref$opt.expiry <- input$opt.expiry
      pref$vv.plot <- input$vv.plot
      pref$rn.plot <- input$rn.plot
      pref$rnb.plot <- input$rnb.plot
      pref$rna.plot <- input$rna.plot
      pref$vol.low <- input$vol.low
      pref$vol.high <- input$vol.high
      pref$vol.auto <- input$vol.auto
      data <- board.react$data
    })
    
    if(input$post.plotly == 0) return(NULL)
    
    if(is.null(data)) return(NULL)
    
    if(data$error != 0) return(NULL)
    
    if(class(try({
      board <- data$board
      vol <- data$vol
      board$PVBid[board$PBid == 0] <- NA
      board$PVAsk[board$PAsk == 0] <- NA
      board$CVBid[board$CBid == 0] <- NA
      board$CVAsk[board$CAsk == 0] <- NA
      plot.ylim <- range(board[, c("PVBid", "PVAsk", "CVBid", "CVAsk", "Vol")], vol[, colnames(vol) != "strike"], na.rm=TRUE)
      plot.ylim <- c(max(plot.ylim[1], pref$vol.low, na.rm=TRUE), min(plot.ylim[2], pref$vol.high, na.rm=TRUE))
      if(!is.null(pref$vol.auto) & !is.na(pref$vol.auto) & pref$vol.auto > 0)
        plot.ylim <- c(max(plot.ylim[1], (min(board$Vol) - pref$vol.auto), na.rm=TRUE), min(plot.ylim[2], (max(board$Vol) + pref$vol.auto), na.rm=TRUE))
      main.title <- paste("Options: ", pref$fut.code, "/", pref$opt.expiry, ", update: ", data$date.time.opt, sep="")
      gg <- ggplot() + theme_bw()
      gg <- gg + ggtitle(main.title)
      gg <- gg + xlab("Strike") + ylab("Volatility, %")
      gg <- gg + geom_line(data=data.frame(x=c(data$fut.price, data$fut.price), y=plot.ylim), mapping=aes(x=x, y=y), color="#808080", size=0.01)
      if(pref$vv.plot)
      {
        gg <- gg + geom_line(data=vol, mapping=aes(x=strike, y=vv), color="#3399ff", size=0.1) + geom_point(data=vol, mapping=aes(x=strike, y=vv), color="#3399ff", size=0.5)
        gg <- gg + geom_line(data=data.frame(x=c(data$vv.strike["put25"], data$vv.strike["put25"]), y=plot.ylim), mapping=aes(x=x, y=y), linetype="dotted", color="#808080", size=0.01)
        gg <- gg + geom_line(data=data.frame(x=c(data$vv.strike["call25"], data$vv.strike["call25"]), y=plot.ylim), mapping=aes(x=x, y=y), linetype="dotted", color="#808080", size=0.01)
      }
      if(pref$rn.plot)
        gg <- gg + geom_line(data=vol, mapping=aes(x=strike, y=rn), color="#fd482f", size=0.1) + geom_point(data=vol, mapping=aes(x=strike, y=rn), color="#fd482f", size=0.5)
      gg <- gg + geom_line(data=vol, mapping=aes(x=strike, y=ex), color="#999999", size=0.1)
      gg <- gg + geom_point(data=board, mapping=aes(x=Strike, y=PVBid), shape=24, color="#ff0000", fill="#ff0000", size=4, alpha=0.50)
      gg <- gg + geom_point(data=board, mapping=aes(x=Strike, y=PVAsk), shape=25, color="#ff0000", fill="#ff0000", size=4, alpha=0.50)
      gg <- gg + geom_point(data=board, mapping=aes(x=Strike, y=CVBid), shape=24, color="#008000", fill="#008000", size=4, alpha=0.50)
      gg <- gg + geom_point(data=board, mapping=aes(x=Strike, y=CVAsk), shape=25, color="#008000", fill="#008000", size=4, alpha=0.50)
      p <- ggplotly(gg) %>%  layout(autosize=F, width=1024, height=768)
      plotly.name <- paste(pref$fut.code, format(as.Date(pref$opt.expiry), "%m%d"), sep="_")
      plotly.name <- paste(config$plotly.dir, plotly.name, sep="/")
      pp <- plotly_POST(p, filename=plotly.name, fileopt="overwrite")
    }, silent=TRUE))[1] == "try-error")
    {
      log.line <- paste(Sys.time(), "ERROR", "post plotly", geterrmessage(), sep=", ")
      log.react$rec <- log.line
      plotly.link.react$link <- NULL
      return(NULL)
    } else {
      log.line <- paste(Sys.time(), "Ok", "post plotly", pp$url, sep=", ")
      log.react$rec <- log.line
      plotly.link.react$link <- pp$url
      return(NULL)
    }
  })
  
  # plotly link
  output$link.plotly <- renderUI({
    a(plotly.link.react$link, href=plotly.link.react$link, target="_blank")
  })
  
})