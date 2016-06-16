
# Description
#
#
#
#


library(package="shiny", lib.loc="/home/mub/R/x86_64-pc-linux-gnu-library/3.2")
library(package="shinyBS", lib.loc="/home/mub/R/x86_64-pc-linux-gnu-library/3.2")
library(package="ggplot2", lib.loc="/home/mub/R/x86_64-pc-linux-gnu-library/3.2")

source(file="/home/mub/ShinyApps/board01/global.R", local=FALSE)


shinyUI(navbarPage(
  
  title = "BOARD",
  
  tabPanel(
    
    title = "Main",
    
    sidebarPanel(
      
      fluidRow(
        column(5, textInput("fut.code", label="Futures code", value="Short code")),
        column(5, textInput("opt.expiry", label="Options expiry", value="YYYY-MM-DD"))
      ),
      
      fluidRow(
        column(3, numericInput("bsm.r", label="r, %", value=0, step=0.01)),
        column(3, numericInput("bsm.b", label="b, %", value=0, step=0.01)),
        column(3, numericInput("bsm.dt", label=strong("dT, days"), value=0, min=0, step=0.01))
      ),
      
      p(strong("Risk-neutral moments:")),
      
      fluidRow(
        column(2, strong("Plot")),
        column(3, strong("Sigma")),
        column(3, strong("Skew")),
        column(3, strong("Kurt"))
      ),
      
      fluidRow(
        column(2, checkboxInput("rn.plot", label=strong(div("RN", style="color:#fd482f")), value=FALSE)),
        column(3, numericInput("rn.sigma", label=NULL, value=NA, min=0, step=0.05)),
        column(3, numericInput("rn.skew", label=NULL, value=NA, step=0.01)),
        column(3, numericInput("rn.kurt", label=NULL, value=NA, step=0.01))
      ),
      
      fluidRow(
        column(2, checkboxInput("rnb.plot", label=strong(div("Bid", style="color:#55bf3b")), value=FALSE)),
        column(3, numericInput("rnb.sigma", label=NULL, value=NA, step=0.01)),
        column(3, numericInput("rnb.skew", label=NULL, value=NA, step=0.01)),
        column(3, numericInput("rnb.kurt", label=NULL, value=NA, step=0.01))
      ),
      
      fluidRow(
        column(2, checkboxInput("rna.plot", label=strong(div("Ask", style="color:#d63b3b")), value=FALSE)),
        column(3, numericInput("rna.sigma", label=NULL, value=NA, step=0.01)),
        column(3, numericInput("rna.skew", label=NULL, value=NA, step=0.01)),
        column(3, numericInput("rna.kurt", label=NULL, value=NA, step=0.01))
      ),
      
      #p(strong("Vanna-volga method:")),
      radioButtons("vv.delta", strong("Vanna-volga method:"), inline=TRUE, c("25D"=25, "20D"=20, "15D"=15, "10D"=10, "5D"=5), selected=NULL),
      
      fluidRow(
        column(2, strong("Plot")),
        column(3, strong("ATM")),
        column(3, strong("RR")),
        column(3, strong("BF"))
      ),
      
      fluidRow(
        column(2, checkboxInput("vv.plot", label=strong(div("VV", style="color:#3399ff")), value=FALSE)),
        column(3, numericInput("vv.atm", label=NULL, value=NA, min=0, step=0.05)),
        column(3, numericInput("vv.rr25", label=NULL, value=NA, step=0.01)),
        column(3, numericInput("vv.bf25", label=NULL, value=NA, step=0.01))
      ),
      
      radioButtons("greek.vol", strong("Greeks volatility:"), inline=TRUE,
                   c("MOEX"="ex", "Risk-neutral"="rn", "Vanna-volga"="vv"), selected="ex"),
      
      fluidRow(
        column(4, bsButton("update.button", "Apply / Refresh", style="primary", size="default", icon=icon(name="refresh"))),
        column(4, sliderInput("update.slider", label=NULL, min=1, max=10, value=1, step=1, tick=FALSE, width="80%",
                              animate=animationOptions(interval=config$auto.update.sec * 1000, loop=TRUE, playButton=paste("Auto update", config$auto.update.sec, "sec"), pauseButton="Stop"))),
        column(4, actionButton("save.db.button", "Save to DB", icon(name="save")))
      ),
      
      fluidRow(
        column(4, ""),
        column(4, textInput("save.csv.name", label=NULL, value=NA)),
        column(4, actionButton("save.csv.button", "Save to csv", icon(name="save")))
      ),
      
      hr(""),
      
      fluidRow(
        column(4, numericInput("strike.low", label="Strike low", min=0, value=NA)),
        column(4, numericInput("strike.high", label="Strike high", min=0, value=NA)),
        column(3, numericInput("strike.delta", label="Delta limit", value=NA, min=0, max=50, step=0.1))
      ),
      
      fluidRow(
        column(4, numericInput("vol.low", label="Vol low", value=NA, min=0, step=0.1)),
        column(4, numericInput("vol.high", label="Vol high", value=NA, min=0, step=0.1)),
        column(3, numericInput("vol.auto", label="Vol auto", value=NA, min=0, step=0.1))
      ),
      
      hr(""),
      
      fluidRow(
        #column(5, selectInput("load.config.name", label=NULL, selected=NA, choices=list.files(path=config$dir))),
        column(4, textInput("load.config.name", label=NULL, value=NA)),
        column(4, actionButton("load.config.button", "Load config", icon(name="download")))
      ),
      
      fluidRow(
        column(4, textInput("save.config.name", label=NULL, value=NA)),
        column(4, actionButton("save.config.button", "Save config", icon(name="save")))
      ),
      
      hr("board.log (5 last records)"),
      verbatimTextOutput("log.text"),
      
      helpText(
        "Developed by", a("Oleg Mubarakshin", href="http://quant-lab.com/about.html", target="_blank"),
        align="right"
      ),
      
      width = 4
      
    ),
    
    mainPanel(
      
      plotOutput("smile.plot", width="100%", height="800px", click="smile.plot.click"),
      
      fluidRow(
        column(5, verbatimTextOutput("smile.plot.click")),
        column(2, actionButton("post.plotly", "Post plotly", icon(name="cloud-upload")), uiOutput("link.plotly"))
      ),
      
      br(),
      
      numericInput("gamma.mult", label="Gamma mult", value=1, min=0),
      
      radioButtons("board.table.pres", strong("Table: Option chain"), inline=TRUE,
                   c("Premium"="prem", "Volatility"="vol", "Hide"="hide"), selected="hide"),
      
      tableOutput("board.table")
      
    )
    
  ),
  
  tabPanel(
    title = "Config",
    p("Page under construction")
  ),
  
  tabPanel(
    title = "Log",
    p("Page under construction")
  )
  
))
