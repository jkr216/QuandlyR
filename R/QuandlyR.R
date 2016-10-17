#' QuandlyR is an RStudio addin designed to simplify the process of downloading data from Fred.
#' This initial version is of the Quandl package.
#' @author Jonathan Regenstein
#' @return Return either a csv file per instrument or a data frame. The csv file is stored in the working directory and the data frame in the globalenv.
#' @title QuandlyR 
#' @export 
#' @import shiny miniUI xts zoo Quandl
#' @seealso \code{Quandl}
#' @seealso \code{shiny}

QuandlyR <- function() {
  requireNamespace("shiny")
  requireNamespace("miniUI")
  requireNamespace("Quandl")
  requireNamespace("stringr")

  #### 1 - UI
  ui <- miniPage(
    gadgetTitleBar(" ",
                   right = miniTitleBarButton("run", "Run", primary = TRUE),
                   left = miniTitleBarButton("close", "Close", primary = TRUE)),
    miniTabstripPanel(
      miniTabPanel("Main",icon = icon("download"),
                   miniContentPanel(
                     ##textInput("api_key", "your Quandl api key", " "),
                     
                     selectInput("dataSource",
                                 "Source",
                                 choices = c("FRED", #fred data from federal reserve
                                             "WORLDBANK", # world bank
                                             "FMAC", #freddie mac
                                             "BIS",  #bank of intl settlements
                                             "ODA"), #IMF
                                 selected = "FRED"),
                     
                     selectInput("frequency",
                                 "freq",
                                 choices = c("annual", 
                                             "quarterly", 
                                             "monthly"),
                                 selected = "monthly"),
                     
                     textInput("instrument", "Instrument", " "),

                     dateRangeInput("dateRange",
                                    "Date range",
                                    start = "1980-01-01",
                                    end   = Sys.Date())
                   )
      ),
      miniTabPanel("Information",icon = icon("info"),
                   div(HTML("This RStudio addin was created by Jonathan Regenstein
                            as an extension of the fidlr package created by thertrader.
                            <br>Enjoy!")
                   )
                   )
                   )
                   )

  #### 2 - SERVER
  server <- function(input, output, session) {
    result <- reactive({
      dataSource <- toupper(gsub(" ", "", input$dataSource, fixed = TRUE))
      instrument <- toupper(gsub(" ", "", input$instrument, fixed = TRUE))
      ##insrumentList <- str_to_upper(gsub(" ", "", input$instrument, fixed = TRUE))
      ##insrumentList <- c(unlist(strsplit(insrumentList,",")))
      ##if you think you'll use this more than 50 times per day, you'll need an api key
      ##Quandl.api_key(as.character(input$api_key))
      
      inst <- paste(dataSource, instrument, sep="/")
      
      data <- Quandl(inst,
                       start_date = format(input$dateRange[1]),
                       end_date = format(input$dateRange[2]),
                       #type = 'xts' 
                       collapse = as.character(input$frequency)
                       )
      
      assign(inst, data, .GlobalEnv)
    })
    observeEvent(input$run, {result()})
    observeEvent(input$close, {stopApp()})
  }

  #### 3 - RUN GADGET
  runGadget(ui,
            server,
            viewer = dialogViewer("QuandlyR", width = 330, height = 420))
}



