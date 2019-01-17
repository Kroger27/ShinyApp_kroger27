library(shiny)
library(quantmod)
library(GenSA)
library(ggplot2)
library(gridExtra)

get.ticker_symbols <- function(tickers, start_date, end_date) {
  getSymbols(tickers, from = start_date, to = end_date)
  
  P <- NULL
  
  for (ticker in tickers) {
    tmp <- Cl(to.daily(eval(parse(text = ticker))))
    P <- cbind(P, tmp)
  }
  
  colnames(P) <- tickers
  
  return(P)
}


ui <- fluidPage(
        titlePanel(em("WELCOME TO ROGERS'S STOCK APPLICATION")),
        br(),
        p(code("The following application allows you to analyize and compare closing stock prices based on the dates selected")),
        br(),
        p(code("Note: Weekend dates will not output closing price, due to stock market being close")),
        br(),  
                selectInput(inputId = "stock", "Please select the stock of your choice", 
                            c("","GE","IBM", "JPM","MSFT","WMT","AMZN", "GOOGL","XOM","FB","AAL")),
        dateInput(inputId = "start_date",label= "From", value = NULL, min= "2010-01-01", format = "yyyy-mm-dd"),
        dateInput(inputId = "end_date", label = "To", value = NULL, format = "yyyy-mm-dd"),
        actionButton(inputId = "clicks", label = "update"),
        verbatimTextOutput("stats"),
        plotOutput("line")
                
)

server <- function(input, output) {
  data <- eventReactive(input$clicks, {
    ticker <- input$stock
    
    if (ticker == "") {
      return(NULL)
    }
    
    as.data.frame(get.ticker_symbols(
      c(ticker),
      start_date = input$start_date,
      end_date = input$end_date
    ))
  }) 
 
  output$stats <- renderPrint({
    t_data <- data()
    
    if (is.null(t_data)) {
      return(NULL)
    }
    
    summary(data())
  }) 
  
  output$line = renderPlot({
    t_df <- as.data.frame(data())
    
    colnames(t_df) <- c("ticker")
    
    if (is.null(t_df)) {
      return(NULL)
    }
    
    withProgress("ploting")
    
    dates <- as.factor(row.names.data.frame(t_df))
    ggplot(t_df, aes(x= dates, y = ticker )) +
      geom_point(shape=20, size=4, col = "purple") +
      geom_line() + 
      ggtitle("Closing Stock Price Plots") +
      theme (axis.text.x = element_text(angle = 90, hjust = 1))
    
    
  })
    
  
}

shinyApp(ui, server=server)

