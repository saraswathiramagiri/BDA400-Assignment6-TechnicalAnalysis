library(shiny)
library(ggplot2)
library(quantmod)
# Fetch historical stock data from Yahoo Finance
stock_symbol <- "AAPL"
start_date <- "2023-01-01"
end_date <- "2023-07-01"

stock_data <- getSymbols(
  stock_symbol,
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)
# User Interface
ui <- fluidPage(
  titlePanel("Stock Portfolio Technical Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = start_date,
        end = end_date
      ),
      
      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly"),
        selected = "Daily"
      ),
      
      checkboxGroupInput(
        "technical_indicators",
        "Technical Indicators:",
        choices = c(
          "Moving Averages",
          "RSI",
          "MACD"
        )
      )
    ),
    
    mainPanel(
      plotOutput("stock_chart"),
      
      conditionalPanel(
        condition = "input.technical_indicators.includes('RSI')",
        plotOutput("rsi_chart")
      ),
      
      conditionalPanel(
        condition = "input.technical_indicators.includes('MACD')",
        plotOutput("macd_chart")
      )
    )
  )
)
# Server
server <- function(input, output) {
  
  output$stock_chart <- renderPlot({
    
    # Filter stock data based on selected date range
    filtered_data <- stock_data[
      index(stock_data) >= input$date_range[1] &
        index(stock_data) <= input$date_range[2]
    ]
    # Apply selected time frame
    if (input$time_frame == "Weekly") {
      filtered_data <- apply.weekly(filtered_data, last)
    } else if (input$time_frame == "Monthly") {
      filtered_data <- apply.monthly(filtered_data, last)
    }
    # Calculate Moving Averages
    close_prices <- Cl(filtered_data)
    
    short_ma <- SMA(close_prices, n = 20)
    long_ma <- SMA(close_prices, n = 50)
    # Calculate RSI
    rsi_values <- RSI(close_prices, n = 14)
    
    # Convert data for ggplot
    # Create Buy, Sell, and Hold trading signals
    signals <- ifelse(
      short_ma > long_ma,
      "Buy",
      ifelse(short_ma < long_ma, "Sell", "Hold")
    )
    
    # Convert data for ggplot
    plot_data <- data.frame(
      Date = index(filtered_data),
      Close = as.numeric(Cl(filtered_data)),
      Signal = as.character(signals)
    )
    
    # Create stock price line chart
    p <- ggplot(plot_data, aes(x = Date, y = Close)) +
      geom_line() +
      labs(
        title = paste(stock_symbol, "Stock Price"),
        x = "Date",
        y = "Closing Price"
      ) +
      theme_minimal()
    
    # Overlay Moving Averages when selected
    if ("Moving Averages" %in% input$technical_indicators) {
      
      ma_data <- data.frame(
        Date = index(filtered_data),
        Short_MA = as.numeric(short_ma),
        Long_MA = as.numeric(long_ma)
      )
      
      p <- p +
        geom_line(
          data = ma_data,
          aes(x = Date, y = Short_MA),
          linewidth = 0.8
        ) +
        geom_line(
          data = ma_data,
          aes(x = Date, y = Long_MA),
          linewidth = 0.8,
          linetype = "dashed"
        )
    }
    # Add Buy and Sell annotations
    signal_data <- plot_data[
      plot_data$Signal %in% c("Buy", "Sell") &
        !is.na(plot_data$Signal),
    ]
    
    p <- p +
      geom_point(
        data = signal_data,
        aes(x = Date, y = Close, color = Signal),
        size = 2
      ) +
      labs(color = "Trading Signal")
    # Overlay RSI when selected
    if ("RSI" %in% input$technical_indicators) {
      
      rsi_values <- RSI(close_prices, n = 14)
    }
    
    print(p)
    # RSI Chart
    output$rsi_chart <- renderPlot({
      
      # Filter stock data based on selected date range
      rsi_stock_data <- stock_data[
        index(stock_data) >= input$date_range[1] &
          index(stock_data) <= input$date_range[2]
      ]
      
      # Apply selected time frame
      if (input$time_frame == "Weekly") {
        rsi_stock_data <- apply.weekly(rsi_stock_data, last)
      } else if (input$time_frame == "Monthly") {
        rsi_stock_data <- apply.monthly(rsi_stock_data, last)
      }
      
      # Calculate RSI
      rsi_values <- RSI(Cl(rsi_stock_data), n = 14)
      
      rsi_data <- data.frame(
        Date = index(rsi_stock_data),
        RSI = as.numeric(rsi_values)
      )
      
      ggplot(rsi_data, aes(x = Date, y = RSI)) +
        geom_line() +
        geom_hline(yintercept = 70, linetype = "dashed") +
        geom_hline(yintercept = 30, linetype = "dashed") +
        labs(
          title = "Relative Strength Index (RSI)",
          x = "Date",
          y = "RSI"
        ) +
        ylim(0, 100) +
        theme_minimal()
    })
  })
  # MACD Chart
  output$macd_chart <- renderPlot({
    
    # Filter stock data based on selected date range
    macd_stock_data <- stock_data[
      index(stock_data) >= input$date_range[1] &
        index(stock_data) <= input$date_range[2]
    ]
    
    # Apply selected time frame
    if (input$time_frame == "Weekly") {
      macd_stock_data <- apply.weekly(macd_stock_data, last)
    } else if (input$time_frame == "Monthly") {
      macd_stock_data <- apply.monthly(macd_stock_data, last)
    }
    
    # Calculate MACD
    macd_values <- MACD(
      Cl(macd_stock_data),
      nFast = 12,
      nSlow = 26,
      nSig = 9,
      maType = "EMA"
    )
    
    macd_data <- data.frame(
      Date = index(macd_stock_data),
      MACD = as.numeric(macd_values[, 1]),
      Signal = as.numeric(macd_values[, 2])
    )
    
    ggplot(macd_data, aes(x = Date)) +
      geom_line(aes(y = MACD, color = "MACD"), linewidth = 0.8) +
      geom_line(aes(y = Signal, color = "Signal"), linewidth = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(
        title = "Moving Average Convergence Divergence (MACD)",
        x = "Date",
        y = "MACD",
        color = "Line"
      ) +
      theme_minimal()
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)