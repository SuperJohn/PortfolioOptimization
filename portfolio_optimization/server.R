#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
## Load Required Libraries & Set Paths
library(shiny)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(timetk)
library(DT)
library(shinycssloaders)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(scales)
#setwd("~/Documents/GitHub/rob_gordon_2018")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  ### Stock Chooser v2 ###
  #output$out6 <- renderPrint(input$in6)
  
  ### Stock Prices: Historical ###
  # randomVals <- eventReactive(input$go, {
  #   #runif(input$go)
  # })
  
  stock_data <- eventReactive(input$go, {
    tq_get(input$in6 # selected stocks
       , get  = "stock.prices"
       , from =  Sys.Date() - months(12)
       , to = Sys.Date() - 1)
      })
  
  ### Stock Prices: Recent ###
  stock_data_recent <- eventReactive(input$go, {
    stock_data() %>% 
      filter(between(date, Sys.Date() - months(input$momLookback), Sys.Date() - 1))
  })
  
  ### Get Stock Returns ###
  stock_returns <- reactive({
    stock_data_recent() %>%
      group_by(symbol) %>%
      tq_transmute(select = adjusted, 
                   mutate_fun = periodReturn, 
                   period     = "monthly", 
                   col_rename = "returns") %>% 
      mutate(returns = round(returns, 6)) %>% 
      arrange(desc(symbol), desc(date))})
  
  ### Historical Stock Returns - winners only ###
  hist_data <- eventReactive(input$optimize, {
    tq_get(winners()$symbol
                 , get  = "stock.prices"
                 , from = Sys.Date() - months(36)
                 , to = Sys.Date()-1) }) 
  
  hist_returns <- eventReactive(input$optimize, {
      hist_data() %>% 
      group_by(symbol) %>%
      tq_transmute(select = adjusted, 
                   mutate_fun = periodReturn, 
                   period = "daily",
                   type = "arithmetic",
                   col_rename = "returns") })
  
    hist_returns_ts <- eventReactive(input$optimize, {
      hist_returns() %>% 
      spread(key = symbol, value = returns) %>%
      tk_xts(date_var = date) %>% 
      na.omit()
    })
 
  ### Stock Price Table ###
  output$stock_data <- DT::renderDataTable(
    DT::datatable(stock_data_recent(), caption = 'Stock Data: past 12 months',
                  class="compact", options = list(scrollX = TRUE, dom = "tp")))
  
  ### Stock Price Plot ###
  output$stockPlot <- renderPlot({
      # filter(symbol %in% c("SPY", "GLD", "QQQ", "DBC")) %>%
      ggplot(stock_data_recent(), aes(x = date, y = adjusted, color = symbol)) +
      geom_line(size = 1) +
      geom_bbands(aes(high = high, low = low, close = close), ma_fun = SMA, n = 5,show.legend=TRUE) +
      labs(title = "Daily Stock Prices",
           x = "", y = "Adjusted Prices", color = "") +
      facet_wrap(~ symbol, ncol = 3, scales = "free") +
      scale_y_continuous(labels = scales::dollar) +
      theme_tq() +
      scale_color_tq()})
  
  ### STOCK RETURNS PLOT ### 
  output$returnsPlot <- renderPlot({
  stock_returns() %>% 
    ggplot(aes(x = date, y = returns, fill = symbol)) +
    geom_line(aes(color=symbol)) +
      geom_smooth(aes(color=symbol),method = 'loess' , formula = y ~ x, se=FALSE) +
    geom_hline(yintercept = 0, color = palette_light()[[1]]) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = " Monthly Returns",
         subtitle = "stocks limited to fit",
         y = "Monthly Returns", x = "") + 
    facet_wrap(~ symbol, ncol = 2,scales = "free") +
    theme_tq() + 
    scale_fill_tq()})

### GROWTH DATA ###
  growth <- reactive({
  init.investment <- 1000
  stock_returns() %>%
    arrange(date) %>%
    mutate(final_value = init.investment * cumprod(1 + returns)) %>%
    arrange(desc(final_value))})

  ### Winners Data ###
  winners <- reactive({
    growth() %>% ungroup() %>% 
      filter(date == max(date)) %>% mutate(rank = row_number()) %>% 
      top_n(5,final_value) %>% select(rank, symbol, final_value) %>% 
      arrange(desc(final_value))
  })
  
  ### GROWTH TABLE ###
  # output$growth_data <- DT::renderDataTable(
  #   DT::datatable(growth() %>% filter(date == max(date)) %>% select(-date), 
  #                 caption = 'Growth of $1k',
  #                 class="compact", options = list(scrollX = TRUE, dom = "tp")))
  
### GROWTH CHART ###
  output$growthPlot <- renderPlot({
    growth() %>% 
      ggplot(aes(x = date, y = final_value, color = symbol)) +
      geom_line() +
      # geom_smooth(method = "loess") +
      labs(title = "Individual Portfolio: Comparing the Growth of $1K",
           subtitle = "Quickly visualize performance",
           x = "", y = "Investment Value") +
      theme_tq() + theme(legend.position = "right") +
      scale_y_continuous(labels = scales::dollar) + 
      geom_text(aes(label = round(final_value,0), vjust = 3, size = 10))})
  
  ### Winners Chart ###
  output$winnersPlot <- renderPlot({
    winners() %>% 
      ggplot(aes(x = symbol, y = final_value, fill = symbol)) +
      geom_bar(stat = "identity") +
      labs(title = "Individual Portfolio: Comparing the Growth of $1K",
           subtitle = "Quickly visualize performance",
           x = "", y = "Investment Value") +
      theme_tq() + theme(legend.position = "right") +
      scale_y_continuous(labels = scales::dollar) + 
      geom_text(aes(label = round(final_value,2), vjust = 3, size = 10))
  })
  
  ### Optimize Portfolio ###
  portf_minvar <- eventReactive(input$optimize, {
      #' Set up initial portfolio with basic constraints.
      portf_minvar <- portfolio.spec(assets = colnames(hist_returns_ts()))
      portf_minvar <- add.constraint(portfolio=portf_minvar, type="full_investment")
      portf_minvar <- add.constraint(portfolio=portf_minvar, type="box", min_sum=0.99, max_sum=1.01)
      
      # Add objective to minimize variance
      portf_minvar <- add.objective(portfolio=portf_minvar, type="risk", name="var")
      # Dummy objectives for plotting and/or further analysis
      portf_minvar <- add.objective(portf_minvar, type="return", name="mean", multiplier=0)
      portf_minvar <- add.objective(portf_minvar, type="risk", name="ES", multiplier=0)
      portf_minvar <- add.objective(portf_minvar, type="risk", name="StdDev", multiplier=0)
      portf_minvar
      })
  # Generate Min Variance Optimal Portfolio Weights via ROI
  opt_gmv <- eventReactive(input$optimize, {
      optimize.portfolio(R=hist_returns_ts(),
                                portfolio=portf_minvar(),
                                optimize_method="ROI",
                                trace=TRUE,
                                search_size=5000)
      })
  
  output$opt_min_var <- renderPrint(round(opt_gmv()$objective_measures$ES,5))
  output$opt_print <- renderPrint(opt_gmv())
  
  opt_weights <- reactive({
    as.tibble(as.list(extractWeights(opt_gmv())))
  })
    
  output$opt_weights <- renderTable(opt_weights())
  
  weights_tbl <- reactive({
    opt_weights() %>%
      gather("symbol", "weight", 1:length(extractWeights(opt_gmv()))) %>%
      mutate(weight = round(weight, 5)) %>%
      arrange(desc(weight))
  })
  
  output$weights_pie <- renderPlot({
    ggplot(weights_tbl(), aes(x="", y=weight, fill=symbol)) +
      geom_bar(stat = "identity") + 
      coord_polar("y", start=0) +
      scale_fill_brewer("Blues") + #blank_theme() +
      theme(axis.text.x=element_blank()) +
      geom_text(aes(y = weight/3 + c(0, cumsum(weight)[-length(weight)]), 
                     label = percent(weight), size=5))
  })
  
  output$weights_tbl <- DT::renderDataTable(
    DT::datatable(weights_tbl(), caption = 'Stock Data: past 12 months', 
                  class="compact", options = list(scrollX = TRUE, dom = "tp")) )
  
   ef <- reactive({
     prt_ef <- create.EfficientFrontier(R=12*hist_returns_ts(), portfolio=portf_minvar(), type="mean-StdDev", 
                             match.col = "StdDev", momentFUN="annualized.moments", scale=12)
    chart.EfficientFrontier(prt_ef, 
                            match.col="StdDev", chart.assets = TRUE, n.portfolios = 1000,
                            labels.assets = TRUE, 
                            xlim=range(prt_ef$frontier[,2])*c(1, 1.5), 
                            ylim=range(prt_ef$frontier[,1])*c(.80, 1.05), 
                            main = "Efficient Frontier")
    points(with(annualized.moments(12*hist_returns_ts(), scale=12), cbind(sqrt(diag(sigma)), mu)), pch=19 ) 
    text(with(annualized.moments(12*hist_returns_ts(), scale=12), cbind(sqrt(diag(sigma)), mu)), 
         labels=colnames(hist_returns_ts()), cex=.8, pos=4)
  })
  
   ### Chart Efficient Frontier ### 
  output$ef <- renderPlot(ef())
  output$rr <- renderPlot(chart.RiskReward(opt_gmv(), rp=TRUE, risk.col = "ES", chart.assets=TRUE))
  
### Chart Portfolio Growth ###
  output$port_growth <- renderPlot({
    principal <- 10000
    portfolio_growth_monthly <- hist_returns() %>%
      # group_by(symbol) %>%
      # tq_transmute(select = adjusted, 
      #              mutate_fun = periodReturn, 
      #              period = "monthly",
      #              type = "arithmetic",
      #              col_rename = "returns") %>%
      tq_portfolio(assets_col   = symbol,
                   returns_col  = returns,
                   weights      = extractWeights(opt_gmv()),
                   col_rename   = "investment.growth",
                   wealth.index = TRUE) %>%
      mutate(investment.growth = investment.growth * principal)

  portfolio_growth_monthly %>%
    ggplot(aes(x = date, y = investment.growth)) +
    geom_line(size = 2, color = palette_light()[[1]]) +
    labs(title = "Optimized Portfolio Growth",
         subtitle = "",
         caption = "Growth of $10k",
         x = "", y = "Portfolio Value") +
    geom_smooth(method = "loess") +
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar) #+ 
    #geom_text(aes(label = scales::dollar(round(investment.growth,0)), vjust = 3, size = 10))
  })
  
  ### RUN BACKTEST ###
  bt_gmv <- eventReactive(input$backtest, {
    optimize.portfolio.rebalancing(R=hist_returns_ts(), portfolio=portf_minvar(),
       optimize_method="ROI", trace = TRUE,
       rebalance_on="months", training_period=input$training_period, 
       rolling_window = input$rolling_window ) })
  
  ### Print Annualized Returns from Backtest ###
  output$bt_yr_ret <- renderPrint(summary(bt_gmv())$annualized_returns[[1]])
  ### Print Annualized Variance (StdDev) from Backtest ###
  output$bt_yr_StdDev <- renderPrint(summary(bt_gmv())$annualized_StdDev[[1]])
  
  ### Chart Backtest Performance Summary ###
  output$bt_perf <- renderPlot({
    charts.PerformanceSummary(
      cbind( summary(bt_gmv())$portfolio_returns, opt_gmv()$R ), 
      main="Optimization Performance", method = "StdDev", wealth.index = TRUE, begin = "axis"
      ) })
  
  ### CHART BACKTEST WEIGHTS ###
  output$bt_weights <- renderPlot({
     chart.Weights(bt_gmv(), ylim=c(0,1), las=1, main="Optimal Weights", cex.axis = 2 ,cex.legend = 2, col=rainbow12equal) # , legend.loc = "topright"
  })
  
  #######################
  ### FILE DOWNLOADER ###
  #######################
  # Downloadable csv 
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste("stock_data", Sys.Date(), ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(stock_data(), file, row.names = FALSE)
  #   })
  
})
