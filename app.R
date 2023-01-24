
#' compound_table
#'
#' @description calculates table with compound interest calculation
#'
#' @param initial_investment present value initial investment
#' @param monthly_deposits regular monthly deposits
#' @param interest_rate interest rate in %
#' @param t time in years
#' @param fees brokerage fees as % of assets
#' @param n compounding frequency. 12 for the 12 months in a year.
#' @param tax_rate tax rate in % on distributions
#' @param pre_tax pre_tax contributions, you are only taxed once you withdraw funds
#'
#'



library(dplyr)
library(tidyr)
library(highcharter)
library(shiny)



compound_table <- function(
    initial_investment = 0,
    monthly_deposits = 1000,
    interest_rate = 10,
    t = 30,
    fees = 2,
    n = 12,
    tax_rate = 25,
    pre_tax = FALSE){
  
  i <- interest_rate/100
  TR <- tax_rate/100
  fees <- fees/100
  MD <- monthly_deposits
  #MD <- ifelse(pre_tax, monthly_deposits, monthly_deposits*(1-TR))
  
  j <- i-fees
  
  Balance_vector_bf <- c()
  Balance_vector_af <- c()
  for (y in 1:t){
    Balance_partial_bf <- initial_investment*((1+i/n)^(n*y)) + MD*( ( ( (1 + i/n)^(n*y) ) - 1) / (i/n))
    Balance_vector_bf <- append(Balance_vector_bf, Balance_partial_bf) 
    
    Balance_partial_af <- initial_investment*((1+j/n)^(n*y)) + MD*( ( ( (1 + j/n)^(n*y) ) - 1) / (j/n))
    Balance_vector_af <- append(Balance_vector_af, Balance_partial_af) 
  }
  
  Balance_vector_bf <- round(Balance_vector_bf, 2)
  Balance_vector_bf <- c(initial_investment, Balance_vector_bf) #%>% tidyr::replace_na(0)
  
  Balance_vector_af <- round(Balance_vector_af, 2)
  Balance_vector_af <- c(initial_investment, Balance_vector_af) #%>% tidyr::replace_na(0)
  
  Deposit_vector <- rep(n * MD, t)
  Deposit_vector <- c(initial_investment, Deposit_vector)
  
  year_vector <- 0:t
  
  compound_df <- cbind(year_vector, Deposit_vector, Balance_vector_bf, Balance_vector_af)
  compound_df <- as.data.frame(compound_df)
  
  compound_df$total_deposits <- cumsum(compound_df$Deposit_vector)
  compound_df <- compound_df %>%
    mutate(Total_interest_bf= ifelse(is.na(Balance_vector_bf), 0, round(Balance_vector_bf-total_deposits, 2))) %>%
    mutate(Total_interest_af= ifelse(is.na(Balance_vector_af), 0, round(Balance_vector_af-total_deposits, 2)))
  
  compound_df$Balance_vector_bf <- replace_na(compound_df$Balance_vector_bf, 0)
  compound_df$Balance_vector_af <- replace_na(compound_df$Balance_vector_af, 0)
  
  # compound_df$Total_interest <- round(compound_df$Total_interest, 2)
  
  compound_df$interest_bf <- compound_df$Total_interest_bf - lag(compound_df$Total_interest_bf)
  compound_df$interest_bf <- compound_df$interest_bf %>% tidyr::replace_na(0)
  compound_df$interest_bf <- round(compound_df$interest_bf, 2)
  
  compound_df$interest_af <- compound_df$Total_interest_af - lag(compound_df$Total_interest_af)
  compound_df$interest_af <- compound_df$interest_af %>% tidyr::replace_na(0)
  compound_df$interest_af <- round(compound_df$interest_af, 2)
  
  
  
  compound_df <- compound_df %>% select(year_vector, Deposit_vector, total_deposits,  interest_bf, interest_af, Total_interest_bf, Total_interest_af, Balance_vector_bf, Balance_vector_af)
  
  names(compound_df) <- c("Year", "Deposits", "Total Deposits", "Interest before fees", "Interest after fees", "Total Interest before fees", "Total Interest after fees", "Total Balance before fees", "Total Balance after fees")
  
  compound_df <- compound_df %>%
    rowwise() %>%
    mutate("Taxes" = ifelse(pre_tax,
                            (`Total Deposits` + `Total Interest after fees`)*TR, # pay taxes on deposits and interest
                            `Total Interest after fees`*TR)) %>% # pay taxes on interest only
    mutate("Fees" = (`Total Balance before fees` - `Total Balance after fees`) *(1-TR)) %>%
    mutate("Net Gain" = round(`Total Interest after fees` - Taxes, 2)) %>%
    mutate("Net Balance" = round(`Total Balance after fees` - Taxes, 2))
  
  return(compound_df)
  
}


ui <- fluidPage(
  
  # Application title
  titlePanel("Compounding Fee Forecaster"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      numericInput("initial_investment", "Initial Investment", 0),
      numericInput("monthly_deposits", "Monthly Deposits", 1000),
      numericInput("t", "Length of Time in Years", 30),
      numericInput("interest_rate", "Interest Rate (%)", 10),
      numericInput("fees", "Brokerage Fees as % of assets", 2),
      #selectInput("compound_frequency", "Compound Frequency", c("Monthly", "Yearly")),
      numericInput("tax_rate", "Tax Rate on Distributions (%)", 25),
      checkboxInput("pre_tax", "Pre-Tax Contributions", value = TRUE, width = NULL),
      actionButton(inputId = "button_plot", label = "CALCULATE"),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      highchartOutput("plot"),
      uiOutput("balance")
    )
  )
)



server <- function(input, output, session) {
  
  
  observeEvent(input$button_plot, {
    
    
    # calculate compound table
    
    compound_table <- compound_table(
      initial_investment = input$initial_investment,
      monthly_deposits = input$monthly_deposits,
      interest_rate = input$interest_rate,
      t = input$t,
      fees = input$fees,
      n = 12, # for 12 months in the year
      tax_rate = input$tax_rate,
      pre_tax = input$pre_tax
    )
    
    #print(compound_table)
    
    
    output$plot <- renderHighchart({ 
      
      highchart() %>%
        hc_chart(type = "column") %>%
        hc_xAxis(title = list(text = "Years")) %>%
        hc_plotOptions(column = list(
          dataLabels = list(enabled = FALSE),
          stacking = "normal",
          enableMouseTracking = TRUE)
        ) %>%
        hc_series(
          list(name="Fees",data=compound_table$`Fees`, color="red"),
          list(name="Taxes",data=compound_table$`Taxes`, color="orange"),
          list(name="Net Gain",data=compound_table$`Net Gain`, color="green"),
          list(name="Deposits",data=compound_table$`Total Deposits`, color="blue")
        )
    })
    
    balance <- tail(compound_table, 1)$`Net Balance`
    fees <- tail(compound_table, 1)$`Fees`
    
    
    output$balance <- renderUI({
      div(
        h3(paste("Final Balance Without Fees:", format(balance+fees, nsmall=1, big.mark=",")), style = "color: #008000;"),
        h3(paste("Final Balance With Fees:", format(balance, nsmall=1, big.mark=","))),
        h3(paste("Value Lost to Fees:", format(fees, nsmall=1, big.mark=",")), style = "color: #C00001;"),
      )
    })
    
    
  })
  
  
  
  
}



shinyApp(ui, server)




