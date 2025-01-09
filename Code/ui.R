library(shiny)
library(shinydashboard)
library(ggplot2)
library(forecast)
library(lubridate)
library(scales)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "European Budget Tracker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Budget Input", tabName = "budget", icon = icon("coins")),
      menuItem("Income History", tabName = "income_history", icon = icon("line-chart")),
      menuItem("Budget Summary", tabName = "summary", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(
        "
        /* Full-height fix */
        body, html, .wrapper, .content-wrapper {
          height: 100% !important;
          overflow: hidden;
        }
        .content {
          height: calc(100vh - 50px); /* Subtract header height */
          overflow-y: auto;
        }
        .main-sidebar {
          height: 100% !important;
        }
        .sidebar-menu {
          height: calc(100vh - 50px); /* Adjust for header */
          overflow-y: auto;
        }
        "
      ))
    ),
    tabItems(
      tabItem(
        tabName = "budget",
        fluidRow(
          box(
            title = "Income", status = "primary", solidHeader = TRUE, width = 6,
            numericInput("starting_income", "Starting Income (€)", value = 0),
            numericInput("rolling_income", "Rolling Income (€/month)", value = 0),
            numericInput("special_income", "Special Income (€)", value = 0)
          ),
          box(
            title = "Expenses", status = "danger", solidHeader = TRUE, width = 6,
            textInput("expense_name", "Expense Name"),
            numericInput("expense_amount", "Amount (€)", value = 0),
            actionButton("add_expense", "Add Expense")
          )
        ),
        fluidRow(
          box(
            title = "Expense List", status = "info", solidHeader = TRUE, width = 12,
            tableOutput("expense_table")
          )
        )
      ),
      tabItem(
        tabName = "income_history",
        fluidRow(
          box(
            title = "Past Monthly Income", status = "primary", solidHeader = TRUE, width = 12,
            dateInput("income_date", "Month", value = Sys.Date(), format = "yyyy-mm"),
            numericInput("monthly_income", "Income (€)", value = 0),
            actionButton("add_income_data", "Add Income Data"),
            tableOutput("income_data_table")
          )
        ),
        fluidRow(
          box(
            title = "Income History and Forecast", status = "info", solidHeader = TRUE, width = 12,
            plotOutput("income_plot")
          )
        )
      ),
      tabItem(
        tabName = "summary",
        fluidRow(
          valueBoxOutput("total_income"),
          valueBoxOutput("total_expenses"),
          valueBoxOutput("remaining_budget")
        ),
        fluidRow(
          box(
            title = "Budget Breakdown", status = "warning", solidHeader = TRUE, width = 12,
            plotOutput("budget_plot")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  expenses <- reactiveVal(data.frame(Name = character(), Amount = numeric(), stringsAsFactors = FALSE))
  income_data <- reactiveVal(data.frame(Date = as.Date(character()), Income = numeric(), stringsAsFactors = FALSE))
  
  observeEvent(input$add_expense, {
    new_expense <- data.frame(Name = input$expense_name, Amount = input$expense_amount)
    expenses(rbind(expenses(), new_expense))
  })
  
  observeEvent(input$add_income_data, {
    new_income_data <- data.frame(Date = input$income_date, Income = input$monthly_income)
    income_data(rbind(income_data(), new_income_data))
  })
  
  total_income <- reactive({
    input$starting_income + input$rolling_income + input$special_income + sum(income_data()$Income)
  })
  
  total_expenses <- reactive({
    sum(expenses()$Amount)
  })
  
  remaining_budget <- reactive({
    total_income() - total_expenses()
  })
  
  output$expense_table <- renderTable({
    expenses()
  })
  
  output$income_data_table <- renderTable({
    income_data()
  })
  
  output$total_income <- renderValueBox({
    valueBox(
      paste0("€", total_income()), "Total Income", icon = icon("arrow-up"), color = "green"
    )
  })
  
  output$total_expenses <- renderValueBox({
    valueBox(
      paste0("€", total_expenses()), "Total Expenses", icon = icon("arrow-down"), color = "red"
    )
  })
  
  output$remaining_budget <- renderValueBox({
    valueBox(
      paste0("€", remaining_budget()), "Remaining Budget", icon = icon("euro-sign"), color = ifelse(remaining_budget() >= 0, "blue", "orange")
    )
  })
  
  output$budget_plot <- renderPlot({
    if (nrow(expenses()) > 0) {
      df <- data.frame(Category = c(expenses()$Name, "Remaining"),
                       Amount = c(expenses()$Amount, remaining_budget()))
      
      ggplot(df, aes(x = "", y = Amount, fill = Category)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        theme_void() +
        scale_fill_brewer(palette = "Set3") +
        labs(title = "Budget Breakdown") +
        geom_text(aes(label = paste0(round(Amount/sum(Amount)*100), "%")), position = position_stack(vjust = 0.5))
    } else {
      NULL
    }
  })
  
  output$income_plot <- renderPlot({
    df <- income_data()
    if (nrow(df) > 0) {
      df$Date <- as.Date(df$Date)
      df <- df[order(df$Date),]
      
      ts_income <- ts(df$Income, frequency = 12, start = c(year(min(df$Date)), month(min(df$Date))))
      
      forecast_income <- forecast(auto.arima(ts_income), h = 6)
      
      forecast_df <- data.frame(
        Date = seq(max(df$Date) + months(1), by = "month", length.out = 6),
        Income = forecast_income$mean,
        Lower = forecast_income$lower[, "95%"],
        Upper = forecast_income$upper[, "95%"]
      )
      plot_df <- rbind(df[,c("Date","Income")], forecast_df[,c("Date","Income")])
      
      ggplot(plot_df, aes(x = Date, y = Income)) +
        geom_line() +
        geom_ribbon(data = forecast_df, aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
        scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Income History and Forecast", x = "Date", y = "Income (€)")+
        scale_y_continuous(labels = scales::dollar_format(prefix = "€"))
      
    } else {
      NULL
    }
  })
}

shinyApp(ui = ui, server = server)
