# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(bslib)
library(plotly)
library(DT)

# Load cleaned dataset
superstore <- readRDS("data/superstore_clean.rds")

# ---- Precompute summaries ----

# Category and Segment summaries for bar charts
summary_category <- superstore %>%
  group_by(Region, Year, Category) %>%
  summarise(Sales = sum(Sales), Profit = sum(Profit), Profit_Margin = mean(Profit_Margin), .groups="drop")

summary_segment <- superstore %>%
  group_by(Region, Year, Segment) %>%
  summarise(Sales = sum(Sales), Profit = sum(Profit), Profit_Margin = mean(Profit_Margin), .groups="drop")

# Monthly profit trends
profit_trends <- superstore %>%
  group_by(Region, Year, Month = month(Order_Date, label = TRUE)) %>%
  summarise(Sales = sum(Sales), Profit = sum(Profit), .groups="drop")

# ---- Precompute profit summary for losses/profits tables ----
profit_summary <- superstore %>%
  mutate(Month = month(Order_Date, label = TRUE)) %>%
  select(Region, Year, Month, Category, `Sub-Category`, Discount, Profit, Profit_Margin)

# Colour Palettes
palette <- list(
  category = c(
    "Furniture" = "#c24dff",
    "Office Supplies" = "#c24dff",
    "Technology" = "#c24dff"
  ),
  segment = c(
    "Consumer" = "#c24dff", 
    "Corporate" = "#c24dff", 
    "Home Office" = "#c24dff"
  ),
  trend = "#9e52bf"
)

# ---- UI ----
ui <- page_sidebar(
  title = "Superstore Sales Dashboard",
  theme = bs_theme(version = 5, bootswatch = "vapor"),
  
  sidebar = sidebar(
    selectInput("region", "Select Region:", choices = sort(unique(superstore$Region))),
    selectInput("year", "Select Year:", choices = sort(unique(superstore$Year)))
  ),
  
  card(
    # KPI cards
    uiOutput("summary_cards"),
    tags$hr(),
    
    # Bar Charts
    fluidRow(
      column(
        width = 6,
        div(
          style = "max-width: 600px; width: 100%; margin: 0 auto;",
          card(
            card_header("Sales by Category"),
            plotlyOutput("plot_category", height = "400px", width = "100%")
          )
        )
      ),
      column(
        width = 6,
        div(
          style = "max-width: 600px; width: 100%; margin: 0 auto;",
          card(
            card_header("Sales by Segment"),
            plotlyOutput("plot_segment", height = "400px", width = "100%")
          )
        )
      )
    ),
    
    tags$hr(),
    
    # Top Losses and Top Profits
    fluidRow(
      column(
        width=6,
        div(
          style = "width: 100%; margin: 0 auto;",
          card(
            card_header("Top Profits"),
            DT::dataTableOutput("table_profits")
          )
        )
      ),
      column(
        width=6,
        div(
          style = "width: 100%; margin: 0 auto;",
          card(
            card_header("Top Losses"),
            DT::dataTableOutput("table_losses")
          )
        )
      )
    ),
    
    tags$hr(),
    
    # Monthly Profit Trends
    fluidRow(
      column(
        width = 12,
        div(
          style = "width: 100%; margin: 0 auto;",
          card(
            full_screen = TRUE,
            card_header("Monthly Profit Trends"),
            plotlyOutput(outputId = "plot_trends", height = "300px", width = "100%")
          )
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # Filtered data
  filtered_data <- reactive({
    superstore %>% filter(Region == input$region, Year == input$year)
  })
  
  filtered_category <- reactive({
    summary_category %>% filter(Region == input$region, Year == input$year)
  })
  
  filtered_segment <- reactive({
    summary_segment %>% filter(Region == input$region, Year == input$year)
  })
  
  filtered_trends <- reactive({
    profit_trends %>% filter(Region == input$region, Year == input$year)
  })
  
  # Filter profit summary and remove Region/Year for display
  filtered_profit_summary <- reactive({
    profit_summary %>%
      filter(Region == input$region, Year == input$year) %>%
      select(-Region, -Year)
  })
  
  # ---- KPI Cards ----
  output$summary_cards <- renderUI({
    data <- filtered_data()
    total_sales <- round(sum(data$Sales), 2)
    total_profit <- round(sum(data$Profit), 2)
    avg_margin <- round(mean(data$Profit_Margin) * 100, 2)
    
    tags$div(
      style = "display: flex; gap: 20px; flex-wrap: wrap; justify-content: space-between; margin-bottom: 20px;",
      
      tags$div(
        style = "flex:1; min-width: 250px; background-color:#e3f2fd; padding:20px; border-radius:12px; box-shadow:0 2px 4px rgba(0,0,0,0.1);",
        tags$h4("Total Sales", style="color:#000"),
        tags$h2(style="color:#1976d2; font-weight:bold;", paste0("$", format(total_sales, big.mark=",", nsmall=2)))
      ),
      tags$div(
        style = "flex:1; min-width: 250px; background-color:#e8f5e9; padding:20px; border-radius:12px; box-shadow:0 2px 4px rgba(0,0,0,0.1);",
        tags$h4("Total Profit", style="color:#000"),
        tags$h2(style="color:#388e3c; font-weight:bold;", paste0("$", format(total_profit, big.mark=",", nsmall=2)))
      ),
      tags$div(
        style = "flex:1; min-width: 250px; background-color:#f3e5f5; padding:20px; border-radius:12px; box-shadow:0 2px 4px rgba(0,0,0,0.1);",
        tags$h4("Avg Profit Margin", style="color:#000"),
        tags$h2(style="color:#8e24aa; font-weight:bold;", paste0(avg_margin, "%"))
      )
    )
  })
  
  # ---- Plots ----
  output$plot_category <- renderPlotly({
    p <- ggplot(filtered_category(), aes(x = Category, y = Sales, fill = Category,
                                         text = paste0("Category: ", Category, "<br>Sales: $", 
                                                       format(round(Sales,2), big.mark=",", nsmall=2)))) +
      geom_col(width = 0.6) +
      scale_fill_manual(values = palette$category) +
      theme_minimal(base_size = 13) +
      labs(y = "Sales ($)", x = NULL) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_segment <- renderPlotly({
    p <- ggplot(filtered_segment(), aes(x = Segment, y = Sales, fill = Segment,
                                        text = paste0("Segment: ", Segment, "<br>Sales: $", 
                                                      format(round(Sales,2), big.mark=",", nsmall=2)))) +
      geom_col(width = 0.6) +
      scale_fill_manual(values = palette$segment) +
      theme_minimal(base_size = 13) +
      labs(y = "Sales ($)", x = NULL) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_trends <- renderPlotly({
    p <- ggplot(filtered_trends(), aes(x = Month, y = Profit, group = 1,
                                       text = paste0("Month: ", Month, "<br>Profit: $", 
                                                     format(round(Profit,2), big.mark=",", nsmall=2)))) +
      geom_line(color = palette$trend, size = 1.2) +
      geom_point(color = palette$trend, size = 2) +
      theme_minimal(base_size = 13) +
      labs(y = "Profit ($)", x = "Month") +
      theme(plot.title = element_text(face = "bold"))
    
    ggplotly(p, tooltip = "text")
  })
  
  # ---- Top Losses and Profits Tables ----
  
  output$table_profits <- DT::renderDataTable({
    filtered_profit_summary() %>%
      arrange(desc(Profit)) %>% head(100)
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  output$table_losses <- DT::renderDataTable({
    filtered_profit_summary() %>%
      arrange(Profit) %>% head(100)
  }, options = list(pageLength = 10, scrollX = TRUE))
}

# ---- Run App ----
shinyApp(ui = ui, server = server)
