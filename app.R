# Load Package
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)

# Load Data
data <- data.frame(
  Day = 1:10,
  Left_Sidebar = c(2.5, 2.7, 2.8, 2.6, 3.0, 2.4, 2.9, 2.5, 2.6, 2.7),
  Center_Page = c(3.8, 3.5, 4.0, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9),
  Right_Sidebar = c(3.1, 2.9, 3.0, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5))

# UI
ui <- dashboardPage(
  dashboardHeader(title = "ANOVA DASHBOARD"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("LOAD DATA", tabName = "load_data"),
      menuItem("STATISTICAL ANALYSIS", tabName = "stat_analysis"),
      menuItem("VISUALIZATIONS", tabName = "visualizations",
               menuSubItem("Box Plot", tabName = "box_plot"),
               menuSubItem("Bar Plot", tabName = "bar_plot"),
               menuSubItem("Line Chart", tabName = "line_chart")
      )
    )
  ),
  dashboardBody(
    tabItems(
      # Tab load data
      tabItem(
        tabName = "load_data",
        fluidPage(
          titlePanel("Input Data"),
          sidebarLayout(
            sidebarPanel(
              textInput("day_input", "Day:", ""),
              numericInput("left_sidebar_input", "Left Sidebar CTR:", value = 0),
              numericInput("center_page_input", "Center Page CTR:", value = 0),
              numericInput("right_sidebar_input", "Right Sidebar CTR:", value = 0),
              actionButton("add_data_button", "Add Data", class = "btn-primary")
            ),
            mainPanel(
              DTOutput("data_table")
            )
          )
        )
      ),
      # Tab statistical analysis
      tabItem(
        tabName = "stat_analysis",
        fluidPage(
          titlePanel("One-Way ANOVA"),
          mainPanel(
            verbatimTextOutput("anova_result")
          )
        )
      ),
      # Tab visualizations - Box Plot
      tabItem(
        tabName = "box_plot",
        fluidPage(
          titlePanel("Box Plot Visualization"),
          mainPanel(
            plotlyOutput("boxplot")
          )
        )
      ),
      # Tab visualizations - Bar Plot
      tabItem(
        tabName = "bar_plot",
        fluidPage(
          titlePanel("Bar Plot Visualization"),
          mainPanel(
            plotlyOutput("barplot")
          )
        )
      ),
      # Tab visualizations - Line Chart
      tabItem(
        tabName = "line_chart",
        fluidPage(
          titlePanel("Line Chart Visualization"),
          mainPanel(
            plotlyOutput("linechart")
          )
        )
      )
    )
  )
)

# SERVER
server <- function(input, output) {
  # Reactive value to store data
  data_all <- reactiveValues(data = data)
  
  # Add data button
  observeEvent(input$add_data_button, {
    new_row <- data.frame(
      Day = input$day_input,
      Left_Sidebar = input$left_sidebar_input,
      Center_Page = input$center_page_input,
      Right_Sidebar = input$right_sidebar_input
    )
    data_all$data <- rbind(data_all$data, new_row)
  })
  
  # Render data table
  output$data_table <- renderDT({
    datatable(data_all$data, options = list(pageLength = 5))
  })
  
  # One-way ANOVA analysis
  output$anova_result <- renderPrint({
    if (nrow(data_all$data) >= 2) {
      anova_result <- aov(Day ~ Left_Sidebar + Center_Page + Right_Sidebar, data = data_all$data)
      summary(anova_result)
    } else {
      "Insufficient data for analysis"
    }
  })
  
  # Box Plot with Plotly
  output$boxplot <- renderPlotly({
    box_data <- data_all$data
    
    # Melting data untuk membuat variabel dalam satu kolom
    box_data_long <- tidyr::gather(box_data, key = "variable", value = "value", -Day)
    
    # Menggambar boxplot untuk setiap variabel dengan plotly
    p <- plot_ly(box_data_long, x = ~variable, y = ~value, type = "box", color = ~variable) %>%
      layout(title = "Box Plot", xaxis = list(title = "Variable"), yaxis = list(title = "Value"))
    
    p # Menampilkan plot
  })
  
  # Bar Plot with Plotly
  output$barplot <- renderPlotly({
    bar_data <- data_all$data
    bar_data_long <- tidyr::gather(bar_data, key = "variable", value = "value", -Day)
    
    # Membuat bar plot yang lebih informatif
    p <- plot_ly(bar_data_long, x = ~variable, y = ~value, type = "bar", color = ~variable,
                 text = ~paste("Value: ", value), 
                 hoverinfo = "text") %>%
      layout(title = "Bar Plot", xaxis = list(title = "Variable"), yaxis = list(title = "Value"))
    
    p # Menampilkan plot
  })
  
  # Line Chart with Plotly
  output$linechart <- renderPlotly({
    line_data <- data_all$data
    
    # Melting data untuk membuat variabel dalam satu kolom
    line_data_long <- tidyr::gather(line_data, key = "variable", value = "value", -Day)
    
    # Menggambar line chart untuk setiap variabel dengan plotly
    p <- plot_ly(line_data_long, x = ~Day, y = ~value, type = "scatter", mode = "lines+markers", color = ~variable) %>%
      layout(title = "Line Chart", xaxis = list(title = "Day"), yaxis = list(title = "Value"))
    
    p # Menampilkan plot
  })
}

# Run the app
shinyApp(ui = ui, server = server)