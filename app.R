# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(RPostgreSQL)

# Simple database connection function
get_db_connection <- function() {
  tryCatch({
    dbConnect(
      PostgreSQL(),
      host = "yamabiko.proxy.rlwy.net",
      port = 25400,
      dbname = "railway",
      user = "postgres",
      password = "KtrbXrizoOvxDGjqYpkYKJTVXmDHuKZo"
    )
  }, error = function(e) {
    cat("Database connection failed:", e$message, "\n")
    NULL
  })
}

# Simple data loading with fallback
load_data <- function(query, default_data) {
  con <- get_db_connection()
  if (is.null(con)) {
    return(default_data)
  }
  
  result <- tryCatch({
    dbGetQuery(con, query)
  }, error = function(e) {
    cat("Query failed:", e$message, "\n")
    default_data
  })
  
  dbDisconnect(con)
  
  if (nrow(result) == 0) {
    return(default_data)
  }
  return(result)
}

# Default sample data
default_daily <- data.frame(
  summary_date = as.Date(c("2025-06-15", "2025-06-16", "2025-06-17", "2025-06-18", "2025-06-19", "2025-06-20")),
  total_visits = c(45, 62, 58, 71, 83, 76),
  unique_users = c(28, 41, 35, 48, 52, 44),
  avg_session_duration_minutes = c(32.5, 28.3, 41.2, 35.8, 29.1, 38.7)
)

default_pages <- data.frame(
  page_url = c("Jurnal Teknologi", "Buku Psikologi", "Skripsi Manajemen", "Buku Ekonomi", "Jurnal Kesehatan"),
  view_count = c(156, 134, 98, 87, 76)
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Perpustakaan Digital"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Halaman Populer", tabName = "pages", icon = icon("star"))
    )
  ),
  dashboardBody(
    tabItems(
      # Main dashboard
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("total_visits"),
          valueBoxOutput("unique_users"),
          valueBoxOutput("avg_duration")
        ),
        fluidRow(
          box(
            title = "Kunjungan Harian", status = "primary", solidHeader = TRUE, width = 12,
            plotlyOutput("visits_chart")
          )
        )
      ),
      # Top pages
      tabItem(
        tabName = "pages",
        fluidRow(
          box(
            title = "Halaman Populer", status = "success", solidHeader = TRUE, width = 6,
            plotlyOutput("pages_chart")
          ),
          box(
            title = "Data Halaman", status = "info", solidHeader = TRUE, width = 6,
            DT::dataTableOutput("pages_table")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Load data
  daily_data <- load_data(
    "SELECT summary_date, total_visits, unique_users, avg_session_duration_minutes FROM daily_visit ORDER BY summary_date DESC LIMIT 10",
    default_daily
  )
  
  pages_data <- load_data(
    "SELECT page_url, view_count FROM top_pages ORDER BY view_count DESC LIMIT 10",
    default_pages
  )
  
  # Ensure date format
  if ("summary_date" %in% names(daily_data)) {
    daily_data$summary_date <- as.Date(daily_data$summary_date)
  }
  
  # Value boxes
  output$total_visits <- renderValueBox({
    valueBox(
      value = sum(daily_data$total_visits, na.rm = TRUE),
      subtitle = "Total Kunjungan",
      icon = icon("eye"),
      color = "blue"
    )
  })
  
  output$unique_users <- renderValueBox({
    valueBox(
      value = sum(daily_data$unique_users, na.rm = TRUE),
      subtitle = "User Unik",
      icon = icon("users"),
      color = "green"
    )
  })
  
  output$avg_duration <- renderValueBox({
    valueBox(
      value = round(mean(daily_data$avg_session_duration_minutes, na.rm = TRUE), 1),
      subtitle = "Rata-rata Durasi (menit)",
      icon = icon("clock"),
      color = "yellow"
    )
  })
  
  # Visits chart
  output$visits_chart <- renderPlotly({
    p <- ggplot(daily_data, aes(x = summary_date)) +
      geom_line(aes(y = total_visits, color = "Total Visits"), size = 1.2) +
      geom_point(aes(y = total_visits, color = "Total Visits"), size = 3) +
      labs(title = "Trend Kunjungan Harian", x = "Tanggal", y = "Jumlah", color = "Metrik") +
      theme_minimal() +
      scale_color_manual(values = c("Total Visits" = "#3498db"))
    
    ggplotly(p)
  })
  
  # Pages chart
  output$pages_chart <- renderPlotly({
    p <- pages_data %>%
      head(5) %>%
      ggplot(aes(x = reorder(page_url, view_count), y = view_count)) +
      geom_col(fill = "#2ecc71", alpha = 0.8) +
      coord_flip() +
      labs(title = "Top 5 Halaman", x = "Halaman", y = "Views") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Pages table
  output$pages_table <- DT::renderDataTable({
    pages_data %>%
      DT::datatable(
        options = list(pageLength = 8),
        colnames = c("Halaman", "Views")
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
