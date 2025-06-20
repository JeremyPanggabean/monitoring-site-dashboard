# Final Shiny Dashboard: Perpustakaan Digital
# Menggunakan PostgreSQL (tabel: daily_visit, top_pages, user_actions)

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(DBI)
library(RPostgreSQL)

# Database connection
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
    cat("Connection failed:", e$message, "\n")
    NULL
  })
}

# Load data safely
safe_query <- function(sql, default = data.frame()) {
  con <- get_db_connection()
  if (is.null(con)) return(default)
  out <- tryCatch(dbGetQuery(con, sql), error = function(e) default)
  dbDisconnect(con)
  out
}

ui <- dashboardPage(
  dashboardHeader(title = "📚 Dashboard Perpustakaan"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Halaman Populer", tabName = "pages", icon = icon("star")),
      menuItem("Aksi Pengguna", tabName = "actions", icon = icon("user"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_visits"),
                valueBoxOutput("unique_users"),
                valueBoxOutput("avg_duration")
              ),
              fluidRow(
                box(title = "Kunjungan Harian", width = 12,
                    plotlyOutput("daily_plot"))
              )
      ),
      tabItem(tabName = "pages",
              fluidRow(
                box(title = "Top Pages", width = 6,
                    DTOutput("pages_table")),
                box(title = "Grafik Halaman Terpopuler", width = 6,
                    plotlyOutput("pages_plot"))
              )
      ),
      tabItem(tabName = "actions",
              fluidRow(
                box(title = "Aktivitas Pengguna", width = 12,
                    DTOutput("actions_table"),
                    plotlyOutput("actions_plot"))
              )
      )
    )
  )
)

server <- function(input, output) {

  daily_data <- reactive({
    safe_query("SELECT * FROM daily_visit ORDER BY summary_date")
  })

  pages_data <- reactive({
    safe_query("SELECT * FROM top_pages ORDER BY view_count DESC")
  })

  actions_data <- reactive({
    safe_query("SELECT * FROM user_actions ORDER BY view_count DESC")
  })

  output$total_visits <- renderValueBox({
    total <- sum(daily_data()$total_visits, na.rm = TRUE)
    valueBox(format(total, big.mark=","), "Total Kunjungan", icon = icon("eye"), color = "blue")
  })

  output$unique_users <- renderValueBox({
    total <- sum(daily_data()$unique_users, na.rm = TRUE)
    valueBox(format(total, big.mark=","), "User Unik", icon = icon("users"), color = "green")
  })

  output$avg_duration <- renderValueBox({
    avg <- round(mean(daily_data()$avg_session_duration_minutes, na.rm = TRUE), 1)
    valueBox(paste(avg, "menit"), "Durasi Rata-rata", icon = icon("clock"), color = "yellow")
  })

  output$daily_plot <- renderPlotly({
    df <- daily_data()
    df$summary_date <- as.Date(df$summary_date)
    plot_ly(df, x = ~summary_date) %>%
      add_lines(y = ~total_visits, name = "Total Visits", line = list(color = "#3498db")) %>%
      add_lines(y = ~unique_users, name = "Unique Users", line = list(color = "#2ecc71")) %>%
      layout(title = "Trend Kunjungan", xaxis = list(title = "Tanggal"), yaxis = list(title = "Jumlah"))
  })

  output$pages_table <- renderDT({
    datatable(pages_data(), options = list(pageLength = 10))
  })

  output$pages_plot <- renderPlotly({
    df <- head(pages_data(), 5)
    colors <- c('#1abc9c', '#3498db', '#9b59b6', '#f39c12', '#e74c3c')
    plot_ly(df, labels = ~page_url, values = ~view_count, type = 'pie',
            textinfo = 'label+percent', insidetextorientation = 'radial',
            marker = list(colors = colors)) %>%
      layout(title = "Proporsi Kunjungan - Top 5 Halaman Terpopuler")
  })

  output$actions_table <- renderDT({
    datatable(actions_data(), options = list(pageLength = 10))
  })

  output$actions_plot <- renderPlotly({
  df <- tail(actions_data(), 5)
  colors <- c('#e67e22', '#2980b9', '#16a085', '#c0392b', '#8e44ad')
  plot_ly(df, labels = ~page_url, values = ~view_count, type = 'pie',
          textinfo = 'label+percent', insidetextorientation = 'radial',
          marker = list(colors = colors)) %>%
    layout(title = "Proporsi Aktivitas Pengguna - 5 Halaman Paling Jarang Dikunjungi")
  })
}

shinyApp(ui, server)
