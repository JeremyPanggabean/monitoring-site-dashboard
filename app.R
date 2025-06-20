# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(lubridate)

# Sample data (replace with your actual data source)
daily_visit <- data.frame(
  summary_date = as.Date(c("2025-06-01", "2025-06-02", "2025-06-03", "2025-06-04", 
                          "2025-06-05", "2025-06-06", "2025-06-07", "2025-06-08", 
                          "2025-06-09", "2025-06-10")),
  total_visits = c(64, 73, 65, 52, 45, 76, 49, 53, 57, 61),
  unique_users = c(26, 39, 38, 48, 37, 26, 30, 22, 29, 34),
  avg_session_duration_minutes = c(27.5, 29.7, 35.0, 42.1, 52.9, 59.8, 38.3, 57.1, 56.3, 46.4)
)

top_pages <- data.frame(
  page_url = c("/perpustakaan/buku/psikologi-modern", "/perpustakaan/jurnal/teknologi-informasi",
               "/perpustakaan/karya-ilmiah/skripsi-manajemen", "/perpustakaan/buku/ekonomi-digital",
               "/perpustakaan/jurnal/kesehatan-masyarakat", "/perpustakaan/buku/sastra-indonesia",
               "/perpustakaan/karya-ilmiah/tesis-pendidikan", "/perpustakaan/jurnal/hukum-perdata",
               "/perpustakaan/buku/desain-komunikasi-visual", "/perpustakaan/jurnal/arsitektur-nusantara"),
  view_count = c(85, 120, 67, 92, 78, 54, 40, 95, 63, 88)
)

user_actions <- data.frame(
  action_type = c("Login", "Search", "Download", "View", "Bookmark", "Share", "Comment", "Upload"),
  action_count = c(245, 423, 156, 789, 89, 34, 67, 23)
)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Perpustakaan Digital"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
      menuItem("Kunjungan Harian", tabName = "daily", icon = icon("chart-line")),
      menuItem("Halaman Populer", tabName = "pages", icon = icon("star")),
      menuItem("Aktivitas User", tabName = "actions", icon = icon("users"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("total_visits_box"),
          valueBoxOutput("unique_users_box"),
          valueBoxOutput("avg_duration_box")
        ),
        fluidRow(
          box(
            title = "Ringkasan Statistik", status = "primary", solidHeader = TRUE, width = 12,
            tableOutput("summary_stats")
          )
        )
      ),
      
      # Daily Visits Tab
      tabItem(
        tabName = "daily",
        fluidRow(
          box(
            title = "Grafik Kunjungan Harian", status = "primary", solidHeader = TRUE, width = 12,
            plotlyOutput("daily_visits_plot")
          )
        ),
        fluidRow(
          box(
            title = "Data Kunjungan Harian", status = "info", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("daily_visits_table")
          )
        )
      ),
      
      # Top Pages Tab
      tabItem(
        tabName = "pages",
        fluidRow(
          box(
            title = "Top 10 Halaman Populer", status = "success", solidHeader = TRUE, width = 6,
            plotlyOutput("top_pages_plot")
          ),
          box(
            title = "Daftar Halaman Populer", status = "success", solidHeader = TRUE, width = 6,
            DT::dataTableOutput("top_pages_table")
          )
        )
      ),
      
      # User Actions Tab
      tabItem(
        tabName = "actions",
        fluidRow(
          box(
            title = "Distribusi Aktivitas User", status = "warning", solidHeader = TRUE, width = 6,
            plotlyOutput("user_actions_plot")
          ),
          box(
            title = "Detail Aktivitas User", status = "warning", solidHeader = TRUE, width = 6,
            DT::dataTableOutput("user_actions_table")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Value Boxes
  output$total_visits_box <- renderValueBox({
    valueBox(
      value = sum(daily_visit$total_visits),
      subtitle = "Total Kunjungan",
      icon = icon("eye"),
      color = "blue"
    )
  })
  
  output$unique_users_box <- renderValueBox({
    valueBox(
      value = sum(daily_visit$unique_users),
      subtitle = "Total User Unik",
      icon = icon("users"),
      color = "green"
    )
  })
  
  output$avg_duration_box <- renderValueBox({
    valueBox(
      value = round(mean(daily_visit$avg_session_duration_minutes), 1),
      subtitle = "Rata-rata Durasi (menit)",
      icon = icon("clock"),
      color = "yellow"
    )
  })
  
  # Summary Statistics
  output$summary_stats <- renderTable({
    data.frame(
      Metrik = c("Total Kunjungan", "Total User Unik", "Rata-rata Durasi Sesi (menit)", "Total Halaman Dilihat"),
      Nilai = c(
        sum(daily_visit$total_visits),
        sum(daily_visit$unique_users),
        round(mean(daily_visit$avg_session_duration_minutes), 1),
        sum(top_pages$view_count)
      )
    )
  })
  
  # Daily Visits Plot
  output$daily_visits_plot <- renderPlotly({
    p <- ggplot(daily_visit, aes(x = summary_date)) +
      geom_line(aes(y = total_visits, color = "Total Kunjungan"), size = 1.2) +
      geom_line(aes(y = unique_users, color = "User Unik"), size = 1.2) +
      geom_point(aes(y = total_visits, color = "Total Kunjungan"), size = 3) +
      geom_point(aes(y = unique_users, color = "User Unik"), size = 3) +
      labs(title = "Trend Kunjungan Harian",
           x = "Tanggal",
           y = "Jumlah",
           color = "Metrik") +
      theme_minimal() +
      scale_color_manual(values = c("Total Kunjungan" = "#3498db", "User Unik" = "#e74c3c"))
    
    ggplotly(p)
  })
  
  # Daily Visits Table
  output$daily_visits_table <- DT::renderDataTable({
    daily_visit %>%
      mutate(summary_date = as.character(summary_date)) %>%
      DT::datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        colnames = c("Tanggal", "Total Kunjungan", "User Unik", "Rata-rata Durasi (menit)")
      )
  })
  
  # Top Pages Plot
  output$top_pages_plot <- renderPlotly({
    p <- top_pages %>%
      arrange(desc(view_count)) %>%
      mutate(page_name = gsub("/perpustakaan/", "", page_url)) %>%
      mutate(page_name = gsub("-", " ", page_name)) %>%
      head(10) %>%
      ggplot(aes(x = reorder(page_name, view_count), y = view_count)) +
      geom_col(fill = "#2ecc71", alpha = 0.8) +
      coord_flip() +
      labs(title = "Top 10 Halaman Populer",
           x = "Halaman",
           y = "Jumlah View") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Top Pages Table
  output$top_pages_table <- DT::renderDataTable({
    top_pages %>%
      arrange(desc(view_count)) %>%
      DT::datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        colnames = c("URL Halaman", "Jumlah View")
      )
  })
  
  # User Actions Plot
  output$user_actions_plot <- renderPlotly({
    p <- user_actions %>%
      ggplot(aes(x = reorder(action_type, action_count), y = action_count)) +
      geom_col(fill = "#f39c12", alpha = 0.8) +
      coord_flip() +
      labs(title = "Distribusi Aktivitas User",
           x = "Jenis Aktivitas",
           y = "Jumlah") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # User Actions Table
  output$user_actions_table <- DT::renderDataTable({
    user_actions %>%
      arrange(desc(action_count)) %>%
      DT::datatable(
        options = list(pageLength = 10),
        colnames = c("Jenis Aktivitas", "Jumlah")
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)