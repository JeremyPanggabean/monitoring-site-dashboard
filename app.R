# Load required libraries (essential only)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(RPostgreSQL)
library(ggplot2)

# Enhanced database connection function
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

# Enhanced data loading with retry mechanism
load_data <- function(query, default_data, max_retries = 3) {
  for (i in 1:max_retries) {
    con <- get_db_connection()
    if (is.null(con)) {
      if (i == max_retries) return(default_data)
      Sys.sleep(1)
      next
    }
    
    result <- tryCatch({
      dbGetQuery(con, query)
    }, error = function(e) {
      cat("Query failed (attempt", i, "):", e$message, "\n")
      default_data
    })
    
    dbDisconnect(con)
    
    if (nrow(result) > 0) {
      return(result)
    }
    
    if (i < max_retries) Sys.sleep(1)
  }
  
  return(default_data)
}

# Enhanced default sample data
default_daily <- data.frame(
  summary_date = as.Date(seq(from = Sys.Date() - 14, to = Sys.Date(), by = "day")),
  total_visits = c(45, 62, 58, 71, 83, 76, 89, 95, 102, 87, 93, 108, 115, 122, 98),
  unique_users = c(28, 41, 35, 48, 52, 44, 58, 61, 69, 55, 59, 72, 78, 81, 64),
  avg_session_duration_minutes = c(32.5, 28.3, 41.2, 35.8, 29.1, 38.7, 42.1, 36.9, 33.4, 40.2, 37.8, 34.6, 39.1, 35.3, 41.8),
  page_views = c(156, 198, 167, 223, 245, 201, 267, 289, 312, 256, 278, 334, 356, 378, 298),
  bounce_rate = c(0.32, 0.28, 0.35, 0.25, 0.22, 0.29, 0.20, 0.18, 0.15, 0.24, 0.21, 0.17, 0.14, 0.12, 0.19)
)

# Generate 60 sample pages
generate_sample_pages <- function(n = 60) {
  categories <- c("Teknologi", "Kesehatan", "Ekonomi", "Hukum", "Pendidikan", "Sains", "Sosial", "Budaya")
  subjects <- c("jurnal", "buku", "skripsi", "artikel", "penelitian")
  topics <- c("informatika", "komputer", "manajemen", "akuntansi", "biologi", "fisika", "kimia", 
              "matematika", "psikologi", "sosiologi", "sejarah", "bahasa", "kesehatan", "nutrisi",
              "administrasi", "bisnis", "ekonomi", "keuangan", "pemasaran", "teknologi")
  
  pages <- data.frame(
    page_url = character(n),
    view_count = integer(n),
    category = character(n),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:n) {
    subject <- sample(subjects, 1)
    topic <- sample(topics, 1)
    category <- sample(categories, 1)
    
    pages$page_url[i] <- paste0("/perpustakaan/", subject, "/", topic, "-", sample(1:999, 1))
    pages$view_count[i] <- max(1, round(rnorm(1, mean = 80, sd = 30)))
    pages$category[i] <- category
  }
  
  # Sort by view_count descending
  pages <- pages[order(pages$view_count, decreasing = TRUE), ]
  return(pages)
}

default_pages <- generate_sample_pages(60)

# Enhanced UI
ui <- dashboardPage(
  dashboardHeader(
    title = "📚 Dashboard Perpustakaan Digital",
    titleWidth = 320
  ),
  
  dashboardSidebar(
    width = 320,
    sidebarMenu(
      id = "sidebar",
      menuItem("🏠 Dashboard Utama", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("⭐ Halaman Populer", tabName = "pages", icon = icon("star")),
      menuItem("📊 Analisis Kategori", tabName = "categories", icon = icon("chart-pie")),
      menuItem("📈 Trend Analysis", tabName = "trends", icon = icon("line-chart"))
    )
  ),
  
  dashboardBody(
    # Custom CSS for better styling
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f7fc;
        }
        .box {
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          border-top: 3px solid #3498db;
        }
        .value-box {
          border-radius: 8px;
        }
        .value-box .value-box-icon {
          border-radius: 8px 0 0 8px;
        }
        .main-header .navbar {
          background-color: #34495e !important;
        }
        .main-sidebar {
          background-color: #2c3e50 !important;
        }
      "))
    ),
    
    tabItems(
      # Enhanced main dashboard
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("total_visits", width = 3),
          valueBoxOutput("unique_users", width = 3),
          valueBoxOutput("avg_duration", width = 3),
          valueBoxOutput("total_pages", width = 3)
        ),
        
        fluidRow(
          box(
            title = "📈 Trend Kunjungan Harian", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 8,
            plotlyOutput("visits_chart", height = "400px")
          ),
          box(
            title = "📊 Statistik Ringkas",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            tableOutput("summary_stats"),
            hr(),
            div(
              style = "text-align: center;",
              actionButton("refresh_data", "🔄 Refresh Data", 
                         class = "btn btn-primary"),
              br(), br(),
              textOutput("last_update")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "🎯 Bounce Rate Trend",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("bounce_chart")
          ),
          box(
            title = "📊 Page Views vs Visitors",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("correlation_chart")
          )
        )
      ),
      
      # Enhanced popular pages with filtering
      tabItem(
        tabName = "pages",
        fluidRow(
          box(
            title = "🔍 Filter & Controls",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(4,
                sliderInput("top_n", "Jumlah halaman teratas:",
                           min = 5, max = 60, value = 15, step = 5)
              ),
              column(4,
                selectInput("category_filter", "Filter Kategori:",
                           choices = c("Semua" = "all"), 
                           selected = "all")
              ),
              column(4,
                br(),
                downloadButton("download_report", "📥 Download Laporan",
                             class = "btn btn-info")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "⭐ Halaman Populer (Chart)", 
            status = "success", 
            solidHeader = TRUE, 
            width = 6,
            plotlyOutput("pages_chart", height = "500px")
          ),
          box(
            title = "📋 Data Detail Halaman", 
            status = "info", 
            solidHeader = TRUE, 
            width = 6,
            DT::dataTableOutput("pages_table")
          )
        )
      ),
      
      # Category analysis tab
      tabItem(
        tabName = "categories",
        fluidRow(
          box(
            title = "📊 Distribusi per Kategori",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("category_pie")
          ),
          box(
            title = "📈 Performa Kategori",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("category_bar")
          )
        ),
        
        fluidRow(
          box(
            title = "📋 Tabel Kategori Detail",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("category_table")
          )
        )
      ),
      
      # Trends analysis tab
      tabItem(
        tabName = "trends",
        fluidRow(
          box(
            title = "📈 Multi-Metric Trends",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("multi_trend_chart", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "📊 Weekly Growth Rate",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("growth_chart")
          ),
          box(
            title = "📈 Performance Metrics",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("performance_chart")
          )
        )
      )
    )
  )
)

# Enhanced Server
server <- function(input, output, session) {
  
  # Reactive data loading
  daily_data <- reactive({
    load_data(
      "SELECT summary_date, total_visits, unique_users, avg_session_duration_minutes, page_views, bounce_rate FROM daily_visit ORDER BY summary_date DESC LIMIT 30",
      default_daily
    )
  })
  
  pages_data <- reactive({
    load_data(
      "SELECT page_url, view_count, category FROM top_pages ORDER BY view_count DESC LIMIT 60",
      default_pages
    )
  })
  
  # Update category choices
  observe({
    pages <- pages_data()
    categories <- c("Semua" = "all", sort(unique(pages$category)))
    updateSelectInput(session, "category_filter", choices = categories)
  })
  
  # Filtered pages data
  filtered_pages <- reactive({
    pages <- pages_data()
    if (input$category_filter != "all") {
      pages <- pages[pages$category == input$category_filter, ]
    }
    head(pages, input$top_n)
  })
  
  # Enhanced value boxes
  output$total_visits <- renderValueBox({
    daily <- daily_data()
    valueBox(
      value = format(sum(daily$total_visits, na.rm = TRUE), big.mark = ","),
      subtitle = "Total Kunjungan",
      icon = icon("eye"),
      color = "blue"
    )
  })
  
  output$unique_users <- renderValueBox({
    daily <- daily_data()
    valueBox(
      value = format(sum(daily$unique_users, na.rm = TRUE), big.mark = ","),
      subtitle = "User Unik",
      icon = icon("users"),
      color = "green"
    )
  })
  
  output$avg_duration <- renderValueBox({
    daily <- daily_data()
    valueBox(
      value = paste0(round(mean(daily$avg_session_duration_minutes, na.rm = TRUE), 1), " min"),
      subtitle = "Rata-rata Durasi",
      icon = icon("clock"),
      color = "yellow"
    )
  })
  
  output$total_pages <- renderValueBox({
    pages <- pages_data()
    valueBox(
      value = format(sum(pages$view_count, na.rm = TRUE), big.mark = ","),
      subtitle = "Total Page Views",
      icon = icon("file-alt"),
      color = "red"
    )
  })
  
  # Enhanced visits chart
  output$visits_chart <- renderPlotly({
    daily <- daily_data()
    if ("summary_date" %in% names(daily)) {
      daily$summary_date <- as.Date(daily$summary_date)
    }
    
    p <- ggplot(daily, aes(x = summary_date)) +
      geom_line(aes(y = total_visits, color = "Total Visits"), size = 1.2) +
      geom_point(aes(y = total_visits, color = "Total Visits"), size = 3) +
      geom_line(aes(y = unique_users, color = "Unique Users"), size = 1.2) +
      geom_point(aes(y = unique_users, color = "Unique Users"), size = 3) +
      labs(title = "", x = "Tanggal", y = "Jumlah", color = "Metrik") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_color_manual(values = c("Total Visits" = "#3498db", "Unique Users" = "#27ae60"))
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Bounce rate chart
  output$bounce_chart <- renderPlotly({
    daily <- daily_data()
    if ("summary_date" %in% names(daily)) {
      daily$summary_date <- as.Date(daily$summary_date)
    }
    
    p <- ggplot(daily, aes(x = summary_date, y = bounce_rate * 100)) +
      geom_line(color = "#e74c3c", size = 1.2) +
      geom_point(color = "#e74c3c", size = 3) +
      labs(title = "", x = "Tanggal", y = "Bounce Rate (%)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Correlation chart
  output$correlation_chart <- renderPlotly({
    daily <- daily_data()
    
    p <- ggplot(daily, aes(x = total_visits, y = unique_users)) +
      geom_point(aes(size = avg_session_duration_minutes), alpha = 0.7, color = "#3498db") +
      geom_smooth(method = "lm", se = FALSE, color = "#e74c3c") +
      labs(title = "", x = "Total Visits", y = "Unique Users", size = "Avg Duration") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Enhanced pages chart
  output$pages_chart <- renderPlotly({
    pages <- filtered_pages()
    
    # Truncate long URLs for better display
    pages$short_url <- sapply(pages$page_url, function(x) {
      if (nchar(x) > 35) {
        paste0("...", substr(x, nchar(x)-32, nchar(x)))
      } else {
        x
      }
    })
    
    p <- pages %>%
      head(10) %>%  # Limit to top 10 for chart readability
      ggplot(aes(x = reorder(short_url, view_count), y = view_count, fill = category)) +
      geom_col(alpha = 0.8) +
      coord_flip() +
      labs(title = "", x = "Halaman", y = "Views", fill = "Kategori") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_fill_brewer(type = "qual", palette = "Set3")
    
    ggplotly(p, tooltip = c("y", "fill"))
  })
  
  # Enhanced pages table
  output$pages_table <- DT::renderDataTable({
    pages <- filtered_pages()
    pages %>%
      select(page_url, view_count, category) %>%
      DT::datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          searching = TRUE
        ),
        colnames = c("Halaman", "Views", "Kategori")
      )
  })
  
  # Summary stats table
  output$summary_stats <- renderTable({
    daily <- daily_data()
    data.frame(
      Metric = c("Avg Daily Visits", "Peak Day Visits", "Avg Session (min)", "Total Days"),
      Value = c(
        round(mean(daily$total_visits, na.rm = TRUE), 0),
        max(daily$total_visits, na.rm = TRUE),
        round(mean(daily$avg_session_duration_minutes, na.rm = TRUE), 1),
        nrow(daily)
      )
    )
  }, striped = TRUE, bordered = TRUE)
  
  # Category analysis
  output$category_pie <- renderPlotly({
    pages <- pages_data()
    category_summary <- pages %>%
      group_by(category) %>%
      summarise(total_views = sum(view_count), .groups = 'drop')
    
    plot_ly(category_summary, 
            labels = ~category, 
            values = ~total_views, 
            type = 'pie',
            textinfo = 'label+percent',
            textposition = 'inside') %>%
      layout(title = "")
  })
  
  output$category_bar <- renderPlotly({
    pages <- pages_data()
    category_summary <- pages %>%
      group_by(category) %>%
      summarise(
        total_views = sum(view_count),
        count = n(),
        .groups = 'drop'
      )
    
    p <- ggplot(category_summary, aes(x = reorder(category, total_views), y = total_views)) +
      geom_col(fill = "#3498db", alpha = 0.8) +
      coord_flip() +
      labs(title = "", x = "Kategori", y = "Total Views") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$category_table <- DT::renderDataTable({
    pages <- pages_data()
    category_summary <- pages %>%
      group_by(category) %>%
      summarise(
        total_views = sum(view_count),
        avg_views = round(mean(view_count), 1),
        count = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(total_views))
    
    DT::datatable(category_summary,
                  options = list(pageLength = 15),
                  colnames = c("Kategori", "Total Views", "Avg Views", "Jumlah Halaman"))
  })
  
  # Multi-metric trends
  output$multi_trend_chart <- renderPlotly({
    daily <- daily_data()
    if ("summary_date" %in% names(daily)) {
      daily$summary_date <- as.Date(daily$summary_date)
    }
    
    p <- ggplot(daily, aes(x = summary_date)) +
      geom_line(aes(y = total_visits, color = "Visits"), size = 1.2) +
      geom_line(aes(y = unique_users, color = "Users"), size = 1.2) +
      geom_line(aes(y = avg_session_duration_minutes * 2, color = "Duration (x2)"), size = 1.2) +
      labs(title = "", x = "Date", y = "Count", color = "Metric") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_color_manual(values = c("Visits" = "#3498db", "Users" = "#27ae60", "Duration (x2)" = "#f39c12"))
    
    ggplotly(p)
  })
  
  # Growth chart
  output$growth_chart <- renderPlotly({
    daily <- daily_data()
    daily <- daily[order(daily$summary_date), ]
    daily$growth_rate <- c(0, diff(daily$total_visits) / daily$total_visits[-nrow(daily)] * 100)
    
    p <- ggplot(daily, aes(x = summary_date, y = growth_rate)) +
      geom_col(fill = ifelse(daily$growth_rate >= 0, "#27ae60", "#e74c3c"), alpha = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "", x = "Date", y = "Growth Rate (%)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Performance chart
  output$performance_chart <- renderPlotly({
    daily <- daily_data()
    
    p <- ggplot(daily, aes(x = avg_session_duration_minutes, y = total_visits)) +
      geom_point(aes(size = unique_users, color = bounce_rate), alpha = 0.7) +
      labs(title = "", x = "Avg Session Duration", y = "Total Visits", 
           size = "Unique Users", color = "Bounce Rate") +
      theme_minimal() +
      scale_color_gradient(low = "#27ae60", high = "#e74c3c")
    
    ggplotly(p)
  })
  
  # Refresh functionality
  observeEvent(input$refresh_data, {
    showNotification("Data sedang di-refresh...", type = "message")
    session$reload()
  })
  
  output$last_update <- renderText({
    paste("Last updated:", format(Sys.time(), "%H:%M:%S"))
  })
  
  # Download handler
  output$download_report <- downloadHandler(
    filename = function() {
      paste("library_report_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(pages_data(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
