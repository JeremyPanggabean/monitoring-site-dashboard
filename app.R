# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(RPostgreSQL)
library(ggplot2)
library(shinydashboardPlus)
library(fresh)
library(shinyWidgets)

# Enhanced database connection function with better error handling
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

default_pages <- data.frame(
  page_url = c(
    "/perpustakaan/jurnal/teknologi-informasi", "/perpustakaan/buku/teknologi-pertanian",
    "/perpustakaan/buku/komputer-dan-teknologi", "/perpustakaan/buku/nutrisi-dan-kesehatan",
    "/perpustakaan/buku/administrasi-bisnis", "/perpustakaan/buku/keperawatan",
    "/perpustakaan/buku/manajemen-proyek", "/perpustakaan/jurnal/hukum-perdata",
    "/perpustakaan/buku/ekonomi-makro", "/perpustakaan/jurnal/psikologi-pendidikan",
    "/perpustakaan/buku/akuntansi-dasar", "/perpustakaan/jurnal/kedokteran-umum",
    "/perpustakaan/buku/statistika-terapan", "/perpustakaan/buku/bahasa-indonesia",
    "/perpustakaan/jurnal/biologi-molekuler", "/perpustakaan/buku/fisika-kuantum",
    "/perpustakaan/buku/kimia-organik", "/perpustakaan/jurnal/sejarah-indonesia",
    "/perpustakaan/buku/matematika-diskrit", "/perpustakaan/jurnal/geografi-fisik"
  ),
  view_count = c(120, 110, 100, 99, 98, 97, 95, 95, 88, 85, 82, 79, 76, 74, 71, 68, 65, 62, 59, 56),
  category = c(
    "Teknologi", "Pertanian", "Teknologi", "Kesehatan", "Bisnis", "Kesehatan",
    "Manajemen", "Hukum", "Ekonomi", "Psikologi", "Ekonomi", "Kedokteran",
    "Matematika", "Bahasa", "Biologi", "Fisika", "Kimia", "Sejarah", "Matematika", "Geografi"
  )
)

# Custom theme
my_theme <- create_theme(
  adminlte_color(
    light_blue = "#3498db",
    blue = "#2980b9",
    navy = "#1f3a93",
    green = "#27ae60",
    yellow = "#f39c12",
    red = "#e74c3c"
  ),
  adminlte_sidebar(
    dark_bg = "#2c3e50",
    dark_hover_bg = "#34495e",
    dark_color = "#ecf0f1"
  ),
  adminlte_global(
    content_bg = "#f8f9fa"
  )
)

# Enhanced UI with better styling
ui <- dashboardPage(
  dashboardHeader(
    title = "📚 Dashboard Perpustakaan Digital",
    titleWidth = 300,
    dropdownMenu(
      type = "notifications",
      notificationItem(
        text = "Database terhubung",
        icon = icon("database"),
        status = "success"
      )
    )
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("🏠 Dashboard Utama", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("⭐ Halaman Populer", tabName = "pages", icon = icon("star")),
      menuItem("📊 Analisis Kategori", tabName = "categories", icon = icon("chart-pie")),
      menuItem("📈 Trend Analysis", tabName = "trends", icon = icon("line-chart")),
      menuItem("📋 Laporan Detail", tabName = "reports", icon = icon("file-alt"))
    )
  ),
  
  dashboardBody(
    use_theme(my_theme),
    
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f7fc;
        }
        .box {
          border-radius: 10px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        .value-box {
          border-radius: 10px;
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #3498db;
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
            tableOutput("summary_stats")
          )
        ),
        
        fluidRow(
          box(
            title = "🎯 Bounce Rate & Engagement",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("engagement_chart")
          ),
          box(
            title = "📱 Aktivitas Real-time",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            div(
              style = "text-align: center; padding: 20px;",
              h3("Status Sistem", style = "color: #27ae60;"),
              br(),
              actionButton("refresh_data", "🔄 Refresh Data", 
                         class = "btn btn-success btn-lg"),
              br(), br(),
              textOutput("last_update")
            )
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
                           min = 5, max = 60, value = 10, step = 5)
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
      
      # New category analysis tab
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
      
      # New trends analysis tab
      tabItem(
        tabName = "trends",
        fluidRow(
          box(
            title = "📈 Multi-Metric Trends",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("multi_trend_chart", height = "500px")
          )
        ),
        
        fluidRow(
          box(
            title = "📊 Korelasi Metrics",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("correlation_chart")
          ),
          box(
            title = "📈 Growth Rate",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("growth_chart")
          )
        )
      ),
      
      # New detailed reports tab
      tabItem(
        tabName = "reports",
        fluidRow(
          box(
            title = "📋 Laporan Komprehensif",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            tabsetPanel(
              tabPanel("Daily Report", 
                      br(),
                      DT::dataTableOutput("daily_report")),
              tabPanel("Weekly Summary", 
                      br(),
                      DT::dataTableOutput("weekly_report")),
              tabPanel("Top Performers", 
                      br(),
                      DT::dataTableOutput("top_performers"))
            )
          )
        )
      )
    )
  )
)

# Enhanced Server with more functionality
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
    categories <- c("Semua" = "all", unique(pages$category))
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
  
  # Enhanced visits chart with multiple metrics
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
      theme(
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      ) +
      scale_color_manual(values = c("Total Visits" = "#3498db", "Unique Users" = "#27ae60"))
    
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      layout(hovermode = "x unified")
  })
  
  # Enhanced pages chart
  output$pages_chart <- renderPlotly({
    pages <- filtered_pages()
    
    # Truncate long URLs for better display
    pages$short_url <- sapply(pages$page_url, function(x) {
      if (nchar(x) > 40) {
        paste0("...", substr(x, nchar(x)-37, nchar(x)))
      } else {
        x
      }
    })
    
    p <- pages %>%
      ggplot(aes(x = reorder(short_url, view_count), y = view_count, fill = category)) +
      geom_col(alpha = 0.8) +
      coord_flip() +
      labs(title = "", x = "Halaman", y = "Views", fill = "Kategori") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      ) +
      scale_fill_brewer(type = "qual", palette = "Set3")
    
    ggplotly(p, tooltip = c("y", "fill")) %>%
      layout(hovermode = "y unified")
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
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        colnames = c("Halaman", "Views", "Kategori"),
        extensions = 'Buttons'
      ) %>%
      formatStyle("view_count", 
                  background = styleColorBar(pages$view_count, '#3498db'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  })
  
  # Summary stats table
  output$summary_stats <- renderTable({
    daily <- daily_data()
    data.frame(
      Metric = c("Avg Daily Visits", "Peak Day Visits", "Avg Session Duration", "Best Engagement Day"),
      Value = c(
        round(mean(daily$total_visits, na.rm = TRUE), 0),
        max(daily$total_visits, na.rm = TRUE),
        paste0(round(mean(daily$avg_session_duration_minutes, na.rm = TRUE), 1), " min"),
        as.character(daily$summary_date[which.max(daily$total_visits)])
      )
    )
  }, striped = TRUE, bordered = TRUE)
  
  # Engagement chart
  output$engagement_chart <- renderPlotly({
    daily <- daily_data()
    if ("summary_date" %in% names(daily)) {
      daily$summary_date <- as.Date(daily$summary_date)
    }
    
    p <- ggplot(daily, aes(x = summary_date)) +
      geom_line(aes(y = bounce_rate * 100), color = "#e74c3c", size = 1.2) +
      geom_point(aes(y = bounce_rate * 100), color = "#e74c3c", size = 3) +
      labs(title = "", x = "Tanggal", y = "Bounce Rate (%)") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    ggplotly(p)
  })
  
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
            textposition = 'inside',
            hovertemplate = '%{label}<br>Views: %{value}<br>Percentage: %{percent}<extra></extra>') %>%
      layout(title = "")
  })
  
  output$category_bar <- renderPlotly({
    pages <- pages_data()
    category_summary <- pages %>%
      group_by(category) %>%
      summarise(
        total_views = sum(view_count),
        avg_views = mean(view_count),
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
        top_page = page_url[which.max(view_count)],
        .groups = 'drop'
      ) %>%
      arrange(desc(total_views))
    
    DT::datatable(category_summary,
                  options = list(pageLength = 15),
                  colnames = c("Kategori", "Total Views", "Avg Views", "Jumlah Halaman", "Top Page"))
  })
  
  # Multi-metric trends
  output$multi_trend_chart <- renderPlotly({
    daily <- daily_data()
    
    # Normalize data for comparison
    daily_norm <- daily %>%
      mutate(
        visits_norm = (total_visits - min(total_visits)) / (max(total_visits) - min(total_visits)),
        users_norm = (unique_users - min(unique_users)) / (max(unique_users) - min(unique_users)),
        duration_norm = (avg_session_duration_minutes - min(avg_session_duration_minutes)) / 
                       (max(avg_session_duration_minutes) - min(avg_session_duration_minutes))
      )
    
    p <- ggplot(daily_norm, aes(x = summary_date)) +
      geom_line(aes(y = visits_norm, color = "Visits"), size = 1.2) +
      geom_line(aes(y = users_norm, color = "Users"), size = 1.2) +
      geom_line(aes(y = duration_norm, color = "Duration"), size = 1.2) +
      labs(title = "", x = "Date", y = "Normalized Value", color = "Metric") +
      theme_minimal() +
      scale_color_manual(values = c("Visits" = "#3498db", "Users" = "#27ae60", "Duration" = "#f39c12"))
    
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
  
  # Report tables
  output$daily_report <- DT::renderDataTable({
    daily <- daily_data()
    DT::datatable(daily, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  output$weekly_report <- DT::renderDataTable({
    daily <- daily_data()
    # Create weekly summary
    daily$week <- format(daily$summary_date, "%Y-W%U")
    weekly <- daily %>%
      group_by(week) %>%
      summarise(
        avg_visits = round(mean(total_visits), 1),
        total_visits = sum(total_visits),
        avg_users = round(mean(unique_users), 1),
        avg_duration = round(mean(avg_session_duration_minutes), 1),
        .groups = 'drop'
      )
    
    DT::datatable(weekly, options = list(pageLength = 10))
  })
  
  output$top_performers <- DT::renderDataTable({
    pages <- head(pages_data(), 20)
    DT::datatable(pages, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # Refresh functionality
  observeEvent(input$refresh_data, {
    showNotification("Data sedang di-refresh...", type = "message")
    # Force reactive data to reload
    session$reload()
    showNotification("Data berhasil di-refresh!", type = "success")
  })
  
  output$last_update <- renderText({
    paste("Last updated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
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

# Run the enhanced app
shinyApp(ui = ui, server = server)
