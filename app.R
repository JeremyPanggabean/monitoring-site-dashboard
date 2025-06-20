library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)

# Data Dummy
default_data <- data.frame(
  date = Sys.Date() - 14:0,
  visits = sample(50:150, 15),
  users = sample(30:100, 15)
)

ui <- dashboardPage(
  dashboardHeader(title = "📊 Dashboard Ringan"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Utama", tabName = "main", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "main",
        fluidRow(
          valueBox(sum(default_data$visits), "Total Kunjungan", icon = icon("eye"), color = "blue"),
          valueBox(sum(default_data$users), "Total Pengguna", icon = icon("users"), color = "green")
        ),
        fluidRow(
          box(
            title = "Trend Kunjungan", width = 12,
            plotlyOutput("visit_plot")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$visit_plot <- renderPlotly({
    p <- ggplot(default_data, aes(x = date)) +
      geom_line(aes(y = visits, color = "Kunjungan"), size = 1.2) +
      geom_line(aes(y = users, color = "Pengguna"), size = 1.2) +
      labs(x = "Tanggal", y = "Jumlah") +
      scale_color_manual(values = c("Kunjungan" = "#3498db", "Pengguna" = "#2ecc71")) +
      theme_minimal()

    ggplotly(p)
  })
}

shinyApp(ui, server)
