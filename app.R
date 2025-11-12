# app.R - UFO Monitoring System
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(DT)
library(tidyverse)

# 数据生成函数
generate_demo_ufo_data <- function(n = 100) {
  data.frame(
    id = 1:n,
    created_at = Sys.time() - runif(n, 0, 86400),
    text = sample(c(
      "Just saw a strange light in the sky! #UFO",
      "Three bright objects moving in formation #UFOSighting",
      "Witnessed something unexplainable tonight #UAP",
      "Strange craft hovering over the city #Aliens"
    ), n, replace = TRUE),
    user = paste0("user_", sample(1:50, n, replace = TRUE)),
    latitude = runif(n, 25, 49),
    longitude = runif(n, -125, -66),
    credibility_score = sample(30:95, n, replace = TRUE),
    location = sample(state.name, n, replace = TRUE)
  )
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "🛸 UFO Monitor"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("About", tabName = "about", icon = icon("info"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              h2("UFO Activity Dashboard"),
              fluidRow(
                valueBoxOutput("total_reports"),
                valueBoxOutput("high_cred"),
                valueBoxOutput("active_states")
              ),
              fluidRow(
                box(
                  title = "Recent Activity",
                  plotlyOutput("timeline"),
                  width = 12
                )
              )
      ),
      
      tabItem(tabName = "map",
              h2("Live UFO Map"),
              leafletOutput("map", height = "600px")
      ),
      
      tabItem(tabName = "about",
              h2("About This System"),
              p("This is a demonstration UFO monitoring system."),
              p("Data is simulated for demonstration purposes.")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # 生成数据
  ufo_data <- reactive({
    invalidateLater(30000)  # 每30秒刷新
    generate_demo_ufo_data(100)
  })
  
  # 输出
  output$total_reports <- renderValueBox({
    valueBox(
      value = nrow(ufo_data()),
      subtitle = "Total Reports",
      icon = icon("eye")
    )
  })
  
  output$high_cred <- renderValueBox({
    valueBox(
      value = sum(ufo_data()$credibility_score >= 70),
      subtitle = "High Credibility",
      icon = icon("check")
    )
  })
  
  output$active_states <- renderValueBox({
    valueBox(
      value = length(unique(ufo_data()$location)),
      subtitle = "Active States",
      icon = icon("map")
    )
  })
  
  output$timeline <- renderPlotly({
    hourly_data <- ufo_data() %>%
      mutate(hour = hour(created_at)) %>%
      group_by(hour) %>%
      summarise(count = n())
    
    plot_ly(hourly_data, x = ~hour, y = ~count, type = "bar") %>%
      layout(title = "Hourly Distribution")
  })
  
  output$map <- renderLeaflet({
    leaflet(ufo_data()) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~credibility_score/10,
        popup = ~paste("Report:", text)
      )
  })
}

shinyApp(ui = ui, server = server)