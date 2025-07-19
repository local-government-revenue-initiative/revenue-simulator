# Set options for deployment
options(
  shiny.port = as.numeric(Sys.getenv("PORT", 3838)),
  shiny.host = Sys.getenv("HOST", "0.0.0.0"),
  shiny.maxRequestSize = 50*1024^2  # 50MB file limit
)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Property Tax Revenue Simulator"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Module 1: Data Input", tabName = "module1", 
               icon = icon("database")),
      menuItem("Module 2: Parameters", tabName = "module2", 
               icon = icon("sliders-h")),
      menuItem("Module 3: Revenue", tabName = "module3", 
               icon = icon("calculator")),
      menuItem("Module 4: Analysis", tabName = "module4", 
               icon = icon("chart-bar")),
      menuItem("Module 5: GIS", tabName = "module5", 
               icon = icon("map"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      module1_ui("module1"),
      
      # Placeholder for other modules
      tabItem(tabName = "module2", h2("Module 2: Coming Soon")),
      tabItem(tabName = "module3", h2("Module 3: Coming Soon")),
      tabItem(tabName = "module4", h2("Module 4: Coming Soon")),
      tabItem(tabName = "module5", h2("Module 5: Coming Soon"))
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Module 1 server
  processed_data <- module1_server("module1")
  
  # Pass processed data to other modules when implemented
  # module2_server("module2", processed_data)
  # etc.
}

# Run the app
shinyApp(ui, server)