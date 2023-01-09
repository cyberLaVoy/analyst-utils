library(shiny)
library(here)

source(here::here("odbc_connection_object.R"))

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello DB Interactions!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      h2("Personal Robot Attributes"),
      
      # Input: Slider for level of intelligence ----
      sliderInput(inputId = "intelligence_slider",
                  label = h4("Intelligence Level:"),
                  min = 1,
                  max = 50,
                  value = 30)
      ,
      # Input: Selector for number of arms ----
      selectInput(inputId = "arm_count_selector", 
                  label = h4("Number of Arms:"), 
                  choices = list("2 Arms" = 2, "4 Arms" = 4,
                                 "6 Arms" = 6), 
                  selected = 1),
      # Input: Text input for robot name ----
      textInput(inputId = "robot_name_text", 
                label = h4("Robot Name:"), 
                value = ""),
      br(),
      # action button to submit selected attributes
      actionButton("create_robot_btn", "Create Robot")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table ----
      tableOutput(outputId = "robot_attributes_table")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # table with data from the database table ----
  # This expression that generates a table is wrapped in a call
  # to renderTable to indicate that:
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a table
  output$robot_attributes_table <- renderTable({
    
    # Take a dependency on input$goButton. This will run once initially,
    # because the value changes from NULL to 0.
    insert_count <- input$create_robot_btn
    
    conn <- get_connection_object()
    on.exit(dbDisconnect(conn), add=TRUE)

    if (insert_count > 0) {
      # Use isolate() to avoid dependency on other inputs
      intelligence_level <- isolate( as.integer(input$intelligence_slider) )
      arm_count <- isolate( as.integer(input$arm_count_selector) )
      full_name <- isolate( input$robot_name_text )
      insert_sql <- "INSERT INTO warehouse.robots (full_name, intelligence_level, arm_count)
                     VALUES (?full_name, ?intelligence_level, ?arm_count);"
      insert_query <- sqlInterpolate(conn, insert_sql, arm_count=arm_count,
                                                       intelligence_level=intelligence_level,
                                                       full_name=full_name)
      dbGetQuery(conn, insert_query)
    }
    
    select_query <- "SELECT * FROM warehouse.robots;"   
    dbGetQuery(conn, select_query)
   
  })
  
}

shinyApp(ui=ui, server=server)