

# UI ####
jobs_summary_UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("refresh_data"), "Refresh Table"),
    br(),
    br(),
    dataTableOutput(outputId = ns("job_attributes_table"))
  )
}

# SERVER ####
jobs_summary_server <- function(input, output, session){
  ns <- session$ns
  
  output$job_attributes_table <- renderDataTable({
    # Take a dependency on input button. This will run once initially,
    # because the value changes from NULL to 0.
    input$refresh_data
    jobs <- get_jobs()
    return(jobs) 
  })
  
}