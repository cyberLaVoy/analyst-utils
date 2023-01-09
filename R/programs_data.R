

# UI ####
programs_data_UI <- function(id) {
  ns <- NS(id)
  dataTableOutput(outputId = ns("programs_table"))
}

# SERVER ####
programs_data_server <- function(input, output, session){
  ns <- session$ns
  
  output$programs_table <- renderDataTable({
    return(programs) 
  }, rownames=FALSE)
  
}