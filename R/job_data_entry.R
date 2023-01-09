
# UI ####
job_data_entry_UI <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Person Search"),
    fluidRow(
      column(3, textInput(ns("banner_id_search"), "Dixie ID:"))
    ),
    uiOutput(ns("job_creation_ui")),
  )
}
  
# SERVER ####
job_data_entry_server <- function(input, output, session){
  ns <- session$ns
  
  output$job_creation_ui <- renderUI({
    if (nchar(input$banner_id_search) == 8) {
      person <- get_person(input$banner_id_search) 
      tagList(
        h3(paste("Job Creation for: ", person)),
        fluidRow (
          column(3, textInput(ns("site_name"), "Site name:")),
          column(3, textInput(ns("location"), "Location:")),   
          column(3, textInput(ns("category"), "Category:")),   
        ),
        fluidRow(
          column(4, radioButtons(ns("job_type"), "Job Type:",
                       choices = c("Internship", "co-op"),
                       selected = "Internship")),       
          column(4, radioButtons(ns("supervisor_is_alumnus"), "Is the supervisor an alumnus:",
                       choices = list("Yes"=1, "No"=0),
                       selected = 0)),   
        ),
        actionButton(ns("create_job_btn"), "Create Job"),
        h3("Jobs List"),
        dataTableOutput(ns("job_attributes_table"))
      )
    }
  })

  output$job_attributes_table <- renderDataTable({
    req(input$banner_id_search)
    # Take a dependency on input button. This will run once initially,
    # because the value changes from NULL to 0.
    insert_count <- input$create_job_btn
    # Use isolate() to avoid dependency on other inputs
    banner_id <- isolate( input$banner_id_search )
    site_name <- isolate( input$site_name )
    job_type <- isolate( input$job_type )
    location <- isolate( input$location )
    supervisor_is_alumnus <- isolate( input$supervisor_is_alumnus )
    
    if (insert_count > 0) {
      create_job(banner_id, 
                 input$job_type, 
                 input$site_name, 
                 input$location, 
                 input$category, 
                 input$supervisor_is_alumnus) 
    }
    person_jobs <- get_person_jobs(banner_id)
    return(person_jobs) 
  })
 
}