library(shiny)
library(shinythemes)
library(DT)
library(here)
library(shinyjs)

source(here::here("r", 'pull.R'))
source(here::here("r", 'mung.R'))
source(here::here("r", 'report_generation.R'))

ui <- fluidPage( theme=shinytheme("flatly"), useShinyjs(),
  
  wellPanel( titlePanel("IPEDS Automation") ),
  fluidRow(
    # INPUT UI ####
    column(2, 
      h2("Global Actions"),
      wellPanel(
        p("Select a term to update the term all reports are checking student enrollment against."),
        selectInput("enrollment_check_term", 
                    "Enrollment Check Term", 
                    available_enrollment_check_terms,
                    selected="202140")
      )
    ),
    # OUTPUT UI ####
    column(10,
      h2("Reports"),
      tabsetPanel(
        tabPanel("Graduation",
                 wellPanel(
                   h3("Actions"),
                   fluidRow(
                     column(2, 
                            selectInput("grad_cohort_term", 
                            "Cohort Term Selection", 
                            available_cohort_terms,
                            selected="201540") 
                     )
                   ),
                   downloadButton("grad_report_download", "Download Report"),
                   h3("Dataset"),
                   p( textOutput("grad_table_headcount") ),
                   DT::dataTableOutput("grad_table"),
                   h3("Unique IDs"),
                   actionButton("grad_table_ids_show_hide", "Show/Hide"),
                   p( textOutput("grad_table_ids") ),
                   h3("Report"),
                   DT::dataTableOutput("grad_report_table")
                 )
        ),
        tabPanel("Graduation 200",
                 wellPanel(
                   h3("Actions"),
                   fluidRow(
                     column(2,                  
                            selectInput("grad_200_cohort_term", 
                                        "Cohort Term Selection", 
                                        available_cohort_terms,
                                        selected="201340"),                  
                     )
                   ),
                   downloadButton("grad_200_report_download", "Download Report"),
                   h3("Dataset"),
                   p( textOutput("grad_200_table_headcount") ),
                   DT::dataTableOutput("grad_200_table"),                  
                   h3("Unique IDs"),
                   actionButton("grad_200_table_ids_show_hide", "Show/Hide"),
                   p( textOutput("grad_200_table_ids") ),
                   h3("Report"),
                   DT::dataTableOutput("grad_200_report_table")
                 )
        ),
        tabPanel("Outcome Measures",
                 wellPanel(
                   h3("Actions"),
                   fluidRow(
                     column(2,                     
                            selectInput("outcomes_cohort_terms", 
                                        "Cohort Terms Selection", 
                                        available_cohort_terms,
                                        selected=c("201340", "201420"),
                                        multiple=TRUE)
                     )
                   ),
                   downloadButton("outcomes_report_download", "Download Report"),                  
                   h3("Dataset"),
                   p( textOutput("outcomes_table_headcount") ),
                   DT::dataTableOutput("outcomes_table"),
                   h3("Unique IDs"),
                   actionButton("outcomes_table_ids_show_hide", "Show/Hide"),
                   p( textOutput("outcomes_table_ids") ),
                   h3("Report"),
                   DT::dataTableOutput("outcomes_report_table")
                 )
      ),
      tabPanel("CDS",
              wellPanel(
                 h3("Actions"),
                 fluidRow(
                   column(2,                     
                          selectInput("cds_cohort_term", 
                                      "Cohort Terms Selection", 
                                      available_cohort_terms,
                                      selected="201540")
                   )
                 ),
                 downloadButton("cds_report_download", "Download Report"),                  
                 h3("Dataset"),
                 p( textOutput("cds_table_headcount") ),
                 DT::dataTableOutput("cds_table"),
                 h3("Unique IDs"),
                 actionButton("cds_table_ids_show_hide", "Show/Hide"),
                 p( textOutput("cds_table_ids") ),
                 h3("Report"),
                 DT::dataTableOutput("cds_report_table")
              )
      )     
      )
    )
  )
)

server <- function(input, output) {
  # REACTIVE ELEMENTS #### 
  reactive_graduation_dataset <- reactive({
    req(input$enrollment_check_term)
    get_graduation_dataset(graduation_full_edify_df, input$enrollment_check_term)
  })
  reactive_grad_report_edify_df <- reactive({
    req(input$grad_cohort_term)
    grad_dataset <- reactive_graduation_dataset()
    get_graduation_report_edify_df(grad_dataset, input$grad_cohort_term)
  })
  reactive_grad_200_report_edify_df <- reactive({
    req(input$grad_200_cohort_term)
    grad_dataset <- reactive_graduation_dataset()
    get_graduation_200_report_edify_df(grad_dataset, input$grad_200_cohort_term)
  }) 
  reactive_outcomes_report_edify_df <- reactive({
    req(input$outcomes_cohort_terms)
    grad_dataset <- reactive_graduation_dataset()
    get_outcomes_report_edify_df(grad_dataset, input$outcomes_cohort_terms)
  })  
  reactive_cds_report_edify_df <- reactive({
    req(input$cds_cohort_term)
    grad_dataset <- reactive_graduation_dataset()
    get_cds_report_edify_df(grad_dataset, input$cds_cohort_term)
  })   
  # reactive headcounts
  reactive_grad_headcount <- reactive({
    req(input$grad_table_rows_all)
    filtered <- reactive_grad_report_edify_df()[input$grad_table_rows_all,]
    length( unique(filtered[["sis_system_id"]]) )
  })
  reactive_grad_200_headcount <- reactive({
    req(input$grad_200_table_rows_all)
    filtered <- reactive_grad_200_report_edify_df()[input$grad_200_table_rows_all,]
    length( unique(filtered[["sis_system_id"]]) )
  }) 
  reactive_outcomes_headcount <- reactive({
    req(input$outcomes_table_rows_all)
    filtered <- reactive_outcomes_report_edify_df()[input$outcomes_table_rows_all,]
    length( unique(filtered[["sis_system_id"]]) )
  })  
  reactive_cds_headcount <- reactive({
    req(input$cds_table_rows_all)
    filtered <- reactive_cds_report_edify_df()[input$cds_table_rows_all,]
    length( unique(filtered[["sis_system_id"]]) )
  })    
  # reactive ids
  reactive_grad_ids <- reactive({
    req(input$grad_table_rows_all)
    filtered <- reactive_grad_report_edify_df()[input$grad_table_rows_all,]
    unique(filtered[["sis_system_id"]]) 
  })
  reactive_grad_200_ids <- reactive({
    req(input$grad_200_table_rows_all)
    filtered <- reactive_grad_200_report_edify_df()[input$grad_200_table_rows_all,]
    unique(filtered[["sis_system_id"]]) 
  }) 
  reactive_outcomes_ids <- reactive({
    req(input$outcomes_table_rows_all)
    filtered <- reactive_outcomes_report_edify_df()[input$outcomes_table_rows_all,]
    unique(filtered[["sis_system_id"]]) 
  })    
  reactive_cds_ids <- reactive({
    req(input$cds_table_rows_all)
    filtered <- reactive_cds_report_edify_df()[input$cds_table_rows_all,]
    unique(filtered[["sis_system_id"]]) 
  })      
  # DATASET HEADCOUNTS ####
  output$grad_table_headcount <- renderText({ 
    paste("Headcount:", reactive_grad_headcount() )
  })
   output$grad_200_table_headcount <- renderText({ 
    paste("Headcount:",  reactive_grad_200_headcount() )
  }) 
   output$outcomes_table_headcount <- renderText({ 
    paste("Headcount:", reactive_outcomes_headcount() )
  })  
  output$cds_table_headcount <- renderText({ 
    paste("Headcount:", reactive_cds_headcount() )
  })   
  # DATASET IDS ####
   output$grad_table_ids <- renderText({ 
    wrapped <- paste0("'", reactive_grad_ids(), "'")
    wrapped <- paste0('(', toString(wrapped), ')')
    return(wrapped)
  })
   output$grad_200_table_ids <- renderText({ 
    wrapped <- paste0("'", reactive_grad_200_ids(), "'")
    wrapped <- paste0('(', toString(wrapped), ')')
    return(wrapped)    
  }) 
   output$outcomes_table_ids <- renderText({ 
    wrapped <- paste0("'", reactive_outcomes_ids(), "'")
    wrapped <- paste0('(', toString(wrapped), ')')
    return(wrapped)        
  })    
  output$cds_table_ids <- renderText({ 
    wrapped <- paste0("'", reactive_cds_ids(), "'")
    wrapped <- paste0('(', toString(wrapped), ')')
    return(wrapped)        
  })      
  # DATASET TABLES #### 
  output$grad_table <- DT::renderDataTable(reactive_grad_report_edify_df(),
                                           filter="top",
                                           extensions=c('FixedColumns'),
                                           options=list( pageLength=10,
                                                         dom='Bfrtip',
                                                         scrollX = TRUE,
                                                         scrollCollapse = TRUE
                                           ),
                                           rownames=FALSE)
  output$grad_200_table <- DT::renderDataTable(reactive_grad_200_report_edify_df(),
                                               filter="top",
                                               extensions=c('FixedColumns'),
                                               options=list( pageLength=10,
                                                             dom='Bfrtip',
                                                             scrollX = TRUE,
                                                             scrollCollapse = TRUE
                                               ),
                                               rownames=FALSE)
  output$outcomes_table <- DT::renderDataTable(reactive_outcomes_report_edify_df(),
                                               filter="top",
                                               extensions=c('FixedColumns'),
                                               options=list( pageLength=10,
                                                             dom='Bfrtip',
                                                             scrollX = TRUE,
                                                             scrollCollapse = TRUE
                                               ),
                                               rownames=FALSE)
  output$cds_table <- DT::renderDataTable(reactive_cds_report_edify_df(),
                                          filter="top",
                                          extensions=c('FixedColumns'),
                                          options=list( pageLength=10,
                                                        dom='Bfrtip',
                                                        scrollX = TRUE,
                                                        scrollCollapse = TRUE
                                          ),
                                          rownames=FALSE) 
   # REPORT TABLES #### 
   output$grad_report_table <- DT::renderDataTable(generate_ipeds_graduation_report(reactive_grad_report_edify_df()),
                                               extensions=c('FixedColumns'),
                                               options=list( pageLength=10,
                                                             dom='Bfrtip',
                                                             scrollX = TRUE,
                                                             scrollCollapse = TRUE
                                               ),
                                               rownames=FALSE)   
   output$grad_200_report_table <- DT::renderDataTable(generate_ipeds_graduation_200_report(reactive_grad_200_report_edify_df()),
                                               extensions=c('FixedColumns'),
                                               options=list( pageLength=10,
                                                             dom='Bfrtip',
                                                             scrollX = TRUE,
                                                             scrollCollapse = TRUE
                                               ),
                                               rownames=FALSE)     
   output$outcomes_report_table <- DT::renderDataTable(generate_ipeds_outcomes_report(reactive_outcomes_report_edify_df()),
                                               extensions=c('FixedColumns'),
                                               options=list( pageLength=10,
                                                             dom='Bfrtip',
                                                             scrollX = TRUE,
                                                             scrollCollapse = TRUE
                                               ),
                                               rownames=FALSE)   
    output$cds_report_table <- DT::renderDataTable(generate_cds_report(reactive_cds_report_edify_df()),
                                                   extensions=c('FixedColumns'),
                                                   options=list( pageLength=10,
                                                                 dom='Bfrtip',
                                                                 scrollX = TRUE,
                                                                 scrollCollapse = TRUE
                                                   ),
                                                   rownames=FALSE)   
    # ACTION BUTTONS ####
   observeEvent(input$grad_table_ids_show_hide, {
     shinyjs::toggle("grad_table_ids")
   }) 
   observeEvent(input$grad_200_table_ids_show_hide, {
     shinyjs::toggle("grad_200_table_ids")
   })   
   observeEvent(input$outcomes_table_ids_show_hide, {
     shinyjs::toggle("outcomes_table_ids")
   })    
   observeEvent(input$cds_table_ids_show_hide, {
     shinyjs::toggle("cds_table_ids")
   })      
   # DOWNLOAD HANDLERS ####
   output$grad_report_download <- downloadHandler(filename=function() {
                                                              "ipeds_graduation_report.dat"
                                                            }, 
                                                  content=function(file){ 
                                                            report_df <- generate_ipeds_graduation_report(reactive_grad_report_edify_df())
                                                            format_and_save_graduation_report(report_df, file)
                                                          })
    output$grad_200_report_download <- downloadHandler(filename=function() {
                                                              "ipeds_graduation_200_report.dat"
                                                            }, 
                                                  content=function(file){ 
                                                            report_df <- generate_ipeds_graduation_200_report(reactive_grad_200_report_edify_df())
                                                            format_and_save_graduation_200_report(report_df, file)
                                                          })  
    output$outcomes_report_download <- downloadHandler(filename=function() {
                                                              "ipeds_outcomes_report.dat"
                                                            }, 
                                                  content=function(file){ 
                                                            report_df <- generate_ipeds_outcomes_report(reactive_outcomes_report_edify_df())
                                                            format_and_save_outcomes_report(report_df, file)
                                                          })  
     output$cds_report_download <- downloadHandler(filename=function() {
                                                              "cds_report.dat"
                                                            }, 
                                                  content=function(file){ 
                                                            report_df <- generate_cds_report(reactive_cds_report_edify_df())
                                                            format_and_save_cds_report(report_df, file)
                                                          })    
}

shinyApp(ui, server)
