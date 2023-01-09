
By_Subject_UI <- function(id) {
  ns <- NS(id)
  
  tagList( 
    titlePanel("FTE - By Subject"),
    wellPanel(
      h4("This tab displays the total FTE taught by faculty, where totals are segmented by subject."),
      p("The data is pooled into two separate groups (to distinquish for separate fund pools): 
          1. Adjunct & Overload, and 2. Contract Faculty."),
      p("There are also tendlines (dashed lines) to give a general idea 
        of how these FTE totals are moving.")
    ),
    fluidRow( 
      column(3,
             pickerInput(ns("instructor_status"), "Instructor Status", 
                         instructor_statuses, 
                         options=list(`actions-box`=TRUE), 
                         multiple=TRUE,
                         selected=instructor_statuses)
      ),
      column(3,
             pickerInput(ns("college"), "College", 
                         colleges, 
                         options=list(`actions-box`=TRUE), 
                         multiple=TRUE,
                         selected=colleges)
      ),
      column(3,
             uiOutput(ns('department_picker'))
      ),
       column(3,
             uiOutput(ns('subject_picker'))
      ),     
      column(3,
             pickerInput(ns("semesters_selection"), "Semesters", 
                        c("Fall", "Spring"), 
                        options=list(`actions-box`=TRUE), 
                        multiple=FALSE,
                        selected="Fall")
      ) 
    ),
    mainPanel(
      fluidRow(
        column(12,
               plotlyOutput( ns("fte_projection_graph" ))
        )
      ),
      br(),
      br(),
      fluidRow(
        DT::dataTableOutput( ns("fte_projection_table" ) )
      )
    )
  )
}


by_subject_server <- function(input, output, session){

  ### reactive department selection ### 
  reactive_department_options <- reactive({
      rows <- (instructional_faculty_workload_df$course_college %in% input$college)
      select_rows <- subset(instructional_faculty_workload_df, rows)
      sort(unique(select_rows$course_department))
  })
  output$department_picker <- renderUI({
    ns <- session$ns
    departments <- reactive_department_options()
    pickerInput(ns("department"), "Department", 
                departments, 
                options=list(`actions-box`=TRUE), 
                multiple=TRUE,
                selected=departments)
  })
  reactive_department_filter <- reactive({
    input$department
  })
  
  ### reactive subject selection ### 
   reactive_subject_options <- reactive({
      rows <- (instructional_faculty_workload_df$course_department %in% input$department)
      select_rows <- subset(instructional_faculty_workload_df, rows)
      sort(unique(select_rows$course_subject))
  })
  output$subject_picker <- renderUI({
    ns <- session$ns
    subjects <- reactive_subject_options()
    pickerInput(ns("subject"), "Subject", 
                subjects, 
                options=list(`actions-box`=TRUE), 
                multiple=TRUE,
                selected=subjects)
  })
  reactive_subject_filter <- reactive({
    input$subject
  }) 
  
  aggregated <- reactive({
    data_mung("subject", input$instructor_status, input$college, reactive_department_filter(), reactive_subject_filter(), input$semesters_selection)
  })
  
  output$fte_projection_graph <- renderPlotly({ 
    # Pause plot execution while input values evaluate. This eliminates an error message.
    req(input$instructor_status, input$college, input$department, input$subject)
    plot_data <- aggregated()
    aesthetic <- aes( x=term, y=as.numeric(gsub(",","",fte)), group=grouping, colour=grouping, 
                      text=paste("Term:", term, "<br />", 
                                 "FTE:", fte, "<br />", 
                                 "Instructor Status:", instructor_status, "<br />",
                                 "Subject:", course_subject, "<br />") )
    get_plot(plot_data, aesthetic)
  })
  
  output$fte_projection_table <- DT::renderDataTable({ 
    plot_data <- aggregated()
    DT::datatable( plot_data[, !(names(plot_data) == "grouping")], 
                   extensions='Buttons', 
                   options=list( pageLength=15, 
                                 dom='Bfrtip',
                                 buttons=c('csv', 'excel') ),
                   rownames=FALSE,
                   colnames=c("Term", "Instructor Status", "Subject", "Student Credit Hours", "FTE")) 
  }, server=TRUE) 
}
