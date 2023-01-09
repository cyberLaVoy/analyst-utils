
By_College_UI <- function(id) {
  ns <- NS(id)
  
  tagList( 
    titlePanel("FTE - By College"),
    
    wellPanel(
      h4("This tab displays the total FTE taught by faculty, where totals are segmented by college."),
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


by_college_server <- function(input, output, session){
  
  aggregated <- reactive( data_mung("college", input$instructor_status, input$college, departments, subjects, input$semesters_selection) )
  
  output$fte_projection_graph <- renderPlotly({ 
    # Pause plot execution while input values evaluate. This eliminates an error message.
    req(input$instructor_status, input$college)
    plot_data <- aggregated()
    aesthetic <- aes( x=term, y=as.numeric(gsub(",","",fte)), group=grouping, colour=grouping, 
                      text=paste("Term:", term, "<br />", 
                                 "FTE:", fte, "<br />", 
                                 "Instructor Status:", instructor_status, "<br />",
                                 "College:", course_college, "<br />") )
    plot <- get_plot(plot_data, aesthetic)
    return(plot)
  })

  output$fte_projection_table <- DT::renderDataTable({ 
    plot_data <- aggregated()
    DT::datatable( plot_data[, !(names(plot_data) == "grouping")], 
                   extensions='Buttons', 
                   options=list( pageLength=15, 
                                 dom='Bfrtip',
                                 buttons=c('csv', 'excel') ),
                   rownames=FALSE,
                   colnames=c("Term", "Instructor Status", "College", "Student Credit Hours", "FTE")) 
  }, server=TRUE)
}

