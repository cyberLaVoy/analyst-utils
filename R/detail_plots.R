
detail_plots_UI <- function(id) {
  ns <- NS(id)
  tagList(
    # BY DEPARTMENT UI DISPLAY ####
    br(),
    br(),
    fluidRow( 
      column(3, pickerInput(ns("college_choices_by_department"), 
                "College", 
                 college_abbreviations, 
                 options=list(`actions-box`=TRUE), 
                 multiple=TRUE,
                 selected=college_abbreviations[1]) )
    ),
    br(),
    fluidRow(
      column(12, plotlyOutput(ns('by_department_plot'), width=NULL) ),
    ),
    # BY SUBJECT UI DISPLAY ####
    br(),
    br(),
    fluidRow( 
      column(3, pickerInput(ns("college_choices_by_subject"), 
                "College", 
                college_abbreviations, 
                options=list(`actions-box`=TRUE), 
                multiple=TRUE,
                selected=college_abbreviations[1]) ),
      column(3, uiOutput(ns('subject_picker')) ) 
    ),
    br(),   
    fluidRow(
      column(12, plotlyOutput(ns('by_subject_plot'), width=NULL) ),
    ),
    # BY INDIVIDUAL UI DISPLAY ####
    br(),
    br(),
    fluidRow( 
      column(3, pickerInput(ns("college_choices_by_individual"), 
                "College", 
                college_abbreviations, 
                options=list(`actions-box`=TRUE), 
                multiple=TRUE,
                selected=college_abbreviations[1]) ),
      column(3, uiOutput(ns('individual_picker')) ) 
    ),
    br(),
    fluidRow(
      column(12, plotlyOutput(ns('by_individual_plot'), width=NULL) ),
    )
  )
}

detail_plots_server <- function(input, output, session, by_department_df, by_subject_df, by_individual_df, y, y_label, y_format) {

  # BY DEPARTMENT FUNCTIONALLITY ####
  by_department_reactive <- reactive({
    filter(by_department_df, college_abbreviation %in% input$college_choices_by_department)
  })
  output$by_department_plot <- renderPlotly({
    # Pause plot execution while input values evaluate. This eliminates an error message.
    req(input$college_choices_by_department)
    generate_standard_plot(by_department_reactive(), 
                           x=academic_year,
                           y={{y}},
                           y_format=y_format,
                           grouping=department,
                           x_label="Academic year",
                           y_label=y_label,
                           group_labeling=paste("College:", college_abbreviation, "<br />",
                                                "Department:", department),
                           title="Historical by department")
  })
  
  # BY SUBJECT FUNCTIONALLITY ####
  reactive_subject_options <- reactive({
    select_rows <- filter(by_subject_df, college_abbreviation %in% input$college_choices_by_subject) 
    sort(unique(select_rows$subject))
  })
  output$subject_picker <- renderUI({
    ns <- session$ns
    subjects <- reactive_subject_options()
    pickerInput(ns("subject_choices"), "Subject", 
                subjects, 
                options=list(`actions-box`=TRUE), 
                multiple=TRUE,
                selected=subjects )
  })
  reactive_subject_filter <- reactive({
    input$subject_choices
  })
  by_subject_reactive <- reactive({
    filter(by_subject_df, 
           subject %in% reactive_subject_filter() & 
             college_abbreviation %in% input$college_choices_by_subject
    )
  })
  output$by_subject_plot <- renderPlotly({
    # Pause plot execution while input values evaluate. This eliminates an error message.
    req(input$college_choices_by_subject, input$subject_choices)
    generate_standard_plot(by_subject_reactive(),
                           x=academic_year,
                           y={{y}},
                           y_format=y_format,
                           grouping=subject,
                           x_label="Academic year",
                           y_label=y_label,
                           group_labeling=paste("College:", college_abbreviation, "<br />",
                                                "Subject:", subject),
                           title="Historical by subject")
  })
  
  # BY INDIVIDUAL FUNCTIONALITY #### 
  reactive_individual_options <- reactive({
    individuals_selected <- by_individual_df %>%
      filter(instructor_college %in% input$college_choices_by_individual)  %>%
      select(banner_id, full_name) %>%
      unique()
    individuals <- individuals_selected$banner_id
    names(individuals) <- individuals_selected$full_name
    return( individuals )
  })
  output$individual_picker <- renderUI({
    ns <- session$ns
    individuals <- reactive_individual_options()
    pickerInput(ns("individual_choices"), "Individual", 
                choices=individuals, 
                options=list(`actions-box`=TRUE), 
                multiple=TRUE,
                selected=sample(individuals, 10, replace=TRUE) )
  })
  reactive_individual_filter <- reactive({
    input$individual_choices
  })
  by_individual_reactive <- reactive({
    filter(by_individual_df, 
           banner_id %in% reactive_individual_filter() & 
             instructor_college %in% input$college_choices_by_individual
    )
  })
  output$by_individual_plot <- renderPlotly({
  # Pause plot execution while input values evaluate. This eliminates an error message.
  req(input$college_choices_by_individual, input$individual_choices)
  generate_standard_plot(by_individual_reactive(),
                         x=academic_year,
                         y={{y}},
                         grouping=full_name,
                         x_label="Academic year",
                         y_label=y_label,
                         y_format=y_format,
                         group_labeling=paste("College: ", instructor_college, "<br />",
                                              "Individual: ", full_name, "<br />",
                                              "DixieID: D", banner_id, sep=''),
                         title="Historical by individual")
  })
}