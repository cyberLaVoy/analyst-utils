# ******************************************************************************
#                   CURRENT MINOR TAB TIME ENROLLMENT LIST
# ******************************************************************************

# UI ####
current_minor_tab_enrollment_list_UI <- function(id) {
  ns <- NS(id)
  
  ## UI elements ####
  tagList(
    h3("Spring 2021 to Fall 2021 Enrollment Action List"),
    h4("Filter, find, and download student enrollment data for further action.",
       style="color:#a6a6a6;"),
    br(),
    br(),
    fluidRow(
      column(3, wellPanel(uiOutput(ns('college_selection_ui')),
                          uiOutput(ns('cohort_year_selection_ui')),
                          uiOutput(ns('cohort_semester_selection_ui')),
                          uiOutput(ns('advisor_selection_ui')),
                          pickerInput(ns("is_enrolled_selection"), "Is Enrolled Filter",
                                      c("True", "False"),
                                      multiple=TRUE,
                                      selected=c("True", "False")) )
      ),
      column(9, 
             h3("Enrollment List"),
             dataTableOutput(outputId = ns("advisor_student_enrollment_table") ),
             br(),
             h3("Holds List"),
             dataTableOutput(outputId = ns("student_holds_table"))
      )
    )
  )
  
}

# SERVER ####
current_minor_tab_enrollment_list_server <- function(input, output, session, input_plot_df){
    ns = session$ns
    
  output$advisor_selection_ui <- renderUI({
    advisors <- input_plot_df[["advisor_full_name"]] %>%
      unique() %>%
      sort()
    pickerInput(ns("advisor_selection"),  "Advisor Selection", 
                advisors, 
                options=list(`actions-box`=TRUE, `live-search`=TRUE), 
                multiple=TRUE,
                selected=advisors)
  })  
  output$college_selection_ui <- renderUI({
    colleges <- input_plot_df[["college_abbreviation"]] %>%
      unique() %>%
      sort()
    pickerInput(ns("college_selection"),  "College Selection", 
                colleges, 
                options=list(`actions-box`=TRUE, `live-search`=TRUE), 
                multiple=TRUE,
                selected=colleges)
  })   
  output$cohort_year_selection_ui <- renderUI({
    cohort_years <- input_plot_df[["cohort_year"]] %>%
      unique() %>%
      sort()
    cohort_years <- c(cohort_years, "NA")
    pickerInput(ns("cohort_year_selection"),  "Cohort Year Selection", 
                cohort_years, 
                options=list(`actions-box`=TRUE, `live-search`=TRUE), 
                multiple=TRUE,
                selected=cohort_years)
  })    
  output$cohort_semester_selection_ui <- renderUI({
    cohort_semesters <- input_plot_df[["cohort_semester"]] %>%
      unique() %>%
      sort()
    cohort_semesters <- c(cohort_semesters, "NA")
    pickerInput(ns("cohort_semester_selection"),  "Cohort Semester Selection", 
                cohort_semesters, 
                options=list(`actions-box`=TRUE, `live-search`=TRUE), 
                multiple=TRUE,
                selected=cohort_semesters)
  })  
    
  student_list <- reactive({
    req(input$advisor_selection, 
        input$is_enrolled_selection, 
        input$college_selection, 
        input$cohort_year_selection,
        input$cohort_semester_selection)
    column_excludes <- c("advisor_pidm", 
                         "advisor_banner_id", 
                         "advisor_full_name", 
                         "cohort_semester_category")
    students <- input_plot_df %>%
      filter( advisor_full_name %in% input$advisor_selection
              & is_enrolled %in% input$is_enrolled_selection
              & ( cohort_year %in% input$cohort_year_selection | 
                    ("NA" %in% input$cohort_year_selection & is.na(cohort_year)) )
              & ( cohort_semester %in% input$cohort_semester_selection | 
                    ("NA" %in% input$cohort_semester_selection & is.na(cohort_semester)) )
              & college_abbreviation %in% input$college_selection)
    return( students[, !(names(students) %in% column_excludes)] )
  })
  
  student_holds <- reactive({
    column_excludes <- c("is_enrolled", 
                         "college_abbreviation", 
                         "major",
                         "credit_hours_earned", 
                         "cumulative_gpa",
                         "term_gpa",
                         "student_email",
                         "student_phone",
                         "cohort_year",
                         "cohort_semester")
    df <- left_join(x=student_list(),
                           y=holds,
                           by="student_pidm" ) %>%
    filter( !is.na( hold ) )
    return( df[, !(names(df) %in% column_excludes)] )
  })
  
  output$student_holds_table <- renderDataTable({
      holds <- student_holds()
      table <- DT::datatable(holds[, !(names(holds) == "student_pidm")], 
                     extensions='Buttons', 
                     options=list( pageLength=15, 
                                   dom='Bfrtip',
                                   buttons=c('csv', 'excel') ),
                     rownames=FALSE,
                     colnames=c("Student", "Student ID", "Hold", "Hold Reason")) 
      return(table)
    }, 
    # allows for download of all data in datatable (not just the current page)
    server=FALSE)
  
   output$advisor_student_enrollment_table <- renderDataTable({
      students <- student_list()
      table <- DT::datatable(students[, !(names(students) == "student_pidm")], 
                     extensions='Buttons', 
                     options=list( pageLength=15, 
                                   dom='Bfrtip',
                                   buttons=c('csv', 'excel') ),
                     rownames=FALSE,
                     colnames=c("Student", "Student ID", "Email Address", "Cell Phone #",
                                "Cohort Year", "Cohort Semester", "College", "Major", 
                                "Cumulative GPA", "Term GPA", "Credits Earned", "Is Enrolled")) 
      return(table)
    }, 
    # allows for download of all data in datatable (not just the current page)
    server=FALSE) 
  
}