# ******************************************************************************
#                          CURRENT MAJOR TABS MODULE
# ******************************************************************************

source(here::here('modules', 'current', 'minor_tabs', 'current_minor_tab_enrollment_rates.R'))
source(here::here('modules', 'current', 'minor_tabs', 'current_minor_tab_enrollment_list.R'))

# UI ####
current_major_tab_UI <- function(id) {
  ns <- NS(id)

  ## UI elements ####
  tagList(
      fluidRow(
        column(6, 
          titlePanel("Current Enrollment (All Students)"),
          h4("Where we see a live feed of student enrollment from one semester to the next, 
              with recent and expected graduates removed.",
              style="color:#a6a6a6;")
        ),
        column(6,
          h4("University Enrollment Progress - Fall 2021  "),
          progressBar(ns("university_enrollment_progress"), 
                      value=0,
                      display_pct=TRUE,
                      status="custom"),
          tags$style(".progress-bar-custom { background-color: #881518;
                                             text-align: center;
                                             font-size: 20px;
                                             padding: 2px; }
                      .progress { height: 25px; 
                                  margin-bottom: 10px;
                                  margin-right: 25px; }"),
          div(
            textOutput( ns("university_enrollment_values") ),
                       style="font-size: 1.6rem;"
          ),
          style="padding-top: 20px;"
        )
      ),
      br(),
      tabsetPanel(type = 'tabs',
                  tabPanel(HTML("Enrollment Rates") ,
                           current_minor_tab_enrollment_rates_UI(ns("rates"))
                  )
                  ,tabPanel(HTML("Enrollment List"),
                           current_minor_tab_enrollment_list_UI(ns("list"))
                  )
      )
  )
  
}

# SERVER ####
current_major_tab_server <- function(input, output, session){
  ns <- session$ns
  callModule(current_minor_tab_enrollment_rates_server, "rates", input_plot_df=advisor_student_enrollment)
  callModule(current_minor_tab_enrollment_list_server, "list", input_plot_df=advisor_student_enrollment)
  
  # university enrollment progress update
  university_enroll_rate <- advisor_student_enrollment %>%
    summarize( rate_metric_count=n_distinct(student_pidm[is_enrolled == "True"]),
               initial_headcount=n_distinct(student_pidm) ) %>%
    mutate( rate_metric=rate_metric_count/initial_headcount )
  
  updateProgressBar(session=session,
                    id=ns("university_enrollment_progress"),
                    value=university_enroll_rate[["rate_metric"]]*100)
  
  output$university_enrollment_values <- renderText({
    total_enrolled_students <- university_enroll_rate[["rate_metric_count"]]
    population_total <- university_enroll_rate[["initial_headcount"]]
    return( paste(total_enrolled_students, '/', population_total) )
  })
  
}