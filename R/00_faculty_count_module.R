# ******************************************************************************
#                           FACULTY COUNT MODULE
# ******************************************************************************

# LOAD DATA ####

# UI ####
faculty_count_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    wellPanel(
      h3("Faculty Efficiency"),
      p( "In this application, we investigate how effectively DSU uses it's instructor resources. 
          We investigate this through several metrics, such as the number of full-time equivalent students taught in a year and course fill rates. 
          We compute each of these metrics for the academic years 2015-2020.")
    ),
    h3("Current faculty by tenure status and course division."),
    summary_boxes_UI(ns("summary_boxes")),
    br(),
    br(),
    summary_plots_UI(ns("summary_plots"))
  )
}

# SERVER ####
faculty_count_server <- function(input, output, server, y_format){
  callModule(summary_boxes_server, "summary_boxes", agg_val="faculty_count", agg_format=y_format)
  callModule(summary_plots_server, "summary_plots", y=faculty_count, y_label="Faculty count", y_format=y_format)
}