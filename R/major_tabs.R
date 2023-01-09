# ******************************************************************************
#                           MAJOR TABS MODULE
# ******************************************************************************

# LOAD DATA ####
by_department <- readRDS(here::here('data', 'by_college_abbreviation_department_academic_year.rds'))
by_instructor_group_department <- readRDS(here::here('data', 'by_instructor_group_college_abbreviation_department_academic_year.rds'))
by_course_division_department <- readRDS(here::here('data', 'by_course_division_college_abbreviation_department_academic_year.rds'))
by_subject <- readRDS(here::here('data', 'by_college_abbreviation_department_subject_academic_year.rds'))
by_instructor_group_subject <- readRDS(here::here('data', 'by_instructor_group_college_abbreviation_department_subject_academic_year.rds'))
by_course_division_subject <- readRDS(here::here('data', 'by_course_division_college_abbreviation_department_subject_academic_year.rds'))
by_individual <- readRDS(here::here('data', 'by_banner_id_full_name_academic_year.rds'))
by_instructor_group_individual <- readRDS(here::here('data', 'by_instructor_group_banner_id_full_name_academic_year.rds'))
by_course_division_individual <- readRDS(here::here('data', 'by_course_division_banner_id_full_name_academic_year.rds'))

# UI ####
major_tabs_UI <- function(id, agg_val_desc='') {
  ns <- NS(id)
  
  ## UI elements ####
  tagList(
    tabsetPanel(type = 'tabs',
                # Summary panel ####
                tabPanel(HTML("<b>Summary</b>"),
                         h3(paste("Current", agg_val_desc, "by tenure status and course division.")),
                         summary_boxes_UI(ns("summary_boxes")),
                         br(),
                         br(),
                         summary_plots_UI(ns("summary_plots"))
                ),
                # End ====
                tabPanel(HTML('All faculty and courses'),
                         detail_plots_UI(ns("detail_plots_all"))
                ),
                tabPanel(HTML("Adjunct faculty"),
                         detail_plots_UI(ns("detail_plots_adjunct"))
                ),
                tabPanel(HTML("Tenure-track faculty"), 
                         detail_plots_UI(ns("detail_plots_tenure_track"))
                ),
                tabPanel(HTML("Non-tenure-track faculty"), 
                         detail_plots_UI(ns("detail_plots_non_tenure_track"))
                ),
                tabPanel(HTML('Lower division'), 
                         detail_plots_UI(ns("detail_plots_lower_division"))
                ),
                tabPanel(HTML('Upper division'), 
                         detail_plots_UI(ns("detail_plots_upper_division"))
                )
    )
  )
}

# SERVER ####
major_tabs_server <- function(input, output, server, y, y_label, y_format){
  callModule(summary_boxes_server, "summary_boxes", agg_val={{y}}, agg_format=y_format)
  callModule(summary_plots_server, "summary_plots", y={{y}}, y_label=y_label, y_format=y_format)
  callModule(detail_plots_server, 
             "detail_plots_all", 
             by_department_df=by_department,
             by_subject_df=by_subject,
             by_individual_df=by_individual,
             y={{y}}, 
             y_label=y_label,
             y_format=y_format)
  callModule(detail_plots_server, 
             "detail_plots_adjunct", 
             by_department_df=filter(by_instructor_group_department, instructor_group=="Adjunct"),
             by_subject_df=filter(by_instructor_group_subject, instructor_group=="Adjunct"),
             by_individual_df=filter(by_instructor_group_individual, instructor_group=="Adjunct"),
             y={{y}}, 
             y_label=y_label,
             y_format=y_format)
  callModule(detail_plots_server, 
             "detail_plots_non_tenure_track", 
             by_department_df=filter(by_instructor_group_department, instructor_group=="Non-tenure-track"),
             by_subject_df=filter(by_instructor_group_subject, instructor_group=="Non-tenure-track"),
             by_individual_df=filter(by_instructor_group_individual, instructor_group=="Non-tenure-track"),
             y={{y}}, 
             y_label=y_label,
             y_format=y_format)
  callModule(detail_plots_server, 
             "detail_plots_tenure_track", 
             by_department_df=filter(by_instructor_group_department, instructor_group=="Tenure-track"),
             by_subject_df=filter(by_instructor_group_subject, instructor_group=="Tenure-track"),
             by_individual_df=filter(by_instructor_group_individual, instructor_group=="Tenure-track"),
             y={{y}}, 
             y_label=y_label,
             y_format=y_format)
  callModule(detail_plots_server, 
             "detail_plots_lower_division", 
             by_department_df=filter(by_course_division_department, course_division=="Lower"),
             by_subject_df=filter(by_course_division_subject, course_division=="Lower"),
             by_individual_df=filter(by_course_division_individual, course_division=="Lower"),
             y={{y}}, 
             y_label=y_label,
             y_format=y_format)
  callModule(detail_plots_server, 
             "detail_plots_upper_division", 
             by_department_df=filter(by_course_division_department, course_division=="Upper"),
             by_subject_df=filter(by_course_division_subject, course_division=="Upper"),
             by_individual_df=filter(by_course_division_individual, course_division=="Upper"),
             y={{y}}, 
             y_label=y_label,
             y_format=y_format)
}