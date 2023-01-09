# ******************************************************************************
#                   RETENTION MINOR TAB TIME BASED MODULE
# ******************************************************************************

# UI ####
retention_minor_tab_time_based_UI <- function(id) {
  ns <- NS(id)
  
  ## UI elements ####
  tagList(
      br(),
      br(),
      cohort_metric_bar_chart_UI(ns("current_retention")),
      br(),
      br(),     
      over_relative_terms_plot_UI(ns("relative_terms")),
      br(),
      br(),
      over_cohorts_plot_UI(ns("cohorts"))
    )
  
}

# SERVER ####
retention_minor_tab_time_based_server <- function(input, output, session){
  input_plot_df <- retention_plot_df
  info_desc <- "Retention"
  callModule(cohort_metric_bar_chart_server, 
             "current_retention",
             current_term_code=202140, 
             metric_term_col="retained_term", 
             metric_desc="Retained",
             input_plot_df=input_plot_df, 
             info_desc)
  callModule(over_relative_terms_plot_server, 
             "relative_terms", 
             agg_val="cohort_year", 
             agg_val_desc="Cohort Year", 
             input_plot_df=input_plot_df, 
             info_desc=info_desc)
  callModule(over_cohorts_plot_server, 
             "cohorts", 
             agg_val="cohort_year", 
             agg_val_desc="Cohort Year", 
             input_plot_df=input_plot_df, 
             info_desc=info_desc)
}