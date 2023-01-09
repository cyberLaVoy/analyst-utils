# ******************************************************************************
#                   OUTCOMES MINOR TAB TIME BASED MODULE
# ******************************************************************************

# UI ####
outcomes_minor_tab_time_based_UI <- function(id) {
  ns <- NS(id)
  
  ## UI elements ####
  tagList(
      br(),
      br(),
      over_relative_terms_plot_UI(ns("relative_terms")),
      br(),
      br(),
      over_cohorts_plot_UI(ns("cohorts"))
    )
  
}

# SERVER ####
outcomes_minor_tab_time_based_server <- function(input, output, session){
  input_plot_df <- outcomes_plot_df
  info_desc <- "Graduation"
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