# ******************************************************************************
#                   OUTCOMES MINOR TAB CATEGORICAL MODULE
# ******************************************************************************

# UI ####
outcomes_minor_tab_categorical_UI <- function(id, info_desc) {
  ns <- NS(id)
  
  ## UI elements ####
  tagList( br(),
           br(),
           rate_metric_UI(ns("rate_metric"))
  )
  
}

# SERVER ####
outcomes_minor_tab_categorical_server <- function(input, output, session, agg_val, agg_val_desc){
  input_plot_df = outcomes_plot_df
  info_desc = "Graduation"
  callModule(rate_metric_server, 
             "rate_metric", 
             agg_val=agg_val, 
             agg_val_desc=agg_val_desc, 
             input_plot_df=input_plot_df, 
             info_desc=info_desc)
}