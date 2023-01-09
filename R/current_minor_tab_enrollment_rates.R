# ******************************************************************************
#                   CURRENT MINOR TAB ENROLLMENT RATES 
# ******************************************************************************

# UI ####
current_minor_tab_enrollment_rates_UI <- function(id, info_desc) {
  ns <- NS(id)
  
  ## UI elements ####
  tagList( 
           br(),
           fluidRow(
             column(3, pickerInput(ns("agg_select"),  
                                   "Grouping Selection", 
                                   c("By Advisor"='advisor_full_name', 
                                     # Line removed to avoid confusion that has occurred with the resulting visual
                                     #"By Cohort Year"='cohort_year', 
                                     "By College"='college_abbreviation'), 
                                   options=list(`actions-box`=TRUE), 
                                   # must be a single selection 
                                   multiple=FALSE,
                                   selected="college_abbreviation")
             )
             # Lines removed to avoid confusion that has occurred with the resulting visual
             # ,column(3, pickerInput(ns("cohort_semester_filter"),
             #                       "Cohort Semester Filter",
             #                       c("Fall", "Spring"),
             #                       options=list(`actions-box`=TRUE), 
             #                       multiple=TRUE,
             #                       selected=c("Fall", "Spring"))
             # )
           ),
           plotlyOutput(ns("rate_metric_plot"))
  )
  
}

# SERVER ####
current_minor_tab_enrollment_rates_server <- function(input, output, session, input_plot_df){
    
    metric_rate_plot_df <- reactive({
     plot_df <- input_plot_df %>%
       # Line removed to avoid confusion that has occurred with the resulting visual
        #filter( cohort_semester_category %in% input$cohort_semester_filter ) %>%
        group_by_at( c(input$agg_select) ) %>%
        summarize( rate_metric_count=n_distinct(student_pidm[is_enrolled == "True"]),
                   initial_headcount=n_distinct(student_pidm) ) %>%
        mutate( y_plot=rate_metric_count/initial_headcount,
                x_plot=!!sym(input$agg_select),
                grouping=if_else(rate_metric_count == 0, "Empty", "Other")) %>%
        drop_na() %>%
        ungroup()       
     # allows for a negative value on bar chart for display
     plot_df$y_plot[plot_df$y_plot == 0] <- -.025
     # Pause plot execution if df has no values. This eliminates an error message.
     req( nrow(plot_df) > 0 )
     return( plot_df ) 
  })
  
  output$rate_metric_plot <- renderPlotly({
      agg_desc <- c("advisor_full_name"="Advisor", "cohort_year"="Cohort Year", "college_abbreviation"="College")
      generate_grouped_bar_plot(metric_rate_plot_df(),
                             x=x_plot,
                             y=y_plot,
                             x_label=agg_desc[input$agg_select],
                             y_label="Enrollment Rate",
                             y_format=make_percent_negative_to_zero,
                             group_labeling= paste("Enrolled Count: ", rate_metric_count, "<br>",
                             "Population Total: ", initial_headcount, "<br>",
                             sep=''),
                             grouping=grouping,
                             title=paste("Enrollment Rates by", agg_desc[input$agg_select]),
                             sub_title="Spring 2021 to Fall 2021 Fill Rates",
                             legend_position="none",
                             plot_height=800
                             )      
  })

}