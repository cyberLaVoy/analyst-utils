
cohort_metric_bar_chart_UI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("cohort_metric_plot"))
}

cohort_metric_bar_chart_server <- function(input, output, session, current_term_code, metric_term_col, metric_desc, input_plot_df, info_desc) {
  ns <- session$ns 
  output$cohort_metric_plot <- renderPlotly({
      df <- input_plot_df %>%
        mutate( numerator_metric = (!!sym(metric_term_col)==current_term_code) ) %>%
        group_by( cohort_year ) %>%
        summarize( population_headcount = n_distinct(pidm),
                   numerator_metric = sum(numerator_metric) ) %>%
        mutate( y_plot = numerator_metric / population_headcount )
        
      generate_grouped_bar_plot(df,
                             x=cohort_year,
                             y=y_plot,
                             x_label="Cohort Year",
                             y_label=paste(info_desc, "Rate"),
                             y_format=make_percent,
                             grouping=as.character(cohort_year), 
                             group_labeling=paste(paste(metric_desc, " Count: "), numerator_metric, "<br>",
                                                  "Population Total: ", population_headcount, "<br>",
                                                  sep=''),
                             title="Current Retention Rates By Cohort Year",
                             sub_title="Fall 2021 retention rates"
                             )      
  })

}