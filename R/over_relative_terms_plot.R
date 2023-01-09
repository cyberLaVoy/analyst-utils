
over_relative_terms_plot_UI <- function(id, info_desc) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    fluidRow(
      column(3, wellPanel( uiOutput(ns('category_selection_ui')) ) ),
      column(9, plotlyOutput(ns('over_time_plot'), width=NULL) )
    ),
  )
  
}

over_relative_terms_plot_server <- function(input, output, session, agg_val, agg_val_desc, input_plot_df, info_desc) {
  ns <- session$ns 
  
  output$category_selection_ui <- renderUI({
    categories <- input_plot_df[[agg_val]] %>%
      unique() %>%
      sort()
    selected_count = min(length(categories), 5)
    pickerInput(ns("category_selector"), paste(agg_val_desc, "Filter"), 
                categories, 
                options=list(`actions-box`=TRUE, `live-search`=TRUE), 
                multiple=TRUE,
                selected=sample(categories, selected_count))
  })  

  # OVER TIME FUNCTIONALLITY ####
  over_time_plot_df <- reactive({
     # Pause plot execution while input values evaluate. This eliminates an error message.
     req(input$category_selector)
 
     plot_df <- input_plot_df %>%
        filter(!!sym(agg_val) %in% input$category_selector &
               relative_term_index <= 12) %>%
        group_by_at( c(agg_val, 
                       "relative_term_index",
                       "relative_term_desc") ) %>%
        summarize( y_plot=n_distinct(pidm)) %>%
        drop_na() %>%
        mutate(grouping=!!sym(agg_val),
               x_plot=relative_term_index ) %>%
        ungroup()     
     # Pause plot execution if df has no values. This eliminates an error message.
     req( nrow(plot_df) > 0 )
     return( plot_df )
  })
  output$over_time_plot <- renderPlotly({
  
    generate_standard_plot(over_time_plot_df(),
                           x=x_plot,
                           y=y_plot,
                           grouping=grouping,
                           x_label="Term",
                           y_label=paste(info_desc, "Headcount"), 
                           y_format=add_comma,
                           x_format=relative_to_actual_semester,
                           group_labeling=paste(agg_val_desc, ": ", !!sym(agg_val), "</br>",
                                                sep=''),
                           title=paste(info_desc, "Headcounts Over Time"),
                           sub_title="Where we examine the progress of cohorts."
                           #,legend_title=agg_val_desc
                           )
  })

}