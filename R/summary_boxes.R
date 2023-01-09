by_instructor_group <- readRDS(here::here('data', 'by_instructor_group.rds'))
by_course_division <- readRDS(here::here('data', 'by_course_division.rds'))

summary_boxes_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, valueBoxOutput(ns('adjunct_box'), width = NULL)),
      column(4, valueBoxOutput(ns('non_tenure_track_box'), width = NULL)), 
      column(4, valueBoxOutput(ns('tenure_track_box'), width = NULL)) 
    ),
    fluidRow(
      column(2,),
      column(4, valueBoxOutput(ns('lower_division_box'), width = NULL)),
      column(4, valueBoxOutput(ns('upper_division_box'), width = NULL)),
      column(2,) 
    )
  )
}

summary_boxes_server <- function(input, output, session, agg_val, agg_format) {
  agg_vals <- c(
    by_instructor_group %>%
      filter(instructor_group == "Adjunct") %>% 
      select({{agg_val}}),
    by_instructor_group %>% 
      filter(instructor_group == "Non-tenure-track") %>% 
      select({{agg_val}}),
    by_instructor_group %>% 
      filter(instructor_group == "Tenure-track") %>% 
      select({{agg_val}}),
    by_course_division %>%
      filter(course_division == "Lower") %>% 
      select({{agg_val}}),
    by_course_division %>% 
      filter(course_division == "Upper") %>% 
      select({{agg_val}})
  )
  names( agg_vals ) <- c("adjunct", "non_tenure_track", "tenure_track", "lower", "upper") 
  
  output$adjunct_box <- renderValueBox({
    valueBox(
      agg_format(as.numeric(agg_vals["adjunct"])), 
      "Adjunct faculty", 
      icon = icon("user-graduate"), 
      color = 'teal'
    )
  })
  output$non_tenure_track_box <- renderValueBox({
    valueBox(
      agg_format(as.numeric(agg_vals["non_tenure_track"])), 
      "Non-tenure-track faculty", 
      icon = icon("user-graduate"), 
      color = 'blue'
    )
  })
  output$tenure_track_box <- renderValueBox({
    valueBox(
      agg_format(as.numeric(agg_vals["tenure_track"])), 
      "Tenure-track faculty", 
      icon = icon("user-graduate"), 
      color = 'green'
    )
  })
  output$lower_division_box <- renderValueBox({
    valueBox(
      agg_format(as.numeric(agg_vals["lower"])), 
      "Lower division", 
      icon = icon("user-graduate"), 
      color = 'maroon'
    )
  })
  output$upper_division_box <- renderValueBox({
    valueBox(
      agg_format(as.numeric(agg_vals["upper"])), 
      "Upper division", 
      icon = icon("user-graduate"), 
      color = 'fuchsia'
    )
  }) 
}