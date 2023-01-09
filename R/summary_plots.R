by_instructor_group_academic_year <- readRDS(here::here('data', 'by_instructor_group_academic_year.rds'))
by_course_division_academic_year <- readRDS(here::here('data', 'by_course_division_academic_year.rds'))

summary_plots_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, plotlyOutput(ns('by_instructor_group_plot'), width=NULL) ),
      column(6, plotlyOutput(ns('by_course_division_plot'), width=NULL) ), 
    )
  )
}

summary_plots_server <- function(input, output, session, y, y_label, y_format) {
  output$by_instructor_group_plot <- renderPlotly({
      generate_standard_plot(by_instructor_group_academic_year, 
                    x=academic_year,
                    y={{y}},
                    y_format=y_format,
                    grouping=instructor_group,
                    x_label="Academic year",
                    y_label=y_label,
                    group_labeling=paste("Tenure status:", instructor_group),
                    title="Historical by tenure status",
                    legend_position="none")
  })
  output$by_course_division_plot <- renderPlotly({
      generate_standard_plot(by_course_division_academic_year, 
                x=academic_year,
                y={{y}},
                y_format=y_format,
                grouping=course_division,
                x_label="Academic year",
                y_label=y_label,
                group_labeling=paste("Course division", course_division),
                title="Historical by course division",
                legend_position="none")
  })
}