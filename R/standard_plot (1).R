make_percent <- function(x) {
  return( percent(x, drop0trailing=TRUE) )
}
add_comma <- function(x) {
  return( format(x, big.mark=',', scientific=FALSE) )
}
no_format <- function(x) {
  return( x )
}

generate_standard_plot <- function(df, x, y, grouping, x_label, y_label, group_labeling, y_format=no_format, title='', sub_title='', legend_title='', legend_position="right") {
  ggplot_object <- ggplot(df, aes(x={{x}}, 
                                  y={{y}}, 
                                  group={{grouping}}, 
                                  color={{grouping}},
                                  text=paste(paste(x_label, ": ", sep=''), {{x}}, "<br />", 
                                             paste(y_label, ": ", sep=''), y_format({{y}}), "<br />",
                                             {{group_labeling}},
                                             sep='') ) ) + 
    geom_line() + 
    geom_point(alpha=.8) +
    scale_y_continuous( labels=y_format ) +
    guides( color=guide_legend( title=legend_title ) ) +
    labs(x=x_label,
         y=y_label) +
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position=legend_position )
  plot <- ggplotly(ggplot_object, tooltip=c('text')) %>%
    config(displayModeBar=FALSE) %>%
    layout(title = list(text = paste0(title,
                                      '<br>',
                                      '<sup style="color:#a6a6a6;">',
                                      sub_title,
                                      '</sup>')))
  return( plot )
}

# EXAMPLE USE ####
#by_instructor_group_department_academic_year <- readRDS(here::here('data', 'by_instructor_group_department_academic_year.rds'))
#generate_standard_plot(by_instructor_group_department_academic_year, 
#                       x=academic_year,
#                       y=fte,
#                       grouping=paste(instructor_group, department, sep='_'),
#                       x_label="Academic year",
#                       y_label="FTE",
#                       y_format=add_comma,
#                       group_labeling=paste("Instructor group:", instructor_group, "<br />",
#                                            "Department:", department),
#                       title="Historical by course division",
#                       legend_position="none")
