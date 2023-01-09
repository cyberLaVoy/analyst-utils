
desert_sand <- c(
  "#E6CCB3",
  "#B8A38F",
  "#938272",
  "#76685B",
  "#5E5349",
  "#4B423A")

rock_reds <-c(
  "#BA1C21",
  "#95161A",
  "#771215",
  "#5F0E11",
  "#4C0B0E",
  "#3D090B")

default_colors <- c( desert_sand, rock_reds, 
                     desert_sand, rock_reds,
                     desert_sand, rock_reds, 
                     desert_sand, rock_reds, 
                     desert_sand, rock_reds,
                     desert_sand, rock_reds,
                     desert_sand, rock_reds, 
                     desert_sand, rock_reds,
                     desert_sand, rock_reds, 
                     desert_sand, rock_reds, 
                     desert_sand, rock_reds,
                     desert_sand, rock_reds,
                     desert_sand, rock_reds, 
                     desert_sand, rock_reds,
                     desert_sand, rock_reds, 
                     desert_sand, rock_reds, 
                     desert_sand, rock_reds,
                     desert_sand, rock_reds,
                     desert_sand, rock_reds, 
                     desert_sand, rock_reds,
                     desert_sand, rock_reds, 
                     desert_sand, rock_reds, 
                     desert_sand, rock_reds,
                     desert_sand, rock_reds )

get_ggplot_object <- function(data, aes, color_palette=default_colors) {
  ggplot_object <- ggplot(data, aes) + 
    geom_line() +
    #xlim(terms_include) +
    geom_smooth(method="lm", fullrange=TRUE, linetype="dashed", size=.5, se=F) +
    geom_point() +
    scale_color_manual( values=color_palette ) +
    labs(x="Term", y="FTE") +
    theme(legend.position="none") +
    theme_minimal() +
    theme(panel.grid.minor.y=element_blank(),
          legend.position="none")
  return(ggplot_object)
}

plotlify <- function(ggplot_object) {
  plot <- ggplotly(ggplot_object, tooltip = c('text')) %>%
    config(displayModeBar = FALSE) %>%
    config(showLink = FALSE) %>%
    layout(margin=list(l = 100), yaxis=list(tickprefix=" ", autorange = TRUE), xaxis=list(autorange = TRUE))
    return(plot)
}

get_plot <- function(data, aes) {
  plot <- plotlify(get_ggplot_object(data, aes))
  return(plot)
}
