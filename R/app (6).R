# LIBRARIES ####
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)
library(rlang)
library(here)
library(tidyverse)
library(purrr)
library(ggplot2)
library(plotly)
library(scales)

# SOURCE SCRIPTS ####
source(here::here('r', 'data_mung.R'))
source(here::here('r', 'standard_plot.R'))

## SOURCE MODULES ####
source(here::here('modules', 'global', 'summary_boxes.R'))
source(here::here('modules', 'global', 'summary_plots.R'))
source(here::here('modules', 'global', 'detail_plots.R'))
source(here::here('modules', 'global', 'major_tabs.R'))
source(here::here('modules', '00_faculty_count', '00_faculty_count_module.R'))

# UI ####
ui <- dashboardPage(skin = "purple",
                    title = "DataBlaze - Faculty Management",
                    dashboardHeader(
                      title = img(style = "align:top; margin:-15px -250px;",
                                  src="transparent-d-data-white.png",
                                  width="50",
                                  height="50",
                                  alt="Dixie Data"
                      )
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Current faculty count", tabName = 'faculty_count'),
                        menuItem("Credits taught", tabName = "credits_taught"),
                        menuItem("FTE", tabName = "fte"),
                        menuItem("Headcount", tabName = "headcount"),
                        menuItem("Maximum enrollment", tabName = "maximum_enrollment"),
                        menuItem("Fill Rate", tabName = "fill_rate"),
                        menuItem("Workload", tabName = "workload"),
                        menuItem("Overload", tabName = "overload")
                      )
                    ),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "dash_theme.css")
                      ),
                      tabItems(
                        tabItem('faculty_count', faculty_count_UI("faculty_count") ),
                        tabItem('credits_taught', major_tabs_UI("credits_taught", agg_val_desc="credit taught")),
                        tabItem('fte', major_tabs_UI("fte", agg_val_desc="FTE")),
                        tabItem('headcount', major_tabs_UI("headcount", agg_val_desc="head count")),
                        tabItem('maximum_enrollment', major_tabs_UI("maximum_enrollment", agg_val_desc="maximum enrollment")),
                        tabItem('fill_rate', major_tabs_UI("fill_rate", agg_val_desc="fill rate")),
                        tabItem('workload', major_tabs_UI("workload", agg_val_desc="workload")),
                        tabItem('overload', major_tabs_UI("overload", agg_val_desc="overload"))
                      )
                    )
)

# SERVER ####
server <- function(input, output) {
  callModule(faculty_count_server, "faculty_count", y_format=add_comma)
  callModule(major_tabs_server, "credits_taught", y=credit_hours, y_label="Credits taught", y_format=add_comma)
  callModule(major_tabs_server, "fte", y=fte, y_label="FTE", y_format=add_comma)
  callModule(major_tabs_server, "headcount", y=headcount, y_label="Headcount", y_format=add_comma)
  callModule(major_tabs_server, "maximum_enrollment", y=maximum_enrollment, y_label="Maximum Enrollment", y_format=add_comma)
  callModule(major_tabs_server, "fill_rate", y=fill_rate, y_label="Fill Rate", y_format=make_percent)
  callModule(major_tabs_server, "workload", y=workload, y_label="Workload", y_format=add_comma)
  callModule(major_tabs_server, "overload", y=overload, y_label="Overload", y_format=add_comma)
}

# CALL TO RUN APP ####
shinyApp(ui, server)
