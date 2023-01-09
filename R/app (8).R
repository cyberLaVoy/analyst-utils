# LIBRARIES ####
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinydashboardPlus)
library(rlang)
library(here)
library(tidyverse)
library(DT)

# SOURCE SCRIPTS ####
#source(here::here('rscripts', 'pull.R'))
source(here::here('rscripts', 'on_load_pull.R'))
source(here::here('rscripts', 'data_mung.R'))
source(here::here('rscripts', 'standard_plot.R'))

## SOURCE MODULES ####
source(here::here('modules', 'global', 'rate_metric_bar_chart.R'))
source(here::here('modules', 'global', 'over_cohorts_plot.R'))
source(here::here('modules', 'global', 'over_relative_terms_plot.R'))
source(here::here('modules', 'global', 'cohort_metric_bar_chart.R'))
source(here::here('modules', 'retention', 'major_tabs', 'retention_major_tab.R'))
source(here::here('modules', 'outcomes', 'major_tabs', 'outcomes_major_tab.R'))
source(here::here('modules', 'current', 'major_tabs', 'current_major_tab.R'))

css <- '.nav-tabs>li>a {
  color: #991518;
}'

# UI ####
ui <- dashboardPage(skin = "purple",
                    title = "DataBlaze - Retention",
                    dashboardHeader(
                      title = img(style="align:top; margin:-15px -250px;",
                                  src="transparent-d-data-white.png",
                                  width="50",
                                  height="50",
                                  alt="Dixie Data"
                      )
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Current Enrollment", tabName="current"),
                        menuItem("Retention", tabName="retention"),
                        menuItem("Graduation", tabName="outcomes")
                      )
                    ),
                    dashboardBody(
                      tags$head(
                        tags$link(rel="stylesheet", type="text/css", href="dash_theme.css")
                      ),
                      tags$head(tags$style(HTML(css))),
                      tabItems(
                        tabItem('current', current_major_tab_UI("current")),
                        tabItem('retention', retention_major_tab_UI("retention")),
                        tabItem('outcomes', outcomes_major_tab_UI("outcomes"))
                      )
                    )
)

# SERVER ####
server <- function(input, output) {
  callModule(current_major_tab_server, "current")
  callModule(retention_major_tab_server, "retention")
  callModule(outcomes_major_tab_server, "outcomes")
}

# CALL TO RUN APP ####
shinyApp(ui, server)