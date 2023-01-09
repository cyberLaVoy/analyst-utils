library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(here)
library(tidyverse)
library(ggplot2)
library(plotly)

source(here::here('rscripts',  'data_mung.R'))
source(here::here('rscripts',  'standard_plot.R'))

source(here::here('modules', '01_fte_projection_university', 'fte_projection_university_module.R'))
source(here::here('modules', '02_fte_projection_by_college', 'fte_projection_by_college_module.R'))
source(here::here('modules', '03_fte_projection_by_department', 'fte_projection_by_department_module.R'))
source(here::here('modules', '04_fte_projection_by_subject', 'fte_projection_by_subject_module.R'))

ui <- dashboardPage(skin = "purple",
                    title = "DataBlaze - FTE Projection",
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
                        menuItem("University", tabName='university_tab'),
                        menuItem("By College", tabName='by_college_tab'),
                        menuItem("By Department", tabName='by_department_tab'),
                        menuItem("By Subject", tabName='by_subject_tab')
                      )
                    ),
                    dashboardBody(
                      tags$head(
                        tags$link(rel="stylesheet", type="text/css", href="dash_theme.css")
                      ),
                      tabItems(
                        tabItem('university_tab', University_UI('university')),
                        tabItem('by_college_tab', By_College_UI('by_college')),
                        tabItem('by_department_tab', By_Department_UI('by_department')),
                        tabItem('by_subject_tab', By_Subject_UI('by_subject'))
                      )
                    )  
)

server <- function(input, output, session) {
  callModule(university_server, "university")
  callModule(by_college_server, "by_college")
  callModule(by_department_server, "by_department")
  callModule(by_subject_server, "by_subject")
}

shinyApp(ui, server)
