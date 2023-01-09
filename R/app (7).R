library(shiny)
library(shinydashboard)
library(DT)
library(here)

source(here::here("db_ops.R"))

source(here::here('modules', 'job_data_entry.R'))
source(here::here('modules', 'jobs_summary.R'))


ui <- dashboardPage(
    skin="purple",
    title = "DataBlaze - Career Services Jobs",
    # header
    dashboardHeader(
      title = img(style = "align:top; margin:-15px -250px;",
                  src="transparent-d-data-white.png",
                  width="50",
                  height="50",
                  alt="Dixie Data"
      )
    ),
    # sidebar
    dashboardSidebar(                      
      sidebarMenu(
      menuItem("Job Data Entry", tabName = 'job_data_entry'),
      menuItem("Jobs Summary ", tabName = "jobs_summary")
      )
    ), 
    # body
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "dash_theme.css")
      ),
      tabItems(
        tabItem('job_data_entry', job_data_entry_UI("job_data_entry") ),
        tabItem('jobs_summary', jobs_summary_UI("jobs_summary") )
      )
   )
)

server <- function(input, output) {
  callModule(job_data_entry_server, "job_data_entry")
  callModule(jobs_summary_server, "jobs_summary")
}

shinyApp(ui=ui, server=server)