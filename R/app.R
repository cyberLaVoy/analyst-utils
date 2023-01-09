library(shiny)
library(shinydashboard)
library(DT)
library(here)

source(here::here("rscripts", "data_pull.R"))

source(here::here('modules', 'programs_summary.R'))
source(here::here('modules', 'programs_data.R'))


ui <- dashboardPage(
    skin="purple",
    title = "DataBlaze - University Programs",
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
      menuItem("Programs Summary", tabName = 'programs_summary'),
      menuItem("Programs Data", tabName = 'programs_data')
      )
    ), 
    # body
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "dash_theme.css")
      ),
      tabItems(
        tabItem('programs_summary', programs_summary_UI("programs_summary") ),
        tabItem('programs_data', programs_data_UI("programs_data") )
      )
   )
)

server <- function(input, output) {
  callModule(programs_summary_server, "programs_summary")
  callModule(programs_data_server, "programs_data")
}

shinyApp(ui=ui, server=server)