# ******************************************************************************
#                          RETENTION MAJOR TABS MODULE
# ******************************************************************************

source(here::here('modules', 'retention', 'minor_tabs', 'retention_minor_tab_categorical.R'))
source(here::here('modules', 'retention', 'minor_tabs', 'retention_minor_tab_time_based.R'))

# UI ####
retention_major_tab_UI <- function(id) {
  ns <- NS(id)
  
  ## UI elements ####
  tagList(
    titlePanel("First-Time Freshmen Retention"),
    h4("A comparative view of retention characteristics and progress.",
        style="color:#a6a6a6;"),
    br(),
    tabsetPanel(type = 'tabs',
                tabPanel(HTML('By Cohorts'),
                         retention_minor_tab_time_based_UI(ns("cohorts"))
                ),
                tabPanel(HTML('By Initial Advisors'),
                         retention_minor_tab_categorical_UI(ns("advisors"))
                ),
                tabPanel(HTML('By Initial Colleges'),
                         retention_minor_tab_categorical_UI(ns("colleges"))
                ),
                tabPanel(HTML('By Initial Programs'),
                         retention_minor_tab_categorical_UI(ns("programs"))
                )
    )
  )
  
}

# SERVER ####
retention_major_tab_server <- function(input, output, session, info_desc){
  callModule(retention_minor_tab_time_based_server, "cohorts")
  callModule(retention_minor_tab_categorical_server, "advisors", agg_val="advisor_full_name", agg_val_desc="Initial Advisor")
  callModule(retention_minor_tab_categorical_server, "colleges", agg_val="college_abbreviation", agg_val_desc="Initial College")
  callModule(retention_minor_tab_categorical_server, "programs", agg_val="initial_program", agg_val_desc="Initial Program")
}