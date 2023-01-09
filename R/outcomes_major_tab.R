# ******************************************************************************
#                          OUTCOMES MAJOR TABS MODULE
# ******************************************************************************

source(here::here('modules', 'outcomes', 'minor_tabs', 'outcomes_minor_tab_categorical.R'))
source(here::here('modules', 'outcomes', 'minor_tabs', 'outcomes_minor_tab_time_based.R'))

# UI ####
outcomes_major_tab_UI <- function(id) {
  ns <- NS(id)

  ## UI elements ####
  tagList(
      titlePanel("First-Time Freshmen Graduation"),
      h4("A comparative view of graduation characteristics and progress.",
          style="color:#a6a6a6;"),
      br(),
      tabsetPanel(type = 'tabs',
                  tabPanel(HTML("By Cohorts") ,
                           outcomes_minor_tab_time_based_UI(ns("cohorts"))
                  ),
                  #tabPanel(HTML('By Initial Advisors'),
                  #         outcomes_minor_tab_categorical_UI(ns("advisors"))
                  #),
                  tabPanel(HTML('By Initial Colleges'),
                           outcomes_minor_tab_categorical_UI(ns("colleges"))
                  ),
                  tabPanel(HTML('By Initial Programs'),
                           outcomes_minor_tab_categorical_UI(ns("programs"))
                  )
      )
  )
  
}

# SERVER ####
outcomes_major_tab_server <- function(input, output, session, info_desc){
  callModule(outcomes_minor_tab_time_based_server, "cohorts")
  #callModule(outcomes_minor_tab_categorical_server, "advisors", agg_val="advisor_full_name", agg_val_desc="Initial Advisor")
  callModule(outcomes_minor_tab_categorical_server, "colleges", agg_val="college_abbreviation", agg_val_desc="Initial College")
  callModule(outcomes_minor_tab_categorical_server, "programs", agg_val="initial_program", agg_val_desc="Initial Program")
}