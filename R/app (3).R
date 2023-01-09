library(shiny)
library(shinythemes)
library(DT)
library(bslib)
library(janitor)
library(dplyr)

source(here::here('rscripts', 'tile_util.R'))
source(here::here('rscripts', 'mung.R'))

litera <- bslib::bs_theme(bootswatch = "litera",
                          bg = "#FFFFFF", fg = "#000",
                          primary = "#B5302A",
                          base_font = bslib::font_google("Source Serif Pro"),
                          heading_font = bslib::font_google("Josefin Sans", wght = 100))

# UI ####
ui <- navbarPage(theme=litera,
                 title = div( style = 'text-align: justify; width:150;',
                              tags$img(style = 'display: block;
                                                margin-left:-20px;
                                                margin-top:-10px;
                                                margin-bottom:-20px',
                                       src = "ie_logo.png",
                                       width="170",
                                       height="50",
                                       alt="UT Data") ),
                 includeCSS("www/style.css"),
    # Workload Summary ####
    tabPanel("Faculty Workload Summary",
             fluidRow(
               column(3,
                 uiOutput('college_selection_ui'),
               ),
               column(3,
                 uiOutput('department_selection_ui')
               ),
               column(3,
                 pickerInput("status_selection_workload_exploration",  "Faculty Status Selection",
                             all_status,
                             options=list(`actions-box`=TRUE, `live-search`=TRUE),
                             multiple=TRUE,
                             selected=all_status)
               ),
               column(3,
                 pickerInput("rank_selection_workload_exploration",  "Faculty Rank Selection",
                             all_ranks,
                             options=list(`actions-box`=TRUE, `live-search`=TRUE),
                             multiple=TRUE,
                             selected=all_ranks)
               )        
             ), 
             fluidRow(
               column(4,
                 radioButtons("time_aggregator", "Aggregate time by:",
                               choices=list("Academic Year"="academic_year",
                                            "Term"="term"),
                               selected="academic_year") 
               )
             ),               
             p("Select rows to expand and see more details in the 'Courses Taught' and 'Non-Instructional' tables below."),
             DT::dataTableOutput("workload_summary_table"),
           
             h3("Courses Taught"),
             DT::dataTableOutput("instructional_workload_expansion_table"),
           
             h3("Non-Instructional"),
             DT::dataTableOutput("non_instructional_workload_expansion_table")
    ),
    # Search By Indvidual ####
    tabPanel("Search Faculty by Individual",
             fluidRow( 
               column(4,
                  wellPanel(
                   uiOutput('faculty_selection_ui'),
                   uiOutput('terms_selection_ui')
                  )
               )
             ),
             fluidRow( 
               column(6,
                      uiOutput('instructional_individual_search_tiles')
               ),
               column(6,
                      uiOutput('non_instructional_individual_search_tiles')
               )     
             )
    ),
    # Full Datasets ####
    tabPanel("Dataset - Instructional Workload",
             h3("Instructional Workload"),
             DT::dataTableOutput("instructional_workload_table")
    ),
    tabPanel("Dataset - Non-Instructional Workload",
             h3("Non-Instructional Workload"),
             DT::dataTableOutput("non_instructional_workload_table")
    ),
    tabPanel("Data Integrity Issues",
            h3("Missing Designated College"),
            DT::dataTableOutput("missing_designated_college_table"),
          
            h3("Missing Designated Department"),
            DT::dataTableOutput("missing_designated_department_table"),
          
            h3("Missing Ranks"),
            DT::dataTableOutput("missing_ranks_table"),

            h3("Duplicate Ranks"),
            DT::dataTableOutput("duplicate_ranks_table"),
             
            h3("Missing Statuses"),
            DT::dataTableOutput("missing_statuses_table"),
                      
            h3("Duplicate Statuses"),
            DT::dataTableOutput("duplicate_statuses_table"),
          
            h3("Missing Contracted Workload"),
            DT::dataTableOutput("missing_contracted_workload_table")
  )
)

# Server ####
server <- function(input, output) {
  
  # selection UI elements ####
  # colleges
  output$college_selection_ui <- renderUI({
    pickerInput("college_selection",  "Faculty College Selection", 
                all_colleges, 
                options=list(`actions-box`=TRUE, `live-search`=TRUE), 
                multiple=TRUE,
                selected=all_colleges)
  })
  # departments
  output$department_selection_ui <- renderUI({
    faculty_base <- summarized_faculty_workload_by_term_df %>%
      dplyr::filter( faculty_college %in% input$college_selection )   
    departments <- sort(unique(faculty_base$faculty_department))
    pickerInput("department_selection",  "Faculty Department Selection", 
                departments, 
                options=list(`actions-box`=TRUE, `live-search`=TRUE), 
                multiple=TRUE,
                selected=departments)
  })
  ## only used for individual search ####
  # individual
  output$faculty_selection_ui <- renderUI({
    faculty_base <- instructional_faculty_workload_df
    faculty <- sort(unique(faculty_base$faculty_info))   
    pickerInput("faculty_selection",  "Faculty Member Selection", 
                faculty, 
                options=list(`actions-box`=TRUE, `live-search`=TRUE), 
                multiple=FALSE)
  })  
  # terms
  output$terms_selection_ui <- renderUI({
    terms_base <- instructional_faculty_workload_df %>%
      dplyr::filter(faculty_info %in% input$faculty_selection)
    terms <- sort(unique(terms_base$term))
    pickerInput("terms_selection",  "Term Selection", 
                terms, 
                options=list(`actions-box`=TRUE, `live-search`=TRUE), 
                multiple=TRUE,
                selected=terms)
  }) 
  
  # WORKLOAD SUMMARY ####  
  workload_exploration_df_reactive <- reactive({
    faculty_base <- faculty_term_df %>%
      dplyr::filter( faculty_college %in% input$college_selection &
              faculty_department %in% input$department_selection )
    faculty_include <- sort(unique(faculty_base$faculty_info))   
    get_workload_exploration_df(input$time_aggregator, faculty_include) %>%
      dplyr::filter( faculty_status %in% input$status_selection_workload_exploration &
              faculty_rank %in% input$rank_selection_workload_exploration ) %>%
      dplyr::select(-faculty_info)
  })
  output$workload_summary_table <- DT::renderDataTable(workload_exploration_df_reactive(),
                                                       filter="top",
                                                       options=list( scrollX = TRUE,
                                                                     lengthMenu=list( c(5, 10, 50, -1), 
                                                                                      c(5, 10, 50, "All") )
                                                                   ),
                                                       rownames=FALSE,
                                                       colnames=colnames( janitor::clean_names(workload_exploration_df_reactive(), case="title") ) )
  get_filtered_expansion_df <- function(base_df) {
    df <- workload_exploration_df_reactive()
    person_selection <- df[input$workload_summary_table_rows_selected,]$sis_id
    # this section of code is what causes the warning
    # needs to be fixed by looking at input$time_aggregator to eliminate warning
    term_selection <- df[input$workload_summary_table_rows_selected,]$term
    academic_year_selection <- df[input$workload_summary_table_rows_selected,]$academic_year
    base_df %>%
      dplyr::filter(  sis_id %in% person_selection
                      & ( term %in% term_selection | academic_year %in% academic_year_selection ) )   
  }
  reactive_instructional_expansion_df <- reactive({
    get_filtered_expansion_df(instructional_faculty_workload_df)
  })
  reactive_non_instructional_expansion_df <- reactive({
    get_filtered_expansion_df(non_instructional_faculty_workload_df)
  }) 
  output$instructional_workload_expansion_table <- DT::renderDataTable( reactive_instructional_expansion_df(),
                     options=list( scrollX = TRUE,
                                   lengthMenu=list( c(5, 10, 50, -1), 
                                                    c(5, 10, 50, "All") ) ),
                     rownames=FALSE,
                     colnames=colnames( janitor::clean_names(reactive_instructional_expansion_df(), case="title") ))                    
  output$non_instructional_workload_expansion_table <- DT::renderDataTable( reactive_non_instructional_expansion_df(),
                     options=list( scrollX = TRUE,
                                   lengthMenu=list( c(5, 10, 50, -1), 
                                                    c(5, 10, 50, "All") )  ),
                     rownames=FALSE,
                     colnames=colnames( janitor::clean_names(reactive_non_instructional_expansion_df(), case="title") ))         
  
  # INDIVIDUAL SEARCH ####  
  # instructional tiles
  instructional_tiles_df <- reactive({
    req( input$faculty_selection,
         input$terms_selection)
    instructional_individual_search_df %>%
      dplyr::filter( faculty_info %in% input$faculty_selection &
              term %in% input$terms_selection )
  })
  output$instructional_individual_search_tiles <- renderUI({
    generate_tiles(instructional_tiles_df(),
                   tiles_name="Instructional Workload",
                   title_col="faculty_info",
                   label_col="course_label")
  }) 
  # non-instructional tiles
  non_instructional_tiles_df <- reactive({
    req( input$faculty_selection,
         input$terms_selection)
    non_instructional_individual_search_df %>%
      dplyr::filter( faculty_info %in% input$faculty_selection &
              term %in% input$terms_selection )
  })
  output$non_instructional_individual_search_tiles <- renderUI({
    generate_tiles(non_instructional_tiles_df(),
                   tiles_name="Non-Instructional Workload",
                   title_col="faculty_info",
                   label_col="label")
  })  
  
  # DATASETS #### 
  output$instructional_workload_table <- renderDataTable({
      display_df <- instructional_faculty_workload_df
      DT::datatable(display_df,
                     filter="top",
                     options=list( scrollX = TRUE,
                                   lengthMenu=list( c(5, 10, 50, -1), 
                                                    c(5, 10, 50, "All") ) ),
                     rownames=FALSE,
                     colnames=colnames( janitor::clean_names(display_df, case="title") ) )                           
    }) 
  output$non_instructional_workload_table <- renderDataTable({
      display_df <- non_instructional_faculty_workload_df
      DT::datatable(display_df,
                     filter="top",
                     options=list( scrollX = TRUE,
                                   lengthMenu=list( c(5, 10, 50, -1), 
                                                    c(5, 10, 50, "All") ) ),
                     rownames=FALSE,
                     colnames=colnames( janitor::clean_names(display_df, case="title") ))                           
    })
  
  # DATA INTEGRITY ISSUES #### 
  make_data_table <- function(display_df) {
     DT::datatable(display_df,
                  extensions='Buttons', 
                  filter="top",
                  options=list( scrollX = TRUE,
                                lengthMenu=list( c(5, 10, 50, -1), 
                                                 c(5, 10, 50, "All") ) ,
                                dom='Blfrtip',
                                buttons=c('csv', 'excel') ),
                  rownames=FALSE,
                  colnames=colnames( janitor::clean_names(display_df, case="title") ))    
  }
  output$missing_designated_college_table <- renderDataTable({
      make_data_table(missing_designated_college_df)
    }, server=FALSE) 
   output$missing_designated_department_table <- renderDataTable({
      make_data_table(missing_designated_department_df)
    }, server=FALSE)  
   output$missing_statuses_table <- renderDataTable({
      make_data_table(missing_statuses_df)
    }, server=FALSE)   
    output$missing_ranks_table <- renderDataTable({
      make_data_table(missing_ranks_df)
    }, server=FALSE)     
    output$duplicate_statuses_table <- renderDataTable({
      make_data_table(duplicate_statuses_df)
    }, server=FALSE) 
    output$duplicate_ranks_table <- renderDataTable({
      make_data_table(duplicate_ranks_df)
    }, server=FALSE)    
    output$missing_contracted_workload_table <- renderDataTable({
      make_data_table(missing_contracted_workload_df)
    }, server=FALSE)     

}

shinyApp(ui, server)