
# UI ####
programs_summary_UI <- function(id) {
  ns <- NS(id)
  tagList(selectInput(ns("college_select"),
                       "College",
                       colleges),
          uiOutput(outputId = ns("programs_display"))
  )
}

# SERVER ####
programs_summary_server <- function(input, output, session){
  ns <- session$ns
  
  output$programs_display <- renderUI({
    filtered_programs <- filter( programs, college==input$college_select )
    departments <- sort( unique( filtered_programs$department) )
    
    programs_display <- tagList()
    lapply(departments, function(department_itter) {
      
      department_text <- paste("Department of", department_itter)
      programs_display <<- tagAppendChild(programs_display, h3(department_text) )
      department_programs <- filter( filtered_programs, department==department_itter )
      programs_list <- tags$ul()  
      majors_list <- tags$ul()  
      majors_flag <- FALSE
      minors_list <- tags$ul()  
      minors_flag <- FALSE
      certs_list <- tags$ul()       
      certs_flag <- FALSE
      conc_list <- tags$ul()       
      conc_flag <- FALSE
      lapply(department_programs$program, function(program_itter) {
        
        # NOTE: we should not have to select the head of the df here
        program <- head( filter( filtered_programs, program==program_itter ), 1)
        
        if (program$available_as_concentration == "Y") {
          conc_info <- paste0(program$major, ' (', program$initial_term, ')') 
          conc_list <<- tagAppendChild(conc_list, tags$li(conc_info) )
          conc_flag <<- TRUE         
        }
        if (program$available_as_minor == "Y") {
          minor_info <- paste0(program$major, ' (', program$initial_term, ')') 
          minors_list <<- tagAppendChild(minors_list, tags$li(minor_info) )
          minors_flag <<- TRUE
        }
        else if (grepl("CER", program$degree)) {
          cert_info <- paste0(program$major, ' (', program$initial_term, ')') 
          certs_list <<- tagAppendChild(certs_list, tags$li(cert_info) )
          certs_flag <<- TRUE
        }
        else {
          major_info <- paste0(program$program, ' (', program$initial_term, ')') 
          majors_list <<- tagAppendChild(majors_list, tags$li(major_info) )
          majors_flag <<- TRUE
        }
      })
      

      if (majors_flag) { 
        programs_display <<- tagAppendChild(programs_display, h4("Majors") )
        programs_display <<- tagAppendChild(programs_display, majors_list)
      }
      if (minors_flag) { 
        programs_display <<- tagAppendChild(programs_display, h4("Minors") )
        programs_display <<- tagAppendChild(programs_display, minors_list)
      }
      if (certs_flag) { 
        programs_display <<- tagAppendChild(programs_display, h4("Certificates") )
        programs_display <<- tagAppendChild(programs_display, certs_list)
      }
      if (conc_flag) { 
        programs_display <<- tagAppendChild(programs_display, h4("Concentrations") )
        programs_display <<- tagAppendChild(programs_display, conc_list)
      }          
    })
    
    return(programs_display)
     
  })
}