
# Libraries ####
source(here::here('rscripts', 'data_io_util.R'))

# hard-coded global parameters ####
# historical terms to include
fall_semesters_include <- c("Fall 2017", "Fall 2018", "Fall 2019", "Fall 2020", "Fall 2021")
spring_semesters_include <- c("Spring 2017", "Spring 2018", "Spring 2019", "Spring 2020", "Spring 2021", "Spring 2022")


# PULL DATA ####
print("Pulling instructional workload data...")
instructional_faculty_workload_df <- get_data_from_pin("instructional_faculty_workload_pin")

# lists for selector values used in UI generation
instructor_statuses <- c("All Faculty", "Contract Faculty", "Adjunct & Overload")
colleges <- sort(unique(instructional_faculty_workload_df$course_college))
departments <- sort(unique(instructional_faculty_workload_df$course_department)) 
subjects <- sort(unique(instructional_faculty_workload_df$course_subject)) 


# categorize faculty into two groups
instructional_faculty_workload_df <- within(instructional_faculty_workload_df, {
  adjunct_overload_indexes <- (instructional_faculty_workload_df$instructor_status == "PT Instructor Faculty" | 
                                 instructional_faculty_workload_df$payroll_contract_type == "Overload")
  contract_faculty_indexes <- !(adjunct_overload_indexes)
  instructor_status[adjunct_overload_indexes] <- "Adjunct & Overload"
  instructor_status[contract_faculty_indexes] <- "Contract Faculty"
})


data_mung <- function(aggregator, instructor_status_include, college_include, department_include, subject_include, semesters_include) {
   
  if (semesters_include == "Spring") {
    terms_include <- spring_semesters_include
  } else if (semesters_include == "Fall") {
    terms_include <- fall_semesters_include
  }
  
  # apply filter args on data
  rows <- (instructional_faculty_workload_df$instructor_status %in% instructor_status_include | "All Faculty" %in% instructor_status_include) &
    (instructional_faculty_workload_df$course_college %in% college_include) &
    (instructional_faculty_workload_df$course_department %in% department_include) &
    (instructional_faculty_workload_df$course_subject %in% subject_include) &
    (instructional_faculty_workload_df$term %in% terms_include)
  instructional_faculty_workload_df <- subset(instructional_faculty_workload_df, rows)
  
  # instantiate aggregation vectors based on chosen aggregator
  if (aggregator == "college") {
    fte_calc_agg <- c("term", "instructor_status", "course_college")
    faculty_totals_agg <- c("term", "course_college")
  } else if (aggregator == "department") {
    fte_calc_agg <- c("term", "instructor_status", "course_department")
    faculty_totals_agg <- c("term", "course_department")
  } else if (aggregator == "subject") {
    fte_calc_agg <- c("term", "instructor_status", "course_subject")
    faculty_totals_agg <- c("term", "course_subject")
  } else {
    fte_calc_agg <- c("term", "instructor_status")
    faculty_totals_agg <- c("term")
  }      

  # summarize data to get FTE calculation
  instructional_faculty_workload_df <- instructional_faculty_workload_df %>% 
    subset( (instructional_faculty_workload_df$term %in% terms_include) ) %>%
    group_by_at( fte_calc_agg ) %>% 
    summarise( student_credit_hours=sum(course_student_credit_hours),
               fte=sum(course_fte) ) %>%
    ungroup()
  
  if ("All Faculty" %in% instructor_status_include) {
    # calculate totals for all faculty
    fte_totals <- instructional_faculty_workload_df %>% 
      group_by_at( faculty_totals_agg ) %>% 
      summarise( fte=sum(fte), student_credit_hours=sum(student_credit_hours) ) %>%
      mutate(instructor_status="All Faculty") %>%
      ungroup()
    # append calculated totals to base dataframe 
    instructional_faculty_workload_df <- rbind(instructional_faculty_workload_df, fte_totals)
  }
  
  # subset data again on instructor status after calculations
  rows <- (instructional_faculty_workload_df$instructor_status %in% instructor_status_include)
  instructional_faculty_workload_df <- subset(instructional_faculty_workload_df, rows)   
  
  # sort the complete dataframe by term 
  instructional_faculty_workload_df <- instructional_faculty_workload_df[order(instructional_faculty_workload_df$term),]
  
  # insert grouping column bases on chosen aggregator
  if (aggregator == "college") {
    instructional_faculty_workload_df$grouping <- paste(instructional_faculty_workload_df$instructor_status, instructional_faculty_workload_df$course_college, sep='_')
  } else if (aggregator == "department") {
    instructional_faculty_workload_df$grouping <- paste(instructional_faculty_workload_df$instructor_status, instructional_faculty_workload_df$course_department, sep='_')
  } else if (aggregator == "subject") {
    instructional_faculty_workload_df$grouping <- paste(instructional_faculty_workload_df$instructor_status, instructional_faculty_workload_df$course_subject, sep='_')
  } else {
    instructional_faculty_workload_df$grouping <- instructional_faculty_workload_df$instructor_status
  }     
  
  # format large numbers to a more aesthetically pleasing format
  instructional_faculty_workload_df$fte <- formatC(instructional_faculty_workload_df$fte, format="d", big.mark=",")
  instructional_faculty_workload_df$student_credit_hours <- formatC(instructional_faculty_workload_df$student_credit_hours, format="d", big.mark=",")
  return(instructional_faculty_workload_df)
}

