library(dplyr)
library(tidyr)

calculate_student_types <- function(student_type_determination_variables, parameter_term, term_two_terms_ago) { 

  return_df <- student_type_determination_variables %>%
    mutate(calculated_student_type = case_when(
      # GROUP: High School ####
      # if the calculated high school graduation term is greater than the passed in parameter term, 
      # then the student is a high school student on the provided parameter term  
      #if calculated_high_school_graduation_term > parameter_term
      #    return "High School"
      calculated_high_school_graduation_term > parameter_term 
        ~ "High School",
      
      # GROUP Graduate ####
      # first_term_enrolled and last_term_enrolled only looking at GRADUATE LEVEL transcript records for local institution
      
      ## Continuing Graduate ####
      #if last_term_enrolled_as_graduate >= (parameter_term - two_terms)
      #    return "Continuing Graduate"
      last_term_enrolled_on_or_after_calculated_hs_graduation_term >= term_two_terms_ago
      & student_level == "GR" 
        ~ "Continuing Graduate",
      
      ## Readmit Graduate ####
      #if last_term_enrolled_as_graduate < (parameter_term - two_terms)
      #    return "Readmit Graduate"
      last_term_enrolled_on_or_after_calculated_hs_graduation_term < term_two_terms_ago
      & student_level == "GR" 
        ~ "Readmit Graduate",
 
      ## Transfer Graduate ####
      #if has_transfer_graduate_credits & ( first_term_enrolled_as_graduate == parameter_term )
      #    return "Transfer Graduate"
      has_transfer_credits_on_or_after_calculated_hs_graduation_term
      & first_term_enrolled_on_or_after_calculated_hs_graduation_term == parameter_term 
      & student_level == "GR" 
      & transfer_credits_level == "GR" 
        ~ "Transfer Graduate",
      
      ## New Graduate ####
      #if first_term_enrolled_as_graduate == parameter_term:
      #    return "New Graduate"
      first_term_enrolled_on_or_after_calculated_hs_graduation_term == parameter_term
      & student_level == "GR" 
        ~ "New Graduate",
            
      # GROUP Undergraduate ####
      # first_term_enrolled, last_term_enrolled after calculated_high_school_graduation_date
      # and only looking at UNDERGRADUATE LEVEL transcript records for local institution.
      
      ## Continuing ####
      #if last_term_enrolled_as_undergraduate_after_hs_grad >= (parameter_term - two_terms):
      #  return "Continuing Undergraduate"
      last_term_enrolled_on_or_after_calculated_hs_graduation_term >= term_two_terms_ago 
      & student_level == "UG" 
        ~ "Continuing Undergraduate",
      
      ## Readmit ####
      #if last_term_enrolled_as_undergraduate_after_hs_grad < (parameter_term - two_terms):
      #  return "Readmit Undergraduate"
      last_term_enrolled_on_or_after_calculated_hs_graduation_term < term_two_terms_ago 
      & student_level == "UG" 
        ~ "Readmit Undergraduate",
      
      ## Transfer ####
      # has_transfer_undergraduate_credits after calculated_high_school_graduation_date.
      #if has_transfer_undergraduate_credits_after_hs_grad
      #   AND (first_term_enrolled_as_undergraduate_after_hs_grad == parameter_term):
      #  return "Transfer Undergraduate"
      has_transfer_credits_on_or_after_calculated_hs_graduation_term
      & first_term_enrolled_on_or_after_calculated_hs_graduation_term == parameter_term 
      & student_level == "UG" 
      & transfer_credits_level == "UG" 
        ~ "Transfer Undergraduate",
       
      ## Freshman ####
      #if first_term_enrolled_as_undergraduate_after_hs_grad == parameter_term:
      #  return "Freshman"
      first_term_enrolled_on_or_after_calculated_hs_graduation_term == parameter_term 
      & student_level == "UG" 
        ~ "Freshman",     
     
      ## Unknown ####
      #else:
      # return "Unknown"
      TRUE 
        ~ "Unknown" ),
    ) %>%
    # set term for what is used in calculations 
    mutate( term_id = parameter_term ) %>%
    # only return relevant variables from calculation
    select(sis_system_id, term_id, calculated_student_type)
  
  return(return_df)
}


get_summarized_faculty_workload_df <- function(instructional_faculty_workload_df, non_instructional_faculty_workload_df, time_aggregator) {
  # options for time_aggregator are:
  # 1. "term"
  # 2. "academic_year"
  grouping <- c("faculty_info", "sis_id", "full_name", time_aggregator)
  
  # Summarize instructional workload.
  summarized_instructional_faculty_workload_df <- instructional_faculty_workload_df %>%
      dplyr::mutate( contracted_workload = tidyr::replace_na(contracted_workload, 0) ) %>%
      dplyr::group_by_at( grouping ) %>%
      dplyr::summarize(credit_hours=sum(course_credit_hours), 
                       students=sum(course_student_count), 
                       student_credit_hours=sum(course_student_credit_hours),
                       fte=sum(course_fte),
                       instructional_workload=sum(course_workload),
                       # For courses_taught we count distinct course_crn if the course is NOT a cross listed course,
                       # otherwise we count all cross listed course sections as one single course.
                       courses_taught=n_distinct(course_crn[is.na(course_cross_list_group)]) + n_distinct(course_cross_list_group[!is.na(course_cross_list_group)]), 
                       faculty_status=first(na.omit(faculty_status)),
                       faculty_rank=first(na.omit(faculty_rank)),
                       faculty_college=first(na.omit(faculty_college)),
                       faculty_department=first(na.omit(faculty_department)),
                       contracted_workload=min(contracted_workload) ) %>%
      dplyr::ungroup()
  
  # Summarize non-instructional workload.
  summarized_non_instructional_faculty_workload_df <- non_instructional_faculty_workload_df %>%
      dplyr::mutate( contracted_workload = tidyr::replace_na(contracted_workload, 0) ) %>%
      dplyr::group_by_at( grouping ) %>%
      dplyr::summarize( non_instructional_workload=sum(non_instructional_workload),
                        faculty_status=first(na.omit(faculty_status)),
                        faculty_rank=first(na.omit(faculty_rank)),
                        faculty_college=first(na.omit(faculty_college)),
                        faculty_department=first(na.omit(faculty_department)),
                        contracted_workload=min(contracted_workload) ) %>%
      dplyr::ungroup()
  
  # full join instructional and non-instructional workload data frames
  workload_exploration_df <- dplyr::full_join(x=summarized_instructional_faculty_workload_df,
                                              y=summarized_non_instructional_faculty_workload_df,
                                              by=grouping) %>%
      mutate(faculty_status = coalesce(faculty_status.x, faculty_status.y),
             faculty_rank = coalesce(faculty_rank.x, faculty_rank.y),
             faculty_college = coalesce(faculty_college.x, faculty_college.y),
             faculty_department = coalesce(faculty_department.x, faculty_department.y),
             contracted_workload = coalesce(contracted_workload.x, contracted_workload.y) ) %>% 
      select( -c(faculty_status.x, faculty_status.y, 
                 faculty_rank.x, faculty_rank.y,
                 faculty_college.x, faculty_college.y,
                 faculty_department.x, faculty_department.y,
                 contracted_workload.x, contracted_workload.y ) ) %>%
      mutate( instructional_workload = replace_na(instructional_workload, 0),
              non_instructional_workload = replace_na(non_instructional_workload, 0),
              contracted_workload=replace_na(contracted_workload, 0) ) %>%     
      # Final workload and overload calculations. ####
      mutate( total_workload = instructional_workload + non_instructional_workload,
              overload = case_when( contracted_workload == 0 ~ 0,
                                    TRUE ~ total_workload - contracted_workload ) ) %>%
      # set the order of columns
      relocate(c("faculty_info", "sis_id", "full_name", time_aggregator, 
                 "faculty_college", "faculty_department", "faculty_status", "faculty_rank",
                 "instructional_workload", "non_instructional_workload", "total_workload",
                 "contracted_workload", "overload", 
                 "courses_taught", "credit_hours", "students", "student_credit_hours", "fte"))
    
  return(workload_exploration_df)
}