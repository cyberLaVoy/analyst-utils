library(utHelpR)
library(dplyr)
library(ngram)

# PULL DATA ####
faculty_term_df <- utHelpR::get_data_from_pin("faculty_term_pin")

print("Pulling instructional workload data...")
instructional_faculty_workload_df <- utHelpR::get_data_from_pin("instructional_faculty_workload_pin")

print("Pulling non-instructional workload data...")
non_instructional_faculty_workload_df <- utHelpR::get_data_from_pin("non_instructional_faculty_workload_pin")

print("Pulling summarized workload data...")
summarized_faculty_workload_by_term_df <- utHelpR::get_data_from_pin("summarized_faculty_workload_by_term_pin")
summarized_faculty_workload_by_academic_year_df <- utHelpR::get_data_from_pin("summarized_faculty_workload_by_academic_year_pin")

print("Data pull complete.")
  
# global unique values in datasets 
all_terms <- sort(unique(summarized_faculty_workload_by_term_df$term))
all_ranks <- sort(unique(summarized_faculty_workload_by_term_df$faculty_rank))
all_status <- sort(unique(summarized_faculty_workload_by_term_df$faculty_status))
all_colleges <- sort(unique(summarized_faculty_workload_by_term_df$faculty_college))

# INDIVIAUL SEARCH ####
# instructional
instructional_individual_search_df <- instructional_faculty_workload_df %>%
      dplyr::mutate( course_label=paste0(course_title, " ",
                                         course_number, "-",
                                         course_section_number, " ",
                                         term ) ) %>%
      dplyr::arrange(term) %>%
      dplyr::select( -c(course_title, course_number, course_section_number, sis_id, full_name) )

# non-instructional
non_instructional_individual_search_df <- non_instructional_faculty_workload_df %>%
      dplyr::mutate( label=description ) %>%
      dplyr::arrange( term ) %>%
      dplyr::select( -c(description, sis_id, full_name) )


# WORKLOAD EXPLORATION ####
get_workload_exploration_df <- function(time_aggregator, faculty_include) {
    if (time_aggregator == 'term') {
    workload_exploration_df <- summarized_faculty_workload_by_term_df %>%
        dplyr::filter(faculty_info %in% faculty_include)
    } else if (time_aggregator == 'academic_year') {
     workload_exploration_df <- summarized_faculty_workload_by_academic_year_df %>%
        dplyr::filter(faculty_info %in% faculty_include)
    }
    return(workload_exploration_df)
}

# DATA INTEGRITY AUDIT ####
## Faculty college and department integrity checks. ####
missing_designated_college_df <- summarized_faculty_workload_by_term_df %>%
  dplyr::filter( faculty_college %in% c("Missing", "No College Designated") ) %>%
  dplyr::left_join( instructional_faculty_workload_df, by=c("sis_id", "term", "full_name") ) %>%
  dplyr::group_by_at( c("full_name", "sis_id", "term") ) %>%
  dplyr::summarize( possible_colleges = ngram::concatenate(unique(na.omit(course_college)), collapse=', ') )

missing_designated_department_df <- summarized_faculty_workload_by_term_df %>%
  dplyr::filter( faculty_department %in% c("Missing", "Undeclared") ) %>%
  dplyr::left_join( instructional_faculty_workload_df, by=c("sis_id", "term", "full_name") ) %>%
  dplyr::group_by_at( c("full_name", "sis_id", "term") ) %>%
  dplyr::summarize( possible_departments = ngram::concatenate(unique(na.omit(course_department)), collapse=', '),
                    possible_colleges = ngram::concatenate(unique(na.omit(course_college)), collapse=', ') )

## Faculty rank integrity check. ####
missing_ranks_df <- summarized_faculty_workload_by_term_df %>%
  dplyr::filter( faculty_rank == "Missing" ) %>%
  dplyr::left_join( instructional_faculty_workload_df, by=c("sis_id", "term", "full_name") ) %>%
  dplyr::group_by_at( c("full_name", "sis_id", "term") ) %>%
  dplyr::summarize( possible_colleges = ngram::concatenate(unique(na.omit(course_college)), collapse=', ') )

duplicate_ranks_df <- summarized_faculty_workload_by_term_df %>%
  dplyr::filter( grepl("|", faculty_rank, fixed=TRUE) ) %>% 
  dplyr::select( c("full_name", "sis_id", "term", "faculty_rank") )

## Faculty status integrity checks. ####
missing_statuses_df <- summarized_faculty_workload_by_term_df %>%
  dplyr::filter( faculty_status == "Missing" ) %>%
  dplyr::select( c("full_name", "sis_id", "term") )

duplicate_statuses_df <- summarized_faculty_workload_by_term_df %>%
  dplyr::filter( grepl("|", faculty_status, fixed=TRUE)) %>% 
  dplyr::select( c("full_name", "sis_id", "term", "faculty_status") )

## Faculty contracted workload integrity checks. ####
missing_contracted_workload_df <- faculty_term_df %>%
  dplyr::filter( is.na(contracted_workload) ) %>%
  dplyr::select( c("full_name", "sis_id", "term") )
