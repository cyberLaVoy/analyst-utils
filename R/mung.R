
# LIBRARIES ####

# LOCAL UTILS ####

get_graduation_dataset <- function(df, enrollment_check_term) {
  df <- df %>%
    mutate(is_enrolled=replace_na(is_enrolled, FALSE))
  
  current_semester_population_df <- df %>%
    filter(is_enrolled & enrolled_term_id == enrollment_check_term & enrolled_level == 'UG')
  
  athlete_population_df <- df %>%
    filter(is_athlete)
  
  df <- df %>%
    mutate(currently_enrolled_ind=sis_system_id %in% current_semester_population_df$sis_system_id) %>%
    
    # change all athletes to bachelors seeking
    mutate(ipeds_dlev_code=if_else(sis_system_id %in% athlete_population_df$sis_system_id, 'BA', ipeds_dlev_code)) %>%
    
    mutate( is_pell_ind = replace_na(is_pell_ind, FALSE),
            sub_loan_ind = replace_na(sub_loan_ind, FALSE),
            currently_enrolled_ind = replace_na(currently_enrolled_ind, FALSE),
            exclusion_ind = replace_na(exclusion_ind, FALSE),
            nsc_transfered_out = replace_na(nsc_transfered_out, FALSE)) %>%
    
    mutate( graduated_ind = !is.na(highest_earned_degree_grad_date) ) %>%
    
                               # graduated with a low credit hour cert within 1 year
    mutate( ipeds_150_ind =  ( ( ipeds_award_level %in% c('1A', '1B') & cert_grad_days <= 365 ) |
                               # graduated with a mid credit hour cert within 1.5 years
                               ( ipeds_award_level == '02' & cert_grad_days <= 547 ) |
                               # graduated with associate within 3 years 
                               ( ipeds_award_level == '03' & as_grad_days <= 1095 ) |
                               # graduated with bachelors within 6 years
                               ( ipeds_award_level == '05' & bs_grad_days <= 2190 ) 
                             ), 
                              # graduated with bachelors within 7 to 8 years
            ipeds_200_ind = ( bs_grad_days >= 2191 & bs_grad_days <= 2920 ) ) %>%
    mutate( ipeds_150_ind = replace_na(ipeds_150_ind, FALSE),
            ipeds_200_ind = replace_na(ipeds_200_ind, FALSE)) %>%
    
    mutate( ipeds_exclusion_ind = if_else( ipeds_150_ind, FALSE, exclusion_ind ) ) %>%
    mutate( ipeds_exclusion_ind = replace_na(ipeds_exclusion_ind, FALSE) ) %>%
     
    mutate( ipeds_transfered_out = if_else( ( ipeds_150_ind  | ipeds_exclusion_ind | graduated_ind ), FALSE, nsc_transfered_out ) ) %>%
    mutate( ipeds_transfered_out = replace_na(ipeds_transfered_out, FALSE) ) %>%
    
    mutate( ipeds_currently_enrolled = if_else( ( nsc_transfered_out | ipeds_150_ind | ipeds_exclusion_ind ), FALSE, currently_enrolled_ind ) ) %>%
    mutate( ipeds_currently_enrolled = replace_na(ipeds_currently_enrolled, FALSE) ) %>%

    mutate( highest_earned_degree_years_to_grad=case_when(
      highest_earned_degree_days_to_grad <= 1460 ~ 'Within 4 Years or Less',
      (highest_earned_degree_days_to_grad >= 1461 & highest_earned_degree_days_to_grad <= 1825) ~ 'Within 5 Years',
      (highest_earned_degree_days_to_grad >= 1826 & highest_earned_degree_days_to_grad <= 2190) ~ 'Within 6 Years',
      highest_earned_degree_days_to_grad >= 1826 ~ 'Within 6 Years or More'
    )) %>%
  
  mutate(
    highest_earned_degree_4_year_status=case_when(
      bs_grad_days <= 1460 ~ "Bachelors",
      as_grad_days <= 1460 ~ "Associates",
      cert_grad_days <= 1460 ~ "Certificates"
    ),
    highest_earned_degree_between_4_to_5_year_status=case_when(
      (bs_grad_days >= 1461 & bs_grad_days <= 1825) ~ "Bachelors",
      (as_grad_days >= 1461 & as_grad_days <= 1825) ~ "Associates",
      (cert_grad_days >= 1461 & cert_grad_days <= 1825) ~ "Certificates"
    ),   
    highest_earned_degree_between_5_to_6_year_status=case_when(
      (bs_grad_days >= 1826 & bs_grad_days <= 2190) ~ "Bachelors",
      (as_grad_days >= 1826 & as_grad_days <= 2190) ~ "Associates",
      (cert_grad_days >= 1826 & cert_grad_days <= 2190) ~ "Certificates"
    ),      
    highest_earned_degree_6_year_status=case_when(
      bs_grad_days <= 2190 ~ "Bachelors",
      as_grad_days <= 2190 ~ "Associates",
      cert_grad_days <= 2190 ~ "Certificates"
    ), 
    highest_earned_degree_8_year_status=case_when(
      bs_grad_days <= 2920 ~ "Bachelors",
      as_grad_days <= 2920 ~ "Associates",
      cert_grad_days <= 2920 ~ "Certificates"
    )    
  )
    
}

# REPORT DATAFRAME FILTER FUNCTIONS ####
### final resulting dataframes for report generation 
# grad 
get_graduation_report_edify_df <- function(df, cohort_term){
  df %>%
    filter( substr(cohort_code, 1, 3) == "FTF" & term_id == cohort_term )
}
# grad 200
get_graduation_200_report_edify_df <- function(df, cohort_term){
  df %>%
    filter( substr(cohort_code, 1, 3) == "FTF" & ipeds_dlev_code == 'BA' & term_id == cohort_term )
}
# outcomes
get_outcomes_report_edify_df <- function(df, cohort_terms) {
  df %>%
    filter( term_id %in% cohort_terms )
}
# cds 
get_cds_report_edify_df <- function(df, cohort_term){
  df %>%
    filter( substr(cohort_code, 1, 3) == "FTF" & ipeds_dlev_code == 'BA' & term_id == cohort_term )
}


