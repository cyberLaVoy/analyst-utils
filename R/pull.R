# LIBRARIES ####
library(here)

# LOCAL UTILS ####
source( here::here('r', 'data_io_util.R') )

# GLOBAL VARIABLES ####
pull_data <- FALSE

if (pull_data) {
  # get initial time
  t0 <- Sys.time()
  print(t0)
  
  # DATA PULL ####
  print("pulling ncs data...")
  nsc_transfered_out_indicators_df <- get_data_from_sql_file('nsc_transfered_out_indicators.sql', 'edify')
  print("pulling programs all band aid data...")
  dsc_programs_all_band_aid_df <- get_data_from_sql_file('dsc_programs_all_band_aid.sql', 'edify')
  print("pulling graduation main data from Edify...")
  graduation_main_edify_df <- get_data_from_sql_file('graduation_main_edify.sql', 'edify')
  
  # get time after data has been pulled
  t1 <- Sys.time()
  delta_t <- t1-t0
  print("Time to pull all data:")
  print( delta_t )
  
  # JOINS ####
  # left join with programs all band aid data
  graduation_full_edify_df <- merge(x=graduation_main_edify_df, 
                                    y=dsc_programs_all_band_aid_df, 
                                    by='highest_earned_degree_program_code', 
                                    all.x=TRUE) 
  
  # left join with nsc data
  graduation_full_edify_df <- merge(x=graduation_full_edify_df, 
                                    y=nsc_transfered_out_indicators_df, 
                                    by='sis_system_id', 
                                    all.x=TRUE) 
  
  # DATA SAVE ####
  save_data_as_rds(graduation_full_edify_df, "graduation_full_edify_df.rds")
} else {
  # DATA LOAD ####
 graduation_full_edify_df <- load_data_from_rds("graduation_full_edify_df.rds") 
}

# GLOBAL UNIQUENESS LISTS ####
available_cohort_terms <- sort(unique(graduation_full_edify_df$term_id))
available_enrollment_check_terms <- sort(unique(graduation_full_edify_df$enrolled_term_id))
