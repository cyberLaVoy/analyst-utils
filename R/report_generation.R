# REPORT GENERATION FUNCTIONS ####

# core filtration function
unique_count_given_rules <- function(df, args=NULL) {
    # allow no filters to be applied if no rules are given
    if (!is.null(args)) {
      filtered <- df %>% 
        filter( (ipeds_race_ethnicity == args["ipeds_race_ethnicity"] | is.na(args["ipeds_race_ethnicity"]) ) & 
                (gender == args["gender"] | is.na(args["gender"]) ) &
                (ipeds_dlev_code == args["ipeds_dlev_code"] | is.na(args["ipeds_dlev_code"]) ) &
                (
                  ( ipeds_award_level == args["ipeds_award_level"] 
                    | ipeds_award_level == args["ipeds_award_level_1"] 
                    | ipeds_award_level == args["ipeds_award_level_2"] 
                    | ipeds_award_level == args["ipeds_award_level_3"] ) |
                  ( is.na(args["ipeds_award_level"])
                    & is.na(args["ipeds_award_level_1"])
                    & is.na(args["ipeds_award_level_2"])
                    & is.na(args["ipeds_award_level_3"]) )
                ) &
                (ipeds_150_ind == args["ipeds_150_ind"] | is.na(args["ipeds_150_ind"]) ) &
                (ipeds_200_ind == args["ipeds_200_ind"] | is.na(args["ipeds_200_ind"]) ) &
                (ipeds_transfered_out == args["ipeds_transfered_out"] | is.na(args["ipeds_transfered_out"]) ) &
                (ipeds_exclusion_ind == args["ipeds_exclusion_ind"] | is.na(args["ipeds_exclusion_ind"]) ) &
                (exclusion_ind == args["exclusion_ind"] | is.na(args["exclusion_ind"]) ) &
                (ipeds_currently_enrolled == args["ipeds_currently_enrolled"] | is.na(args["ipeds_currently_enrolled"]) ) &
                (is_pell_ind == args["is_pell_ind"] | is.na(args["is_pell_ind"]) ) &
                (sub_loan_ind == args["sub_loan_ind"] | is.na(args["sub_loan_ind"]) ) &
                (no_pell_sub_loan_ind == args["no_pell_sub_loan_ind"] | is.na(args["no_pell_sub_loan_ind"]) ) &
                (ipeds_cohort_desc == args["ipeds_cohort_desc"] | is.na(args["ipeds_cohort_desc"]) ) &
                (highest_earned_degree_years_to_grad == args["highest_earned_degree_years_to_grad"] | is.na(args["highest_earned_degree_years_to_grad"]) ) &
                (highest_earned_degree_4_year_status == args["highest_earned_degree_4_year_status"] | is.na(args["highest_earned_degree_4_year_status"]) ) &
                (highest_earned_degree_between_4_to_5_year_status == args["highest_earned_degree_between_4_to_5_year_status"] | is.na(args["highest_earned_degree_between_4_to_5_year_status"]) ) &
                (highest_earned_degree_between_5_to_6_year_status == args["highest_earned_degree_between_5_to_6_year_status"] | is.na(args["highest_earned_degree_between_5_to_6_year_status"]) ) &
                (highest_earned_degree_6_year_status == args["highest_earned_degree_6_year_status"] | is.na(args["highest_earned_degree_6_year_status"]) ) &
                (highest_earned_degree_8_year_status == args["highest_earned_degree_8_year_status"] | is.na(args["highest_earned_degree_8_year_status"]) ) 
              ) 
    }
    else {
      filtered <- df
    }
    v <- length( unique(filtered[["sis_system_id"]]) )
    return(v)
}

# GRADUATION ####
format_graduation_report <- function(df) {
   ipeds_graduation_report <- df %>%
    mutate( filler_1=str_pad(filler_1, 5, side="right"),
            line=str_pad(line, 2, side="right"),
            filler_2=str_pad(filler_2, 1, side="right"),
            grrace01=str_pad(grrace01, 6, side="right"),
            grrace02=str_pad(grrace02, 6, side="right"),
            grrace25=str_pad(grrace25, 6, side="right"),
            grrace26=str_pad(grrace26, 6, side="right"),
            grrace27=str_pad(grrace27, 6, side="right"),
            grrace28=str_pad(grrace28, 6, side="right"),
            grrace29=str_pad(grrace29, 6, side="right"),
            grrace30=str_pad(grrace30, 6, side="right"),
            grrace31=str_pad(grrace31, 6, side="right"),
            grrace32=str_pad(grrace32, 6, side="right"),
            grrace33=str_pad(grrace33, 6, side="right"),
            grrace34=str_pad(grrace34, 6, side="right"),
            grrace35=str_pad(grrace35, 6, side="right"),
            grrace36=str_pad(grrace36, 6, side="right"),
            grrace37=str_pad(grrace37, 6, side="right"),
            grrace38=str_pad(grrace38, 6, side="right"),
            grrace13=str_pad(grrace13, 6, side="right"),
            grrace14=str_pad(grrace14, 6, side="right")
          )  
   return(ipeds_graduation_report)
}
format_and_save_graduation_report <- function(df, file) {
   ipeds_graduation_report <- format_graduation_report(df)
   write_delim(ipeds_graduation_report, 
               file,
               quote="none",
               delim="",
               col_names=FALSE) 
}
generate_ipeds_graduation_report <- function(input_df) {
   row_rules <- list( 
                   # part 'B' rows
                   c( # first row has no additional rules
                        "unitid" = "230171", "survsect" = "GR1", "part" = "B", "section" = 1, "line" = 1 ),
                   c("ipeds_dlev_code" = "BA",
                        "unitid" = "230171", "survsect" = "GR1", "part" = "B", "section" = 1, "line" = 2 ),
                   c("ipeds_dlev_code" = "BA", "ipeds_award_level_1" = "1A", "ipeds_award_level_2" = "1B", "ipeds_award_level_3" = "02", "ipeds_150_ind" = TRUE,
                        "unitid" = "230171", "survsect" = "GR1", "part" = "B", "section" = 2, "line" = 11 ),
                   c("ipeds_dlev_code" = "BA", "ipeds_award_level" = "03", "ipeds_150_ind" = TRUE,
                        "unitid" = "230171", "survsect" = "GR1", "part" = "B", "section" = 2, "line" = 12 ),
                   c("ipeds_dlev_code" = "BA", "ipeds_award_level" = "05", "ipeds_150_ind" = TRUE,
                        "unitid" = "230171", "survsect" = "GR1", "part" = "B", "section" = 2, "line" = 18 ),
                   c("ipeds_dlev_code" = "BA", "ipeds_award_level" = "05", "ipeds_150_ind" = TRUE, "highest_earned_degree_years_to_grad" = "Within 4 Years or Less",
                        "unitid" = "230171", "survsect" = "GR1", "part" = "B", "section" = 2, "line" = 19 ),
                   c("ipeds_dlev_code" = "BA", "ipeds_award_level" = "05", "ipeds_150_ind" = TRUE, "highest_earned_degree_years_to_grad" = "Within 5 Years",
                        "unitid" = "230171", "survsect" = "GR1", "part" = "B", "section" = 2, "line" = 20 ),
                   c("ipeds_dlev_code" = "BA", "ipeds_transfered_out" = TRUE,
                        "unitid" = "230171", "survsect" = "GR1", "part" = "B", "section" = 2, "line" = 30 ),
                   c("ipeds_dlev_code" = "BA", "ipeds_exclusion_ind" = TRUE,
                        "unitid" = "230171", "survsect" = "GR1", "part" = "B", "section" = 2, "line" = 45 ),
                   c("ipeds_dlev_code" = "BA", "ipeds_currently_enrolled" = TRUE,
                        "unitid" = "230171", "survsect" = "GR1", "part" = "B", "section" = 2, "line" = 51 ),
                   c("ipeds_dlev_code" = "OT", "ipeds_award_level_1" = "1A", "ipeds_award_level_2" = "1B", "ipeds_award_level_3" = "02", "ipeds_150_ind" = TRUE,
                        "unitid" = "230171", "survsect" = "GR1", "part" = "B", "section" = 3, "line" = 11 ),
                   c("ipeds_dlev_code" = "OT", "ipeds_award_level" = "03", "ipeds_150_ind" = TRUE,
                        "unitid" = "230171", "survsect" = "GR1", "part" = "B", "section" = 3, "line" = 12 ),
                   c("ipeds_dlev_code" = "OT", "ipeds_award_level" = "05", "ipeds_150_ind" = TRUE,
                        "unitid" = "230171", "survsect" = "GR1", "part" = "B", "section" = 3, "line" = 18 ),
                   c("ipeds_dlev_code" = "OT", "ipeds_transfered_out" = TRUE,
                        "unitid" = "230171", "survsect" = "GR1", "part" = "B", "section" = 3, "line" = 30 ),
                   c("ipeds_dlev_code" = "OT", "ipeds_exclusion_ind" = TRUE,
                        "unitid" = "230171", "survsect" = "GR1", "part" = "B", "section" = 3, "line" = 45 ),
                   c("ipeds_dlev_code" = "OT", "ipeds_currently_enrolled" = TRUE,
                        "unitid" = "230171", "survsect" = "GR1", "part" = "B", "section" = 3, "line" = 51 ),
                   # part 'C' rows
                   c("ipeds_dlev_code" = "BA",
                        "unitid" = "230171", "survsect" = "GR1", "part" = "C", "section" = 2, "line" = 10 ),                  
                   c("ipeds_dlev_code" = "BA", "ipeds_award_level" = "05", "ipeds_150_ind" = TRUE,
                        "unitid" = "230171", "survsect" = "GR1", "part" = "C", "section" = 2, "line" = 18 ),                  
                   c("ipeds_dlev_code" = "BA", "ipeds_150_ind" = TRUE,
                        "unitid" = "230171", "survsect" = "GR1", "part" = "C", "section" = 2, "line" = 29 ),                  
                   c("ipeds_dlev_code" = "BA", "ipeds_exclusion_ind" = TRUE,
                        "unitid" = "230171", "survsect" = "GR1", "part" = "C", "section" = 2, "line" = 45 ),                  
                   c("ipeds_dlev_code" = "OT",
                        "unitid" = "230171", "survsect" = "GR1", "part" = "C", "section" = 3, "line" = 10 ),                  
                   c("ipeds_dlev_code" = "OT", "ipeds_award_level" = "05", "ipeds_150_ind" = TRUE,
                        "unitid" = "230171", "survsect" = "GR1", "part" = "C", "section" = 3, "line" = 18 ),                  
                   c("ipeds_dlev_code" = "OT", "ipeds_150_ind" = TRUE,
                        "unitid" = "230171", "survsect" = "GR1", "part" = "C", "section" = 3, "line" = 29 ),                  
                   c("ipeds_dlev_code" = "OT", "ipeds_exclusion_ind" = TRUE,
                        "unitid" = "230171", "survsect" = "GR1", "part" = "C", "section" = 3, "line" = 45 )                  
                   )
   # order of entries in this list matters
   race_ethnicity_gender_combinations <- list( c("ipeds_race_ethnicity" = 'Non-Resident Alien', "gender" = 'M'), 
                                              c("ipeds_race_ethnicity" = 'Non-Resident Alien', "gender" = 'F'),
                                              c("ipeds_race_ethnicity" = 'Hispanic', "gender" = 'M'),
                                              c("ipeds_race_ethnicity" = 'Hispanic', "gender" = 'F'),
                                              c("ipeds_race_ethnicity" = 'American Indian/Alaskan', "gender" = 'M'),
                                              c("ipeds_race_ethnicity" = 'American Indian/Alaskan', "gender" = 'F'),
                                              c("ipeds_race_ethnicity" = 'Asian', "gender" = 'M'),
                                              c("ipeds_race_ethnicity" = 'Asian', "gender" = 'F'),
                                              c("ipeds_race_ethnicity" = 'Black/African American', "gender" = 'M'),
                                              c("ipeds_race_ethnicity" = 'Black/African American', "gender" = 'F'),
                                              c("ipeds_race_ethnicity" = 'Hawaiian/Pacific Islander', "gender" = 'M'),
                                              c("ipeds_race_ethnicity" = 'Hawaiian/Pacific Islander', "gender" = 'F'),
                                              c("ipeds_race_ethnicity" = 'White', "gender" = 'M'),
                                              c("ipeds_race_ethnicity" = 'White', "gender" = 'F'),               
                                              c("ipeds_race_ethnicity" = 'Multiracial', "gender" = 'M'),
                                              c("ipeds_race_ethnicity" = 'Multiracial', "gender" = 'F'),
                                              c("ipeds_race_ethnicity" = 'Unspecified', "gender" = 'M'),
                                              c("ipeds_race_ethnicity" = 'Unspecified', "gender" = 'F') ) 
   
  columns = c("unitid", "survsect", "part", "section", "filler_1", "line", "filler_2", 
              "grrace01", "grrace02", "grrace25", "grrace26", "grrace27", "grrace28", 
              "grrace29", "grrace30", "grrace31", "grrace32", "grrace33", "grrace34",
              "grrace35", "grrace36", "grrace37", "grrace38", "grrace13", "grrace14") 
  df = data.frame(matrix(nrow=0, ncol=length(columns))) 
  for (rule in row_rules) {
    row <- c(rule[["unitid"]], rule[["survsect"]], rule[["part"]], rule[["section"]], " ", rule[["line"]], " ")
    if (rule[["part"]] == 'C') {
        v1 <- unique_count_given_rules(input_df, c(rule, "is_pell_ind" = TRUE) )
        v2 <- unique_count_given_rules(input_df, c(rule, "sub_loan_ind" = TRUE) )
        row <- c(row, v1, v2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
    }
    else {
      for (combination in race_ethnicity_gender_combinations) {
        row <- c(row, unique_count_given_rules(input_df, c(rule, combination) ) )   
      }     
    }
    df <- rbind(df, row)
    df[] <- lapply(df, as.character)
  }
  colnames(df) = columns
  return(df)
}

# OUTCOMES ####
format_outcomes_report <- function(df) {
   ipeds_outcomes_report <- df %>%
    mutate( recepient_type=str_pad(recepient_type, 2, side="right"),
            award_cert_and_cohort=str_pad(award_cert_and_cohort, 6, side="right"),
            award_as_and_exclusions=str_pad(award_as_and_exclusions, 6, side="right"),
            award_bs_and_filler=str_pad(award_bs_and_filler, 6, side="right"),
            filler_0=str_pad(filler_0, 6, side="right"),
            filler_1=str_pad(filler_1, 6, side="right"),
            still_enrolled=str_pad(still_enrolled, 6, side="right"),
            enrolled_another=str_pad(enrolled_another, 6, side="right"),
          )  
   return(ipeds_outcomes_report)
}
format_and_save_outcomes_report <- function(df, file) {
   ipeds_outcomes_report <- format_outcomes_report(df)
   write_delim(ipeds_outcomes_report, 
               file,
               quote="none",
               delim="",
               col_names=FALSE)  
}
   
generate_ipeds_outcomes_report <- function(input_df) {
    # TODO: finish filling out these rules
    row_rules <- list( 
                  # part A rules
                   c("is_pell_ind" = TRUE, "ipeds_cohort_desc" = "FTFT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "A", "line" = 1, "recipient_type" = 1 ),
                   c("is_pell_ind" = FALSE, "ipeds_cohort_desc" = "FTFT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "A", "line" = 1, "recipient_type" = 2 ),
                   c("is_pell_ind" = TRUE, "ipeds_cohort_desc" = "FTPT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "A", "line" = 2, "recipient_type" = 1 ),
                   c("is_pell_ind" = FALSE, "ipeds_cohort_desc" = "FTPT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "A", "line" = 2, "recipient_type" = 2 ),
                   c("is_pell_ind" = TRUE, "ipeds_cohort_desc" = "NFTFT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "A", "line" = 3, "recipient_type" = 1 ),
                   c("is_pell_ind" = FALSE, "ipeds_cohort_desc" = "NFTFT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "A", "line" = 3, "recipient_type" = 2 ),                  
                   c("is_pell_ind" = TRUE, "ipeds_cohort_desc" = "NFTPT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "A", "line" = 4, "recipient_type" = 1 ),
                   c("is_pell_ind" = FALSE, "ipeds_cohort_desc" = "NFTPT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "A", "line" = 4, "recipient_type" = 2 ),                  
                  # part B rules
                  c("is_pell_ind" = TRUE, "ipeds_cohort_desc" = "FTFT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "B", "line" = 1, "recipient_type" = 1 ),
                  c("is_pell_ind" = FALSE, "ipeds_cohort_desc" = "FTFT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "B", "line" = 1, "recipient_type" = 2 ), 
                   c("is_pell_ind" = TRUE, "ipeds_cohort_desc" = "FTPT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "B", "line" = 2, "recipient_type" = 1 ),
                  c("is_pell_ind" = FALSE, "ipeds_cohort_desc" = "FTPT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "B", "line" = 2, "recipient_type" = 2 ),                  
                  c("is_pell_ind" = TRUE, "ipeds_cohort_desc" = "NFTFT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "B", "line" = 3, "recipient_type" = 1 ),
                  c("is_pell_ind" = FALSE, "ipeds_cohort_desc" = "NFTFT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "B", "line" = 3, "recipient_type" = 2 ),                 
                  c("is_pell_ind" = TRUE, "ipeds_cohort_desc" = "NFTPT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "B", "line" = 4, "recipient_type" = 1 ),
                  c("is_pell_ind" = FALSE, "ipeds_cohort_desc" = "NFTPT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "B", "line" = 4, "recipient_type" = 2 ),                  
                  # part C rules
                   c("is_pell_ind" = TRUE, "ipeds_cohort_desc" = "FTFT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "C", "line" = 1, "recipient_type" = 1 ),
                   c("is_pell_ind" = FALSE, "ipeds_cohort_desc" = "FTFT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "C", "line" = 1, "recipient_type" = 2 ), 
                   c("is_pell_ind" = TRUE, "ipeds_cohort_desc" = "FTPT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "C", "line" = 2, "recipient_type" = 1 ),
                  c("is_pell_ind" = FALSE, "ipeds_cohort_desc" = "FTPT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "C", "line" = 2, "recipient_type" = 2 ),                  
                  c("is_pell_ind" = TRUE, "ipeds_cohort_desc" = "NFTFT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "C", "line" = 3, "recipient_type" = 1 ),
                  c("is_pell_ind" = FALSE, "ipeds_cohort_desc" = "NFTFT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "C", "line" = 3, "recipient_type" = 2 ),                 
                  c("is_pell_ind" = TRUE, "ipeds_cohort_desc" = "NFTPT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "C", "line" = 4, "recipient_type" = 1 ),
                  c("is_pell_ind" = FALSE, "ipeds_cohort_desc" = "NFTPT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "C", "line" = 4, "recipient_type" = 2 ),                 
                  # part D rules
                  c("is_pell_ind" = TRUE, "ipeds_cohort_desc" = "FTFT",
                       "unitid" = "230171", "survsect" = "OM1", "part" = "D", "line" = 1, "recipient_type" = 1 ),              
                  c("is_pell_ind" = FALSE, "ipeds_cohort_desc" = "FTFT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "D", "line" = 1, "recipient_type" = 2 ), 
                  c("is_pell_ind" = TRUE, "ipeds_cohort_desc" = "FTPT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "D", "line" = 2, "recipient_type" = 1 ),
                  c("is_pell_ind" = FALSE, "ipeds_cohort_desc" = "FTPT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "D", "line" = 2, "recipient_type" = 2 ),                  
                  c("is_pell_ind" = TRUE, "ipeds_cohort_desc" = "NFTFT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "D", "line" = 3, "recipient_type" = 1 ),
                  c("is_pell_ind" = FALSE, "ipeds_cohort_desc" = "NFTFT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "D", "line" = 3, "recipient_type" = 2 ),                 
                  c("is_pell_ind" = TRUE, "ipeds_cohort_desc" = "NFTPT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "D", "line" = 4, "recipient_type" = 1 ),
                  c("is_pell_ind" = FALSE, "ipeds_cohort_desc" = "NFTPT",
                        "unitid" = "230171", "survsect" = "OM1", "part" = "D", "line" = 4, "recipient_type" = 2 )          
                   )
  columns = c("unitid", "survsect", "part", "line", "recepient_type",  
              "award_cert_and_cohort", "award_as_and_exclusions", "award_bs_and_filler", "filler_0", "filler_1", "still_enrolled", "enrolled_another")
  df = data.frame(matrix(nrow=0, ncol=length(columns))) 
  for (rule in row_rules) {
    row <- c(rule[["unitid"]], rule[["survsect"]], rule[["part"]], rule[["line"]], rule[["recipient_type"]])
    if (rule[["part"]] == 'A') {
        v1 <- unique_count_given_rules(input_df, rule )
        v2 <- unique_count_given_rules(input_df, c(rule, "ipeds_exclusion_ind" = TRUE) )
        row <- c(row, v1, v2, 0, 0, 0, 0, 0)
    }
    else if (rule[["part"]] == 'B') {
        v1 <- unique_count_given_rules(input_df, c(rule, "highest_earned_degree_4_year_status" = "Certificates") )
        v2 <- unique_count_given_rules(input_df, c(rule, "highest_earned_degree_4_year_status" = "Associates") )
        v3 <- unique_count_given_rules(input_df, c(rule, "highest_earned_degree_4_year_status" = "Bachelors") )
        row <- c(row, v1, v2, v3, 0, 0, 0, 0)       
    }
    else if (rule[["part"]] == 'C') {
        v1 <- unique_count_given_rules(input_df, c(rule, "highest_earned_degree_6_year_status" = "Certificates") )
        v2 <- unique_count_given_rules(input_df, c(rule, "highest_earned_degree_6_year_status" = "Associates") )
        v3 <- unique_count_given_rules(input_df, c(rule, "highest_earned_degree_6_year_status" = "Bachelors") )
        row <- c(row, v1, v2, v3, 0, 0, 0, 0)          
    }
    else if (rule[["part"]] == 'D') {
        v1 <- unique_count_given_rules(input_df, c(rule, "highest_earned_degree_8_year_status" = "Certificates") )
        v2 <- unique_count_given_rules(input_df, c(rule, "highest_earned_degree_8_year_status" = "Associates") )
        v3 <- unique_count_given_rules(input_df, c(rule, "highest_earned_degree_8_year_status" = "Bachelors") )
        v4 <- unique_count_given_rules(input_df, c(rule, "ipeds_currently_enrolled" = TRUE) )
        v5 <- unique_count_given_rules(input_df, c(rule, "ipeds_transfered_out" = TRUE) )
        row <- c(row, v1, v2, v3, 0, 0, v4, v5)      
    }
    else {
      # nothing yet
    }
    df <- rbind(df, row)
    df[] <- lapply(df, as.character)
  }
  colnames(df) = columns
  return(df)
}

# GRADUATION 200 ####
format_graduation_200_report <- function(df) {
   ipeds_grad_200_report <- df %>%
   mutate( adexcl=str_pad(adexcl, 6, side="right"),
           compy7_8=str_pad(compy7_8, 6, side="right"),
           still_enrolled=str_pad(still_enrolled, 6, side="right")
         )  
}
format_and_save_graduation_200_report <- function(df, file) {
   ipeds_grad_200_report <- format_graduation_200_report(df)
   write_delim(ipeds_grad_200_report, 
               file,
               quote="none",
               delim="",
               col_names=FALSE)   
}
generate_ipeds_graduation_200_report <- function(input_df) {
    row_rules <- list( 
                  # part A rules
                   c("ipeds_150_ind" = FALSE,
                        "unitid" = "230171", "survsect" = "G21", "part" = "A")
                    )
  columns = c("unitid", "survsect", "part", "adexcl", "compy7_8", "still_enrolled")
  df = data.frame(matrix(nrow=0, ncol=length(columns))) 
  for (rule in row_rules) {
    row <- c( rule[["unitid"]], rule[["survsect"]], rule[["part"]] ) 
    # TODO: somehow indicate only new exclusions (from prev submission) 
    v1 <- unique_count_given_rules(input_df, c(rule, "ipeds_exclusion_ind" = TRUE) )
    v2 <- unique_count_given_rules(input_df, c(rule, "ipeds_200_ind" = TRUE, "ipeds_exclusion_ind" = FALSE) )
    v3 <- unique_count_given_rules(input_df, c(rule, "ipeds_currently_enrolled" = TRUE, "ipeds_200_ind" = FALSE) )
    row <- c(row, v1, v2, v3)
    df <- rbind(df, row)
    df[] <- lapply(df, as.character)
  }
  colnames(df) = columns
  return(df)
}

# CDS ####
format_cds_report <- function(df) {
  # does nothing for now
  return(df)
}
format_and_save_cds_report <- function(df, file) {
   cds_report <- format_cds_report(df)
   write_delim(cds_report, 
               file,
               quote="none",
               delim="",
               col_names=FALSE)   
}
generate_cds_report <- function(input_df) {
  r1_label <- "Initial 2015 cohort of first-time, full-time, bachelor's (or equivalent) degree-seeking undergraduate students"
  ##################### SECTION B4-B21-A ################################################
  r1c1 <- unique_count_given_rules(input_df, c("is_pell_ind" = TRUE))
  r1c2 <- unique_count_given_rules(input_df, c("sub_loan_ind" = TRUE))
  r1c3 <- unique_count_given_rules(input_df, c("no_pell_sub_loan_ind" = TRUE))
  # sum of row values
  r1c4 <- r1c1+r1c2+r1c3
  
  r2_label <- "Of the initial 2015 cohort, how many did not persist and did not graduate for the following reasons: Deceased, Permanently Disabled, Armed Forces, Foreign Aid Service of the Federal Government, Official church missions, or Report Total Allowable Exclusions"
  ##################### SECTION B4-B21-B ################################################   
  r2c1 <- unique_count_given_rules(input_df, c("is_pell_ind" = TRUE, "ipeds_exclusion_ind" = TRUE))
  r2c2 <- unique_count_given_rules(input_df, c("sub_loan_ind" = TRUE, "ipeds_exclusion_ind" = TRUE))
  r2c3 <- unique_count_given_rules(input_df, c("no_pell_sub_loan_ind" = TRUE, "ipeds_exclusion_ind" = TRUE))
  # sum of row values
  r2c4 <- r2c1+r2c2+r2c3
  
  r3_label <- "Final 2015 cohort, after adjusting for allowable exclusions"
  r3c1 <- r1c1-r2c1
  r3c2 <- r1c2-r2c2
  r3c3 <- r1c3-r2c3
  # sum of row values
  r3c4 <- r3c1+r3c2+r3c3
  
  r4_label <- "Of the initial 2015 cohort, how many completed the program in four years or less (by Aug. 31, 2019)"
  ##################### SECTION B4-B21-D ################################################ 
  r4c1 <- unique_count_given_rules(input_df, c("is_pell_ind" = TRUE,  "highest_earned_degree_4_year_status" = 'Bachelors'))
  r4c2 <- unique_count_given_rules(input_df, c("sub_loan_ind" = TRUE, "highest_earned_degree_4_year_status" = 'Bachelors'))
  r4c3 <- unique_count_given_rules(input_df, c("no_pell_sub_loan_ind" = TRUE, "highest_earned_degree_4_year_status" = 'Bachelors'))
  # sum of row values
  r4c4 <- r4c1+r4c2+r4c3
  
  r5_label <- "Of the initial 2015 cohort, how many completed the program in more than four years but in five years or less (after Aug. 31, 2019 and by Aug. 31, 2020)"
  ##################### SECTION B4-B21-E ################################################ 
  r5c1 <- unique_count_given_rules(input_df, c("is_pell_ind" = TRUE, "highest_earned_degree_between_4_to_5_year_status" = 'Bachelors'))
  r5c2 <- unique_count_given_rules(input_df, c("sub_loan_ind" = TRUE, "highest_earned_degree_between_4_to_5_year_status" = 'Bachelors'))
  r5c3 <- unique_count_given_rules(input_df, c("no_pell_sub_loan_ind" = TRUE, "highest_earned_degree_between_4_to_5_year_status" = 'Bachelors'))
  # sum of row values
  r5c4 <- r5c1+r5c2+r5c3
  
  r6_label <- "Of the initial 2015 cohort, how many completed the program in more than five years but in six years or less (after Aug. 31, 2020 and by Aug. 31, 2021)" 
  ##################### SECTION B4-B21-F ################################################ 
  r6c1 <- unique_count_given_rules(input_df, c("is_pell_ind" = TRUE, "highest_earned_degree_between_5_to_6_year_status" = 'Bachelors'))
  r6c2 <- unique_count_given_rules(input_df, c("sub_loan_ind" = TRUE, "highest_earned_degree_between_5_to_6_year_status" = 'Bachelors'))
  r6c3 <- unique_count_given_rules(input_df, c("no_pell_sub_loan_ind" = TRUE, "highest_earned_degree_between_5_to_6_year_status" = 'Bachelors'))
  # sum of row values
  r6c4 <- r6c1+r6c2+r6c3
  
  r7_label <- "Total graduating within six years (sum of lines D, E, and F)"
  r7c1 <- r4c1+r5c1+r6c1
  r7c2 <- r4c2+r5c2+r6c2
  r7c3 <- r4c3+r5c3+r6c3
  # sum of row values
  r7c4 <- r7c1+r7c2+r7c3 
  
  r8_label <- "Six-year graduation rate for 2015 cohort (G divided by C)"
  r8c1 <- r7c1/r3c1
  r8c2 <- r7c2/r3c2
  r8c3 <- r7c3/r3c3
  # sum of row values
  r8c4 <- r8c1+r8c2+r8c3  
  
  columns = c("Descriptions",
              "Recipients of a Federal Pell Grant", 
              "Recipients of a Subsidized Stafford Loan who did not receive a Pell Grant", 
              "Students who did not receive either a Pell Grant or a subsidized Stafford Loan", 
              "Total (sum of 3 columns to the left)")
  
   df <- data.frame( c(r1_label, r2_label, r3_label, r4_label, r5_label, r6_label, r7_label, r8_label),
                    c(r1c1, r2c1, r3c1, r4c1, r5c1, r6c1, r7c1, r8c1),
                    c(r1c2, r2c2, r3c2, r4c2, r5c2, r6c2, r7c2, r8c2),
                    c(r1c3, r2c3, r3c3, r4c3, r5c3, r6c3, r7c3, r8c3),
                    c(r1c4, r2c4, r3c4, r4c4, r5c4, r6c4, r7c4, r8c4) )
   
  colnames(df) = columns
  return(df)
}

