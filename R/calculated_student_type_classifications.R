library(pins)
library(tidyverse)

# Obtain the API key from environment variable.
api_key <- Sys.getenv("RSCONNECT_SERVICE_USER_API_KEY")
if (api_key == "") {
  api_key <- keyring::key_get("pins", "api_key") 
}
# Register the connection to the pinning board.
board_register_rsconnect(key=api_key, server="") # fill in
# pin name set from original pin
pin_name <- "student_type_audit_student_type_determination_variables_pin"
# pull data from the pin
student_type_determination_variables <- pin_get(pin_name, board="rsconnect")

# hard coded parameter term
parameter_term <- '202240'
term_two_terms_ago <- '202220'

# the following this is NOT what the intention is:
#if not_same(sgbstdn_student_type, calculated_student_type):
#    choose calculated_student_type
# the intention is to compare these values and validate them in our system of record ####

# GROUP: High School ####
# if the expected high school graduation date is greater than the passed in parameter date, 
# then the student is a high school student on the provided parameter date
student_type_determination_variables <- student_type_determination_variables %>%
    mutate(calculated_student_type = case_when(
      # GROUP High school ####
      #if expected_high_school_graduation_date_as_term > parameter_term
      #    return "High School"
      expected_high_school_graduation_term > parameter_term ~ "High School",
      
      # GROUP Undergraduate ####
      # first_term_enrolled, last_term_enrolled after expected_high_school_graduation_date
      # and only looking at UNDERGRADUATE LEVEL transcript records for local institution.
        
      ## Freshman ####
      #if first_term_enrolled_as_undergraduate_after_hs_grad == parameter_term:
      #  return "Freshman"
      first_term_enrolled_on_or_after_expected_hs_graduation_term == parameter_term & enrollment_level == "UG" 
        ~ "Freshman",
      
      ## Transfer ####
      # has_transfer_undergraduate_credits after expected_high_school_graduation_date.
      #if has_transfer_undergraduate_credits_after_hs_grad
      #   AND (first_term_enrolled_as_undergraduate_after_hs_grad == parameter_term):
      #  return "Transfer Undergraduate"
      has_transfer_credits_on_or_after_expected_hs_graduation_term
          & first_term_enrolled_on_or_after_expected_hs_graduation_term == parameter_term 
          & enrollment_level == "UG" 
          & transfer_credits_level == "UG" 
        ~ "Transfer Undergraduate",
      
      ## Continuing ####
      #if last_term_enrolled_as_undergraduate_after_hs_grad >= (parameter_term - two_terms):
      #  return "Continuing Undergraduate"
      last_term_enrolled_on_or_after_expected_hs_graduation_term >= term_two_terms_ago 
          & enrollment_level == "UG" 
        ~ "Continuing Undergraduate",
      
      ## Readmit ####
      #if last_term_enrolled_as_undergraduate_after_hs_grad < (parameter_term - two_terms):
      #  return "Readmit Undergraduate"
      last_term_enrolled_on_or_after_expected_hs_graduation_term < term_two_terms_ago 
          & enrollment_level == "UG" 
        ~ "Continuing Undergraduate",
      
      
      # GROUP Graduate ####
      # first_term_enrolled and last_term_enrolled only looking at GRADUATE LEVEL transcript records for local institution
      ## New Graduate ####
      #if first_term_enrolled_as_graduate == parameter_term:
      #    return "New Graduate"
      first_term_enrolled_on_or_after_expected_hs_graduation_term == parameter_term
        & enrollment_level == "GR" 
      ~ "New Graduate",
      
      ## Transfer Graduate ####
      #if has_transfer_graduate_credits & ( first_term_enrolled_as_graduate == parameter_term )
      #    return "Transfer Graduate"
       
      has_transfer_credits_on_or_after_expected_hs_graduation_term
        & first_term_enrolled_on_or_after_expected_hs_graduation_term == parameter_term 
        & enrollment_level == "GR" 
        & transfer_credits_level == "GR" 
      ~ "Transfer Graduate",
      
      ## Continuing Graduate ####
      #if last_term_enrolled_as_graduate >= (parameter_term - two_terms)
      #    return "Continuing Graduate"
      
      last_term_enrolled_on_or_after_expected_hs_graduation_term >= term_two_terms_ago
          & enrollment_level == "GR" 
        ~ "Continuing Graduate",
      
      ## Readmit Graduate ####
      #if last_term_enrolled_as_graduate < (parameter_term - two_terms)
      #    return "Readmit Graduate"
      
      last_term_enrolled_on_or_after_expected_hs_graduation_term < term_two_terms_ago
          & enrollment_level == "GR" 
        ~ "Continuing Graduate",
      
      # GROUP MISC ####
      ## Personal Interest, Non-Degree ####
      # can only be determined by their current major
      # non-high school
      #if current_major == "ND-CE" | current_major == "ND-ESL":
      #  return "Personal Interest, Non-Degree Seeking"
      TRUE 
        ~ "Personal Interest, Non-Degree Seeking" )
    )
