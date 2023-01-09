
#' Generate Student Submission File
#'
#' @param input_df A Data Frame. Must contain the following data fields: (season, academic_year_code, version_id, student_id,
#'                                                                        ssn, ssid, previous_student_id,
#'                                                                        last_name, first_name, middle_name, name_suffix,
#'                                                                        previous_last_name, previous_first_name, previous_middle_name,
#'                                                                        previous_name_suffix, local_address_zip_code, mailing_address_zip_code,
#'                                                                        us_citizenship_code, first_admit_county_code, first_admit_state_code,
#'                                                                        first_admit_country_iso_code, first_admit_state_code, birth_date, gender_code,
#'                                                                        is_hispanic_latino_ethnicity, is_asian, is_black, is_american_indian_alaskan,
#'                                                                        is_hawaiian_pacific_islander, is_white, is_international, is_other_race,
#'                                                                        residency_code, primary_major_cip_code, student_type_code,
#'                                                                        primary_level_class_id, primary_degree_id, level_id,
#'                                                                        institutional_cumulative_credits_earned, institutional_cumulative_gpa,
#'                                                                        transfer_cumulative_credits_earned, total_cumulative_clep_credits_earned,
#'                                                                        total_cumulative_ap_credits_earned, full_time_part_time_code, version_date,
#'                                                                        birth_date, first_admit_country_iso_code, high_school_code,
#'                                                                        house_bill_75_waiver, secondary_major_cip_code, act_composite_score,
#'                                                                        primary_major_cip_code, act_english_score, act_math_score, act_reading_score,
#'                                                                        act_science_score, high_school_graduation_date, institutional_gpa,
#'                                                                        is_pell_eligible, is_pell_awarded, is_bia, primary_major_college_id,
#'                                                                        primary_major_desc, secondary_major_college_id, secondary_major_desc,
#'                                                                        institutional_term_gpa, college_desc,
#'                                                                        secondary_ipeds_award_level_code).
#'
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return
#' A Data Frame, with all of the USHE elements required for upload submission.
#' This will also include intermediate values, used to calculate USHE data elements, if option is set.
#'
#' @examples
#' generate_student_submission_file()
#'
#' @export
#'
generate_student_submission_file <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  ushe_data_elements <- c("s_01", "s_02", "s_03", "s_04",
                          "s_05", "s_06", "s_07", "s_08",
                          "s_09", "s_10", "s_11", "s_12",
                          "s_13", "s_14", "s_15", "s_16",
                          "s_17", "s_18", "s_19", "s_20",
                          "s_21", "s_22", "s_23", "s_24",
                          "s_25", "s_26", "s_27", "s_28",
                          "s_29", "s_30", "s_31", "s_32",
                          "s_33", "s_34", "s_35", "s_36",
                          "s_37", "s_38", "s_39", "s_40",
                          "s_41", "s_42", "s_43", "s_44",
                          "s_45", "s_46", "s_47", "s_48",
                          "s_49", "s_50")

  output_df <- input_df %>%
    s_01() %>%
    s_02() %>%
    s_03() %>%
    s_04() %>%
    s_05() %>%
    s_06() %>%
    s_07() %>%
    s_08() %>%
    s_09() %>%
    s_10() %>%
    s_11() %>%
    s_12() %>%
    s_13() %>%
    s_14() %>%
    s_15() %>%
    s_16() %>%
    s_17() %>%
    s_18() %>%
    s_19() %>%
    s_20() %>%
    s_21() %>%
    s_22() %>%
    s_23() %>%
    s_24() %>%
    s_25() %>%
    s_26() %>%
    s_27() %>%
    s_28() %>%
    s_29() %>%
    s_30() %>%
    s_31() %>%
    s_32() %>%
    s_33() %>%
    s_34() %>%
    s_35() %>%
    s_36() %>%
    s_37() %>%
    s_38() %>%
    s_39() %>%
    s_40() %>%
    s_41() %>%
    s_42() %>%
    s_43() %>%
    s_44() %>%
    s_45() %>%
    s_46() %>%
    s_47() %>%
    s_48() %>%
    s_49() %>%
    s_50() %>%
    clean()

  if (!with_intermediates) {
    output_df <- output_df %>%
        dplyr::select( ushe_data_elements )
    }

  return(output_df)
}


#' Calculate USHE Element s_xx (Name of Element)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (list, of, required, data, fields).
#'
#'
#' @return Original data frame, with USHE data element s_xx appended.
#' @export
#'
#' @examples
#' s_xx()
#'
s_xx <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( intermediate_field_1 = "1") %>%
    mutate( intermediate_field_2 = "2") %>%
    # Append USHE data element s_xx
    mutate( s_xx = paste0(intermediate_field_1, intermediate_field_2) )

  return(output_df)
}

#' @rdname s_xx
#' @examples s_alias()
#' @export
s_alias <- s_xx

#' Calculate USHE Element s_04 (Student ID Flag)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Student ID Flag
#' - FIELD NAME: S_ID_FLAG
#' - FIELD FORMAT: Varchar, 1 Character
#' - DEFINITION: Flag indicating if Student ID is the student's actual Social Security Number or an institutionally assigned ID number (to identify students who don't have SSN's, i.e. International  Students).
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#'
#'
#' @param input_df A Data Frame. Must contain the following data fields: (ssn).
#'
#'
#' @return Original data frame, with USHE data element s_04 appended.
#' @export
#'
#' @examples
#' s_04()
#'
s_04 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_id_flag = if_else(is_valid_ssn(ssn),
                                'S',
                                'I' ) ) %>%
    # Append USHE data element s_04
    mutate( s_04 = s_id_flag  )

  return(output_df)
}

#' Calculate USHE Element s_05 (Previous ID)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (previous_student_id).
#'
#'
#' @return Original data frame, with USHE data element s_05 appended.
#' @export
#'
#' @examples
#' s_05()
#'
s_05 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_previous_id = previous_student_id) %>%
    # Append USHE data element s_05
    mutate( s_05 = previous_student_id )

  return(output_df)
}

#' Calculate USHE Element s_07 (Student Previous Name)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr coalesce
#'
#' @param input_df A Data Frame. Must contain the following data fields: (previous_last_name, previous_first_name, previous_middle_name, previous_name_suffix).
#'
#'
#' @return Original data frame, with USHE data element s_07 appended.
#' @export
#'
#' @examples
#' s_07()
#'
s_07 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_prev_last = coalesce(previous_last_name, ''),
            s_prev_first = coalesce(previous_first_name, ''),
            s_prev_middle = coalesce(previous_middle_name, ''),
            s_prev_suffix = coalesce(previous_name_suffix, '') ) %>%
    # Append USHE data element s_07
    mutate( s_07 = paste(s_prev_last, s_prev_first, s_prev_middle, s_prev_suffix, sep='|') )

  return(output_df)
}


#' Calculate USHE Element s_08 (Zip Code)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Current Zip Code
#' - FIELD NAME: S_CURR_ZIP
#' - FIELD FORMAT: Varchar, 10 Characters
#' - DEFINITION: The postal code of the student's current local address while attending classes.
#'
#'  Using Regex to check that zip code is in a valid US zip code format.
#'   More information can be found: https://regexlib.com/Search.aspx?k=us+zip+code
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr coalesce
#' @importFrom dplyr if_else
#' @importFrom stringr str_detect
#'
#' @param input_df A Data Frame. Must contain the following data fields: (local_address_zip_code, mailing_address_zip_code).
#'
#'
#' @return Original data frame, with USHE data element s_08 appended.
#' @export
#'
#' @examples
#' s_08()
#'
s_08 <- function(input_df=usheUtils::fake_student_df) {
  # Regex to check zip code, from:
  # https://regexlib.com/Search.aspx?k=us+zip+code

  zipcode_regex <- "^[0-9]{5}(-[0-9]{4})?$"

  output_df <- input_df %>%

  # Calculate intermediate fields
  mutate(s_curr_zip = coalesce(if_else(str_detect(local_address_zip_code, zipcode_regex),
                                       local_address_zip_code, NA_character_ ),
                               if_else(str_detect(mailing_address_zip_code, zipcode_regex),
                                       mailing_address_zip_code, NA_character_ ))) %>%
  # Append USHE data element s_08
  mutate( s_08 = s_curr_zip )

  return(output_df)
}


#' Calculate USHE Element s_09 (Citizenship Code)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Citizenship Status
#' - FIELD NAME: S_CITZ_CODE
#' - FIELD FORMAT: Varchar, 1 Character
#' - DEFINITION: Indicates student's current citizenship status.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (us_citizenship_code).
#'
#'
#' @return Original data frame, with USHE data element s_09 appended.
#' @export
#'
#' @examples
#' s_09()
#'
s_09 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_citz_code = case_when( us_citizenship_code == "6" ~ "9",
                                    TRUE ~ us_citizenship_code) ) %>%
    # Append USHE data element s_09
    mutate( s_09 = s_citz_code )

  return(output_df)
}


#' Calculate USHE Element s_11 (State Origin Code)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr case_when
#'
#' @param input_df A Data Frame. Must contain the following data fields: (first_admit_state_code, first_admit_country_iso_code).
#'
#'
#' @return Original data frame, with USHE data element s_11 appended.
#' @export
#'
#' @examples
#' s_11()
#'
s_11 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_state_origin = case_when( ( first_admit_country_iso_code != "US"
                                          & !is.na(first_admit_country_iso_code) ) ~ "XX",
                                        is.na(first_admit_state_code) ~ "UN",
                                        TRUE ~ first_admit_state_code ) ) %>%
    # Append USHE data element s_11
    mutate( s_11 = s_state_origin )

  return(output_df)
}

#' Calculate USHE Element s_15 (Residency)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Regent Residency Status
#' - FIELD NAME: S_REGENT_RES
#' - FIELD FORMAT: Varchar, 1 Character,
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (residency_code).
#'
#'
#' @return Original data frame, with USHE data element s_15 appended.
#' @export
#'
#' @examples
#' s_15()
#'
s_15 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_regent_res = case_when( residency_code == "R" ~ 'R',
                                      residency_code == "C" ~ 'R',
                                      residency_code == "N" ~ 'N',
                                      residency_code == "S" ~ ' ',
                                      residency_code == "H" ~ 'N',
                                      residency_code == "0" ~ ' ',
                                      residency_code == "A" ~ 'A',
                                      residency_code == "M" ~ 'M',
                                      residency_code == "G" ~ 'G') ) %>%
    # Append USHE data element s_15
    mutate( s_15 = s_regent_res )

  return(output_df)
}


#' Calculate USHE Element s_16 (CIP Code)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (primary_major_cip_code).
#'
#'
#' @return Original data frame, with USHE data element s_16 appended.
#' @export
#'
#' @examples
#' s_16()
#'
s_16 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_curr_cip = primary_major_cip_code) %>%
    # Append USHE data element s_16
    mutate( s_16 = s_curr_cip )

  return(output_df)
}


#' Calculate USHE Element s_17 (Registration Status)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Registration Status
#' - FIELD NAME: S_REG_STATUS
#' - FIELD FORMAT: Varchar, 2 Characters,
#' - DEFINITION: Student's entry status at the beginning of the term.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr case_when
#'
#' @param input_df A Data Frame. Must contain the following data fields: (student_type_code, high_school_graduation_date, term_start_date).
#'
#'
#' @return Original data frame, with USHE data element s_17 appended.
#' @export
#'
#' @examples
#' s_17()
#'
s_17 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(student_type_code_2 = case_when( ( student_type_code == "F" & is.na(high_school_graduation_date) == TRUE ) ~ "FF",
                                            # + 365 indicates ~1 year after high school graduation date
                                            ( student_type_code == "F" & as.Date(high_school_graduation_date) + 365 >= as.Date(term_start_date) ) ~ "FH",
                                            # + 365 indicates ~1 year after high school graduation date
                                            ( student_type_code == "F" & as.Date(high_school_graduation_date) + 365 <= as.Date(term_start_date) ) ~ "FF",
                                            TRUE ~ student_type_code ) ) %>%
    mutate(s_reg_status = case_when( student_type_code == "1" ~ 'NG', # New Graduate
                                     student_type_code == "2" ~ 'TG', # Transfer Graduate
                                     student_type_code == "3" ~ 'RG', # Readmit Graduate
                                     student_type_code == "5" ~ 'CG', # Continuing Graduate
                                     student_type_code == "C" ~ 'CS', # Continuing Registration
                                     student_type_code == "H" ~ 'HS', # High School
                                     student_type_code == "P" ~ 'NM', # Personal Interest, Non-Degree
                                     student_type_code == "R" ~ 'RS', # Readmit
                                     student_type_code == "T" ~ 'TU', # Transfer
                                     TRUE ~ student_type_code_2) ) %>%
    # Append USHE data element s_17
    mutate( s_17 = s_reg_status )

  return(output_df)
}


#' Calculate USHE Element s_18 (Class Level)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (primary_level_class_id).
#'
#'
#' @return Original data frame, with USHE data element s_18 appended.
#' @export
#'
#' @examples
#' s_18()
#'
s_18 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_level = primary_level_class_id) %>%
    # Append USHE data element s_18
    mutate( s_18 = s_level )

  return(output_df)
}

#' Calculate USHE Element s_20 (Total Cumulative Undergrad Hrs)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Cumulative Institutional Undergraduate Hours
#' - FIELD NAME: S_CUM_HRS_UGRAD
#' - FIELD FORMAT: Numeric(5,1)
#' - DEFINITION: Total number of credit hours the student has earned as an undergraduate student at this institution.
#'               Hours should be semester hours. Value should be zero for graduate students (see element S 18).
#'               (Does not include transfer hours.)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @param input_df A Data Frame. Must contain the following data fields: (level_id, institutional_cumulative_credits_earned).
#'
#'
#' @return Original data frame, with USHE data element s_20 appended.
#' @export
#'
#' @examples
#' s_20()
#'
s_20 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_cum_hrs_ugrad = if_else(level_id == "GR",
                                     0,
                                     round(institutional_cumulative_credits_earned, digits = 1))) %>%
    # Append USHE data element s_20
    mutate( s_20 = s_cum_hrs_ugrad )

  return(output_df)
}


#' Calculate USHE Element s_21 (Cumulative Inst Undergrad GPA)
#'
#' @details
#'
#' **USHE Documentation**
#' * ELEMENT NAME: Cumulative Institutional Undergraduate GPA
#' * FIELD NAME: S_CUM_GPA_UGRAD
#' * FIELD FORMAT: Numeric (4,3)
#' * DEFINITION: Student's Cumulative GPA as of the present academic term as an undergraduate student.
#'               This should tie to Cumulative Institutional Hours field.
#'               No transfer hours are to be included.
#'               All credit hours should represent average course grade on a 4.0 scale.
#'               This GPA ties to earned credits in  Cumulative Institutional Undergraduate Hours.
#'               Value should be zero for graduate students (see element  S-18).
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @param input_df A Data Frame. Must contain the following data fields: (level_id, institutional_cumulative_gpa).
#'
#'
#' @return Original data frame, with USHE data element s_21 appended.
#' @export
#'
#' @examples
#' s_21()
#'
s_21 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_cum_gpa_ugrad = if_else(level_id == "GR",
                                    0,
                                    round(institutional_cumulative_gpa, digits =  3))) %>%
    # Append USHE data element s_21
    mutate( s_21 = s_cum_gpa_ugrad )

  return(output_df)
}


#' Calculate USHE Element s_22 (Total Cumulative Inst Grad Hrs)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Cumulative Institutional Graduate Hours
#' - FIELD NAME: S_CUM_HRS_GRAD
#' - FIELD FORMAT: Numeric (5,1)
#' - DEFINITION: Total number of credit hours the student has earned as a graduate student at this institution.
#'               Hours should be semester hours.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (level_id, institutional_cumulative_credits_earned).
#'
#'
#' @return Original data frame, with USHE data element s_22 appended.
#' @export
#'
#' @examples
#' s_22()
#'
s_22 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_cum_hrs_grad = if_else(level_id == "GR",
                                    round(institutional_cumulative_credits_earned, digits =  1),
                                    0) ) %>%
    # Append USHE data element s_22
    mutate( s_22 = s_cum_hrs_grad )

  return(output_df)
}


#' Calculate USHE Element s_23 (Cumulative Inst Grad GPA)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Cumulative Institutional Graduate GPA
#' - FIELD NAME: S_CUM_GPA_GRAD
#' - FIELD FORMAT: Numeric (4,3)
#' - DEFINITION: Student's Cumulative GPA as of the present academic term as a graduate student.
#'               All credit hours should represent average course grade on a 4.0 scale.
#'               This GPA ties to earned credits in Cumulative Institutional Graduate Hours.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @param input_df A Data Frame. Must contain the following data fields: (level_id, institutional_cumulative_gpa).
#'
#'
#' @return Original data frame, with USHE data element s_23 appended.
#' @export
#'
#' @examples
#' s_23()
#'
s_23 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_cum_gpa_grad = if_else(level_id == "GR",
                                    round(institutional_cumulative_gpa, digits = 3),
                                    0) ) %>%
    # Append USHE data element s_20
    mutate( s_23 = s_cum_gpa_grad )

  return(output_df)
}


#' Calculate USHE Element s_24 (Total Cum U-grad Transfer Hrs Accepted)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Cumulative Undergraduate Transfer Accepted
#' - FIELD NAME: S_TRANS_TOTAL
#' - FIELD FORMAT: Numeric (5,1)
#' - DEFINITION: Total number of credit hours accepted to date at your institution (e.g. transfer credit from another institution).
#'               Applies to undergraduate and graduate hours.
#'               This includes credit from other institutions, Challenge, Military, and other test credit except CLEP and AP (see S-32 and S-33).
#'               This does not include credits earned at your institution (see S-20).
#'               Hours should all be converted to semester hours.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (transfer_cumulative_credits_earned).
#'
#'
#' @return Original data frame, with USHE data element s_24 appended.
#' @export
#'
#' @examples
#' s_24()
#'
s_24 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_trans_total = round(transfer_cumulative_credits_earned, digits = 1))  %>%
    # Append USHE data element s_24
    mutate(s_24 = s_trans_total)

  return(output_df)
}


#' Calculate USHE Element s_25 (Part-time/Full-time)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (full_time_part_time_code).
#'
#'
#' @return Original data frame, with USHE data element s_25 appended.
#' @export
#'
#' @examples
#' s_25()
#'
s_25 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_pt_ft = full_time_part_time_code) %>%
    # Append USHE data element s_25
    mutate( s_25 = s_pt_ft )

  return(output_df)
}


#' Calculate USHE Element s_26 (Age)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Age at 3rd Week
#' - FIELD NAME: S_AGE
#' - FIELD FORMAT: Numeric (3,0)
#' - DEFINITION: The age of the student at third week extract date.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (version_date, birth_date).
#'
#'
#' @return Original data frame, with USHE data element s_26 appended.
#' @export
#'
#' @examples
#' s_26()
#'
s_26 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_age = floor( as.numeric( (as.Date(version_date) - as.Date(birth_date)) / 365.24 ) ) ) %>%
    # Append USHE data element s_26
    mutate( s_26 = s_age )

  return(output_df)
}


#' Calculate USHE Element s_27 (Country Origin Codes)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (first_admit_country_iso_code).
#'
#'
#' @return Original data frame, with USHE data element s_27 appended.
#' @export
#'
#' @examples
#' s_27()
#'
s_27 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_country_origin = first_admit_country_iso_code) %>%
    # Append USHE data element s_27
    mutate( s_27 = s_country_origin )

  return(output_df)
}


#' Calculate USHE Element s_29 (Non Resident Tuition Waiver)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (house_bill_75_waiver).
#'
#'
#' @return Original data frame, with USHE data element s_29 appended.
#' @export
#'
#' @examples
#' s_29()
#'
s_29 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    #mutate( s_hb75_waiver = house_bill_75_waiver ) %>%
    mutate( s_hb75_waiver = if_else(house_bill_75_waiver > 100, 100, house_bill_75_waiver) ) %>%
    # Append USHE data element s_29
    mutate( s_29 = s_hb75_waiver )

  return(output_df)
}


#' Calculate USHE Element s_30 (CIP Code for Second Major)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (secondary_major_cip_code).
#'
#'
#' @return Original data frame, with USHE data element s_30 appended.
#' @export
#'
#' @examples
#' s_30()
#'
s_30 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_curr_cip2 = secondary_major_cip_code ) %>%
    # Append USHE data element s_30
    mutate( s_30 = s_curr_cip2 )

  return(output_df)
}


#' Calculate USHE Element s_31 (Cumulative Membership Hours)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: ().
#'
#'
#' @return Original data frame, with USHE data element s_31 appended.
#' @export
#'
#' @examples
#' s_31()
#'
s_31 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    # Na because we do not currently have Cumulative Membership Hours.
    mutate( s_cum_membership = NA ) %>%
    # Append USHE data element s_31
    mutate( s_31 = s_cum_membership )

  return(output_df)
}


#' Calculate USHE Element s_32 (Total Cum U-grad CLEP Cr Accepted)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (total_cumulative_clep_credits_earned).
#'
#'
#' @return Original data frame, with USHE data element s_32 appended.
#' @export
#'
#' @examples
#' s_32()
#'
s_32 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_total_clep = round(total_cumulative_clep_credits_earned, digits =  1) ) %>%
    # Append USHE data element s_32
    mutate( s_32 = s_total_clep )

  return(output_df)
}


#' Calculate USHE Element s_33 (Total Cum U-grad AP Cr Accepted)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (total_cumulative_ap_credits_earned).
#'
#'
#' @return Original data frame, with USHE data element s_33 appended.
#' @export
#'
#' @examples
#' s_33()
#'
s_33 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_total_ap = round(total_cumulative_ap_credits_earned, digits = 1) ) %>%
    # Append USHE data element s_33
    mutate( s_33 = s_total_ap )

  return(output_df)
}


#' Calculate USHE Element s_36 (ACT Composite Score)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (act_composite_score).
#'
#'
#' @return Original data frame, with USHE data element s_36 appended.
#' @export
#'
#' @examples
#' s_36()
#'
s_36 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_act = act_composite_score) %>%
    # Append USHE data element s_36
    mutate( s_36 = s_act )

  return(output_df)
}


#' Calculate USHE Element s_37 (Student's Long-term Intended CIP)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (primary_major_cip_code).
#'
#'
#' @return Original data frame, with USHE data element s_37 appended.
#' @export
#'
#' @examples
#' s_37()
#'
s_37 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_intent_cip = if_else( primary_major_cip_code == "999999",
                                    "240102",
                                    primary_major_cip_code ) ) %>%
    # Append USHE data element s_37
    mutate( s_37 = s_intent_cip )

  return(output_df)
}


#' Calculate USHE Element s_38 (ACT English Sub-score)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (act_english_score).
#'
#'
#' @return Original data frame, with USHE data element s_38 appended.
#' @export
#'
#' @examples
#' s_38()
#'
s_38 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_act_eng = act_english_score) %>%
    # Append USHE data element s_38
    mutate( s_38 = s_act_eng )

  return(output_df)
}

#' Calculate USHE Element s_39 (ACT Math Sub-score)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (act_math_score).
#'
#'
#' @return Original data frame, with USHE data element s_39 appended.
#' @export
#'
#' @examples
#' s_39()
#'
s_39 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_act_math = act_math_score ) %>%
    # Append USHE data element s_39
    mutate( s_39 = s_act_math )

  return(output_df)
}

#' Calculate USHE Element s_40 (ACT Reading Sub-score)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (act_reading_score).
#'
#'
#' @return Original data frame, with USHE data element s_40 appended.
#' @export
#'
#' @examples
#' s_40()
#'
s_40 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_act_read = act_reading_score ) %>%
    # Append USHE data element s_40
    mutate( s_40 = s_act_read )

  return(output_df)
}


#' Calculate USHE Element s_41 (ACT Science Sub-score)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (act_science_score).
#'
#'
#' @return Original data frame, with USHE data element s_41 appended.
#' @export
#'
#' @examples
#' s_41()
#'
s_41 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_act_sci = act_science_score) %>%
    # Append USHE data element s_41
    mutate( s_41 = s_act_sci )

  return(output_df)
}


#' Calculate USHE Element s_42 (High School Graduating Date)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_remove_all
#'
#' @param input_df A Data Frame. Must contain the following data fields: (high_school_graduation_date).
#'
#'
#' @return Original data frame, with USHE data element s_42 appended.
#' @export
#'
#' @examples
#' s_42()
#'
s_42 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_hs_grad_date = str_remove_all(high_school_graduation_date, "-")) %>%
    # Append USHE data element s_42
    mutate( s_42 = s_hs_grad_date )

  return(output_df)
}

#' Calculate USHE Element s_43 (Term GPA)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (institutional_term_gpa).
#'
#'
#' @return Original data frame, with USHE data element s_43 appended.
#' @export
#'
#' @examples
#' s_43()
#'
s_43 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_term_gpa = institutional_term_gpa ) %>%
    # Append USHE data element s_43
    mutate( s_43 = s_term_gpa )

  return(output_df)
}

#' Calculate USHE Element s_44 (Pell Indicator)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Pell Indicator
#' - FIELD NAME: S_PELL
#' - FIELD FORMAT: Varchar, 1 Character,
#' - DEFINITION: This flags the student as having received a Pell grant during the reporting semester.
#'               This field is needed for end of term extracts only.
#'               It will be accepted at 3rd week if it is convenient for the institution to provide it.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr case_when
#'
#' @param input_df A Data Frame. Must contain the following data fields: (is_pell_eligible, is_pell_awarded).
#'
#'
#' @return Original data frame, with USHE data element s_44 appended.
#' @export
#'
#' @examples
#' s_44()
#'
s_44 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_pell = case_when( as.logical(is_pell_awarded) ~ "R",
                                as.logical(is_pell_eligible) ~ "E" ) ) %>%
    # Append USHE data element s_44
    mutate( s_44 = s_pell )

  return(output_df)
}

#' Calculate USHE Element s_45 (BIA Flag)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: BIA Flag
#' - FIELD NAME: S_BIA
#' - FIELD FORMAT: Varchar, 1 Character,
#' - DEFINITION: This flags the student as having received assistance from the Bureau of Indian Affairs (BIA) during the reporting semester.
#'               This field is needed for end of term extracts only.
#'               It will be  accepted at 3rd week if it is convenient for the institution to provide it.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @param input_df A Data Frame. Must contain the following data fields: (is_bia).
#'
#'
#' @return Original data frame, with USHE data element s_45 appended.
#' @export
#'
#' @examples
#' s_45()
#'
s_45 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_bia = if_else(is_bia, "B", "")) %>%
    # Append USHE data element s_45
    mutate( s_45 = s_bia )

  return(output_df)
}

#' Calculate USHE Element s_46 (Major College)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (college_desc).
#'
#'
#' @return Original data frame, with USHE data element s_46 appended.
#' @export
#'
#' @examples
#' s_46()
#'
s_46 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_college = college_desc) %>%
    # Append USHE data element s_46
    mutate( s_46 = s_college )

  return(output_df)
}

#' Calculate USHE Element s_47 (Major Name)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (primary_major_desc).
#'
#'
#' @return Original data frame, with USHE data element s_47 appended.
#' @export
#'
#' @examples
#' s_47()
#'
s_47 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_major = primary_major_desc) %>%
    # Append USHE data element s_47
    mutate( s_47 = s_major )

  return(output_df)
}

#' Calculate USHE Element s_48 (Major College for Second Major)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (secondary_major_college_id).
#'
#'
#' @return Original data frame, with USHE data element s_48 appended.
#' @export
#'
#' @examples
#' s_48()
#'
s_48 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_college2 = secondary_major_college_id ) %>%
    # Append USHE data element s_48
    mutate( s_48 = s_college2 )

  return(output_df)
}

#' Calculate USHE Element s_49 (Major Name for Second Major)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (secondary_major_desc).
#'
#'
#' @return Original data frame, with USHE data element s_49 appended.
#' @export
#'
#' @examples
#' s_49()
#'
s_49 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_major2 = secondary_major_desc) %>%
    # Append USHE data element s_49
    mutate( s_49 = s_major2 )

  return(output_df)
}

#' Calculate USHE Element s_50 (Degree Intent of Second Degree)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Degree Intent of Second Degree
#' - FIELD NAME: S_DEG_INTENT2
#' - FIELD FORMAT: Varchar, 2 Character
#' - DEFINITION: The length/nature of the degree that the student is working toward.
#'               S_DEG_INTENT2 with S_CURR_CIP2 represents the student’s major and ties to S_MAJOR2
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (secondary_ipeds_award_level_code).
#'
#'
#' @return Original data frame, with USHE data element s_50 appended.
#' @export
#'
#' @examples
#' s_50()
#'
s_50 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_deg_intent2 = secondary_ipeds_award_level_code) %>%
    # Append USHE data element s_50
    mutate( s_50 = s_deg_intent2 )

  return(output_df)
}
