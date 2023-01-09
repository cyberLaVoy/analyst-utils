#' Generate Graduation Submission File
#'
#' @param input_df A Data Frame. Must contain the following data fields: (student_id,
#'                                                                        ssn,
#'                                                                        last_name,
#'                                                                        first_name,
#'                                                                        middle_name,
#'                                                                        name_suffix,
#'                                                                        first_admit_state_code,
#'                                                                        first_admit_county_code,
#'                                                                        first_admit_country_iso_code,
#'                                                                        birth_date,
#'                                                                        gender_code,
#'                                                                        is_hispanic_latino_ethnicity,
#'                                                                        is_asian,
#'                                                                        is_black,
#'                                                                        is_american_indian_alaskan,
#'                                                                        is_hawaiian_pacific_islander,
#'                                                                        is_white,
#'                                                                        is_international,
#'                                                                        is_other_race,
#'                                                                        graduation_date,
#'                                                                        primary_major_cip_code,
#'                                                                        degree_id,
#'                                                                        degree_status_code,
#'                                                                        cumulative_graduation_gpa,
#'                                                                        transfer_cumulative_credits_earned,
#'                                                                        total_cumulative_ap_credits_earned,
#'                                                                        total_cumulative_clep_credits_earned,
#'                                                                        overall_cumulative_credits_earned,
#'                                                                        total_remedial_hours,
#'                                                                        total_cumulative_credits_attempted_other_sources,
#'                                                                        level_id,
#'                                                                        previous_degree_type,
#'                                                                        ipeds_award_level_code,
#'                                                                        required_credits,
#'                                                                        high_school_code,
#'                                                                        ssid,
#'                                                                        earned_contact_hrs ,
#'                                                                        program_hrs,
#'                                                                        graduated_term_id,
#'                                                                        financial_aid_year_id,
#'                                                                        season,
#'                                                                        college_desc,
#'                                                                        primary_major_desc,
#'                                                                        degree_desc)
#'
#'
#'
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return
#' A Data Frame, with all of the USHE elements required for upload submission.
#' This will also include intermediate values, used to calculate USHE data elements, if option is set.
#'
#' @examples
#' generate_graduation_submission_file()
#'
#' @export
#'
generate_graduation_submission_file <- function(input_df=usheUtils::fake_graduation_df, with_intermediates=FALSE) {

  ushe_data_elements <- c("g_01", "g_02", "g_03", "g_04",
                          "g_05", "g_06", "g_07", "g_08",
                          "g_09", "g_10", "g_11", "g_12",
                          "g_13", "g_14", "g_15", "g_16",
                          "g_17", "g_18", "g_19", "g_20",
                          "g_21", "g_22", "g_23", "g_24",
                          "g_25", "g_26", "g_27", "g_28")

  output_df <- input_df %>%
    g_01() %>%
    g_02() %>%
    g_03() %>%
    g_04() %>%
    g_05() %>%
    g_06() %>%
    g_07() %>%
    g_08() %>%
    g_09() %>%
    g_10() %>%
    g_11() %>%
    g_12() %>%
    g_13() %>%
    g_14() %>%
    g_15() %>%
    g_16() %>%
    g_17() %>%
    g_18() %>%
    g_19() %>%
    g_20() %>%
    g_21() %>%
    g_22() %>%
    g_23() %>%
    g_24() %>%
    g_25() %>%
    g_26() %>%
    g_27() %>%
    g_28()


  if (!with_intermediates) {
    output_df <- output_df %>%
      dplyr::select( ushe_data_elements )
  }

  return(output_df)
}


#' Calculate USHE Element g_09 (Degree CIP Code)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Degree CIP Code
#' - FIELD NAME: g_cip
#' - FIELD FORMAT: Varchar, 6 Characters
#' - DEFINITION: Use the 2010 version of the Classification of Instructional Programs (CIP) to best  identify the specific programs in which the degree is awarded.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (primary_major_cip_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element g_09 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' g_09()
#'
g_09 <- function(input_df=usheUtils::fake_graduation_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(g_cip = primary_major_cip_code) %>%
    # Append USHE data element g_09
    mutate( g_09 = g_cip  )

  return(output_df)
}


#' Calculate USHE Element g_11 (Graduation GPA )
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Graduation GPA
#' - FIELD NAME: g_gpa
#' - FIELD FORMAT: Numeric (4,3)
#' - DEFINITION: Student's overall cumulative GPA tied to their graduation award. All credit hours should represent average course grade on a 4.000 scale.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (cumulative_graduation_gpa).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element g_11 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' g_11()
#'

g_11 <- function(input_df=usheUtils::fake_graduation_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(g_gpa = round(cumulative_graduation_gpa, digits = 3) )%>%
    # Append USHE data element g_11
    mutate( g_11 = g_gpa  )

  return(output_df)
}

#' Calculate USHE Element (Total Cum Transfer Hrs Accepted)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Cumulative Transfer Semester Credit Hours Accepted
#' - FIELD NAME: G_TRANS_TOTAL
#' - FIELD FORMAT: Numeric (5,1)
#' - DEFINITION: Total number of credit hours accepted by graduation date at your institution relevant to the degree being awarded.
#'  (e.g. Transfer credit from another institution). This does not include credits earned at your institution (see S-20).
#'  This does not include AP, CLEP, Challenge, or Military  Credit. These hours are included in the G_HRS_OTHER field.
#'  Hours should all be converted to  semester hours.

#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (transfer_cumulative_credits_earned, total_cumulative_ap_credits_earned, total_cumulative_clep_credits_earned ).
#'
#'
#' @return Original data frame, with USHE data element g_12 appended.
#' @export
#'
#' @examples
#' g_12()
#'
g_12 <- function(input_df=usheUtils::fake_graduation_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( g_trans_total_itermediate = (as.numeric(transfer_cumulative_credits_earned) -
                                         as.numeric(total_cumulative_credits_attempted_other_sources)) ) %>%
    mutate( g_trans_total = round(g_trans_total_itermediate, digits = 1) )  %>%
    # Append USHE data element g_12
    mutate(g_12 = g_trans_total)

  return(output_df)
}

#' Calculate USHE Element g_13 (Total Hours at Graduation )
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Total Hours at Graduation
#' - FIELD NAME: g_grad_hrs
#' - FIELD FORMAT: Numeric (5,1)
#' - DEFINITION: Total number of overall hours relevant to the degree awarded when the student graduated.
#'   This field should include all college-level hours earned for undergraduate or graduate  coursework.
#'   Remedial hours should be excluded. Hours should all be converted to semester
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr coalesce
#'
#' @param input_df A Data Frame. Must contain the following data fields: (overall_cumulative_credits_earned, total_remedial_hours).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element g_13 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' g_13()
#'
g_13 <- function(input_df=usheUtils::fake_graduation_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(overall_cumulative_credits_earned = coalesce(overall_cumulative_credits_earned, 0) ) %>%
    mutate(total_remedial_hours = coalesce(total_remedial_hours, 0) ) %>%
    mutate(g_grad_hrs_intermediate = overall_cumulative_credits_earned - total_remedial_hours) %>%
    mutate(g_grad_hrs = round(g_grad_hrs_intermediate, digits = 1) ) %>%
    # Append USHE data element g_13
    mutate( g_13 = g_grad_hrs  )

  return(output_df)
}

#' Calculate USHE Element g_14 (Accepted Credit from Other Sources )
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Accepted Credit from Other Sources
#' - FIELD NAME: g_hrs_other
#' - FIELD FORMAT: Numeric (4,1)
#' - DEFINITION: Hours from AP credit, CLEP test, Language test, Challenge, Military, etc. Hours  should all be converted to semester hours.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (total_cumulative_credits_attempted_other_sources).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element g_14 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' g_14()
#'
g_14 <- function(input_df=usheUtils::fake_graduation_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(g_hrs_other = round(as.numeric(total_cumulative_credits_attempted_other_sources), digits = 1) ) %>%
    # Append USHE data element g_14
    mutate( g_14 = g_hrs_other  )

  return(output_df)
}

#' Calculate USHE Element g_15 (Remedial Hours )
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Remedial Hours
#' - FIELD NAME: g_remedial_hrs
#' - FIELD FORMAT: Numeric (4,1)
#' - DEFINITION: Remedial Hours which are included in S-20. Hours should all be converted to  semester hours. Applies only to undergrad degrees; grad degrees should be 0
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (total_remedial_hours, level_id).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element g_15 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' g_15()
#'
g_15 <- function(input_df=usheUtils::fake_graduation_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(g_remedial_hrs = ifelse(level_id == "UG", round(as.numeric(total_remedial_hours), digits = 1), "0") ) %>%
    # Append USHE data element g_15
    mutate( g_15 = g_remedial_hrs  )

  return(output_df)
}

#' Calculate USHE Element g_16 (Previous Degree Type  )
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Previous Degree Type
#' - FIELD NAME: g_prev_deg_type
#' - FIELD FORMAT: Varchar, 3 Characters
#' - DEFINITION: Type of highest degree awarded earned prior to this reporting period at any  institution, including your own institution
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (previous_degree_type).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element g_16 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' g_16()
#'
g_16 <- function(input_df=usheUtils::fake_graduation_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(g_prev_deg_type = previous_degree_type ) %>%
    # Append USHE data element g_16
    mutate( g_16 = g_prev_deg_type  )

  return(output_df)
}


#' Calculate USHE Element g_18 (Required Hours for Degree)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Required Hours for Degree
#' - FIELD NAME: g_req_hrs_deg
#' - FIELD FORMAT: Numeric(3,0)
#' - DEFINITION: The minimum number of credit hours required for the degree including any course pre requisites.
#' Not to include any major remedial hours. Hours should all be converted to semester hours.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (required_credits).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element g_18 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' g_18()
#'
g_18 <- function(input_df=usheUtils::fake_graduation_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(g_req_hrs_deg = required_credits ) %>%
    # Append USHE data element g_18
    mutate( g_18 = g_req_hrs_deg  )

  return(output_df)
}



#' Calculate USHE Element g_22 (Workforce Education Earned Contact Hours)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Workforce Education Earned Contact Hours
#' - FIELD NAME: g_we_earned_contact_hrs
#' - FIELD FORMAT: Numeric(5,1)
#' - DEFINITION:The number of noncredit program hours the student earned in completion of the  Workforce Education Certificate program.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (earned_contact_hrs).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element g_22 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' g_22()
#'
g_22 <- function(input_df=usheUtils::fake_graduation_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(g_we_earned_contact_hrs = as.numeric(earned_contact_hrs) ) %>%
    # Append USHE data element g_22
    mutate( g_22 = g_we_earned_contact_hrs  )

  return(output_df)
}

#' Calculate USHE Element g_23 (Workforce Education Program Hours)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Workforce Education Program Hours
#' - FIELD NAME: g_we_program_hrs
#' - FIELD FORMAT: Numeric(5,1)
#' - DEFINITION: The official length of the program completed (e.g. 900 hr program, 600 hr program).
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (program_hrs).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element g_23 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' g_23()
#'
g_23 <- function(input_df=usheUtils::fake_graduation_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(g_we_program_hrs = as.numeric(program_hrs) ) %>%
    # Append USHE data element g_23
    mutate( g_23 = g_we_program_hrs  )

  return(output_df)
}

#' Calculate USHE Element g_24 (Fiscal Year of Graduation )
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Fiscal Year of Graduation
#' - FIELD NAME: g_fis_year
#' - FIELD FORMAT: Varchar, 4 Characters (YYYY Format, i.e. 0809)
#' - DEFINITION: The fiscal or academic year the formal award was conferred by the institution.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_remove
#' @importFrom stringr str_pad
#'
#' @param input_df A Data Frame. Must contain the following data fields: (financial_aid_year_id, season).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element g_24 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' g_24()
#'
g_24 <- function(input_df=usheUtils::fake_graduation_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields  # bring in previous financial aid year
    # this is giving us the summer ushe year which is the next year.
    # For example: summer 2022 should look like 2223.
    mutate(this_year = str_remove(academic_year_code, "20") )  %>%
    mutate(next_year = as.numeric(this_year) + 1) %>%
    mutate(next_year = str_pad(next_year, 2, "left", "0")) %>%
    mutate(ushe_summer_year = paste0(this_year, next_year) ) %>%
    mutate(g_fis_year = ifelse(season == "Summer", ushe_summer_year , financial_aid_year_id) ) %>%
    # Append USHE data element g_24
    mutate( g_24 = g_fis_year  )

  return(output_df)
}


#' Calculate USHE Element g_26 (Major College)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Major College
#' - FIELD NAME: g_college
#' - FIELD FORMAT: Varchar, 100 Characters
#' - DEFINITION: The college or school that houses the student’s major. Should be tied to element G_09 CIP CODE.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (college_desc).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element g_26 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' g_26()
#'
g_26 <- function(input_df=usheUtils::fake_graduation_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(g_college = college_desc)%>%
    # Append USHE data element g_26
    mutate( g_26 = g_college  )

  return(output_df)
}

#' Calculate USHE Element g_27 (Major Name)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Major Name
#' - FIELD NAME: g_major
#' - FIELD FORMAT: Varchar, 100 Characters
#' - DEFINITION: Name of student’s major as listed on transcript. Should be tied to element G_09 CIP CODE.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (primary_major_desc).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element g_27 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' g_27()
#'
g_27 <- function(input_df=usheUtils::fake_graduation_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(g_major = primary_major_desc)%>%
    # Append USHE data element g_27
    mutate( g_27 = g_major  )

  return(output_df)
}

#' Calculate USHE Element g_28 (Degree Type Name )
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Degree Type Name
#' - FIELD NAME: g_deg_type_name
#' - FIELD FORMAT: Varchar, 100 Characters
#' - DEFINITION: Used to describe the acronyms/abbreviations in G-10 G_DEG_TYPE
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (degree_desc).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element g_28 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' g_28()
#'
g_28 <- function(input_df=usheUtils::fake_graduation_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(g_major = degree_desc)%>%
    # Append USHE data element g_28
    mutate( g_28 = g_major  )

  return(output_df)
}
