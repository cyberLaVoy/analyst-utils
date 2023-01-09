

#' Generate Course Submission File
#'
#' @param input_df A Data Frame. Must contain the following data fields: (course_reference_number,
#'                                                                        course_number,
#'                                                                        subject_code,
#'                                                                        class_size,
#'                                                                        budget_code,
#'                                                                        campus_id,
#'                                                                        section_number,
#'                                                                        attribute_code,
#'                                                                        academic_department_id,
#'                                                                        college_id,
#'                                                                        section_format_type_code,
#'                                                                        instructor_first_name,
#'                                                                        instructor_last_name,
#'                                                                        instructor_employee_id,
#'                                                                        course_title,
#'                                                                        course_end_date,
#'                                                                        course_start_date,
#'                                                                        room_use_code_3,
#'                                                                        room_max_occupancy_3,
#'                                                                        meet_room_number_3,
#'                                                                        building_number_3,
#'                                                                        meet_building_id_3,
#'                                                                        meet_days_3,
#'                                                                        meet_end_time_3,
#'                                                                        meet_start_time_3,
#'                                                                        room_use_code_2,
#'                                                                        room_max_occupancy_2,
#'                                                                        meet_room_number_2,
#'                                                                        building_number_2,
#'                                                                        meet_building_id_2,
#'                                                                        meet_days_2,
#'                                                                        meet_end_time_2,
#'                                                                        meet_start_time_2,
#'                                                                        room_use_code_1,
#'                                                                        room_max_occupancy_1,
#'                                                                        meet_room_number_1,
#'                                                                        building_number_1,
#'                                                                        meet_building_id_1,
#'                                                                        meet_days_1,
#'                                                                        meet_end_time_1,
#'                                                                        meet_start_time_1,
#'                                                                        course_level_id,
#'                                                                        program_type,
#'                                                                        instruction_method_code,
#'                                                                        contact_hours,
#'                                                                        course_max_credits,
#'                                                                        course_min_credits,
#'                                                                        season,
#'                                                                        academic_year_code,
#'                                                                        version_id).
#'
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#'
#' @return
#' A Data Frame, with all of the USHE elements required for upload submission.
#' This will also include intermediate values, used to calculate USHE data elements, if option is set.
#'
#' @export
#'
#' @examples
#' generate_course_submission_file()
#'
generate_course_submission_file <- function(input_df=usheUtils::fake_course_df, with_intermediates=FALSE) {

  ushe_data_elements <- c("c_01", "c_02", "c_03", "c_04",
                          "c_05", "c_06", "c_07", "c_08",
                          "c_09", "c_10", "c_11", "c_12",
                          "c_13", "c_14", "c_15", "c_16",
                          "c_17", "c_18", "c_19", "c_20",
                          "c_21", "c_22", "c_23", "c_24",
                          "c_25", "c_26", "c_27", "c_28",
                          "c_29", "c_30", "c_31", "c_32",
                          "c_33", "c_34", "c_35", "c_36",
                          "c_37", "c_38", "c_39", "c_40",
                          "c_41", "c_42", "c_43", "c_44",
                          "c_45", "c_46", "c_47", "c_48",
                          "c_49", "c_50", "c_51", "c_52",
                          "c_53", "c_54")

  output_df <- input_df %>%
    c_01() %>%
    c_02() %>%
    c_03() %>%
    c_04() %>%
    c_05() %>%
    c_06() %>%
    c_07() %>%
    c_08() %>%
    c_09() %>%
    c_10() %>%
    c_11() %>%
    c_12() %>%
    c_13() %>%
    c_14() %>%
    c_15() %>%
    c_16() %>%
    c_17() %>%
    c_18() %>%
    c_19() %>%
    c_20() %>%
    c_21() %>%
    c_22() %>%
    c_23() %>%
    c_24() %>%
    c_25() %>%
    c_26() %>%
    c_27() %>%
    c_28() %>%
    c_29() %>%
    c_30() %>%
    c_31() %>%
    c_32() %>%
    c_33() %>%
    c_34() %>%
    c_35() %>%
    c_36() %>%
    c_37() %>%
    c_38() %>%
    c_39() %>%
    c_40() %>%
    c_41() %>%
    c_42() %>%
    c_43() %>%
    c_44() %>%
    c_45() %>%
    c_46() %>%
    c_47() %>%
    c_48() %>%
    c_49() %>%
    c_50() %>%
    c_51() %>%
    c_52() %>%
    c_53() %>%
    c_54() %>%
    clean()

  if (!with_intermediates) {
    output_df <- output_df %>%
        dplyr::select( ushe_data_elements )
    }

  return(output_df)
}


#' Calculate USHE Element c_03 (Course Subject)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (subject_code).
#'
#' @return Original data frame, with USHE data element c_03 appended.
#' @export
#'
#' @examples
#' c_03()
#'
c_03 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_crs_sbj = subject_code ) %>%
    # Append USHE data element c_03
    mutate( c_03 = c_crs_sbj  )

  return(output_df)
}


#' Calculate USHE Element c_04 (Course Number)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (course_number).
#'
#' @return Original data frame, with USHE data element c_04 appended.
#' @export
#'
#' @examples
#' c_04()
#'
c_04 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_crs_num = course_number ) %>%
    # Append USHE data element c_04
    mutate( c_04 = c_crs_num  )

  return(output_df)
}

#' Calculate USHE Element c_05 (Course Section)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (section_number).
#'
#' @return Original data frame, with USHE data element c_05 appended.
#' @export
#'
#' @examples
#' c_05()
#'
c_05 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_crs_sec = section_number) %>%
    # Append USHE data element c_05
    mutate( c_05 = c_crs_sec )

  return(output_df)
}

#' Calculate USHE Element c_06 (Min Credit Hours)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (course_min_credits).
#'
#'
#' @return Original data frame, with USHE data element c_06 appended.
#' @export
#'
#' @examples
#' c_06()
#'
c_06 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_min_credit = as.numeric(course_min_credits) ) %>%
    # Append USHE data element c_06
    mutate( c_06 = c_min_credit )

  return(output_df)
}


#' Calculate USHE Element c_07 (Max Credit Hours)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (course_max_credits).
#'
#'
#' @return Original data frame, with USHE data element c_07 appended.
#' @export
#'
#' @examples
#' c_07()
#'
c_07 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_max_credit = as.numeric(course_max_credits) ) %>%
    # Append USHE data element c_07
    mutate( c_07 = c_max_credit )

  return(output_df)
}


#' Calculate USHE Element c_08 (Contact Hours)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Non-Credit Course Contact Hours
#' - FIELD NAME: C_CONTACT_HRS
#' - FIELD FORMAT: Numeric (5,0)
#' - DEFINITION: The maximum number of contact hours the course is scheduled to meet within the academic term.
#'               Sometimes referred to as clock hours.
#'               This field is for non-credit courses only.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr case_when
#'
#' @param input_df A Data Frame. Must contain the following data fields: (course_level_id, contact_hours).
#'
#'
#' @return Original data frame, with USHE data element c_08 appended.
#' @export
#'
#' @examples
#' c_08()
#'
c_08 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_contact_hrs = case_when( (course_level_id == "CE" | course_level_id == "NC") ~ contact_hours ) ) %>%
    # Append USHE data element c_08
    mutate( c_08 = c_contact_hrs )

  return(output_df)
}


#' Calculate USHE Element c_09 (Line Item)
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
#' @return Original data frame, with USHE data element c_09 appended.
#' @export
#'
#' @examples
#' c_09()
#'
c_09 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_line_item = "A" ) %>%
    # Append USHE data element c_09
    mutate( c_09 = c_line_item )

  return(output_df)
}

#' Calculate USHE Element c_10 (Site Type)
#'
#' @details
#'
#' **USHE Documentation**
#' ELEMENT NAME: Course Site Type
#' FIELD NAME: C_SITE_TYPE
#' FIELD FORMAT: Varchar, 3 Characters
#' DEFINITION: Code used to specify a site type where instruction originates.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr case_when
#' @importFrom stringr str_starts
#'
#' @param input_df A Data Frame. Must contain the following data fields: (campus_id).
#'
#'
#' @return Original data frame, with USHE data element c_10 appended.
#' @export
#'
#' @examples
#' c_10()
#'
c_10 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(c_site_type = case_when( campus_id %in% c("AC1", "AU1", "ACE", "OU1") ~ 'A01',
                                    campus_id == "B8C" ~ 'B80',
                                    campus_id == "UOS" ~ 'C',
                                    str_starts(campus_id, "C") ~ "C",
                                    campus_id == "O03" ~ "O",
                                    campus_id %in% c("O01", "V01") ~ "V",
                                    TRUE ~ campus_id) ) %>%
    # Append USHE data element c_10
    mutate( c_10 = c_site_type )

  return(output_df)
}

#' Calculate USHE Element c_11 (Budget Code)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (budget_code).
#'
#'
#' @return Original data frame, with USHE data element c_11 appended.
#' @export
#'
#' @examples
#' c_11()
#'
c_11 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_budget_code = budget_code ) %>%
    # Append USHE data element c_11
    mutate( c_11 = c_budget_code )

  return(output_df)
}


#' Calculate USHE Element c_12 (Delivery Method)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @param input_df A Data Frame. Must contain the following data fields: (instruction_method_code).
#'
#'
#' @return Original data frame, with USHE data element c_12 appended.
#' @export
#'
#' @examples
#' c_12()
#'
c_12 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_delivery_method = if_else(instruction_method_code == "E", "H", instruction_method_code )) %>%
    # Append USHE data element c_12
    mutate( c_12 = c_delivery_method )

  return(output_df)
}


#' Calculate USHE Element c_13 (Program Type)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (program_type).
#'
#'
#' @return Original data frame, with USHE data element c_13 appended.
#' @export
#'
#' @examples
#' c_13()
#'
c_13 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_program_type = program_type ) %>%
    # Append USHE data element c_11
    mutate( c_13 = c_program_type )

  return(output_df)
}


#' Calculate USHE Element c_14 (Credit Indicator)
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
#' @param input_df A Data Frame. Must contain the following data fields: (course_level_id).
#'
#'
#' @return Original data frame, with USHE data element c_14 appended.
#' @export
#'
#' @examples
#' c_14()
#'
c_14 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(c_credit_ind = case_when( course_level_id %in% c("CE", "NC") ~ 'N',
                                     course_level_id %in% c("UG", "GR") ~ 'C',
                                     TRUE ~ course_level_id)) %>%
    # Append USHE data element c_14
    mutate(c_14 = c_credit_ind )

  return(output_df)
}

#' Calculate USHE Element c_15 (Course Start Time 1)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (meet_start_time_1).
#'
#'
#' @return Original data frame, with USHE data element c_15 appended.
#' @export
#'
#' @examples
#' c_15()
#'
c_15 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_start_time = meet_start_time_1 ) %>%
    # Append USHE data element c_15
    mutate( c_15 = c_start_time )

  return(output_df)
}



#' Calculate USHE Element c_16 (Course Stop Time 1)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (meet_end_time_1).
#'
#'
#' @return Original data frame, with USHE data element c_16 appended.
#' @export
#'
#' @examples
#' c_16()
#'
c_16 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_stop_time = meet_end_time_1) %>%
    # Append USHE data element c_16
    mutate( c_16 = c_stop_time )

  return(output_df)
}


#' Calculate USHE Element c_17 (Course Days 1)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (meet_days_1).
#'
#'
#' @return Original data frame, with USHE data element c_17 appended.
#' @export
#'
#' @examples
#' c_17()
#'
c_17 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_days = meet_days_1 ) %>%
    # Append USHE data element c_17
    mutate( c_17 = c_days )

  return(output_df)
}


#' Calculate USHE Element c_18 (Building Short Name)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (meet_building_id_1).
#'
#'
#' @return Original data frame, with USHE data element c_18 appended.
#' @export
#'
#' @examples
#' c_18()
#'
c_18 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_bldg_sname = meet_building_id_1 ) %>%
    # Append USHE data element c_18
    mutate( c_18 = c_bldg_sname )

  return(output_df)
}


#' Calculate USHE Element c_19 (Building Number)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (building_number_1).
#'
#'
#' @return Original data frame, with USHE data element c_19 appended.
#' @export
#'
#' @examples
#' c_19()
#'
c_19 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_bldg_num = building_number_1 ) %>%
    # Append USHE data element c_19
    mutate( c_19 = c_bldg_num )

  return(output_df)
}

#' Calculate USHE Element c_20 (Room Number)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (meet_room_number_1).
#'
#'
#' @return Original data frame, with USHE data element c_20 appended.
#' @export
#'
#' @examples
#' c_20()
#'
c_20 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_room_num = meet_room_number_1 ) %>%
    # Append USHE data element c_20
    mutate( c_20 = c_room_num )

  return(output_df)
}


#' Calculate USHE Element c_21 (Maximum Room Occupancy)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (room_max_occupancy_1).
#'
#'
#' @return Original data frame, with USHE data element c_21 appended.
#' @export
#'
#' @examples
#' c_21()
#'
c_21 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_room_max = room_max_occupancy_1 ) %>%
    # Append USHE data element c_21
    mutate( c_21 = c_room_max )

  return(output_df)
}


#' Calculate USHE Element c_22 (Room Use Type)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (room_use_code_1).
#'
#'
#' @return Original data frame, with USHE data element c_22 appended.
#' @export
#'
#' @examples
#' c_22()
#'
c_22 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_room_type = room_use_code_1 ) %>%
    # Append USHE data element c_22
    mutate( c_22 = c_room_type )

  return(output_df)
}




#' Calculate USHE Element c_23 (Course Start Time 2)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (meet_start_time_2).
#'
#'
#' @return Original data frame, with USHE data element c_23 appended.
#' @export
#'
#' @examples
#' c_23()
#'
c_23 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_start_time2 = meet_start_time_2 ) %>%
    # Append USHE data element c_23
    mutate( c_23 = c_start_time2 )

  return(output_df)
}



#' Calculate USHE Element c_24 (Course Stop Time 2)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (meet_end_time_2).
#'
#'
#' @return Original data frame, with USHE data element c_24 appended.
#' @export
#'
#' @examples
#' c_24()
#'
c_24 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_stop_time2 = meet_end_time_2) %>%
    # Append USHE data element c_24
    mutate( c_24 = c_stop_time2 )

  return(output_df)
}


#' Calculate USHE Element c_25 (Course Days 2)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (meet_days_2).
#'
#'
#' @return Original data frame, with USHE data element c_25 appended.
#' @export
#'
#' @examples
#' c_25()
#'
c_25 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_days = meet_days_2 ) %>%
    # Append USHE data element c_25
    mutate( c_25 = c_days )

  return(output_df)
}


#' Calculate USHE Element c_26 (Building Short Name 2)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (meet_building_id_2).
#'
#'
#' @return Original data frame, with USHE data element c_26 appended.
#' @export
#'
#' @examples
#' c_26()
#'
c_26 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_bldg_sname2 = meet_building_id_2 ) %>%
    # Append USHE data element c_26
    mutate( c_26 = c_bldg_sname2 )

  return(output_df)
}


#' Calculate USHE Element c_27 (Building Number 2)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (building_number_2).
#'
#'
#' @return Original data frame, with USHE data element c_27 appended.
#' @export
#'
#' @examples
#' c_27()
#'
c_27 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_bldg_num2 = building_number_2 ) %>%
    # Append USHE data element c_27
    mutate( c_27 = c_bldg_num2 )

  return(output_df)
}

#' Calculate USHE Element c_28 (Room Number 2)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (meet_room_number_2).
#'
#'
#' @return Original data frame, with USHE data element c_28 appended.
#' @export
#'
#' @examples
#' c_28()
#'
c_28 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_room_num2 = meet_room_number_2 ) %>%
    # Append USHE data element c_28
    mutate( c_28 = c_room_num2 )

  return(output_df)
}


#' Calculate USHE Element c_29 (Maximum Room Occupancy 2)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (room_max_occupancy_2).
#'
#'
#' @return Original data frame, with USHE data element c_29 appended.
#' @export
#'
#' @examples
#' c_29()
#'
c_29 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_room_max2 = room_max_occupancy_2 ) %>%
    # Append USHE data element c_29
    mutate( c_29 = c_room_max2 )

  return(output_df)
}


#' Calculate USHE Element c_30 (Room Use Type 2)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (room_use_code_2).
#'
#'
#' @return Original data frame, with USHE data element c_30 appended.
#' @export
#'
#' @examples
#' c_30()
#'
c_30 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_room_type2 = room_use_code_2 ) %>%
    # Append USHE data element c_30
    mutate( c_30 = c_room_type2 )

  return(output_df)
}


#' Calculate USHE Element c_31 (Course Start Time 3)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (meet_start_time_3).
#'
#'
#' @return Original data frame, with USHE data element c_31 appended.
#' @export
#'
#' @examples
#' c_31()
#'
c_31 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_start_time3 = meet_start_time_3 ) %>%
    # Append USHE data element c_31
    mutate( c_31 = c_start_time3 )

  return(output_df)
}



#' Calculate USHE Element c_32 (Course Stop Time 3)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (meet_end_time_3).
#'
#'
#' @return Original data frame, with USHE data element c_32 appended.
#' @export
#'
#' @examples
#' c_32()
#'
c_32 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_stop_time3 = meet_end_time_3) %>%
    # Append USHE data element c_32
    mutate( c_32 = c_stop_time3 )

  return(output_df)
}


#' Calculate USHE Element c_33 (Course Days 3)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (meet_days_3).
#'
#'
#' @return Original data frame, with USHE data element c_33 appended.
#' @export
#'
#' @examples
#' c_33()
#'
c_33 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_days = meet_days_3 ) %>%
    # Append USHE data element c_33
    mutate( c_33 = c_days )

  return(output_df)
}


#' Calculate USHE Element c_34 (Building Short Name 3)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (meet_building_id_3).
#'
#'
#' @return Original data frame, with USHE data element c_34 appended.
#' @export
#'
#' @examples
#' c_34()
#'
c_34 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_bldg_sname3 = meet_building_id_3 ) %>%
    # Append USHE data element c_34
    mutate( c_34 = c_bldg_sname3 )

  return(output_df)
}


#' Calculate USHE Element c_35 (Building Number 3)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (building_number_3).
#'
#'
#' @return Original data frame, with USHE data element c_35 appended.
#' @export
#'
#' @examples
#' c_35()
#'
c_35 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_bldg_num3 = building_number_3 ) %>%
    # Append USHE data element c_35
    mutate( c_35 = c_bldg_num3 )

  return(output_df)
}

#' Calculate USHE Element c_36 (Room Number 3)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (meet_room_number_3).
#'
#'
#' @return Original data frame, with USHE data element c_36 appended.
#' @export
#'
#' @examples
#' c_36()
#'
c_36 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_room_num3 = meet_room_number_3 ) %>%
    # Append USHE data element c_36
    mutate( c_36 = c_room_num3 )

  return(output_df)
}


#' Calculate USHE Element c_37 (Maximum Room Occupancy 3)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (room_max_occupancy_3).
#'
#'
#' @return Original data frame, with USHE data element c_37 appended.
#' @export
#'
#' @examples
#' c_37()
#'
c_37 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_room_max3 = room_max_occupancy_3 ) %>%
    # Append USHE data element c_37
    mutate( c_37 = c_room_max3 )

  return(output_df)
}


#' Calculate USHE Element c_38 (Room Use Type 3)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (room_use_code_3).
#'
#'
#' @return Original data frame, with USHE data element c_38 appended.
#' @export
#'
#' @examples
#' c_38()
#'
c_38 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_room_type3 = room_use_code_3 ) %>%
    # Append USHE data element c_38
    mutate( c_38 = c_room_type3 )

  return(output_df)
}


#' Calculate USHE Element c_39 (Course Start Date)
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
#' @param input_df A Data Frame. Must contain the following data fields: (course_start_date).
#'
#'
#' @return Original data frame, with USHE data element c_39 appended.
#' @export
#'
#' @examples
#' c_39()
#'
c_39 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_start_date = str_remove_all(course_start_date, "-") ) %>%
    # Append USHE data element c_39
    mutate( c_39 = c_start_date )

  return(output_df)
}

#' Calculate USHE Element c_40 (Course End Date)
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
#' @param input_df A Data Frame. Must contain the following data fields: (course_end_date).
#'
#'
#' @return Original data frame, with USHE data element c_40 appended.
#' @export
#'
#' @examples
#' c_40()
#'
c_40 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_end_date =  str_remove_all(course_end_date, "-") ) %>%
    # Append USHE data element c_40
    mutate( c_40 = c_end_date )

  return(output_df)
}


#' Calculate USHE Element c_41 (Course Title)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_replace_all
#'
#' @param input_df A Data Frame. Must contain the following data fields: (course_title).
#'
#'
#' @return Original data frame, with USHE data element c_41 appended.
#' @export
#'
#' @examples
#' c_41()
#'
c_41 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_title_intermediate_1 = str_replace_all(course_title, "([,.;:-])", "") ) %>%
    mutate( c_title_intermediate_2 = str_replace_all(c_title_intermediate_1, "&", "and") ) %>%
    mutate( c_title = c_title_intermediate_2) %>%
    # Append USHE data element c_41
    mutate( c_41 = c_title )

  return(output_df)
}


#' Calculate USHE Element c_42 (Instructor ID)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (instructor_employee_id).
#'
#'
#' @return Original data frame, with USHE data element c_42 appended.
#' @export
#'
#' @examples
#' c_42()
#'
c_42 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_instruct_id = paste0('D', instructor_employee_id) ) %>%
    # Append USHE data element c_42
    mutate( c_42 = c_instruct_id )

  return(output_df)
}

#' Calculate USHE Element c_43 (Instructor Name)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (instructor_first_name, instructor_last_name, instructor_middle_name).
#'
#'
#' @return Original data frame, with USHE data element c_43 appended.
#' @export
#'
#' @examples
#' c_43()
#'
c_43 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_instruct_name = paste0(instructor_last_name, ', ', instructor_first_name, ' ', instructor_middle_name) ) %>%
    # Append USHE data element c_43
    mutate( c_43 = c_instruct_name )

  return(output_df)
}


#' Calculate USHE Element c_44 (Course Instruction Type)
#'
#' @details
#'
#' **USHE Documentation**
#' Element: C-44
#' ELEMENT NAME: Instructional Type
#' FIELD NAME: C_INSTRUCT_TYPE
#' FIELD FORMAT: Varchar, 3 Characters
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr case_when
#'
#' @param input_df A Data Frame. Must contain the following data fields: (section_format_type_code).
#'
#'
#' @return Original data frame, with USHE data element c_44 appended.
#' @export
#'
#' @examples
#' c_44()
#'
c_44 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(c_instruct_type = case_when( section_format_type_code %in% c("LEV", "LEX", "LES", "INS")	~	"LEC",
                                        section_format_type_code %in% c("STU", "ENS")	~	"LEL",
                                        section_format_type_code %in%	c("LBV", "LBS", "LBC")	~	"LAB",
                                        section_format_type_code %in%	c("PRA", "INT", "CLN")	~	"SUP",
                                        section_format_type_code %in% c("MUN", "MUM", "ACT")	~	"INV",
                                        section_format_type_code == "CLS"	~	"OTH",
                                        TRUE ~ section_format_type_code)) %>%

    # Append USHE data element c_44
    mutate( c_44 = c_instruct_type )

  return(output_df)
}


#' Calculate USHE Element c_45 (Course College)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (college_id).
#'
#'
#' @return Original data frame, with USHE data element c_45 appended.
#' @export
#'
#' @examples
#' c_45()
#'
c_45 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_college = college_id ) %>%
    # Append USHE data element c_45
    mutate( c_45 = c_college )

  return(output_df)
}

#' Calculate USHE Element c_46 (Course Department)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (academic_department_id).
#'
#'
#' @return Original data frame, with USHE data element c_46 appended.
#' @export
#'
#' @examples
#' c_46()
#'
c_46 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_dept = academic_department_id) %>%
    # Append USHE data element c_46
    mutate( c_46 = c_dept )

  return(output_df)
}

#' Calculate USHE Element c_47 (Course General Education Code)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: General Education Course Codes
#' - FIELD NAME: C_GEN_ED
#' - FIELD FORMAT: Varchar, 4 Characters
#' - DEFINITION: The codes which designate the required General Education requirements as outlined in Regent Policy R470.
#'                Do NOT report courses beyond the Breadth General Education Requirements such as:  writing or quantitative intensive.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (attribute_code).
#'
#'
#' @return Original data frame, with USHE data element c_47 appended.
#' @export
#'
#' @examples
#' c_47()
#'
c_47 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_gen_ed = case_when( attribute_code == "EN" ~ "C",
                                  attribute_code == "IL" ~ "IR",
                                  attribute_code == "CP" ~ "CL",
                                  attribute_code == "MA" ~ "QL",
                                  attribute_code == "QL" ~ attribute_code,
                                  attribute_code == "AI" ~ attribute_code,
                                  attribute_code == "FA" ~ attribute_code,
                                  attribute_code == "HU" ~ attribute_code,
                                  attribute_code == "SS" ~ attribute_code,
                                  attribute_code == "LS" ~ attribute_code,
                                  attribute_code == "PS" ~ attribute_code,
                                  attribute_code == "ID" ~ attribute_code,
                                  attribute_code == "IR" ~ attribute_code,
                                  attribute_code == "DV" ~ attribute_code,
                                  attribute_code == "CL" ~ attribute_code,
                                  attribute_code == "FL" ~ attribute_code ) ) %>%
    # Append USHE data element c_47
    mutate( c_47 = c_gen_ed )

  return(output_df)
}

#' Calculate USHE Element c_48 (Course Destination Site)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Course Destination Site
#' - FIELD NAME: C_DEST_SITE
#' - FIELD FORMAT: Varchar, 6 Characters
#' - DEFINITION: Designed for concurrent enrollment courses.
#'               The High School or Special  Secondary School code which uniquely identifies the destination site of a concurrent enrollment course.
#'               This code will only be used where the site may be clearly identified and left blank when not.
#'               The codes for any secondary institution located within the United States can be found by accessing the URL (http://www.act.org/aap/regist/lookuphs.html).
#'               Sample codes are listed below.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (budget_code, campus_id, instruction_method_code, section_number).
#'
#'
#' @return Original data frame, with USHE data element c_48 appended.
#' @export
#'
#' @examples
#' c_48()
#'
c_48 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(c_dest_site = case_when(
           budget_code %in% c("SF", "BC") & campus_id == "C01" ~ "450350",
           budget_code %in% c("SF", "BC") & campus_id == "C02" ~ "450354",
           budget_code %in% c("SF", "BC") & campus_id == "C03" ~ "450353",
           budget_code %in% c("SF", "BC") & campus_id == "C04" ~ "450135",
           budget_code %in% c("SF", "BC") & campus_id == "C05" ~ "450444",
           budget_code %in% c("SF", "BC") & campus_id == "C06" ~ "450075",
           budget_code %in% c("SF", "BC") & campus_id == "C07" ~ "450045",
           budget_code %in% c("SF", "BC") & campus_id == "C08" ~ "450150",
           budget_code %in% c("SF", "BC") & campus_id == "C09" ~ "450060",
           budget_code %in% c("SF", "BC") & campus_id == "C10" ~ "450275",
           budget_code %in% c("SF", "BC") & campus_id == "C11" ~ "450010",
           budget_code %in% c("SF", "BC") & campus_id == "C12" ~ "450150",
           budget_code %in% c("SF", "BC") & campus_id == "C13" ~ "450359",
           budget_code %in% c("SF", "BC") & campus_id == "C15" ~ "450348",
           budget_code %in% c("SF", "BC") & campus_id == "C16" ~ "450140",
           budget_code %in% c("SF", "BC") & campus_id == "C17" ~ "450247",
           budget_code %in% c("SF", "BC") & campus_id == "C18" ~ "450248",
           budget_code %in% c("SF", "BC") & campus_id == "C19" ~ "450434",
           budget_code %in% c("SF", "BC") & campus_id == "C20" ~ "450031",
           budget_code %in% c("SF", "BC") & campus_id == "C21" ~ "450481",
           budget_code %in% c("SF", "BC") & campus_id == "C22" ~ "450056",
           budget_code %in% c("SF", "BC") & campus_id == "C23" ~ "450021",
           budget_code %in% c("SF", "BC") & campus_id == "C24" ~ "450391",
           budget_code %in% c("SF", "BC") & campus_id == "C25" ~ "450285",
           budget_code %in% c("SF", "BC") & campus_id == "C26" ~ "450435",
           budget_code %in% c("SF", "BC") & campus_id == "C27" ~ "450330",
           budget_code %in% c("SF", "BC") & campus_id == "C28" ~ "450402",
           budget_code %in% c("SF", "BC") & campus_id == "C29" ~ "450405",
           budget_code %in% c("SF", "BC") & campus_id == "C50" ~ "450068",
           budget_code %in% c("SF", "BC") & campus_id == "C51" ~ "450039",
           budget_code %in% c("SF", "BC") & campus_id == "C52" ~ "450327",
           (budget_code == "BC"
             & campus_id %in% c("A01", "B80")
             & instruction_method_code == "R"
             & grepl("K", section_number) ) ~ "450150",
           budget_code %in% c("SF", "BC") & campus_id %in% c("C53", "C14") ~ NA_character_,
           TRUE ~ NA_character_))  %>%
    # Append USHE data element c_48
    mutate( c_48 = c_dest_site )

  return(output_df)
}

#' Calculate USHE Element c_49 (Class Size)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (class_size).
#'
#'
#' @return Original data frame, with USHE data element c_49 appended.
#' @export
#'
#' @examples
#' c_49()
#'
c_49 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_class_size = class_size ) %>%
    # Append USHE data element c_49
    mutate( c_49 = c_class_size )

  return(output_df)
}

#' Calculate USHE Element c_50 (Delivery Model)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Delivery Model
#' - FIELD NAME: C_ DELIVERY_MODEL
#' - FIELD FORMAT: Varchar, 1 Character
#' - DEFINITION: The model in which the course was taught.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @param input_df A Data Frame. Must contain the following data fields: (instruction_method_code).
#'
#'
#' @return Original data frame, with USHE data element c_50 appended.
#' @export
#'
#' @examples
#' c_50()
#'
c_50 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(c_delivery_model = if_else( instruction_method_code == "E", "E", NA_character_)) %>%

    # Append USHE data element c_50
    mutate( c_50 = c_delivery_model )

  return(output_df)
}

#' Calculate USHE Element c_51 (Class Level)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Course Level
#' - FIELD NAME: C_LEVEL
#' - FIELD FORMAT: Varchar, 1 Character
#' - DEFINITION: Indicates whether the course is offered as remedial or for undergraduate or  graduate level credit.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (course_level_id, course_number, subject_code).
#'
#'
#' @return Original data frame, with USHE data element c_51 appended.
#' @export
#'
#' @examples
#' c_51()
#'
c_51 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(c_level = case_when( course_level_id == "UG"
                                & course_number <= 1000
                                & (subject_code == "ENGL" | subject_code == "ESL" | subject_code == "MATH")  ~ 'R',
                                course_level_id == "UG" ~ 'U',
                                course_level_id == "GR" ~ 'G',
                                course_level_id == "NC" ~ 'U',
                                course_level_id == "CE" ~ 'U' ) ) %>%
    # Append USHE data element c_51
    mutate( c_51 = c_level )

  return(output_df)
}

#' Calculate USHE Element c_52 (Course Reference Number)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (course_reference_number).
#'
#'
#' @return Original data frame, with USHE data element c_52 appended.
#' @export
#'
#' @examples
#' c_52()
#'
c_52 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_crn = course_reference_number ) %>%
    # Append USHE data element c_52
    mutate( c_52 = c_crn )

  return(output_df)
}

#' Calculate USHE Element c_53 (Course Site Type 2)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (meet_building_id_2, campus_id).
#'
#'
#' @return Original data frame, with USHE data element c_53 appended.
#' @export
#'
#' @examples
#' c_53()
#'
c_53 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_site_type2 = case_when(
      !is.na(meet_building_id_2) &  (campus_id == "ONLINE" | campus_id == "VIRT") ~ "V",
      meet_building_id_2 == "HURCTR" ~ "B80",
      !is.na(meet_building_id_2) ~ "A01",
      TRUE ~ NA_character_)) %>%
    # Append USHE data element c_53
    mutate( c_53 = c_site_type2 )

  return(output_df)
}

#' Calculate USHE Element c_54 (Course Site Type 3)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (meet_building_id_3, campus_id).
#'
#'
#' @return Original data frame, with USHE data element c_54 appended.
#' @export
#'
#' @examples
#' c_54()
#'
c_54 <- function(input_df=usheUtils::fake_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_site_type3 = case_when(
      !is.na(meet_building_id_3) &  (campus_id == "ONLINE" | campus_id == "VIRT") ~ "V",
      meet_building_id_3 == "HURCTR" ~ "B80",
      !is.na(meet_building_id_2) ~ "A01",
      TRUE ~ NA_character_)) %>%
    # Append USHE data element c_54
    mutate( c_54 = c_site_type3 )

  return(output_df)
}
