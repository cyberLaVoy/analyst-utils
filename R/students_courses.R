
#' Generate Students Courses Submission File
#'
#' @param input_df A Data Frame. Must contain the following data fields: (subject_code,
#'                                                                        course_number,
#'                                                                        section_number,
#'                                                                        attempted_credits,
#'                                                                        earned_credits,
#'                                                                        course_level_id,
#'                                                                        final_grade,
#'                                                                        latest_student_type_code,
#'                                                                        budget_code,
#'                                                                        is_concurrent_course,
#'                                                                        student_id,
#'                                                                        ssn,
#'                                                                        course_reference_number).
#'
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return
#' A Data Frame, with all of the USHE elements required for upload submission.
#' This will also include intermediate values, used to calculate USHE data elements, if option is set.
#'
#' @export
#'
#' @examples
#' generate_student_course_submission_file()
#'
generate_student_course_submission_file <- function(input_df=usheUtils::fake_student_course_df, with_intermediates=FALSE) {

  ushe_data_elements <- c("sc_01", "sc_02", "sc_03", "sc_04",
                          "sc_05", "sc_06", "sc_07", "sc_08",
                          "sc_09", "sc_10", "sc_11", "sc_12",
                          "sc_13", "sc_14", "sc_15")

  output_df <- input_df %>%
    sc_01() %>%
    sc_02() %>%
    sc_03() %>%
    sc_04() %>%
    sc_05() %>%
    sc_06() %>%
    sc_07() %>%
    sc_08() %>%
    sc_09() %>%
    sc_10() %>%
    sc_11() %>%
    sc_12() %>%
    sc_13() %>%
    sc_14() %>%
    sc_15() %>%
    clean()

    if (!with_intermediates) {
      output_df <- output_df %>%
        dplyr::select( ushe_data_elements )
    }

  return(output_df)
}


#' Calculate USHE Element sc_04 (Course Subject)
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
#'
#' @return Original data frame, with USHE data element sc_04 appended.
#' @export
#'
#' @examples
#' sc_04()
#'
sc_04 <- function(input_df=usheUtils::fake_student_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( sc_crs_sbj = subject_code ) %>%
    # Append USHE data element sc_04
    mutate( sc_04 = sc_crs_sbj  )

  return(output_df)
}

#' Calculate USHE Element sc_05 (Course Number)
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
#'
#' @return Original data frame, with USHE data element sc_05 appended.
#' @export
#'
#' @examples
#' sc_05()
#'
sc_05 <- function(input_df=usheUtils::fake_student_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( sc_src_num = course_number ) %>%
    # Append USHE data element sc_05
    mutate( sc_05 = sc_src_num )

  return(output_df)
}

#' Calculate USHE Element sc_06 (Course Section)
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
#'
#' @return Original data frame, with USHE data element sc_06 appended.
#' @export
#'
#' @examples
#' sc_06()
#'
sc_06 <- function(input_df=usheUtils::fake_student_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( sc_crs_sec = section_number ) %>%
    # Append USHE data element sc_06
    mutate( sc_06 = sc_crs_sec )

  return(output_df)
}


#' Calculate USHE Element sc_07 (Attempted Credit Hours)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (attempted_credits).
#'
#'
#' @return Original data frame, with USHE data element sc_07 appended.
#' @export
#'
#' @examples
#' sc_07()
#'
sc_07 <- function(input_df=usheUtils::fake_student_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( sc_att_cr = attempted_credits ) %>%
    # Append USHE data element sc_07
    mutate( sc_07 = sc_att_cr )

  return(output_df)
}


#' Calculate USHE Element sc_08 (Earned Credit Hours)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Credit Hours Earned
#' - FIELD NAME: SC_EARNED_CR
#' - FIELD FORMAT: Numeric (3,1)
#' - DEFINITION: Credit hours earned by the student for the course. This field is for credit courses only.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (earned_credits).
#'
#'
#' @return Original data frame, with USHE data element sc_08 appended.
#' @export
#'
#' @examples
#' sc_08()
#'
sc_08 <- function(input_df=usheUtils::fake_student_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( sc_earned_cr = earned_credits ) %>%
    # Append USHE data element sc_08
    mutate( sc_08 = sc_earned_cr )

  return(output_df)
}


#' Calculate USHE Element sc_09 (Non-Credit Contact Hours)
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
#' @param input_df A Data Frame. Must contain the following data fields: (earned_credits, course_level_id).
#'
#'
#' @return Original data frame, with USHE data element sc_09 appended.
#' @export
#'
#' @examples
#' sc_09()
#'
sc_09 <- function(input_df=usheUtils::fake_student_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( sc_contact_hrs = case_when( course_level_id == "NC" ~ earned_credits ) ) %>%
    # Append USHE data element sc_09
    mutate( sc_09 = sc_contact_hrs )

  return(output_df)
}

#' Calculate USHE Element sc_10 (Course Grade)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (final_grade).
#'
#'
#' @return Original data frame, with USHE data element sc_10 appended.
#' @export
#'
#' @examples
#' sc_10()
#'
sc_10 <- function(input_df=usheUtils::fake_student_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( sc_grade = final_grade ) %>%
    # Append USHE data element sc_10
    mutate( sc_10 = sc_grade )

  return(output_df)
}

#' Calculate USHE Element sc_11 (Membership Hours)
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
#' @return Original data frame, with USHE data element sc_11 appended.
#' @export
#'
#' @examples
#' sc_11()
#'
sc_11 <- function(input_df=usheUtils::fake_student_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( sc_membership_hrs = NA ) %>%
    # Append USHE data element sc_11
    mutate( sc_11 = sc_membership_hrs )

  return(output_df)
}


#' Calculate USHE Element sc_12 (Student Type)
#'
#' @details
#'
#' **USHE Documentation**
#' * ELEMENT NAME: Student Type Code
#' * FIELD NAME: SC_STUDENT_TYPE
#' * FIELD FORMAT: Varchar, 2 Characters
#' * DEFINITION: The specific code which would identify students according to special registration/tuition status.
#'               Average Daily Membership (ADM) is a count of Utah public education students that is taken at different times of the year to satisfy local, state, and federal data collection  needs and also to ensure that school districts are adequately funded, according to student population.
#'               The Concurrent Enrollment Master List is a list of college-level courses which have been approved by the Utah State Board of Education for Concurrent Enrollment.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (latest_student_type_code, budget_code, is_concurrent_course).
#'
#'
#'
#' @return Original data frame, with USHE data element sc_12 appended.
#' @export
#'
#' @examples
#' sc_12()
#'
sc_12 <- function(input_df=usheUtils::fake_student_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( sc_student_type = case_when( (latest_student_type_code == "H" & budget_code != "BC" & budget_code != "SF")  ~ "EC",
                                         (latest_student_type_code == "H" & (budget_code == "BC" | budget_code == "SF") & is_concurrent_course) ~ "CC",
                                         (latest_student_type_code == "H" & (budget_code == "BC" | budget_code == "SF") & !is_concurrent_course) ~ "DC") ) %>%
    # Append USHE data element sc_12
    mutate( sc_12 = sc_student_type )

  return(output_df)
}


#' Calculate USHE Element sc_13 (Institutionally Assigned ID)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (student_id).
#'
#'
#' @return Original data frame, with USHE data element sc_13 appended.
#' @export
#'
#' @examples
#' sc_13()
#'
sc_13 <- function(input_df=usheUtils::fake_student_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( sc_banner_id = paste0('D', student_id) ) %>%
    # Append USHE data element sc_11
    mutate( sc_13 = sc_banner_id )

  return(output_df)
}


#' Calculate USHE Element sc_14 (Course Reference Number)
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
#' @return Original data frame, with USHE data element sc_14 appended.
#' @export
#'
#' @examples
#' sc_14()
#'
sc_14 <- function(input_df=usheUtils::fake_student_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( sc_crn = course_reference_number ) %>%
    # Append USHE data element sc_14
    mutate( sc_14 = sc_crn )

  return(output_df)
}

#' Calculate USHE Element sc_15 (Student Credit Hour Type)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (course_level_id).
#'
#'
#' @return Original data frame, with USHE data element sc_15 appended.
#' @export
#'
#' @examples
#' sc_15()
#'
sc_15 <- function(input_df=usheUtils::fake_student_course_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( sc_cr_type = course_level_id ) %>%
    # Append USHE data element sc_15
    mutate( sc_15 = sc_cr_type )

  return(output_df)
}
