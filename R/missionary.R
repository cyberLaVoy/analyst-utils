
#' Generate Missionary Submission File
#'
#' @param input_df A Data Frame. Must contain the following data fields: (last_name,
#'                                                                        first_name,
#'                                                                        middle_name,
#'                                                                        birth_date,
#'                                                                        term_end_date,
#'                                                                        student_id).
#'
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return
#' A Data Frame, with all of the USHE elements required for upload submission.
#' This will also include intermediate values, used to calculate USHE data elements, if option is set.
#'
#' @examples
#' generate_missionary_submission_file()
#'
#' @export
#'
generate_missionary_submission_file <- function(input_df=usheUtils::fake_mission_df, with_intermediates=FALSE) {

  ushe_data_elements <- c("m_01", "m_02", "m_03", "m_04",
                          "m_05", "m_06")

  output_df <- input_df %>%
    m_01() %>%
    m_02() %>%
    m_03() %>%
    m_04() %>%
    m_05() %>%
    m_06() %>%
    clean()

  if (!with_intermediates) {
    output_df <- output_df %>%
      dplyr::select( ushe_data_elements )
  }

  return(output_df)
}

#' Calculate USHE Element m_02 (Student Name)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr coalesce
#' @importFrom stringr str_sub
#'
#' @param input_df A Data Frame. Must contain the following data fields: (last_name, first_name, middle_name).
#'
#' @return Original data frame, with USHE data element m_02 appended.
#' @export
#'
#' @examples
#' m_02()
#'
m_02 <- function(input_df=usheUtils::fake_mission_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( m_last = str_sub(coalesce(last_name, ''), end = 60),
            m_first = str_sub(coalesce(first_name, ''), end = 15),
            m_middle = str_sub(coalesce(middle_name, ''), end = 15) ) %>%
    # Append USHE data element m_02
    mutate( m_02 = paste(m_last, m_first, m_middle, sep='|'))

  return(output_df)
}

#' Calculate USHE Element m_04 (Start Search Date)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME:
#' - FIELD NAME: m_end_date
#' - FIELD FORMAT:
#' - DEFINITION:
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (term_end_date).
#'
#' @return Original data frame, with USHE data element m_04 appended.
#' @export
#'
#' @examples
#' m_04()
#'
m_04 <- function(input_df=usheUtils::fake_mission_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( m_start_dt = gsub("-", "", term_end_date )) %>%
    # Append USHE data element m_04
    mutate( m_04 =  m_start_dt  )

  return(output_df)
}
#' Calculate USHE Element m_05 (End Search Date)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME:
#' - FIELD NAME: m_start_date
#' - FIELD FORMAT:
#' - DEFINITION:
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: ().
#'
#' @return Original data frame, with USHE data element m_05 appended.
#' @export
#'
#' @examples
#' m_05()
#'
m_05 <- function(input_df=usheUtils::fake_mission_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( m_end_dt = gsub("-", "", Sys.Date())) %>%
    # Append USHE data element m_05
    mutate( m_05 =  m_end_dt  )

  return(output_df)
}

