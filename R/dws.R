#' Generate DWS Submission File
#'
#' @param input_df A Data Frame. Must contain the following data fields: (ssn,
#'                                                                        student_id,
#'                                                                        graduation_date,
#'                                                                        end_search_date,
#'                                                                        program_id).
#'
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return
#' A Data Frame, with all of the USHE elements required for upload submission.
#' This will also include intermediate values, used to calculate USHE data elements, if option is set.
#'
#' @examples
#' generate_dws_submission_file()
#'
#' @export
#'
generate_dws_submission_file <- function(input_df=usheUtils::fake_dws_df, with_intermediates=FALSE) {

  ushe_data_elements <- c("d_01", "d_02", "d_03", "d_04",
                          "d_05", "d_06")

  output_df <- input_df %>%
    d_01() %>%
    d_02() %>%
    d_03() %>%
    d_04() %>%
    d_05() %>%
    d_06()


  if (!with_intermediates) {
    output_df <- output_df %>%
      dplyr::select( ushe_data_elements )
  }

  return(output_df)
}

#' Calculate USHE Element d_05 (End Search Date)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: End Search Date
#' - FIELD NAME: d_end_date
#' - FIELD FORMAT: Varchar, 8 Characters (YYYYMMDD)
#' - DEFINITION: The calendar date of the End Search Date.
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (end_search_date).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element d_05 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' d_05()
#'
d_05 <- function(input_df=usheUtils::fake_dws_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( d_end_date = end_search_date ) %>%
    # Append USHE data element d_05
    mutate( d_05 = d_end_date)

  return(output_df)

}

#' Calculate USHE Element d_06 ( Description)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: E Description
#' - FIELD NAME:  Description
#' - FIELD FORMAT: Varchar, 100 Characters
#' - DEFINITION: Institutions may use this field according to their preference or to differentiate students if submitting multiple files.
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (program_id).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element d_06 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' d_06()
#'
d_06 <- function(input_df=usheUtils::fake_dws_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( description = program_id ) %>%
    # Append USHE data element d_06
    mutate( d_06 = description)

  return(output_df)

}

