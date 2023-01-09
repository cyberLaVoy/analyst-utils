
#' Generate Programs Submission File
#'
#' @param input_df A Data Frame. Must contain the following data fields: (major_desc,
#'                                                                        required_credits,
#'                                                                        is_perkins,
#'                                                                        primary_degree_id,
#'                                                                        ipeds_award_level_code,
#'                                                                        cip_code,
#'                                                                        academic_year).
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
#' generate_program_submission_file()
#'
generate_program_submission_file <- function(input_df=usheUtils::fake_program_df, with_intermediates=FALSE) {

  ushe_data_elements <- c("pf_01", "pf_02", "pf_03", "pf_04",
                          "pf_05", "pf_06", "pf_07", "pf_08",
                          "pf_09", "pf_10")

  output_df <- input_df %>%
    pf_01() %>%
    pf_02() %>%
    pf_03() %>%
    pf_04() %>%
    pf_05() %>%
    pf_06() %>%
    pf_07() %>%
    pf_08() %>%
    pf_09() %>%
    pf_10() %>%
    clean()

  if (!with_intermediates) {
    output_df <- output_df %>%
        dplyr::select( ushe_data_elements )
    }

  return(output_df)
}

#' Calculate USHE Element pf_02 (Academic Year)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (academic_year).
#'
#'
#' @return Original data frame, with USHE data element pf_02 appended.
#' @export
#'
#' @examples
#' pf_02()
#'
pf_02 <- function(input_df=usheUtils::fake_program_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( pf_year = academic_year ) %>%
    # Append USHE data element pf_04
    mutate( pf_02 = pf_year  )

  return(output_df)
}


#' Calculate USHE Element pf_03 (CIP Code)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (cip_code).
#'
#'
#' @return Original data frame, with USHE data element pf_03 appended.
#' @export
#'
#' @examples
#' pf_03()
#'
pf_03 <- function(input_df=usheUtils::fake_program_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( pf_deg_level = cip_code ) %>%
    # Append USHE data element pf_04
    mutate( pf_03 = pf_deg_level  )

  return(output_df)
}


#' Calculate USHE Element pf_06 (Perkins)
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
#' @param input_df A Data Frame. Must contain the following data fields: (is_perkins).
#'
#'
#' @return Original data frame, with USHE data element pf_06 appended.
#' @export
#'
#' @examples
#' pf_06()
#'
pf_06 <- function(input_df=usheUtils::fake_program_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( pf_perkins = if_else(is_perkins, "Y", "N") ) %>%
    # Append USHE data element pf_06
    mutate( pf_06 = pf_perkins )

  return(output_df)
}


#' Calculate USHE Element pf_07 (Technical Education)
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
#' @return Original data frame, with USHE data element pf_07 appended.
#' @export
#'
#' @examples
#' pf_07()
#'
pf_07 <- function(input_df=usheUtils::fake_program_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( pf_te = 'N' ) %>%
    # Append USHE data element pf_07
    mutate( pf_07 = pf_te )

  return(output_df)
}


#' Calculate USHE Element pf_08 (Minimum Required Credit Hours)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (required_credits).
#'
#'
#' @return Original data frame, with USHE data element pf_08 appended.
#' @export
#'
#' @examples
#' pf_08()
#'
pf_08 <- function(input_df=usheUtils::fake_program_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( pf_credit_hrs = required_credits ) %>%
    # Append USHE data element pf_08
    mutate( pf_08 = pf_credit_hrs )

  return(output_df)
}


#' Calculate USHE Element pf_09 (Program Participation Agreement)
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
#' @return Original data frame, with USHE data element pf_09 appended.
#' @export
#'
#' @examples
#' pf_09()
#'
pf_09 <- function(input_df=usheUtils::fake_program_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( pf_something = 'N' ) %>%
    # Append USHE data element pf_09
    mutate( pf_09 = pf_something )

  return(output_df)
}

#' Calculate USHE Element pf_10 (Major Name)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (major_desc).
#'
#'
#' @return Original data frame, with USHE data element pf_10 appended.
#' @export
#'
#' @examples
#' pf_10()
#'
pf_10 <- function(input_df=usheUtils::fake_program_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( pf_major = major_desc ) %>%
    # Append USHE data element pf_10
    mutate( pf_10 = pf_major )

  return(output_df)
}
