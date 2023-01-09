#' Generate Financial Aid Submission File
#'
#' @param input_df A Data Frame. Must contain the following data fields: (financial_aid_year_id,
#'                                                                        term_id,
#'                                                                        season,
#'                                                                        ssn,
#'                                                                        student_id,
#'                                                                        financial_aid_fund_id,
#'                                                                        amount_offered).
#'
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return
#' A Data Frame, with all of the USHE elements required for upload submission.
#' This will also include intermediate values, used to calculate USHE data elements, if option is set.
#'
#' @examples
#' generate_financial_aid_submission_file()
#'
#' @export
#'
generate_financial_aid_submission_file <- function(input_df=usheUtils::fake_fin_aid_df, with_intermediates=FALSE) {

  ushe_data_elements <- c("f_01", "f_02", "f_03", "f_04",
                          "f_05", "f_06", "f_07")

  output_df <- input_df %>%
    f_01() %>%
    f_02() %>%
    f_03() %>%
    f_04() %>%
    f_05() %>%
    f_06() %>%
    f_07()


  if (!with_intermediates) {
    output_df <- output_df %>%
      dplyr::select( ushe_data_elements )
  }

  return(output_df)
}

#' Calculate USHE Element Year (Financial Aid Year)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Year
#' - FIELD NAME: f_year
#' - FIELD FORMAT: Number, 4 Characters (YYYY format),
#' - DEFINITION: The previous Financial Aid year for which the student was awarded financial aid.
#'   Format YYYY is made up of Financial Aid year YYYY. For degree granting institutions,
#'   this should  match the USHE year for the relevant term
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (financial_aid_year_id).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element gen_ushe_ipeds appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' f_02()
#'
f_02 <- function(input_df=usheUtils::fake_fin_aid_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( f_year = financial_aid_year_id ) %>%
    # Append USHE data element f_02
    mutate( f_02 = f_year)

  return(output_df)

}

#' Calculate USHE Element f_06 (Financial Aid Type)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Financial Aid Type
#' - FIELD NAME: f_type_code.
#' - FIELD FORMAT: Varchar, 6 Characters
#' - DEFINITION: The code that defines what type of financial aid the student was awarded.
#'   Aid grouping is how aid is classified for both federal and state reporting.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (financial_aid_fund_id).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element f_06 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' f_06()
#'
f_06 <- function(input_df=usheUtils::fake_fin_aid_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( f_type_code = financial_aid_fund_id ) %>%
    # Append USHE data element f_06
    mutate( f_06 = f_type_code)

  return(output_df)

}

#' Calculate USHE Element f_07 (Financial Aid Amount)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Financial Aid Amount
#' - FIELD NAME: f_amount.
#' - FIELD FORMAT: Numeric (8,2)
#' - DEFINITION: Amount of Aid awarded to or Loans accepted by a student.
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (amount_offered).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element f_07 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' f_07()
#'
f_07 <- function(input_df=usheUtils::fake_fin_aid_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( f_type_code = amount_offered ) %>%
    # Append USHE data element f_07
    mutate( f_07 = f_type_code)

  return(output_df)

}
