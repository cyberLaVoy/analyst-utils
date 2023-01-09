#' Set Edify Password
#'
#' `set_edify_password` will prompt you with a dialog box to input your new password
#' for Edify DB connections.
#'
#' @details **To reset your Edify password.**
#'
#' - Navigate to \href{<url>}{Edify}.
#' - Select the SSO login.
#' - Login with your UT single sign-on credentials.
#' - Click the question mark in the upper-right corner.
#' - Select User Profile.
#' - Select Generate new credential.
#' - New password can be found here.
#'
#' @importFrom keyring key_set
#'
#' @export
#'
set_edify_password <- function() {
  # for storing an individual's password
  key_set(service="edify", username="password")
}

#' Set Edify User Name
#'
#' `set_edify_username` will prompt you with a dialog box to input your user name
#' for Edify DB connections.
#'
#' @details **To set your Edify user name.**
#'
#' - Navigate to \href{<url>}{Edify}.
#' - Select the SSO login.
#' - Login with your UT single sign-on credentials.
#' - Click the question mark in the upper-right corner.
#' - Select User Profile.
#' - Edify username can be found here.
#'
#' @importFrom keyring key_set
#'
#' @export
#'
set_edify_username <- function() {
  # for storing an individual's username
  key_set(service="edify", username="username")
}

#' Set SIS DB Password
#'
#' `set_sis_db_password` will prompt you with a dialog box to input your new password
#' for SIS DB.
#'
#' @details **To reset your SIS DB password.**
#'
#' @importFrom keyring key_set
#'
#' @export
#'
set_sis_db_password <- function() {
  # for storing an individual's password
  key_set(service="sis_db", username="password")
}

#' Set SIS DB User Name
#'
#' `set_sis_db_username` will prompt you with a dialog box to input your user name
#' for SIS DB.
#'
#' @details **To set your SIS DB user name. **
#'
#' @importFrom keyring key_set
#'
#' @export
#'
set_sis_db_username <- function() {
  # for storing an individual's username
  key_set(service="sis_db", username="username")
}


#' Set API key for pins
#'
#' `set_pins_api_key` will prompt you with a dialog box to input the API key for pins access.
#'
#' @details **To set the API key to access pins. **
#'
#' @importFrom keyring key_set
#'
#' @export
#'
set_pins_api_key <- function() {
  # for storing API key for pins
  key_set(service="pins", username="api_key")
}

