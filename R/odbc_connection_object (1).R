library(DBI)
library(odbc)
library(keyringr)

get_connection_object <- function() {
  if ( DBI::dbCanConnect(odbc::odbc(), "oracle") ) {
    # set connection using DSN entry, if exists
    conn <- DBI::dbConnect(odbc::odbc(), "oracle")
  } else {
    # otherwise use manual connection variables
    conn <- DBI::dbConnect(odbc::odbc(),
                          Driver = "Oracle",
                          DBQ    = "<host>:<port>/<service>",
                          UID    = decrypt_kc_pw("dsu_banner_username"),
                          PWD    = decrypt_kc_pw("dsu_banner_password") )
  }
}
