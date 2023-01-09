library(DBI)
library(odbc)
library(keyring)

if ( DBI::dbCanConnect(odbc::odbc(), "oracle") ) {
  # set connection using DSN entry, if exists
  con <- DBI::dbConnect(odbc::odbc(), "oracle")
} else {
  # otherwise use manual connection variables
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = "Oracle",
                        DBQ    = "<host>:<port>/<service>", # fill in
                        UID = keyring::key_get("sis_db", "username"),
                        PWD = keyring::key_get("sis_db", "password") )
}
