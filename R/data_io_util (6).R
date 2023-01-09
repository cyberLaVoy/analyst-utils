# LIBRARIES ####
library(here)
library(tidyverse)
library(odbc)
library(DBI)
library(janitor)
library(keyring)
library(pins)
library(readxl)

# DATA FROM DATABASES ####

get_conn <- function(dsn) {
  # Server-side db connection with RStudio Connect
  if ( DBI::dbCanConnect(odbc::odbc(), DSN=dsn) ) {
    conn <- DBI::dbConnect(odbc::odbc(), DSN=dsn)
  }
  else if ( DBI::dbCanConnect(RPostgres::Postgres(), DSN=dsn) ) {
    conn <- DBI::dbConnect(RPostgres::Postgres(), DSN=dsn)
  }
  # Local db connection
  else if ( dsn == "edify" ) {
    conn <- DBI::dbConnect( RPostgres::Postgres(),
                            dbname="analytics",
                            host="", # fill in
                            port=, # fill in
                            user=keyring::key_get("edify", "username"),
                            password=keyring::key_get("edify", "password") )
  }
  else {
    conn <- DBI::dbConnect( odbc::odbc(),
                            DSN=dsn,
                            UID=keyring::key_get("sis_db", "username"),
                            PWD=keyring::key_get("sis_db", "password") )
  }
  return(conn)
}

mung_dataframe <- function(df) {
  df <- df %>% 
    mutate_if(is.factor, as.character) %>% 
    clean_names() %>% 
    as_tibble() 
  return(df) 
}

get_data_from_sql_file <- function(file_name, dsn) {
  conn <- get_conn(dsn)
  query <- read_file( here::here('sql', file_name) )
  df <- dbGetQuery(conn, query) %>% 
    mung_dataframe()
  return(df)
}

get_data_from_sql_url <- function(query_url, dsn) {
  conn <- get_conn(dsn)
  query <- read_file(query_url)
  df <- dbGetQuery(conn, query) %>% 
    mung_dataframe()
  return(df)
}

# DATA FROM PINS ####

get_data_from_pin <- function(pin_name) {
  # Obtain the API key from environment variable.
  api_key <- Sys.getenv("RSCONNECT_SERVICE_USER_API_KEY")
  # If API key is not available as environment variable, use keyring entry.
  # NOTE: The API key should only be an environment variable on the server
  #       For local machines, set a keyring entry.
  if (api_key == "") {
    api_key <- keyring::key_get("pins", "api_key") 
  }
  # Register the connection to the pinning board.
  board_register_rsconnect(key=api_key, server="") # fill in
  # pull data from the pin
  df <- pin_get(pin_name, board="rsconnect") %>%
    mung_dataframe()
  return(df)
}

# DATA FROM RDS FILES ####

load_data_from_rds <- function(file_name) {
  df <- readRDS( here::here('data', file_name) ) %>%
    mung_dataframe()
  return(df)
}

save_data_as_rds <- function(df, file_name) {
  saveRDS(df, file=here::here('data', file_name), compress=FALSE)
}

# DATA FROM XLSX FILES ####

load_data_from_xlsx <- function(file_name) {
  df <- read_excel( here::here('data', file_name) )
  return(df) 
}
