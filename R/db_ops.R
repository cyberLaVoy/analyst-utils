# LIBRARIES ####
library(here)
library(tidyverse)
library(odbc)
library(DBI)
library(janitor)
library(keyring)

# FUNCTIONS ####
get_conn <- function(dsn='BRPT') {
  # Server-side db connection with RStudio Connect
  if ( DBI::dbCanConnect(odbc::odbc(), 
                         DSN="oracle") ) {
    conn <- DBI::dbConnect(odbc::odbc(), 
                           DSN="oracle")
    # Local db connection
  } else {
    conn <- DBI::dbConnect(odbc::odbc(),
                           DSN = dsn,
                           UID = keyring::key_get("sis_db", "username"),
                           PWD = keyring::key_get("sis_db", "password") )
  }
  return(conn)
}

clean_df <- function(df) {
  df <- df %>% 
    mutate_if(is.factor, as.character) %>% 
    clean_names() %>% 
    as_tibble()
  return(df)
}

get_headcounts_by_program <- function(program_code) {
  conn <- get_conn()
  on.exit(dbDisconnect(conn), add=TRUE)
  select_sql <-  read_file( here::here('sql', 'headcounts_by_program.sql') )
  # replace oracle SQL parameter indicator (colon), with a question mark
  select_sql <- gsub(':', '?', select_sql)
  select_query <- sqlInterpolate( conn, 
                                  select_sql, 
                                  p_program_code=program_code )
  headcounts <- dbGetQuery(conn, select_query)
  return(clean_df(headcounts))
}

get_headcounts_by_department_given_program <- function(program_code) {
  conn <- get_conn()
  on.exit(dbDisconnect(conn), add=TRUE)
  select_sql <-  read_file( here::here('sql', 'headcounts_by_department_given_program.sql') )
  # replace oracle SQL parameter indicator (colon), with a question mark
  select_sql <- gsub(':', '?', select_sql)
  select_query <- sqlInterpolate( conn, 
                                  select_sql, 
                                  p_program_code=program_code )
  headcounts <- dbGetQuery(conn, select_query)
  return(clean_df(headcounts))
}

get_graduates_by_program <- function(program_code) {
  conn <- get_conn()
  on.exit(dbDisconnect(conn), add=TRUE)
  select_sql <-  read_file( here::here('sql', 'graduates_by_program.sql') )
  # replace oracle SQL parameter indicator (colon), with a question mark
  select_sql <- gsub(':', '?', select_sql)
  select_query <- sqlInterpolate( conn, 
                                  select_sql, 
                                  p_program_code=program_code )
  grads <- dbGetQuery(conn, select_query)
  return(clean_df(grads))
}

get_graduates_by_department_given_program <- function(program_code) {
  conn <- get_conn()
  on.exit(dbDisconnect(conn), add=TRUE)
  select_sql <-  read_file( here::here('sql', 'graduates_by_department_given_program.sql') )
  # replace oracle SQL parameter indicator (colon), with a question mark
  select_sql <- gsub(':', '?', select_sql)
  select_query <- sqlInterpolate( conn, 
                                  select_sql, 
                                  p_program_code=program_code )
  grads <- dbGetQuery(conn, select_query)
  return(clean_df(grads))
}

get_current_programs <- function() {
  conn <- get_conn()
  on.exit(dbDisconnect(conn), add=TRUE)
  select_sql <-  read_file( here::here('sql', 'current_programs.sql') )
  programs <- dbGetQuery(conn, select_sql)
  return(clean_df(programs))
}



