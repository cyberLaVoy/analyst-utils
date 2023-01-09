# This script loads the data

# LIBRARIES ####
library(tidyverse)
library(DBI)
library(here)
library(janitor)

# FUNCTIONS ####
get_data <- function(file) {
  output_file <- dbGetQuery(con, read_file(here::here('sql', file))) %>% 
    mutate_if(is.factor, as.character) %>% 
    clean_names() %>% 
    as_tibble()
}

# CONNECTION OBJECT ####
source(here::here('rscripts', 'dsu_odbc_connection_object.R'))

# DATA PULL ####
advisor_student_enrollment <- get_data('student_enrollment.sql')

# DATA SAVE ####

#save(advisor_student_enrollment, file=here::here('data', 'advisor_student_enrollment.RData'))