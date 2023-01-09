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
cohort_info <- get_data('cohort_info.sql') %>% 
  mutate(pidm = as.character(pidm))
return_term <- get_data('return_term.sql') %>% 
  mutate(pidm = as.character(pidm))
achievements <- get_data('achievements.sql') %>% 
  mutate(pidm = as.character(pidm))
holds <- get_data('student_holds.sql') %>% 
  mutate(student_pidm = as.character(student_pidm))

# DATA SAVE ####
save(cohort_info, file=here::here('data', 'cohort_info.RData'))
save(return_term, file=here::here('data', 'return_term.RData'))
save(achievements, file=here::here('data', 'achievements.RData'))
save(holds, file=here::here('data', 'holds.RData'))