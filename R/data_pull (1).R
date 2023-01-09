library(tidyverse)
library(DBI)
library(odbc)
library(janitor)

source(here::here('r', 'data_io_util.R'))

#financial_aid_data <- get_data('common_data_set.sql') %>%
#    select(everything())

students_data <- get_data_from_sql_file('students03.sql', 'DSCIR')
degrees_data <- get_data_from_sql_file('degrees.sql', 'DSCIR')
#cohort_2014_data <- get_data_from_sql_file('ipeds_graduation_rates_201440.sql', 'EDIFY')#Get Brenten's script for IPEDS Grad Rate
admissions_data <- get_data_from_sql_file('admissions_data.sql', 'PPRD')# change connection - REPT
test_score_data <- get_data_from_sql_file('test_scores.sql', 'PPRD')# change to REPT
