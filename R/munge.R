# LIBRARIES ####
library(here)

# DATA IMPORT ####
source(here::here('rscript', 'data_io_util.R'))

headcount_df <- get_data_from_sql_file('basic_headcount.sql', 'REPT')

View(headcount_df)
