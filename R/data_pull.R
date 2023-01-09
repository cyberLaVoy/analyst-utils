
source(here::here("rscripts", "data_io_util.R"))

programs <- get_data_from_sql_file("select_programs.sql", "REPT") 

colleges <- programs$college %>%
  unique() %>%
  sort()