# code to prepare `fake_building_df` dataset goes here

sample_size <- 1000

fake_building_df <- data.frame(
  building_location_code = sample(c("1234", NA), sample_size, replace = TRUE),
  building_ownership_code = sample(c("", NA), sample_size, replace = TRUE),
  submission_year = sample(c("", NA), sample_size, replace = TRUE),
  building_name = sample(c("", NA), sample_size, replace = TRUE),
  building_number = sample(c("", NA), sample_size, replace = TRUE),
  building_abbrv = sample(c("", NA), sample_size, replace = TRUE),
  building_construction_year = sample(c("", NA), sample_size, replace = TRUE),
  building_remodel_year = sample(c("", NA), sample_size, replace = TRUE),
  building_cost_replacement = sample(c("100", NA), sample_size, replace = TRUE),
  building_condition_code = sample(c("", NA), sample_size, replace = TRUE),
  building_area_gross = sample(c("", NA), sample_size, replace = TRUE),
  building_cost_myr = sample(c("", NA), sample_size, replace = TRUE),
  building_number = sample(c("", NA), sample_size, replace = TRUE),
  building_auxiliary = sample(c("", NA), sample_size, replace = TRUE)

)

usethis::use_data(fake_building_df, overwrite = TRUE)
