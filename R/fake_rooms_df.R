## code to prepare `fake_rooms_df` dataset goes here

sample_size <- 1000

fake_rooms_df <- data.frame(
  submission_year = sample(c("", NA), sample_size, replace = TRUE),
  building_number = sample(c("", NA), sample_size, replace = TRUE),
  room_number = sample(c("", NA), sample_size, replace = TRUE),
  room_group1_code = sample(c("", NA), sample_size, replace = TRUE),
  room_use_code_group = sample(c("", NA), sample_size, replace = TRUE),
  room_use_code = sample(c("", NA), sample_size, replace = TRUE),
  room_name = sample(c("", NA), sample_size, replace = TRUE),
  room_stations = sample(c("", NA), sample_size, replace = TRUE),
  room_area = sample(c("", NA), sample_size, replace = TRUE),
  room_disabled_access = sample(c("", NA), sample_size, replace = TRUE),
  room_prorated = sample(c("", NA), sample_size, replace = TRUE),
  room_prorated_area = sample(c("", NA), sample_size, replace = TRUE),
  room_activity_date  = sample(c("", NA), sample_size, replace = TRUE)

)

usethis::use_data(fake_rooms_df, overwrite = TRUE)
