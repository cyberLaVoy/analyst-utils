## code to prepare `fake_fin_aid_df` dataset goes here

sample_size <- 3000

fake_fin_aid_df <- data.frame(
  financial_aid_year_id = sample(c("", NA), sample_size, replace = TRUE),
  term_id = sample(c("", NA), sample_size, replace = TRUE),
  season = sample(c("Summer", "Spring", "Fall", NA), sample_size, replace = TRUE),
  ssn = sample(c("", NA), sample_size, replace = TRUE),
  student_id = sample(c("", NA), sample_size, replace = TRUE),
  financial_aid_fund_id = sample(c("", NA), sample_size, replace = TRUE),
  amount_offered = sample(c("", NA), sample_size, replace = TRUE) )


usethis::use_data(fake_fin_aid_df, overwrite = TRUE)
