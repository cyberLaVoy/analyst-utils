## code to prepare `fake_program_df` dataset goes here

sample_size <- 3000

fake_program_df <- data.frame(
  major_desc = sample(c("", NA), sample_size, replace = TRUE),
  required_credits = sample(c("", NA), sample_size, replace = TRUE),
  is_perkins = sample(c(TRUE, FALSE, NA), sample_size, replace = TRUE),
  primary_degree_id = sample(c("AS", "BME", "MAT", "TR", "AB", "BAS", "BFA", "CER1", "APE", "AC", "MA", "CER0", "AAS", "BIS", "", "BSN", "MACC", "BA", "ND", "BM", "AA", "MMFT", "BS"), sample_size, replace = TRUE),
  ipeds_award_level_code = sample(as.character(c(1:7, NA)), sample_size, replace = TRUE),
  cip_code = sample(c("100305", "430403", "511505", "090101", NA), sample_size, replace = TRUE),
  academic_year = as.character( sample(1978:2022, sample_size, replace = TRUE) )
)

usethis::use_data(fake_program_df, overwrite = TRUE)
