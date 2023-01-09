# code to prepare `fake_graduation_df` dataset goes here

sample_size <- 1000

fake_graduation_df <- data.frame(
  student_id = stringr::str_pad( sample(1:999999, sample_size) , 8, pad = '0'),
  ssn = sample(c("123-45-6789", "123-456-789", "12-34-5678", "999-99-9999", NA), sample_size, replace = TRUE),
  last_name = sample(c("Doe", "Smith", "Johnson", "Williams", NA), sample_size, replace = TRUE),
  first_name = sample(c("Jane", "Joe", "Robert", "Mary", NA), sample_size, replace = TRUE),
  middle_name = sample(c("Elizabeth", "Louise", "Ann", "Lee", NA), sample_size, replace = TRUE),
  name_suffix = sample(c("Jr.", "Sr.", "I", "II", "III", NA), sample_size, replace = TRUE),
  first_admit_county_code = sample(c("015", "01", "039", NA), sample_size, replace = TRUE),
  first_admit_country_iso_code = sample(c("US", "XX", NA), sample_size, replace = TRUE),
  first_admit_state_code = sample(c("UT","CA","AZ", NA), sample_size, replace = TRUE),
  birth_date = sample(seq(as.Date('1978/01/01'), as.Date('2022/01/01'), by="day"), sample_size, replace = TRUE),
  gender_code = sample(c("M", "F", NA), sample_size, replace = TRUE),
  is_hispanic_latino_ethnicity = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_asian = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_black = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_american_indian_alaskan = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_hawaiian_pacific_islander = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_white = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_international = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_other_race = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  graduation_date = sample(seq(as.Date('1978/01/01'), as.Date('2022/01/01'), by="day"), sample_size, replace = TRUE),
  primary_major_cip_code = sample(c("", NA), sample_size, replace = TRUE),
  primary_degree_id = sample(c("AS", "BME", "MAT", "TR", "AB", "BAS", "BFA", "CER1", "APE", "AC", "MA", "CER0", "AAS", "BIS", "", "BSN", "MACC", "BA", "ND", "BM", "AA", "MMFT", "BS"), sample_size, replace = TRUE),
  degree_status_code = sample(c("AW", NA), sample_size, replace = TRUE),
  cumulative_graduation_gpa = sample(0:400, sample_size, replace = TRUE) / 100,
  transfer_cumulative_credits_earned = sample(c(0:32, NA_integer_), sample_size, replace = TRUE),
  total_cumulative_ap_credits_earned = sample(0:32, sample_size, replace = TRUE),
  total_cumulative_clep_credits_earned = sample(0:32, sample_size, replace = TRUE),
  total_cumulative_credits_attempted_other_sources = sample(c(100.5, 36.7, 192), sample_size, replace = TRUE),
  total_remedial_hours = sample(c(0:32, NA_integer_), sample_size, replace = TRUE),
  overall_cumulative_credits_earned = sample(0:32, sample_size, replace = TRUE),
  level_id = sample(c("UG", NA), sample_size, replace = TRUE),
  previous_degree_type = sample(c("", NA), sample_size, replace = TRUE),
  ipeds_award_level_code = sample(as.character(c(1:7, NA)), sample_size, replace = TRUE),
  required_credits = sample(c("", NA), sample_size, replace = TRUE),
  high_school_code = sample(c("", NA), sample_size, replace = TRUE),
  ssid = as.character( sample(1000000:3000000, sample_size) ),
  earned_contact_hrs  = sample(c("", NA), sample_size, replace = TRUE),
  program_hrs = sample(c("", NA), sample_size, replace = TRUE),
  graduated_term_id = sample(c("", NA), sample_size, replace = TRUE),
  financial_aid_year_id = sample(c("1920", "2021", NA), sample_size, replace = TRUE),
  season = sample(c("Fall", "Summer", "spring", NA), sample_size, replace = TRUE),
  college_desc = sample(c("Psychology", "Marriage Family Therapy", NA), sample_size, replace = TRUE),
  primary_major_desc = sample(c("Great Major", "Awesome Major", NA), sample_size, replace = TRUE),
  degree_desc = sample(c("Great Major", "Awesome Major", NA), sample_size, replace = TRUE),
  academic_year_code = as.character( sample(1978:2022, sample_size, replace = TRUE) )

)

usethis::use_data(fake_graduation_df, overwrite = TRUE)
