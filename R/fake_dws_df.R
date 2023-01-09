## code to prepare `fake_dws_df` dataset goes here

sample_size <- 3000

fake_dws_df <- data.frame(
  ssn = sample(c("123-45-6789", "123-456-789", "12-34-5678", "999-99-9999", NA), sample_size, replace = TRUE),
  student_id = stringr::str_pad( sample(1:999999, sample_size) , 8, pad = '0'),
  graduation_date = sample(seq(as.Date('1978/01/01'), as.Date('2022/01/01'), by="day"), sample_size, replace = TRUE),
  end_search_date = sample(c("", NA), sample_size, replace = TRUE),
  program_id = sample(c("Great Major", "Awesome Major", NA), sample_size, replace = TRUE) )


usethis::use_data(fake_dws_df, overwrite = TRUE)
