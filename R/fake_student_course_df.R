## code to prepare `fake_student_course_df` dataset goes here

sample_size <- 3000

fake_student_course_df <- data.frame(
  subject_code = sample(c("", NA), sample_size, replace = TRUE),
  course_number = sample(c("", NA), sample_size, replace = TRUE),
  section_number = sample(c("", NA), sample_size, replace = TRUE),
  attempted_credits = sample(c(0:120, NA), sample_size, replace = TRUE),
  earned_credits = sample(c(0:120, NA), sample_size, replace = TRUE),
  course_level_id = sample(c("", NA), sample_size, replace = TRUE),
  final_grade = sample(c("", NA), sample_size, replace = TRUE),
  latest_student_type_code = sample(c("", NA), sample_size, replace = TRUE),
  budget_code = sample(c("", NA), sample_size, replace = TRUE),
  is_concurrent_course = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  student_id = sample(c("", NA), sample_size, replace = TRUE),
  ssn = sample(c("123-45-6789", NA), sample_size, replace = TRUE),
  course_reference_number = sample(c("", NA), sample_size, replace = TRUE),
  season = sample(c("", NA), sample_size, replace = TRUE),
  academic_year_code = sample(c("", NA), sample_size, replace = TRUE),
  version_id = sample(c("", NA), sample_size, replace = TRUE)
)

usethis::use_data(fake_student_course_df, overwrite = TRUE)
