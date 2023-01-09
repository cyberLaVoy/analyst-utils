#STUDENT TYPE CHECKS ####

# Student Type - Checks to make sure student is returning student ####
# First term enrolled is blank ####
stype_check_01 <- filter(student_sql,
                         student_type == 'R' &
                           is.na(first_term_enrolled_start_date)) %>%
  fn_return_data('Student Type', 'First term enrolled is blank') %>%
  select(all_of(student_columns01), student_type, entry_action, first_term_enrolled_start_date, high_school_grad_date, all_of(student_columns02))


# Student Type - HS Concurrent Enrollment ####
# Start Term Date is Greater Than HS Grad Date ####
stype_check_02 <- filter(student_sql, term_start_date > high_school_grad_date & student_type == 'H') %>%
  fn_return_data('Student Type', 'Start Term Date is Greater Than HS Grad Date', 'shrtgpa, sfrstcr', 'shrtgpa_term_code, sfrstcr_term_code') %>%
  select(all_of(student_columns01), term_start_date, high_school_grad_date, student_type, entry_action, all_of(student_columns02))

# High School Student not in a HS Program ####
stype_check_03 <- filter(student_sql, 
                         student_type == 'H' & 
                           !cur_prgm %in% c('ND-ACE', 'ND-CONC', 'ND-SA') &
                           (season != 'Summer' | cur_prgm != 'ND-CE') ) %>%
  fn_return_data('Student Type', 'High School Student not in a HS Program') %>%
  select(all_of(student_columns01), cur_prgm, high_school_grad_date, student_type, entry_action, all_of(student_columns02))

# Student Type - Personal Interest Students - NM ####
# Degree Seeking Program, but Personal Interest Student Type ####
stype_check_04 <- filter(student_sql, student_type == 'P' & !cur_prgm %in% c('ND-CE', 'ND-ESL') & ! is.na(cur_prgm)) %>%
  fn_return_data('Student Type', 'Degree Seeking Program, but Personal Interest Student Type') %>%
  select(all_of(student_columns01), cur_prgm, student_type, entry_action, all_of(student_columns02))

# STUDENT LEVEL ####
# Student level and student type does not align ####
stype_check_05 <- filter(student_sql, 
                         !student_level == 'GR' & student_type == '1' | #New GR
                           !student_level == 'GR' & student_type == '5' | #Continuing GR
                           !student_level == 'GR' & student_type == '2' | #Transfer GR
                           !student_level == 'GR' & student_type == '4' | #Readmit GR
                           !student_level == 'UG' & student_type == 'T' | #UG Transfers
                           !student_level == 'UG' & student_type == 'F' | #First-time Freshman (FF)
                           !student_level == 'UG' & student_type == 'N' | #First-time Freshman Highschool (FH)
                           !student_level == 'UG' & entry_action == 'FF' | #Entry Action (FF)
                           !student_level == 'UG' & entry_action == 'FH' #Entry Action (FH)
) %>%
  fn_return_data('Student Type', 'Student level and student type does not align') %>%
  select(all_of(student_columns01), student_level, student_type, entry_action, all_of(student_columns02))

# Exclude student type for students that started in summer and enrolled in fall ####
stype_check_06_summer <- filter(student_sql, season == 'Summer') %>%
  mutate(term = as.double(term)) %>%
  select(banner_id, student_type, term) %>%
  arrange(banner_id)

stype_check_06_fall <- filter(student_sql, season == 'Fall') %>%
  select(banner_id, student_type, term = prior_term) %>%
  arrange(banner_id)

exclude_students <- intersect(stype_check_06_summer, stype_check_06_fall) %>%
  select(banner_id) %>%
  distinct() %>%
  arrange(banner_id)

# Student has already attended.  Student Type must be C or R. ####
stype_check_06 <- filter(student_sql,
                         (student_level == 'GR' & student_type %in% c('1', '2') & !is.na(first_term_enrolled) & first_term_enrolled != term |
                            student_level == 'UG' & student_type == 'T' & !is.na(first_term_enrolled) & first_term_enrolled != term |
                            student_level == 'UG' & student_type %in% c('F', 'N') & !is.na(first_term_enrolled) & first_term_enrolled < term & first_term_enrolled_start_date > high_school_grad_date |
                            student_level == 'UG' & entry_action %in% c('FF', 'FH') & !is.na(first_term_enrolled) & first_term_enrolled < term & first_term_enrolled_start_date > high_school_grad_date)
) %>%
  fn_return_data('Student Type', 'Student has already attended.  Student Type must be C or R.') %>%
  select(all_of(student_columns01), student_level, first_term_enrolled, student_type, entry_action, all_of(student_columns02)) %>%
  # filter students that started in summer as FT, TU and enrolled in fall
  filter(!banner_id %in% c(exclude_students$banner_id))

# Student has transfer record ####
stype_check_07 <- filter(student_sql, 
                         student_level == 'GR' & student_type == '1' & !is.na(last_transfer_term) |
                           student_level == 'UG' & student_type %in% c('N', 'F') &  !is.na(last_transfer_term) & high_school_grad_date < last_transfer_term_start_date
) %>%
  fn_return_data('Student Type', 'Student has transfer record') %>%
  select(all_of(student_columns01), student_level, last_transfer_term, student_type, entry_action, all_of(student_columns02))

# Student has not attended before ####
stype_check_08 <- select(student_sql, everything()) %>%
  mutate(days_since_last_enrolled = difftime(term_start_date,last_term_enrolled_end_date)) %>%
  filter(student_level == 'GR' & student_type == '4' & ! is.na(first_term_enrolled)|
           student_level == 'GR' & student_type == '4' & days_since_last_enrolled < 240
  ) %>%
  fn_return_data('Student Type', 'Student has not attended before') %>%
  select(all_of(student_columns01), student_level, first_term_enrolled, student_type, entry_action, all_of(student_columns02))

# Student has a cohort record.  Student Type must be C or R. ####
stype_check_09 <- filter(student_sql, 
                         student_level == 'UG' & student_type %in% c('T', 'F', 'N') & !is.na(sgrchrt_chrt_code) & sgrchrt_term_code_eff != term |
                           student_level == 'UG' & entry_action %in% c('FF', 'FH') & !is.na(sgrchrt_chrt_code) & sgrchrt_term_code_eff != term) %>%
  fn_return_data('Student Type', 'Student has a cohort record.  Student Type must be C or R.') %>%
  select(all_of(student_columns01), student_type, sgrchrt_chrt_code, last_term_enrolled, entry_action, all_of(student_columns02))

# Graduated from HS within a year ####
stype_check_10 <- select(student_sql, everything()) %>%
  mutate(days_since_hs_graduation = difftime(term_start_date, high_school_grad_date)) %>%
  filter(student_level == 'UG'  & 
           student_type == 'F' & !is.na(first_term_enrolled) & 
           days_since_hs_graduation < 365
  )  %>%
  fn_return_data('Student Type', 'Graduated from HS within a year') %>%
  select(all_of(student_columns01), first_term_enrolled, term_start_date, high_school_grad_date, days_since_hs_graduation, student_type, entry_action, all_of(student_columns02))

# Graduated from HS within a year ####
stype_check_11 <- select(student_sql, everything()) %>%
  mutate(days_since_hs_graduation = difftime(term_start_date, high_school_grad_date)) %>%
  filter(student_level == 'UG'  & 
           student_type == 'N' & !is.na(first_term_enrolled) & 
           days_since_hs_graduation > 365
  ) %>%
  fn_return_data('Student Type', 'Graduated from HS within a year') %>%
  select(all_of(student_columns01), first_term_enrolled, term_start_date, high_school_grad_date, days_since_hs_graduation, student_type, entry_action, all_of(student_columns02))

# Graduated from HS within a year ####
stype_check_12 <- select(student_sql, everything()) %>%
  mutate(days_since_hs_graduation = difftime(term_start_date, high_school_grad_date)) %>%
  filter(student_level == 'UG'  & 
           entry_action == 'FF' & !is.na(first_term_enrolled) & 
           days_since_hs_graduation < 365
  ) %>%
  fn_return_data('Student Type', 'Graduated from HS within a year') %>%
  select(all_of(student_columns01), first_term_enrolled, term_start_date, high_school_grad_date, days_since_hs_graduation, student_type, entry_action, all_of(student_columns02))

# Graduated from HS within a year ####
stype_check_13 <- select(student_sql, everything()) %>%
  mutate(days_since_hs_graduation = difftime(term_start_date, high_school_grad_date)) %>%
  filter(student_level == 'UG'  & 
           entry_action == 'FH' & !is.na(first_term_enrolled) & 
           days_since_hs_graduation > 365
  ) %>%
  fn_return_data('Student Type', 'Graduated from HS within a year') %>%
  select(all_of(student_columns01), first_term_enrolled, term_start_date, high_school_grad_date, days_since_hs_graduation, student_type, entry_action, all_of(student_columns02))

# Student Level is missing ####
stype_check_14 <- filter(student_sql, is.na(student_level)) %>%
  fn_return_data('Student Level', 'Student Level is missing') %>%
  select(all_of(student_columns01), student_level, all_of(student_columns02))

# IE CHECKS ####
# Entry Action does not match student type ####
ie_check_01 <-  mutate(student_sql, entry_action_mapped = case_when(
  entry_action == 'CS' ~ 'C',
  entry_action == 'FF' ~ 'F',
  entry_action == 'FH' ~ 'F',
  entry_action == 'HS' ~ 'H',
  entry_action == 'RS' ~ 'R',
  entry_action == 'NM' ~ 'P',
  entry_action == 'TU' ~ 'T',
  entry_action == 'NG' ~ '1',
  entry_action == 'RG' ~ '3',
  entry_action == 'CG' ~ '5',
  entry_action == 'NM' ~ '0',
  entry_action == 'TG' ~ '2'
)
) %>%
  filter(entry_action_mapped != student_type) %>%
  fn_return_data('Student Type', 'Entry Action does not match student type') %>%
  select(all_of(student_columns01), student_type, entry_action, first_term_enrolled, last_term_enrolled, all_of(student_columns02))