
# LOAD DATA ####
load(here::here('data', 'return_term.RData'))
load(here::here('data', 'cohort_info.RData'))
load(here::here('data', 'achievements.RData'))
load(here::here('data', 'holds.RData'))
#load(here::here('data', 'advisor_student_enrollment.RData'))

retention_plot_df <- left_join( x=return_term, 
                             y=cohort_info, 
                             by=c("pidm", "cohort_year") ) %>%
  mutate(all="All", achievement_type="Retained",
         cohort_year=as.numeric(cohort_year) ) %>% 
  filter( college_abbreviation != "Global & Community Outreach" )

achievements <- achievements %>%
          # remove achievements of no interest to current client
          filter( achievement_type != "Certificate" )
        
  
outcomes_plot_df <- left_join(x=cohort_info, 
                                   y=achievements, 
                                   by=c("pidm", "cohort_year") ) %>%
  mutate(all="All") %>%
  filter( college_abbreviation != "Global & Community Outreach" ) %>%
  # this filter is giving people time to get a bachelors
  filter( cohort_year <= 2016 )


advisor_student_enrollment <- advisor_student_enrollment %>% 
  mutate(advisor_pidm = as.character(advisor_pidm),
         student_pidm = as.character(student_pidm),
         cohort_year = as.numeric(cohort_year) ) 
 # %>% filter( college_abbreviation != "Global & Community Outreach" )