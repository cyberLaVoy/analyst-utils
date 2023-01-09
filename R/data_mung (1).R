
# Libraries ####
source(here::here('r', 'data_io_util.R'))

# global variables ####
academic_years_include <- c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")

# PULL DATA ####
print("Pulling instructional and non instructional workload data...")
instructional_and_non_instructional_faculty_workload_df <- get_data_from_pin("instructional_and_non_instructional_faculty_workload_pin")
print("Data pull complete.")

# GLOBAL DATA MUNG ####
instructional_and_non_instructional_faculty_workload_df <- instructional_and_non_instructional_faculty_workload_df %>% 
  mutate( instructor_group = case_when( instructor_status == "PT Instructor Faculty" ~ 'Adjunct',
                                        instructor_status == 'Tenured' ~ 'Tenure-track',
                                        instructor_status == 'On Tenure-Track' ~ 'Tenure-track',
                                        # data integrity issue
                                        is.na(instructor_status) ~ 'No status', 
                                        TRUE ~ "Non-tenure-track" ) ) %>%
  # renaming just to make things work
  mutate( college_abbreviation = course_college,
         department = course_department, 
         subject = course_subject,
         academic_year = academic_year_x ) %>%
  filter( instructor_group != "No status" ) 

# global unique variable availability ####
college_abbreviations <- sort(unique(instructional_and_non_instructional_faculty_workload_df$college_abbreviation))
departments <- sort(unique(instructional_and_non_instructional_faculty_workload_df$department)) 
subjects <- sort(unique(instructional_and_non_instructional_faculty_workload_df$subject)) 
academic_years <- sort(unique(instructional_and_non_instructional_faculty_workload_df$academic_year)) 
latest_academic_year <- max(instructional_and_non_instructional_faculty_workload_df$academic_year)

# global filter data function ####
filter_data <- function(df, includes) {
  # applies iterative filters to dataframe bases on includes list
  for ( include in includes) {
    df <- filter(df, !!sym(include[[1]]) %in% include[[2]])
  }
  return( df )
}

# MODULES DATAFRAMES GENERATION ####
module_df_save <- function(df, aggs, includes=list()) {
   df <- df %>% 
    filter_data( includes ) %>%
    group_by_at( aggs ) %>% 
    summarise( faculty_count=n_distinct(banner_id),
               credit_hours=round(sum( course_student_credit_hours )),
               headcount=sum( course_student_count ),
               maximum_enrollment=sum( course_maximum_enrollment ),
               instructional_workload=sum( course_workload ),
               non_instructional_workload = sum( non_instructional_workload ),
               contracted_workload= sum( contracted_workload ),
               # mitigation of missing faculty college info on base dataset 
               instructor_college=course_college[which.max(credit_hours)] ) %>%

    mutate( fte=round(credit_hours/12),
            fill_rate=round(headcount/maximum_enrollment, 3) ) %>%
     mutate( instructional_workload = replace_na(instructional_workload, 0),
             non_instructional_workload = replace_na(non_instructional_workload, 0) ) %>%
     # final workload and overload calculations ####
   mutate( workload = instructional_workload + non_instructional_workload ) %>%
   mutate( overload = case_when( contracted_workload == 0 ~ 0,
                                 TRUE ~ workload - contracted_workload ) ) %>%  
   
    ungroup()
  file_name <- paste( "by_", paste(aggs, collapse='_'), ".rds", sep='' )
  saveRDS(df, file=here::here('data', file_name), compress=FALSE )  
}

# This function gets immediately called to run below, as an etl step.
generate_modules_dataframes <- function() {
   includes <- list( 
     list("academic_year", academic_years[academic_years != latest_academic_year] ) 
   )
   aggregators <- list(  c("instructor_group", "academic_year"),
                         c("course_division", "academic_year"),
                         c("college_abbreviation", "department", "academic_year"),
                         c("banner_id", "full_name", "academic_year"),
                         c("course_division", "banner_id", "full_name", "academic_year"),
                         c("instructor_group", "banner_id", "full_name", "academic_year"),
                         c("course_division", "college_abbreviation", "department", "academic_year"),
                         c("instructor_group", "college_abbreviation", "department", "academic_year"),
                         c("college_abbreviation", "department", "subject", "academic_year"),
                         c("course_division", "college_abbreviation", "department", "subject", "academic_year"),
                         c("instructor_group", "college_abbreviation", "department", "subject", "academic_year") 
                      )
  for (aggs in aggregators) {
    module_df_save(instructional_and_non_instructional_faculty_workload_df, aggs, includes)
  } 
  # Current data
  includes <- list( 
    list("academic_year", latest_academic_year) 
  )
  module_df_save(instructional_and_non_instructional_faculty_workload_df, "instructor_group", includes)
  module_df_save(instructional_and_non_instructional_faculty_workload_df, "course_division", includes)
}
generate_modules_dataframes()