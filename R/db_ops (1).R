library(here)

source(here::here("odbc_connection_object.R"))

create_job <- function(banner_id, job_type, site_name, location, category, supervisor_is_alumnus) {
  if (nchar(banner_id) != 8) {
    return(NULL)
  }
  conn <- get_connection_object()
  on.exit(dbDisconnect(conn), add=TRUE)
  
  insert_sql <- "INSERT INTO warehouse.career_services_jobs (banner_id, job_type, site_name, location, category, supervisor_is_alumnus)
                     VALUES (?banner_id, ?job_type, ?site_name, ?location, ?category, ?supervisor_is_alumnus);"
  insert_query <- sqlInterpolate( conn, 
                                  insert_sql, 
                                  banner_id=banner_id,
                                  job_type=job_type,
                                  site_name=site_name,
                                  location=location,
                                  category=category,
                                  supervisor_is_alumnus=as.integer(supervisor_is_alumnus) )
  dbGetQuery(conn, insert_query) 
}

get_jobs <- function() {
  conn <- get_connection_object()
  on.exit(dbDisconnect(conn), add=TRUE)
  select_query <- "SELECT a.banner_id AS \"Dixie ID\", 
                          b.spriden_first_name || ', ' || b.spriden_last_name AS \"Full Name\",
                          a.job_type AS \"Job Type\", 
                          a.site_name AS \"Site Name\", 
                          a.location AS \"Location\", 
                          a.category AS \"Category\", 
                          a.supervisor_is_alumnus AS \"Supervisor is Alumnus\"
                   FROM warehouse.career_services_jobs a
                   LEFT JOIN saturn.spriden b
                          ON a.banner_id = b.spriden_id;"   
  jobs <- dbGetQuery(conn, select_query)
  return(jobs)
}

get_person_jobs <- function(banner_id) {
  conn <- get_connection_object()
  on.exit(dbDisconnect(conn), add=TRUE)
  select_sql <- "SELECT a.banner_id AS \"Dixie ID\", 
                          b.spriden_first_name || ', ' || b.spriden_last_name AS \"Full Name\",
                          a.job_type AS \"Job Type\", 
                          a.site_name AS \"Site Name\", 
                          a.location AS \"Location\", 
                          a.category AS \"Category\", 
                          a.supervisor_is_alumnus AS \"Supervisor is Alumnus\"
                   FROM warehouse.career_services_jobs a
                   LEFT JOIN saturn.spriden b
                          ON a.banner_id = b.spriden_id
                   WHERE a.banner_id = ?banner_id;"   
   select_query <- sqlInterpolate(conn, 
                                  select_sql, 
                                  banner_id=banner_id) 
  person_jobs <- dbGetQuery(conn, select_query) 
  return(person_jobs) 
}

get_person <- function(banner_id) {
  conn <- get_connection_object()
  on.exit(dbDisconnect(conn), add=TRUE)
  select_sql <- "SELECT spriden_first_name || ', ' || spriden_last_name AS full_name
                 FROM saturn.spriden
                 WHERE spriden_id = ?banner_id;"   
  select_query <- sqlInterpolate( conn, 
                                  select_sql, 
                                  banner_id=banner_id) 
  person <- dbGetQuery(conn, select_query)
  return(person) 
}