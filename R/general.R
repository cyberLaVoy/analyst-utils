#' Clean Data Frame
#'
#' Performs various operations over each value in data frame.
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all
#'
#' @param input_df A Data Frame.
#'
#' @return Original data frame, with cleaning operations applied.
#' @export
#'
#' @examples
#' clean()
#'

clean <- function(input_df=usheUtils::fake_student_df) {

  # Remove special characters
  input_df[] <- lapply(input_df, function(x) { remove_special_characters(x) } )
  # Replace & characters with and
  input_df[] <- lapply(input_df,  function(x) { str_replace_all(x, "&", "and") } )

  return(input_df)
}

#' Remove Special Characters
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all
#'
#' @param x any value or vector of values
#'
#' @return x with special characters removed from x
#' @export
#'
#' @examples
#' remove_special_characters()
remove_special_characters  <- function(x = "spe;c.l-cha,ac?ers"){

  # Remove special characters
  if( is.character(x) )
    {x <- x %>%
      str_replace_all("[.,;:?]", "") %>%
      str_replace_all("[-]", "")
    }

return(x)

}

#' Check for valid SSN characters
#'
#' @details
#'
#' Is valid ssn id
#'
#' @importFrom stringr str_detect
#' @importFrom dplyr if_else
#'
#' @param ssn a vector of ssn to check for validity.
#'
#'
#' @return A Boolean TRUE or FALSE if SSN is valid.
#' @export
#'
#' @examples
#' is_valid_ssn(c("123-45-6789", "987-65-4321"))
#'
is_valid_ssn <- function(ssn) {
  # Note: ssn that start with 9 are actually IRS issued tax IDs; USHE only wants valid ssn.
  ssn_regex <- "^[0-8][0-9][0-9]-[0-9][0-9]-[0-9][0-9][0-9][0-9]$"

  valid_ssn_check <- str_detect(ssn, ssn_regex)

  valid_ssn_check <- if_else(is.na(valid_ssn_check), FALSE, valid_ssn_check )

  return(valid_ssn_check)
}

#' Calculate USHE Element Institution (Institution)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Institution
#' - FIELD NAME: C_INST, S_INST, G_INST, SC_INST, M_INST, F_INST, R_INST, B_INST, & PF_INST
#' - FIELD FORMAT: Varchar, 4 Characters
#' - DEFINITION: The identification number used by the U.S. Department of Education’s Office of Postsecondary Education (OPE) to identify schools that have Program Participation Agreements (PPA) so that its students are eligible to participate in Federal Student Financial Assistance programs under Title IV regulations.
#'               This is a 6-digit number followed by a 2-digit suffix used to identify branches,  additional locations, and other entities that are part of the eligible institution.
#'               USHE will use the last 4 digits of the 6-digit number for the field value as additional locations will be specified by C-09 Line Item.
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (no data fields required).
#'
#'
#' @return Original data frame, with USHE data element s_01 appended.
#' @export
#'
#' @examples
#' gen_ushe_inst()
#'
gen_ushe_inst <- function(input_df=usheUtils::fake_student_df) {
  institution <- NULL
  gen_ushe_inst <- NULL
  s_01 <- NULL
  c_01 <- NULL
  sc_01 <- NULL
  pf_01 <- NULL
  m_01 <- NULL
  b_01 <- NULL
  r_01 <- NULL
  g_01 <- NULL
  f_01 <- NULL
  d_01 <- NULL

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( institution = "3671") %>%
    # Append USHE data element gen_ushe_inst
    mutate( gen_ushe_inst = institution,
            s_01 = institution,
            c_01 = institution,
            sc_01 = institution,
            pf_01 = institution,
            m_01 = institution,
            b_01 = institution,
            r_01 = institution,
            g_01 = institution,
            f_01 = institution,
            d_01 = institution )

  return(output_df)
}

#' @rdname gen_ushe_inst
#' @examples s_01()
#' @export
s_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples c_01()
#' @export
c_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples sc_01()
#' @export
sc_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples pf_01()
#' @export
pf_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples m_01()
#' @export
m_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples b_01()
#' @export
b_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples r_01()
#' @export
r_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples g_01()
#' @export
g_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples f_01()
#' @export
f_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples d_01()
#' @export
d_01 <- gen_ushe_inst


#' Calculate USHE Elements for Names (Student Name)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr coalesce
#'
#' @param input_df A Data Frame. Must contain the following data fields: (last_name, first_name, middle_name, name_suffix).
#'
#'
#' @return Original data frame, with USHE data element s_06 appended.
#' @export
#'
#' @examples
#' gen_ushe_name()
#'
gen_ushe_name <- function(input_df=usheUtils::fake_student_df) {
  gen_ushe_name <- NULL


  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( last_name = as.character(last_name),
            first_name = as.character(first_name),
            middle_name = as.character(middle_name),
            name_suffix = as.character(name_suffix) ) %>%
    # Append USHE data element gen_ushe_name
    mutate( name = paste(coalesce(last_name, ''),
                         coalesce(first_name, ''),
                         coalesce(middle_name, ''),
                         coalesce(name_suffix, ''),
                         sep = "|") ) %>%
    mutate(gen_ushe_name = name,
           s_06 = name,
           g_03 = name)

  return(output_df)
}

#' @rdname gen_ushe_name
#' @examples s_06()
#' @export
s_06 <- gen_ushe_name

#' @rdname gen_ushe_name
#' @examples g_03()
#' @export
g_03 <- gen_ushe_name

#' Calculate USHE Element Ethnicity (Ethnic Origin)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Ethnicity
#' - FIELD NAME: s_ethnic, g_ethnic
#' - FIELD FORMAT: Varchar, 8 Characters,
#' - DEFINITION: The racial and ethnic categories used to classify students.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @param input_df A Data Frame. Must contain the following data fields: (is_hispanic_latino_ethnicity, is_asian, is_black, is_american_indian_alaskan, is_hawaiian_pacific_islander, is_white, is_international, is_other_race).
#'
#'
#' @return Original data frame, with USHE data element gen_ushe_ethnicty appended.
#' @export
#'
#' @examples
#' gen_ushe_ethnicty()
#'
gen_ushe_ethnicty <- function(input_df=usheUtils::fake_student_df) {
  gen_ushe_ethnicty <- NULL

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(ethnic_h = if_else(is_hispanic_latino_ethnicity, "H", ""),
           ethnic_a = if_else(is_asian, "A", ""),
           ethnic_b = if_else(is_black, "B", ""),
           ethnic_i = if_else(is_american_indian_alaskan, "I", ""),
           ethnic_p = if_else(is_hawaiian_pacific_islander, "P", ""),
           ethnic_w = if_else(is_white, "W", ""),
           ethnic_n = if_else(is_international, "N", ""),
           ethnic_u = if_else(is_other_race, "U", "") ) %>%
    mutate( ethnicty_intermediate = paste(ethnic_h, ethnic_a, ethnic_b, ethnic_i, ethnic_p, ethnic_w, ethnic_n, ethnic_u, sep= "|" )) %>%
    # Append USHE data element gen_ushe_ethnicty
    mutate( ethnicty = if_else(is_international, paste("", "", "", "", "", "", ethnic_n, "", sep = "|"), ethnicty_intermediate) ) %>%
    mutate(gen_ushe_ethnicty = ethnicty,
           s_14 = ethnicty,
           g_07 = ethnicty )

  return(output_df)
}

#' @rdname gen_ushe_ethnicty
#' @examples s_14()
#' @export
s_14 <- gen_ushe_ethnicty

#' @rdname gen_ushe_ethnicty
#' @examples g_07()
#' @export
g_07 <- gen_ushe_ethnicty

#' Calculate USHE Element Utah County Code (Utah County Code)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Utah County Code
#' - FIELD NAME: S_COUNTY_ORIGIN, and G_COUNTY_ORIGIN,
#' - FIELD FORMAT: Varchar, 5 Characters,
#' - DEFINITION: The Utah county code indicating the student’s county of origin as described at the time of first application to the institution for enrollment and if the S_STATE_ORIGIN is UT.
#'               Enter UT030 if  county is Unknown.
#'               Enter UT097 if student is Out of State, Out of US.
#'               Enter UT099 if student is Out  of State, In the US.
#'               This element should be logically consistent with S-11: S_STATE_ORIGIN and S 27: S_COUNTRY_ORIGIN.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (first_admit_county_code, first_admit_state_code, first_admit_country_iso_code).
#'
#'
#' @return Original data frame, with USHE data element s_10 appended.
#' @export
#'
#' @examples
#' gen_ushe_county_origin()
#'
gen_ushe_county_origin <- function(input_df=usheUtils::fake_student_df) {
  gen_ushe_county_origin <- NULL

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(county_origin = case_when(
      ( first_admit_state_code == "UT" & !is.na(first_admit_county_code) ) ~ paste0("UT", first_admit_county_code),
      first_admit_country_iso_code == "US" ~ "UT099",
      first_admit_country_iso_code != "US" ~ "UT097",
      TRUE ~ "UT030")) %>%
    # Append USHE data element gen_ushe_county_origin
    mutate( gen_ushe_county_origin = county_origin,
            s_10 = county_origin,
            g_04 = county_origin)

  return(output_df)
}

#' @rdname gen_ushe_county_origin
#' @examples s_10()
#' @export
s_10 <- gen_ushe_county_origin

#' @rdname gen_ushe_county_origin
#' @examples g_04()
#' @export
g_04 <- gen_ushe_county_origin

#' Calculate USHE Element Social Security Number or Institutionally Assigned ID (Student ID)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Institutionally Assigned ID
#' - FIELD NAME: s_id, sc_id, d_id & g_id
#' - FIELD FORMAT: Varchar, 9 Characters
#' - DEFINITION: The unique institutionally assigned identification number for each student intended to be used in lieu of using a student’s social security number.
#'               G_Banner_ID must begin with an Alpha character representing the institution.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#' @importFrom stringr str_replace_all
#'
#' @param input_df A Data Frame. Must contain the following data fields: (student_id, ssn).
#'
#'
#' @return Original data frame, with USHE data element s_03 appended.
#' @export
#'
#' @examples
#' gen_ushe_id()
#'
gen_ushe_id <- function(input_df=usheUtils::fake_student_df) {
  gen_ushe_id <- NULL

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( id = if_else(is_valid_ssn(ssn),
                         str_replace_all(ssn, "-", ""),
                         paste0('D', student_id) ) )  %>%

    # Append USHE data element gen_ushe_id
    mutate( gen_ushe_id = id,
            s_03 = id,
            sc_03 = id,
            g_02 = id,
            f_04 = id,
            d_02 = id )

  return(output_df)
}

#' @rdname gen_ushe_id
#' @examples s_03()
#' @export
s_03 <- gen_ushe_id

#' @rdname gen_ushe_id
#' @examples sc_03()
#' @export
sc_03 <- gen_ushe_id

#' @rdname gen_ushe_id
#' @examples g_02()
#' @export
g_02 <- gen_ushe_id

#' @rdname gen_ushe_id
#' @examples f_04()
#' @export
f_04 <- gen_ushe_id

#' @rdname gen_ushe_id
#' @examples d_02()
#' @export
d_02 <- gen_ushe_id

#' Calculate USHE Element (Birth Date)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Date of Birth
#' - FIELD NAME: S_BIRTH_DT, M_BIRTH_DT, & G_BIRTH_DT
#' - FIELD FORMAT: Varchar, 8 Characters (YYYYMMDD),
#' - DEFINITION: The calendar student date of birth, as designated by student.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (birth_date).
#'
#'
#' @return Original data frame, with USHE data elements gen_ushe_birth_date and m_03 appended.
#' @export
#'
#' @examples
#' gen_ushe_birth_date()
#'
gen_ushe_birth_date <- function(input_df=usheUtils::fake_student_df) {
  gen_ushe_birth_date <- NULL

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( birth_dt = gsub("-", "", birth_date) ) %>%
    # Append USHE data element gen_ushe_birth_date
    mutate( gen_ushe_birth_date = birth_dt,
            s_12 = birth_dt,
            m_03 = birth_dt,
            g_05 = birth_dt)

  return(output_df)
}

#' @rdname gen_ushe_birth_date
#' @examples s_12()
#' @export
s_12 <- gen_ushe_birth_date

#' @rdname gen_ushe_birth_date
#' @examples m_03()
#' @export
m_03 <- gen_ushe_birth_date

#' @rdname gen_ushe_birth_date
#' @examples g_05()
#' @export
g_05 <- gen_ushe_birth_date

#' Calculate USHE Element ushe_banner_id (Institution Assigned ID)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Institutionally Assigned ID
#' - FIELD NAME: gen_ushe_banner_id, s_banner_id, m_banner_id & g_banner_id
#' - FIELD FORMAT: Varchar, 5 Characters,
#' - DEFINITION: The unique institutionally assigned identification number for each student.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (student_id).
#'
#'
#' @return Original data frame, with USHE data element s_35, m_06, g_21 appended.
#' @export
#'
#' @examples
#' gen_ushe_banner_id()
#'
gen_ushe_banner_id <- function(input_df=usheUtils::fake_student_df) {
  gen_ushe_banner_id <- NULL

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( banner_id = paste0('D', student_id )) %>%
    # Append USHE data element gen_ushe_banner_id
    mutate( gen_ushe_banner_id = banner_id,
            s_35 = banner_id,
            m_06 = banner_id,
            g_21 = banner_id,
            f_05 = banner_id,
            d_03 = banner_id )

  return(output_df)
}

#' @rdname gen_ushe_banner_id
#' @examples s_35()
#' @export
s_35 <- gen_ushe_banner_id

#' @rdname gen_ushe_banner_id
#' @examples m_06()
#' @export
m_06 <- gen_ushe_banner_id

#' @rdname gen_ushe_banner_id
#' @examples g_21()
#' @export
g_21 <- gen_ushe_banner_id

#' @rdname gen_ushe_banner_id
#' @examples f_05()
#' @export
f_05 <- gen_ushe_banner_id

#' @rdname gen_ushe_banner_id
#' @examples d_03()
#' @export
d_03 <- gen_ushe_banner_id

#' Calculate USHE Element s_34, g_20 (SSID aka USOE Unique ID)
#'
#' @details
#'
#' **USHE Documentation**
#' ELEMENT NAME: Unique USOE State Student Identifier
#' FIELD NAME: gen_ushe_ssid, s_ssid & s_G_SSID
#' FIELD FORMAT: Varchar, 9 Characters
#' DEFINITION: The unique Utah State Student Identifier as assigned to each Utah public education student by the Utah State Board of Education.
#' This 9-digit number will appear on all high school transcripts beginning the 2006-2007 academic year.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (ssid).
#'
#'
#' @return Original data frame, with USHE data element gen_ushe_ssid appended.
#' @export
#'
#' @examples
#' gen_ushe_ssid()
#'
gen_ushe_ssid <- function(input_df=usheUtils::fake_student_df) {
  gen_ushe_ssid <- NULL

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( ssid = ssid ) %>%
    # Append USHE data element s_34
    mutate( gen_ushe_ssid = ssid,
            s_34 = ssid,
            g_20 = ssid )

  return(output_df)
}

#' @rdname gen_ushe_ssid
#' @examples s_34()
#' @export
s_34 <- gen_ushe_ssid

#' @rdname gen_ushe_ssid
#' @examples g_20()
#' @export
g_20 <- gen_ushe_ssid

#' Calculate USHE Element(Gender)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Gender
#' - FIELD NAME: gen_ushe_gender, g_gender, s_gender
#' - FIELD FORMAT: Varchar, 1 Character
#' - DEFINITION: A code indicating the gender of the student
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (gender_code).
#'
#'
#' @return Original data frame, with USHE data element gen_ushe_gender appended.
#' @export
#'
#' @examples
#' gen_ushe_gender()
#'
gen_ushe_gender <- function(input_df=usheUtils::fake_student_df) {
  gen_ushe_gender <- NULL

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( gender = gender_code ) %>%
    # Append USHE data element gen_ushe_gender
    mutate( gen_ushe_gender = gender,
            s_13 = gender,
            g_06 = gender )

  return(output_df)
}

#' @rdname gen_ushe_gender
#' @examples s_13()
#' @export
s_13 <- gen_ushe_gender

#' @rdname gen_ushe_gender
#' @examples g_06()
#' @export
g_06 <- gen_ushe_gender

#' Calculate USHE Element (Year, Term, & Extract)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Year, Term & Extract Code
#' - FIELD NAME: gen_ushe_, s_, sc_term_id & c_term_id
#' - FIELD FORMAT: Number, 4 Characters (YYYY format)
#' - Number, 1 Character (T format)
#' - Varchar, 1 character (E format)
#' - DEFINITION: The current academic year, term, and extract in which course is offered.
#'               Format YYYYTE is made up of academic year YYYY, academic term T, and extract E (3rd week or End of Term).
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (season, academic_year_code, version_id).
#'
#'
#' @return Original data frame, with USHE data element gen_ushe_year_term_extract s_02, sc_02, c_02 appended.
#' @export
#'
#' @examples
#' gen_ushe_year_term_extract()
#'
gen_ushe_year_term_extract <- function(input_df=usheUtils::fake_student_df) {
  gen_ushe_year_term_extract <- NULL

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(year = case_when(
      season == "Summer" ~ as.character( (as.numeric(academic_year_code) + 1) ),
      season == "Fall" ~ as.character(academic_year_code),
      season == "Spring" ~ as.character( academic_year_code ),
      TRUE ~ " ")) %>%
    mutate(term = case_when(
      season == "Summer" ~ '1',
      season == "Fall" ~ '2',
      season == "Spring" ~ '3',
      TRUE ~ " ")) %>%
    mutate(extract = case_when(
      # 'C' for current
      version_id == '1' ~ 'C',
      # '3' for 3rd term
      version_id == '2' ~ '3',
      # 'E' for End of Term
      version_id == '3' ~ 'E',
      TRUE ~ " ")) %>%
    # Append USHE data element year_term_extract
    mutate(year_term_extract = paste(year, term, extract, sep= "|") ) %>%
    mutate(gen_ushe_year_term_extract = year_term_extract,
           s_02 = year_term_extract,
           c_02 = year_term_extract,
           sc_02 = year_term_extract)

  return(output_df)
}

#' @rdname gen_ushe_year_term_extract
#' @examples s_02()
#' @export
s_02 <- gen_ushe_year_term_extract

#' @rdname gen_ushe_year_term_extract
#' @examples c_02()
#' @export
c_02 <- gen_ushe_year_term_extract

#' @rdname gen_ushe_year_term_extract
#' @examples sc_02()
#' @export
sc_02 <- gen_ushe_year_term_extract

#' Calculate USHE Element (Degree Type)
#'
#' @details
#'
#' **USHE Documentation**
#' ELEMENT NAME: Degree Type
#' FIELD NAME: gen_ushe_deg_type, pf_deg_type & g_deg_type
#' FIELD FORMAT: Varchar, 6 Characters
#' DEFINITION: The Level of Degree or Certificate. Refer to the  Degree Type Table for all degrees.

#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @param input_df A Data Frame. Must contain the following data fields: (primary_degree_id).
#'
#'
#' @return Original data frame, with USHE data element gen_ushe_deg_type appended.
#' @export
#'
#' @examples
#' gen_ushe_deg_type()
#'
gen_ushe_deg_type <- function(input_df=usheUtils::fake_program_df) {
  gen_ushe_deg_type <- NULL

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( deg_type = if_else( primary_degree_id == "MMFT",
                                "MMF",
                                primary_degree_id ) ) %>%
    # Append USHE data element gen_ushe_deg_type
    mutate( gen_ushe_deg_type = deg_type,
            pf_05 = deg_type,
            g_10 = deg_type)

  return(output_df)
}

#' @rdname gen_ushe_deg_type
#' @examples pf_05()
#' @export
pf_05 <- gen_ushe_deg_type

#' @rdname gen_ushe_deg_type
#' @examples g_10()
#' @export
g_10 <- gen_ushe_deg_type

#' Calculate USHE Element (High School Codes)
#'
#' @details
#'
#' **USHE Documentation**
#' ELEMENT NAME: High School Codes
#' FIELD NAME: gen_ushe_high_school, g_high_school and s_high_school.
#' FIELD FORMAT: Varchar, 6 Characters
#' DEFINITION: The High School or Special Secondary School code which uniquely identifies each  student’s secondary institution.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr case_when
#'
#' @param input_df A Data Frame. Must contain the following data fields: (high_school_code).
#'
#'
#' @return Original data frame, with USHE data element gen_ushe_high_school appended.
#' @export
#'
#' @examples
#' gen_ushe_high_school()
#'
gen_ushe_high_school <- function(input_df=usheUtils::fake_student_df) {
  gen_ushe_high_school <- NULL
  high_school_code_intermediate <- NULL
  g_19 <- NULL
  s_28 <- NULL

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( high_school_code_intermediate = case_when(
      high_school_code ==  "CHSPE" ~ "459700",
      high_school_code ==  "459997" ~ "459100",
      high_school_code ==  "459992" ~ "459600",
      high_school_code ==  "459993" ~ "459050",
      high_school_code ==  "459995" ~ "459300",
      high_school_code ==  "960000" ~ "459400",
      high_school_code ==  "459999" ~ "459150",
      ( high_school_code %in% c("459994", "459996")
        | is.na(high_school_code) ) ~ "459200",
      high_school_code %in% c("459998", "969999") ~ "459500",
      TRUE ~ high_school_code) ) %>%
    # Append USHE data element gen_ushe_high_school
    mutate( gen_ushe_high_school = high_school_code_intermediate,
            g_19 = high_school_code_intermediate,
            s_28 = high_school_code_intermediate )

  return(output_df)
}

#' @rdname gen_ushe_high_school
#' @examples g_19()
#' @export
g_19 <- gen_ushe_high_school

#' @rdname gen_ushe_high_school
#' @examples s_28()
#' @export
s_28 <- gen_ushe_high_school

#' Calculate USHE Element (IPEDS Award Level Code  )
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: IPEDS Award Level Code / Degree Level (for the programs file).
#' - FIELD NAME: gen_ushe_ipeds, s_ipeds, pf_deg_level & g_ipeds
#' - FIELD FORMAT: Number, 2 Characters
#' - DEFINITION: The Award Level codes that correspond with the IPEDS Completion Survey.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (ipeds_award_level_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element gen_ushe_ipeds appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' gen_ushe_ipeds()
#'
gen_ushe_ipeds <- function(input_df=usheUtils::fake_graduation_df, with_intermediates=FALSE) {
  gen_ushe_ipeds <- NULL

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( deg_level = ipeds_award_level_code,
            ipeds = ipeds_award_level_code ) %>%
    # Append USHE data element gen_ushe_ipeds
    mutate( gen_ushe_ipeds = deg_level,
            s_19 = deg_level,
            pf_04 = deg_level,
            g_17 = deg_level )

  return(output_df)

}

#' @rdname gen_ushe_ipeds
#' @examples s_19()
#' @export
s_19 <- gen_ushe_ipeds

#' @rdname gen_ushe_ipeds
#' @examples pf_04()
#' @export
pf_04 <- gen_ushe_ipeds

#' @rdname gen_ushe_ipeds
#' @examples g_17()
#' @export
g_17 <- gen_ushe_ipeds

#' Calculate USHE Element (Term)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Term
#' - FIELD NAME: gen_ushe_term, g_term, f_term.
#' - FIELD FORMAT: Varchar, 1 Character
#' - DEFINITION: The term in which the student is evolved in either gradution or financial aid.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr case_when
#'
#' @param input_df A Data Frame. Must contain the following data fields: (season).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element gen_ushe_ipeds appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' gen_ushe_term()
#'
gen_ushe_term <- function(input_df=usheUtils::fake_graduation_df, with_intermediates=FALSE) {
  gen_ushe_term <- NULL

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(term = case_when(
      season == "Summer" ~ "1",
      season == "Fall" ~ "2",
      season == "Spring" ~ "3") )%>%
    # Append USHE data element gen_ushe_term
    mutate( gen_ushe_term = term,
            g_25 = term,
            f_03 = term)

  return(output_df)

}

#' @rdname gen_ushe_term
#' @examples g_25()
#' @export
g_25 <- gen_ushe_term

#' @rdname gen_ushe_term
#' @examples f_03()
#' @export
f_03 <- gen_ushe_term


#' Calculate USHE Element g_08 (Date of Graduation)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Date of Graduation
#' - FIELD NAME: gen_ushe_grad_date, g_date & d_start_dt
#' - FIELD FORMAT: Varchar, 8 Characters (YYYYMMDD Format)
#' - DEFINITION: The calendar date the formal award was conferred by the institution.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (graduation_date).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element g_08 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' gen_ushe_grad_date()
#'
gen_ushe_grad_date <- function(input_df=usheUtils::fake_graduation_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(graduation_date_intermediate = gsub("-", "", graduation_date) ) %>%
    # Append USHE data element gen_ushe_grad_date
    mutate( gen_ushe_grad_date = graduation_date_intermediate,
            g_08 = graduation_date_intermediate,
            d_start_dt = graduation_date_intermediate,
            d_04 = graduation_date_intermediate )

  return(output_df)
}

#' @rdname gen_ushe_grad_date
#' @examples g_08()
#' @export
g_08 <- gen_ushe_grad_date

#' @rdname gen_ushe_grad_date
#' @examples d_04()
#' @export
d_04 <- gen_ushe_grad_date
