#' Generate Space Inventory Building Submission File
#'
#' @param input_df A Data Frame. Must contain the following data fields: (building_location_code,
#'                                                                        ownership_code,
#'                                                                        submission_year,
#'                                                                        building_name,
#'                                                                        building_number,
#'                                                                        building_abbrv,
#'                                                                        building_construction_year,
#'                                                                        building_remodel_year,
#'                                                                        building_cost_replacement,
#'                                                                        building_condition_code,
#'                                                                        building_area_gross,
#'                                                                        building_cost_myr,
#'                                                                        building_number,
#'                                                                        building_auxiliary).
#'
#'
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return
#' A Data Frame, with all of the USHE elements required for upload submission.
#' This will also include intermediate values, used to calculate USHE data elements, if option is set.
#'
#' @examples
#' generate_space_inventory_building_submission_file()
#'
#' @export
#'
generate_space_inventory_building_submission_file <- function(input_df=usheUtils::fake_building_df, with_intermediates=FALSE) {

  ushe_data_elements <- c("b_01", "b_02", "b_03", "b_04",
                          "b_05", "b_06", "b_07", "b_08",
                          "b_09", "b_10", "b_11", "b_12",
                          "b_13", "b_14", "b_15")

  output_df <- input_df %>%
    b_01() %>%
    b_02() %>%
    b_03() %>%
    b_04() %>%
    b_05() %>%
    b_06() %>%
    b_07() %>%
    b_08() %>%
    b_09() %>%
    b_10() %>%
    b_11() %>%
    b_12() %>%
    b_13() %>%
    b_14() %>%
    b_15()


  if (!with_intermediates) {
    output_df <- output_df %>%
      dplyr::select( ushe_data_elements )
  }

  return(output_df)
}

#' Calculate USHE Element b_02 (Building Location Code)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Building Location Code
#' - FIELD NAME: b_location
#' - FIELD FORMAT: Varchar, 3 Characters
#' - DEFINITION: This field has been established to identify the campus location or other site  where the space is located.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (building_location_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element b_02 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' b_02()
#'
b_02 <- function(input_df=usheUtils::fake_building_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( b_location = building_location_code ) %>%
    # Append USHE data element b_02
    mutate( b_02 =  b_location  )

  return(output_df)
}

#' Calculate USHE Element b_03 (Building Ownership Code)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Building Ownership Status
#' - FIELD NAME: b_ownership
#' - FIELD FORMAT: Varchar, 1 Character
#' - DEFINITION: This field has been established to identify the type of ownership of space included in the inventory including space that is not owned by the institution but leased for its use from other parties.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (building_ownership_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element b_03 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' b_03()
#'
b_03 <- function(input_df=usheUtils::fake_building_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( b_ownership = building_ownership_code ) %>%
    # Append USHE data element b_03
    mutate( b_03 =  b_ownership  )

  return(output_df)
}

#' Calculate USHE Element b_04 (Year Submission)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Submission Year
#' - FIELD NAME: b_year
#' - FIELD FORMAT: Varchar, 4 Characters (YYYY format)
#' - DEFINITION:The fiscal year in which the Space Inventory - Building inventory was conducted.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (submission_year).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element b_04 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' b_04()
#'
b_04 <- function(input_df=usheUtils::fake_building_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( b_year = gsub("-", "", submission_year) ) %>%
    # Append USHE data element b_04
    mutate( b_04 =  b_year  )

  return(output_df)
}

#' Calculate USHE Element b_05 (Building Name)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Building Name
#' - FIELD NAME: b_name
#' - FIELD FORMAT: Varchar, 255 Characters
#' - DEFINITION: A unique identifier assigned by the institution to the specific building.

#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (building_name).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element b_05 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' b_05()
#'
b_05 <- function(input_df=usheUtils::fake_building_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( b_name = building_name ) %>%
    # Append USHE data element b_05
    mutate( b_05 =  b_name  )

  return(output_df)
}

#' Calculate USHE Element b_06 (Building Name)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Building Number
#' - FIELD NAME: b_number
#' - FIELD FORMAT: Varchar, 7 Characters
#' - DEFINITION: A unique identifier assigned by the institution for the building where the room is located.

#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (building_number).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element b_06 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' b_06()
#'
b_06 <- function(input_df=usheUtils::fake_building_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( b_number = building_number ) %>%
    # Append USHE data element b_06
    mutate( b_06 =  b_number  )

  return(output_df)
}

#' Calculate USHE Element b_07 (Building Abbreviation)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Building Abbreviation
#' - FIELD NAME: b_sname
#' - FIELD FORMAT: Varchar, 6 Characters
#' - DEFINITION: A unique institutionally given building abbreviation for the unique building name on your campus

#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (building_abbrv).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element b_07 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' b_07()
#'
b_07 <- function(input_df=usheUtils::fake_building_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( b_sname = building_abbrv ) %>%
    # Append USHE data element b_07
    mutate( b_07 =  b_sname  )

  return(output_df)
}

#' Calculate USHE Element b_08 (Year Building Constructed)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Year Building Constructed
#' - FIELD NAME: b_year_cons
#' - FIELD FORMAT: Varchar, 4 Characters
#' - DEFINITION: Year Building Constructed.

#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (building_construction_year).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element b_08 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' b_08()
#'
b_08 <- function(input_df=usheUtils::fake_building_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( b_year_cons = building_construction_year ) %>%
    # Append USHE data element b_08
    mutate( b_08 =  b_year_cons  )

  return(output_df)
}

#' Calculate USHE Element b_09 (Year of Latest Major Remodeling)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Year of Latest Major Remodeling
#' - FIELD NAME: b_year_rem
#' - FIELD FORMAT: Varchar, 4 Characters
#' - DEFINITION: The year of the most recent major renovation that brought the entire building to current codes and resulted in building systems (i.e. HVAC, electrical, plumbing, structural, envelope) being in good condition.


#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (building_remodel_year).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element b_09 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' b_09()
#'
b_09 <- function(input_df=usheUtils::fake_building_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( b_year_rem = building_remodel_year ) %>%
    # Append USHE data element b_09
    mutate( b_09 =  b_year_rem  )

  return(output_df)
}

#' Calculate USHE Element b_10 (Replacement Cost)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Replacement Cost
#' - FIELD NAME: b_replace_cost
#' - FIELD FORMAT: Numeric (12,2) Characters. Two trailing 0’s added to reflect implied decimal places.
#' - DEFINITION: The cost to replace the building as reported and supported by Risk Management. We need to have trailing "00"s.
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (building_cost_replacement).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element b_10 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' b_10()
#'
b_10 <- function(input_df=usheUtils::fake_building_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( b_replace_cost = building_cost_replacement) %>%
    # Append USHE data element b_10
    mutate( b_10 =  b_replace_cost  )

  return(output_df)
}

#' Calculate USHE Element b_11 (Building Condition)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Building Condition
#' - FIELD NAME: b_condition
#' - FIELD FORMAT: Numeric 1 Character.
#' - DEFINITION: Indicates the physical status of the building included in the inventory, 5 - 1 (good - unsatisfactory)
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (building_condition_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element b_11 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' b_11()
#'
b_11 <- function(input_df=usheUtils::fake_building_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( b_condition = building_condition_code ) %>%
    # Append USHE data element b_11
    mutate( b_11 =  b_condition  )

  return(output_df)
}

#' Calculate USHE Element b_12 (Gross Area)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Gross Area
#' - FIELD NAME: b_gross
#' - FIELD FORMAT:  Numeric (10, 0) Characters.
#' - DEFINITION: The total floor area of the structure within the outside faces of the exterior walls. B_GROSS must be greater than the sum of R_AREA for the relevant B_NUMBER. This is required for buildings which are owned.
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (building_area_gross).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element b_12 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' b_12()
#'
b_12 <- function(input_df=usheUtils::fake_building_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( b_gross = building_area_gross ) %>%
    # Append USHE data element b_12
    mutate( b_12 =  b_gross  )

  return(output_df)
}

#' Calculate USHE Element b_13 (Cost of Operating the Building)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Cost of Operating the Building
#' - FIELD NAME: b_cost_myr
#' - FIELD FORMAT:  Numeric (8, 2) Characters.
#' - DEFINITION:
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (building_cost_myr).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element b_13 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' b_13()
#'
b_13 <- function(input_df=usheUtils::fake_building_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( b_cost_myr = building_cost_myr ) %>%
    # Append USHE data element b_13
    mutate( b_13 =  b_cost_myr  )

  return(output_df)
}

#' Calculate USHE Element b_14 (Building Risk Management Number)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Building Risk Management Number
#' - FIELD NAME: b_rsknbr
#' - FIELD FORMAT:  Numeric 5 Characters.
#' - DEFINITION: A unique identifier assigned by State Risk Management for the building where the room is located. At some institutions, this number and B_NUMBER are the same value.
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (building_number).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element b_14 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' b_14()
#'
b_14 <- function(input_df=usheUtils::fake_building_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( b_rsknbr = building_number ) %>%
    # Append USHE data element b_14
    mutate( b_14 =  b_rsknbr )

  return(output_df)
}

#' Calculate USHE Element b_15 (Auxiliary Building)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Auxiliary Building
#' - FIELD NAME: b_aux
#' - FIELD FORMAT:  Numeric 1 Character.
#' - DEFINITION: Business space or other support facilities (as distinguished from primary programs of  instruction, research, and public service, and from organized activities and intercollegiate athletics)
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @param input_df A Data Frame. Must contain the following data fields: (building_auxiliary).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element b_15 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' b_15()
#'
b_15 <- function(input_df=usheUtils::fake_building_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( b_aux = if_else(building_auxiliary == "Y", "A", building_auxiliary) ) %>%
    # Append USHE data element b_15
    mutate( b_15 =  b_aux )

  return(output_df)
}
