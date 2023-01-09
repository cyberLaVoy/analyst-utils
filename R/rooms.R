#' Generate Space Inventory Rooms Submission File
#'
#' @param input_df A Data Frame. Must contain the following data fields: (submission_year,
#'                                                                        building_number,
#'                                                                        room_number,
#'                                                                        room_group1_code,
#'                                                                        room_use_code_group,
#'                                                                        room_use_code,
#'                                                                        room_name,
#'                                                                        room_stations,
#'                                                                        room_area,
#'                                                                        room_disabled_access,
#'                                                                        room_prorated,
#'                                                                        room_prorated_area,
#'                                                                        room_activity_date).
#'
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return
#' A Data Frame, with all of the USHE elements required for upload submission.
#' This will also include intermediate values, used to calculate USHE data elements, if option is set.
#'
#' @examples
#' generate_space_inventory_rooms_submission_file()
#'
#' @export
#'
generate_space_inventory_rooms_submission_file <- function(input_df=usheUtils::fake_rooms_df, with_intermediates=FALSE) {

  ushe_data_elements <- c("r_01", "r_02", "r_03", "r_04",
                          "r_05", "r_06", "r_07", "r_08",
                          "r_09", "r_10", "r_11", "r_12",
                          "r_13", "r_14", "r_15")

  output_df <- input_df %>%
    r_01() %>%
    r_02() %>%
    r_03() %>%
    r_04() %>%
    r_05() %>%
    r_06() %>%
    r_07() %>%
    r_08() %>%
    r_09() %>%
    r_10() %>%
    r_11() %>%
    r_12() %>%
    r_13() %>%
    r_14() %>%
    r_15()


  if (!with_intermediates) {
    output_df <- output_df %>%
      dplyr::select( ushe_data_elements )
  }

  return(output_df)
}

#' Calculate USHE Element r_02 (Year Submission)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Submission Year
#' - FIELD NAME: r_year
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
#' @return Original data frame, with USHE data element r_02 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' r_02()
#'
r_02 <- function(input_df=usheUtils::fake_building_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( r_year = gsub("-", "", submission_year) ) %>%
    # Append USHE data element r_02
    mutate( r_02 =  r_year  )

  return(output_df)
}

#' Calculate USHE Element r_03 (Building Number )
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Building Number
#' - FIELD NAME: r_build_number
#' - FIELD FORMAT: Varchar, 7 Characters
#' - DEFINITION: A unique identifier assigned by the institution for the building where the room  is located. For rooms assigned as classroom space, this should match C_BLDG_NUM, C_BLDG_NUM2, or C_BLDG_NUM3 in the Courses file for student enrollments.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (building_number).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element r_03 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' r_03()
#'
r_03 <- function(input_df=usheUtils::fake_rooms_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( r_build_number = building_number ) %>%
    # Append USHE data element r_03
    mutate( r_03 =  r_build_number  )

  return(output_df)
}

#' Calculate USHE Element r_04 (Room Number )
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Room Number
#' - FIELD NAME: r_number
#' - FIELD FORMAT: Varchar, 30 Characters
#' - DEFINITION: A unique institutionally given room number which will assist the institution in determining the location and possible use of a room.

#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (room_number).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element r_03 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' r_04()
#'
r_04 <- function(input_df=usheUtils::fake_rooms_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( r_number = room_number ) %>%
    # Append USHE data element r_04
    mutate( r_04 =  r_number  )

  return(output_df)
}

#' Calculate USHE Element r_05 (Room Suffix )
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Room Suffix
#' - FIELD NAME: r_suffix
#' - FIELD FORMAT: Varchar, 10 Characters
#' - DEFINITION: A unique institutionally given suffix to room’s coding structure to define subset rooms in a suite or defined area of use.

#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: ().
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element r_03 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' r_05()
#'
r_05 <- function(input_df=usheUtils::fake_rooms_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( r_suffix = NA_character_ ) %>%
    # Append USHE data element r_05
    mutate( r_05 =  r_suffix  )

  return(output_df)
}

#' Calculate USHE Element r_06 (Room Grouping Codes)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Room Grouping Codes
#' - FIELD NAME: r_group1
#' - FIELD FORMAT: Varchar, 1 Character
#' - DEFINITION: These space categories have been established to assist directly with the CDP Process of capital facilities needs in the Utah System of Higher Education.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (room_group1_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element r_03 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' r_06()
#'
r_06 <- function(input_df=usheUtils::fake_rooms_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( r_group1 = room_group1_code) %>%
    # Append USHE data element r_06
    mutate( r_06 =  r_group1  )

  return(output_df)
}

#' Calculate USHE Element r_07 (Category of Room Use )
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Category of room use
#' - FIELD NAME: r_use_category
#' - FIELD FORMAT: Varchar, 3 Characters
#' - DEFINITION: This field identifies which category of use a room is assigned.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (room_use_code_group).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element r_03 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' r_07()
#'
r_07 <- function(input_df=usheUtils::fake_rooms_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( r_use_category = room_use_code_group) %>%
    # Append USHE data element r_07
    mutate( r_07 =  r_use_category  )

  return(output_df)
}

#' Calculate USHE Element r_08 (Room Use Codes)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Room Use Codes
#' - FIELD NAME: r_use_code
#' - FIELD FORMAT: Varchar, 3 Characters
#' - DEFINITION: Room use classification codes. Service codes are used for associated support rooms.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (room_use_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element r_03 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' r_08()
#'
r_08 <- function(input_df=usheUtils::fake_rooms_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( r_use_code = room_use_code) %>%
    # Append USHE data element r_08
    mutate( r_08 =  r_use_code  )

  return(output_df)
}

#' Calculate USHE Element r_09 (Room Name)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Room Name
#' - FIELD NAME: r_name
#' - FIELD FORMAT: Varchar, 255 Characters
#' - DEFINITION: The name commonly given to the room. This is the locally used name for the room rather than the name of the room use category.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (room_name).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element r_03 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' r_09()
#'
r_09 <- function(input_df=usheUtils::fake_rooms_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( r_name = room_name) %>%
    # Append USHE data element r_09
    mutate( r_09 =  r_name  )

  return(output_df)
}

#' Calculate USHE Element r_10 (Number of Seats or Stations in the Space)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Number of Seats or Stations in the Space
#' - FIELD NAME: r_stations
#' - FIELD FORMAT:  Numeric (5, 0) Characters,
#' - DEFINITION:  Identified the capacity of the room for selected room use categories where  information about capacity (number of workstations, seats, or beds, for example)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (room_stations).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element r_03 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' r_10()
#'
r_10 <- function(input_df=usheUtils::fake_rooms_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( r_stations = room_stations) %>%
    # Append USHE data element r_10
    mutate( r_10 =  r_stations  )

  return(output_df)
}

#' Calculate USHE Element r_11 (Assignable Area of the Room)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Assignable Area of the Room
#' - FIELD NAME: r_area
#' - FIELD FORMAT:  Numeric (10, 0) Characters,
#' - DEFINITION:The assignable floor area of the room, measured in assignable square feet. This is the total floor area of the room available
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (room_area).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element r_03 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' r_11()
#'
r_11 <- function(input_df=usheUtils::fake_rooms_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( r_area = room_area) %>%
    # Append USHE data element r_11
    mutate( r_11 =  r_area  )

  return(output_df)
}

#' Calculate USHE Element r_12 (Disabled Access to Room)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Disabled Access to Room
#' - FIELD NAME: r_disab_acc
#' - FIELD FORMAT: Varchar, 1 Characters
#' - DEFINITION: Indicates whether the room is barrier-free for its assigned use.
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (room_disabled_access).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element r_03 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' r_12()
#'
r_12 <- function(input_df=usheUtils::fake_rooms_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( r_disab_acc = room_disabled_access) %>%
    # Append USHE data element r_12
    mutate( r_12 =  r_disab_acc )

  return(output_df)
}

#' Calculate USHE Element r_13 (Proration)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Proration
#' - FIELD NAME: r_proration
#' - FIELD FORMAT: Varchar, 1 Characters
#' - DEFINITION: It is recommended that a room’s use, function, and organizational unit normally be coded on the basis of a single, primary classification
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (room_prorated).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element r_03 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' r_13()
#'
r_13 <- function(input_df=usheUtils::fake_rooms_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( r_proration = room_prorated ) %>%
    # Append USHE data element r_13
    mutate( r_13 =  r_proration )

  return(output_df)
}

#' Calculate USHE Element r_14 (Room’s Prorated Area)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Room’s Prorated Area
#' - FIELD NAME: r_prorated_area
#' - FIELD FORMAT: Numeric, (6, 0) Characters
#' - DEFINITION: Indicates the room’s prorated square footage.
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (room_prorated_area).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element r_03 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' r_14()
#'
r_14 <- function(input_df=usheUtils::fake_rooms_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( r_prorated_area = room_prorated_area ) %>%
    # Append USHE data element r_14
    mutate( r_14 =  r_prorated_area )

  return(output_df)
}

#' Calculate USHE Element r_15 (Data Updated Date)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Data Updated Date
#' - FIELD NAME: r_update_date
#' - FIELD FORMAT: Varchar, 8 Characters (YYYYMMDD)
#' - DEFINITION: he date in which any of the data for the particular room was updated.
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (room_activity_date).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element r_03 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' r_15()
#'
r_15 <- function(input_df=usheUtils::fake_rooms_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( r_update_date = gsub("-", "", room_activity_date ) ) %>%
    # Append USHE data element r_15
    mutate( r_15 =  r_update_date )

  return(output_df)
}
