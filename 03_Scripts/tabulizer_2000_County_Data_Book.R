################################################################################
## Auhtor: Alexander Vogt
## Content of the code:
## The Callaway & Sant'Anna (2021) uses the pre-treatment county information 
## from the 2000 County Data Book. Unfortunately, the County Data Book is only 
## available as Pdf. Thus, the goal of this code script is too digitalize the
## the relevant pre-treatment information on education, poverty, population and
## race in order to have the relevant information to reproduce Callaway &
## Sant'Anna (2021).
################################################################################


rm(list = ls(all = TRUE))
library(tabulizer)
library(rJava)
library(tidyverse)
library(styler)
library(lintr)

################################################################################
## 1. Step: Reading in the pdf-tables & Creating a dataframe from the ouput of 
##          extract_tables (is a list)
################################################################################

# ---- File - Paths ------------------------------------------------------------

data_path <- "01_Data"
table_path <- "02_Tables"

# ---- Extract Pdf-Tables ------------------------------------------------------

# Data on Education and Poverty Information
extr_educ_pov <- extract_tables(file = paste0("./", data_path,"/", 
                                              "2000 County Data Book.pdf"),
                                output = "data.frame",
                                area = list(c(107, 27, 632, 592)),
                                columns = list(c(117, 156, 195, 234, 276, 302, 
                                                 327, 358, 388, 414, 453, 488, 
                                                 527, 553, 579)),
                                guess = FALSE,
                                pages = c(223:270)
                                )


# Data on Size of the Population
extr_pop      <- extract_tables(file = paste0("./", data_path,"/", 
                                              "2000 County Data Book.pdf"),
                                output = "data.frame",
                                area = list(c(100, 84, 618, 578)),
                                columns = list(c(164, 199, 240, 262, 290, 332, 354,
                                                 395, 433, 470, 495, 519, 554)),
                                guess = FALSE,
                                pages = c(31:78)
                                )

# Data on Race and Age Distribution
extr_race     <- extract_tables(file = paste0("./", data_path,"/", 
                                          "2000 County Data Book.pdf"),
                               output = "data.frame",
                               area = list(c(95, 34, 625, 581)),
                               columns = list(c(117, 138, 158, 178, 198, 218, 238,
                                               257, 276, 300, 327, 368, 405, 439,
                                               476, 508, 545, 581)),
                               guess = FALSE,
                               pages = c(79:126)
                               )


# ---- Define vectors ----------------------------------------------------------

# Vector of column names for the dataframe of Educ & Pov
column_names_educ_pov <- c("county", "PSE_FALL_1998_99", "PSE_FALL_1994_95", "PSE_1990", 
                  "Educ_Attain1990_NR_>24", "Educ_Attain1990_HS_Perc", 
                  "Educ_Attain1990_BAorGreater", "Median_Inc_1997_USD",
                  "Median_Inc_1998_USD", "Median_Inc_DeltaPerc_97_98",
                  "Pov97_Tot", "Pov97_Delta_97_98", "Pov97_<18",
                  "Pov97_Perc_AllAge", "Pov97_Perc_<18")

# Vector of colum names for the dataframe, which contains information about Pop
column_names_pop <- c("county", "land_area_2000", "pop_nr_2000", "pop_rank_2000",
                      "pop_per_sqm_2000", "pop_nr_1990", "pop_rank_1990",
                      "pop_nr_1980", "delta_abs_pop_1990_00", "delta_abs_pop_1980_90",
                      "delta_perc_pop_1990_00", "delta_perc_pop_1980_90",
                      "hispanic_pop_2000", "hispanic_pop_2000_perc" )


# Vector of colum names for the dataframe, which contains information about Race
column_names_race <- c("county", "perc_ybelow_5y", "perc_5to17y", "perc_16to24y",
                       "perc_24to44y", "perc_45to64y", "perc_65to74y",
                       "perc_75to84y", "perc_above_85y", "median_age", 
                       "males_per_100_women", "nr_white_2000", "nr_black_2000",
                       "nr_native_american_2000", "nr_asian_2000", 
                       "nr_native_hawaiian_2000", "nr_other_race_2000",
                       "nr_two_or_more_race_2000")

# Vector of names of states in the US
state_names <- c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", 
                "CONNECTICUT", "DELAWARE", "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", 
                "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", 
                "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", 
                "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", 
                "NEW_HAMPSHIRE", "NEW_JERSEY", "NEW_MEXICO", "NEW_YORK", 
                "NORTH_CAROLINA", "NORTH_DAKOTA", "OHIO", "OKLAHOMA", "OREGON", 
                "PENNSYLVANIA", "RHODE_ISLAND", "SOUTH_CAROLINA", "SOUTH_DAKOTA", 
                "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", 
                "WEST_VIRGINIA", "WISCONSIN", "WYOMING", "DISTRICT_OF_COLUMBIA",
                "Independent_City", "Independent_Cities")

length(extr_educ_pov)
length(extr_pop)
length(extr_race)
# ---- Function to create data frame -------------------------------------------

creating_dataframe <- function(data, column_names) {
  
  # Assuming data is a list of data frames
  number_of_lists <- length(data)
  
  for (i in 1:number_of_lists) {
    
    # Getting the name of the first column
    first_col_name <- names(data[[i]])[1]
    
    # Process each data frame
    data[[i]] <- data[[i]] %>%
      # mutate(
      #   !!first_col_name := na_if(.data[[first_col_name]], ""),
      #   !!first_col_name := na_if(.data[[first_col_name]], "County"),
      #   !!first_col_name := na_if(.data[[first_col_name]], "UNITED STATES......")
      # ) %>%
      # drop_na(!!sym(first_col_name)) %>%
      rename_with(~ column_names, .cols = everything())
  }
  
  # Bind all data frames into a single data frame
  data_final <- bind_rows(data)
  
  return(data_final)
}

# ---- List to complete data frame ---------------------------------------------

# Dataframe for Educ & Pov
data_educ_pov <- creating_dataframe(extr_educ_pov, column_names_educ_pov)
  
# Dataframe for Population
data_pop <- creating_dataframe(extr_pop, column_names_pop)
  
# Dataframe for Race
data_race <- creating_dataframe(extr_race, column_names_race)



################################################################################
# 2. Step: Cleaning the Data frames
################################################################################

################################################################################
# 2.1 Step: Cleaning Education and Poverty Data
################################################################################

# ---- Clean County Variable --------------------------------------------------

# Clean the county variable in order to be able to create the state variable

data_educ_pov_final <- data_educ_pov |>
  mutate(county = str_replace_all(county, fixed("."), ""), 
         county = str_remove(county, "\\s+$"),
         county = str_replace_all(county, fixed(" "), "_"),
         county = str_replace_all(county, fixed("~"), "_"),
         county = str_replace_all(county, fixed("'"), ""),
         county = str_replace_all(county, fixed("’"), "")) |> 
  filter(!grepl("Revised", county, ignore.case = TRUE)) |>
  filter(!str_detect(county, "censusgov")) |>
  filter(!str_detect(county, fixed("Sources:"))) |> 
  mutate(county = str_remove_all(county, regex("mCon", ignore_case = FALSE)))
  

# Create the state variable as there several counties with the same name but
# located in different counties

data_educ_pov_final <- data_educ_pov_final |>
  mutate(state = map_chr(county, ~{
    detected_state <- NA_character_  # Default to NA
    for (state in state_names) {
      if (str_detect(.x, state)) {
        detected_state <- state
        break  # Stop at the first match
      }
    }
    detected_state
  })) |>
  fill(state, .direction = "down") |> 
  mutate(state = str_replace_all(state, "_", " ")) |>
  mutate(state = str_to_title(state)) |>
  mutate(state = str_replace_all(state, " ", "_")) |> 
  mutate(state = str_replace(state, "Independent_Cities", "Independent_City")) |>
  filter(!county %in% state_names)
  


# Find the counties which names are two rows long in order to delete empty rows

data_educ_pov_final <- data_educ_pov_final |>
  relocate(ncol(data_educ_pov_final), .after = 1) |>
  # There are two counties name which are written over two rows - Correct this 
  # By using relative expressions and rename the county to their correct county
  # names.
  mutate(county = case_when(
    # Check if the previous row in 'county' ends with "_"
    lag(str_ends(county, "_"), default = FALSE) ~ "Prince_of_Wales_Outer_Ketchikan",
    # Otherwise, keep the original 'county' value
    TRUE ~ county
  )) |>
  filter(!str_ends(county, "_")) |>
  mutate( county = case_when(
    lag(str_detect(county, "Yellowstone_National"), default = FALSE) ~ "Yellowstone_National_Park",
    TRUE ~ county
  )) |>
  filter(!county == "Yellowstone_National")

# Continue to filter more unwanted row based on string-patterns 
data_educ_pov_final <- data_educ_pov_final |>
  filter(!str_detect(county, regex("School_Enrollment", ignore_case = FALSE))) |>
  filter(!str_detect(county, regex("homerssdcensusgov", ignore_case = FALSE))) |>
  filter(!str_detect(county, regex("–Con", ignore_case = FALSE))) 


# ---- Clean the rest of the variables -----------------------------------------

# General cleaning of all variables except of the first two columns
# Replacing all footnotes, empty space, incorrect NA (aka X) and unrecognized 
# minus

data_educ_pov_final <- data_educ_pov_final |>
    mutate(across(-1:-2, ~str_remove_all(.x, "\\s")))|>
    mutate(across(-1:-2, ~na_if(.x, "X"))) |>
    mutate(across(-1:-2, ~str_replace_all(.x, "–", "-"))) |>
    mutate(across(-1:-2, ~str_replace_all(.x, "\\(|\\)", NA_character_))) |>
    mutate(across(-c(1:2), ~if_else(.x == "-", NA_character_, .x)))


# Substitute elements with footnotes at the beginning of the string, due to
# incorrect tabulizer scraping

data_educ_pov_final <-  data_educ_pov_final |>
  mutate(across(c(3:8, 10), ~if_else(county == "Yukon_Koyukuk", str_sub(.x, 2), .x))) |>
  mutate(across(3:4, ~if_else(county == "Kings", str_sub(.x, 2), .x))) |>
  mutate(across(3:4, ~if_else(county == "Alleghany", str_sub(.x, 2), .x))) |>
  mutate(across(3:4, ~if_else(county == "Bedford", str_sub(.x, 2), .x))) |>
  mutate(across(3:4, ~if_else(county == "Fairfax", str_sub(.x, 2), .x))) |>
  mutate(across(3:4, ~if_else(county == "Greensville", str_sub(.x, 2), .x))) |>
  mutate(across(10,  ~if_else(county == "Halifax", str_sub(.x, 2), .x))) |>
  mutate(across(10,  ~if_else(county == "James_City", str_sub(.x, 2), .x)))

# ---- Format Dataframe and Save as RDS ----------------------------------------

data_educ_pov_final <- data_educ_pov_final |>
  as_tibble() |> 
  mutate(across(-1:-2, as.double))

saveRDS(data_educ_pov_final, paste0("./", data_path, "/", "educ_pov_data.rds"))

################################################################################
# 2.2 Step: Cleaning Education and Poverty Data
################################################################################


# Drop NAs in county
data_pop_final <- data_pop |>
  mutate(county = na_if(county, ""),
         county = str_replace_all(county, "UNITED STATES", NA_character_)) |>
  drop_na(county)

# Clean county-variable from non-regular signs & non-relevant string patterns
data_pop_final <- data_pop_final |>
  mutate(county = str_replace_all(county, fixed("."), ""), 
         county = str_remove(county, "\\s+$"),
         county = str_replace_all(county, fixed(" "), "_"),
         county = str_replace_all(county, fixed("~"), "_"),
         county = str_replace_all(county, fixed("'"), ""),
         county = str_replace_all(county, fixed("’"), "")) |>
  mutate(county = str_remove_all(county, regex("mCon", ignore_case = FALSE)),
         county = str_remove_all(county, regex("–Con")))

# Filter row with footnote informations
data_pov_final <- data_pop_final |>
  filter(!str_detect(county, "Processing")) |>
  filter(!str_detect(county, "covered")) |>
  filter(!str_detect(county, "share")) |>
  filter(!str_detect(county, "legally")) |>
  filter(!str_detect(county, "Census")) 

# Create State Variable in order to create a key consisting out of county & state

#data_pov_final <-
data_pov_final |>
  mutate(state = map_chr(county, ~{
    detected_state <- NA_character_  # Default to NA
    for (state in state_names) {
      if (str_detect(.x, state)) {
        detected_state <- state
        break  # Stop at the first match
      }
    }
    detected_state
  })) |> 
  fill(state, .direction = "down") |>
  mutate(state = str_replace_all(state, "_", " ")) |>
  mutate(state = str_to_title(state)) |>
  mutate(state = str_replace_all(state, " ", "_")) |> 
  mutate(state = str_replace(state, "Independent_Cities", "Independent_City")) |>
  filter(!county %in% state_names) |> View()
