rm(list = ls())
library(tabulizer)
library(rJava)
library(tidyverse)
library(styler)
library(lintr)
data_path <- "01_Data"
table_path <- "02_Tables"

data_educ_pov <- extract_tables(file = paste0("./", data_path,"/", "2000 County Data Book.pdf"),
                       output = "data.frame",
                       area = list(c(107, 27, 632, 592)),
                       columns = list(c(117, 156, 195, 234, 276, 302, 327, 358,
                                        388, 414, 453, 488, 527, 553, 579)),
                       guess = FALSE,
                       pages = c(223:270)
                       )


# Vector of column names for the df of 
column_names <- c("county", "PSE_FALL_1998_99", "PSE_FALL_1994_95", "PSE_1990", 
                  "Educ_Attain1990_NR_>24", "Educ_Attain1990_HS_Perc", 
                  "Educ_Attain1990_BAorGreater", "Median_Inc_1997_USD",
                  "Median_Inc_1998_USD", "Median_Inc_DeltaPerc_97_98",
                  "Pov97_Tot", "Pov97_Delta_97_98", "Pov97_<18",
                  "Pov97_Perc_AllAge", "Pov97_Perc_<18")

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


# calculate the number of elemtns in list
number_of_lists <- sum(sapply(data_educ_pov, is.list))

# delete empty rows and change column names
for (i in 1:number_of_lists) {

  data_educ_pov[[i]] <- data_educ_pov[[i]] |>
     mutate(X = na_if(X, ""),
            X = na_if(X, "County"),
            X = na_if(X, "UNITED STATES......")) |>
     drop_na(X)

  # if (is.na(data_educ_pov[[1]][1 ,"X.1"])) {
  #   data_educ_pov[[1]] <- data_educ_pov[[1]] |> select(-X.1)
  # }
  
  data_educ_pov[[i]] <- data_educ_pov[[i]] |>
     rename_with(~ column_names, .col = everything() )

}

data_educ_pov_final <- data_educ_pov[[1]]

# creating final dataset over all counties in the US (no data cleaning so far)
for (j in 2:number_of_lists) {
  
  data_educ_pov[[j]] <- data_educ_pov[[j]] |>
    as.data.frame()
  
  data_educ_pov_final <- bind_rows(data_educ_pov_final, data_educ_pov[[j]])
  
}
  


# ---- Clean County Variable --------------------------------------------------

# Clean the county variable in order to be able to create the state variable

data_educ_pov_final <- data_educ_pov_final |>
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


## ---- Clean the rest of the variables ----------------------------------------

# General cleaning of all variables except of the first two columns
# Replacing all footnotes, empty space, incorrect NA (aka X) and unrecognized minus

data_educ_pov_final <- data_educ_pov_final |>
    mutate(across(-1:-2, ~str_remove_all(.x, "\\s")),
           across(-1:-2, ~na_if(.x, "X")),
           across(-1:-2, ~str_replace_all(.x, "–", "-")),
           across(-1:-2, ~str_replace_all(.x, "\\(|\\)", NA_character_)))


# Substitute elements with footnotes at the beginning of the string, due to
# incorrect tabulizer scraping

# data_educ_pov_final <- 
data_educ_pov_final |>
  mutate(across(c(3:8, 10), ~if_else(county == "Yukon_Koyukuk", str_sub(.x, 2), .x))) |>
  mutate(across(3:4, ~if_else(county == "Kings", str_sub(.x, 2), .x))) |> View()
  mutate(across(3:4, ~if_else(county == "Alleghany", str_sub(.x, 2), .x))) |>
  mutate(across(3:4, ~if_else(county == "Bedford", str_sub(.x, 2), .x))) |>
  mutate(across(3:4, ~if_else(county == "Fairfax", str_sub(.x, 2), .x))) |>
  mutate(across(3:4, ~if_else(county == "Greensville", str_sub(.x, 2), .x))) |>
  mutate(across(10,  ~if_else(county == "Halifax", str_sub(.x, 2), .x))) |>
  mutate(across(10,  ~if_else(county == "James_City", str_sub(.x, 2), .x))) |> View()


         