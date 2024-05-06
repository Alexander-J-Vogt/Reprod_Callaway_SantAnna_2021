rm(list = ls(all = TRUE))

library(tidyverse)
library(haven)
library(plm)
# library(httr)

data_path <- "01_Data"
table_path <- "02_Tables"

# Import data set of QWI for the period 1995 to 2010
qwi_po_raw <- read_csv(paste0("./", data_path, "/", "qwi_all_privatownership.csv"))

# Import the cleaned "2000 County Data Book" dataset
cdb_2000 <- read_rds(paste0("./", data_path, "/", "county_data_book_2000.rds"))

# Identification of Region and State according to US Bureau of Labor Statistics
region_abbreviations <- c(
  AL = "South", AK = "West", AZ = "West", AR = "South", 
  CA = "West", CO = "West", CT = "Northeast", 
  DE = "South", FL = "South", GA = "South", HI = "West", 
  ID = "West", IL = "Midwest", IN = "Midwest", IA = "Midwest", 
  KS = "Midwest", KY = "South", LA = "South", ME = "Northeast", 
  MD = "South", MA = "Northeast", MI = "Midwest", 
  MN = "Midwest", MS = "South", MO = "Midwest", 
  MT = "West", NE = "Midwest", NV = "West", NH = "Northeast", 
  NJ = "Northeast", NM = "West", NY = "Northeast", 
  NC = "South", ND = "Midwest", OH = "Midwest", 
  OK = "South", OR = "West", PA = "Northeast", 
  RI = "Northeast", SC = "South", SD = "Midwest", 
  TN = "South", TX = "South", UT = "West", VT = "Northeast", 
  VA = "South", WA = "West", WV = "South", 
  WI = "Midwest", WY = "West"
)


treated_general <- c("AZ", "AK", "CO", "FL", "IL", "ME", "MD", "MI", "MN", "MO",
                     "MT", "NV", "NM", "NC", "OH", "PA", "WV", "I")

above_fed_mw <- c("AK", "CT", "HI", "MA", "OR", "VT")

# Select the relevant pre-treatment county characteristics

cdb_2000_restricted <- cdb_2000 |>
  select(county,
         state,
         state_abbrev,
         pop_nr_2000, # Total Population of each county in 2000
         nr_white_2000, # Total White Population for each county in year 2000
         Median_Inc_1997_USD, # Median Income (USD) for each county in 1997 
         Educ_Attain1990_HS_Perc, # Fraction of High-School graduates or a higher degree in 1990 
         Pov97_Perc_AllAge # Fraction of persons of all ages below poverty level in 1997
         ) |>
  mutate(white_pop_2000_perc = nr_white_2000 / pop_nr_2000,
         pop_2000_nr_1000s = pop_nr_2000 / 1000,
         median_income_1997_1000s = Median_Inc_1997_USD / 1000) |>
  rename(poverty_allages_1997_perc = Pov97_Perc_AllAge,
         HS_1990_perc = Educ_Attain1990_HS_Perc,
         state_name = state,
         state = state_abbrev) |>
  relocate(state, .after = county) |>
  arrange(state, county) |> 
  mutate(county = str_to_lower(county)) 

# Restrict the data set according to the procedure of Callaway & Sant'Anna (2021)
# Described in the Supplementary Appendix


# Data Cleaning - Firms with all private ownership
# Selecting the relevant variables & restrict the dataset on teenagers 
qwi_po <- qwi_po_raw |>
  select(geography, geography_label.value, agegrp, agegrp_label.value, 
         year, quarter, Emp, EmpEnd, EmpS, EmpSpv, EmpTotal, sEmp, sEmpEnd,
         sEmpS, sEmpSpv, sEmpTotal) |> 
  rename(geography_label = geography_label.value,
         agegrp_label = agegrp_label.value,
         county_id = geography) |>
  filter(agegrp == "A01")
  


# Create Key Variables for clear identification: county, state and year
qwi_po <-  qwi_po |>
  separate(col  = geography_label, into = c("county", "state"), sep = ",", remove = TRUE, fill = "right", extra = "drop") |>
  mutate(state  = str_trim(state, side = "both"),
         county = str_replace_all(county, " ", "_"),
         date_q = paste0(year, ": Q", quarter)) |>
  mutate(date_q = yq(date_q)) |>
  mutate(date_q = as.Date(date_q)) |>
  relocate(date_q, .after = "state") |>
  select(-c("agegrp", "agegrp_label")) |>
  filter(!state %in% c("DC", "PR")) |>
  arrange(state, county) |>
  mutate(county = str_replace_all(county, "-", "_")) |> 
  mutate(county = str_to_lower(county))


# Create Year-Indicator of Treatment
qwi_po <- qwi_po |>
  mutate(treat_g2004 = ifelse(state == "IL", 1, 0),
         treat_g2006 = ifelse(state %in% c("FL", "MN", "WI"), 1, 0),
         treat_g2007 = ifelse(state %in% c("CO", "MD", "MI", "MO", "MT", "NV" ,
                                           "NC", "OH", "WV" ), 1, 0)) |>
  mutate(general_treat_ind = ifelse(state %in% c("IL", "FL", "MN", "WI", "CO",
                                                 "MD", "MI", "MO", "MT", "NV",
                                                 "NC", "OH", "WV" ), 1, 0))

# Filter for all relevant states and create a region indicator 
qwi_restricted <- qwi_po |>
  filter(!state %in% c("AL", "AK", "AZ", "AR", "CA", "CT", "DE", "HI", "KY", 
                       "ME", "MA", "MS", "NH", "NJ", "NY", "OR", "PA", "RI",
                       "VT", "WA", "WY", "DC", "PR")) |> 
  mutate(region = region_abbreviations[state]) |>
  relocate(region, .after = state) |>
  filter(year >= 2001 & year <= 2007) |>
  group_by(county_id) |>
  filter(!any(is.na(Emp))) |>
  ungroup() 

# Matching data of data of cdb with qwi
qwi_matched <- qwi_restricted |>
  left_join(cdb_2000_restricted, by = c("state", "county")) |>
  group_by(county, state) |>
  filter(!(is.na(pop_nr_2000) & 
           is.na(nr_white_2000) & 
           is.na(HS_1990_perc) & 
           is.na(poverty_allages_1997_perc) &
           is.na(white_pop_2000_perc) & 
           is.na(pop_2000_nr_1000s) & 
           is.na(median_income_1997_1000s))) |>
  ungroup() 

# Special case of Baltimore (MD), Fairfax (VA), Franklin (VA), Richmond (VA),
# Roanoke (MD): These are listed as Independent Cities in CDB but are similarly
# the label in QWI does not distinguish between county and city in 

# Isolate Independent Cities in CDB (Hardcoded)
cdb_specialcase <- cdb_2000_restricted |>
  filter(state == "IC") |>
  filter(str_detect(county, regex("baltimore|fairfax|richmond|roanoke|franklin", ignore_case = TRUE))) |>
  mutate(county_id = c("24510", "51600", "51620", "51760","51770")) |>
  relocate(county_id, .before = county)

# Replace the relevant values of all 5 independent citis
qwi_matched <- qwi_matched |>
  left_join(cdb_specialcase, by = c("county_id")) |>
  mutate(
         pop_nr_2000.x = ifelse(!is.na(pop_nr_2000.y), pop_nr_2000.y, pop_nr_2000.x ),
         nr_white_2000.x = ifelse(!is.na(nr_white_2000.y), nr_white_2000.y, nr_white_2000.x),
         Median_Inc_1997_USD.x = ifelse(!is.na(Median_Inc_1997_USD.y), Median_Inc_1997_USD.y, Median_Inc_1997_USD.x ),
         HS_1990_perc.x = ifelse(!is.na(HS_1990_perc.y), HS_1990_perc.y, HS_1990_perc.x),
         poverty_allages_1997_perc.x = ifelse(!is.na(poverty_allages_1997_perc.y), poverty_allages_1997_perc.y, poverty_allages_1997_perc.x),
         white_pop_2000_perc.x = ifelse(!is.na(white_pop_2000_perc.y), white_pop_2000_perc.y, white_pop_2000_perc.x),
         pop_2000_nr_1000s.x = ifelse(!is.na(pop_2000_nr_1000s.y), pop_2000_nr_1000s.y, pop_2000_nr_1000s.x),
         median_income_1997_1000s.x = ifelse(!is.na(median_income_1997_1000s.y), median_income_1997_1000s.y, median_income_1997_1000s.x)
         ) |>
  select(-matches(".y")) |>
  rename_with(.fn = ~ sub("\\.x$", "", .x), .cols = ends_with(".x")) 





# Check if all counties are abvialable for all time quarters

yearly_counts <- qwi_matched |>
  group_by(county_id) |>
  summarise(yearly_obs = n(), .groups = "drop")


test <-  qwi_matched |> filter(str_detect(date_q, regex("-01-01"))) 

v <- data.frame(year = rep(NA, 7), county_nr = rep(NA, 7))

for(i in 2001:2007) {

  v[i - 2000, 1] <- i
    
  v[i - 2000, 2] <- qwi_matched |> 
  filter(year == i) |>
  distinct(county, state) |>
  nrow()
}
  

v



################################################################################
################################# END ##########################################
################################################################################

