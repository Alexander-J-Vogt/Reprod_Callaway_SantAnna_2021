rm(list = ls(all = TRUE))

library(tidyverse)
library(haven)
# library(httr)

data_path <- "01_Data"
table_path <- "02_Tables"

# Import data set of QWI for the period 1995 to 2010
qwi_po_raw <- read_csv(paste0("./", data_path, "/", "qwi_all_privatownership.csv"))

# Import the cleaned "2000 County Data Book" dataset
cdb_2000 <- read_rds(paste0("./", data_path, "/", "county_data_book_2000.rds"))

# Identification of Region and State accroding to US Bureau of Labor Statistics
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
  arrange(state, county)



# Restrict the data set according to the procedure of Callaway & Sant'Anna (2021)
# Described in the Supplementary Appendix


# Data Cleaning - Firms with all private ownership
# Selecting the relevant variables & restrict the dataset on teenagers 
qwi_po <- qwi_po_raw |> 
  select(geography, geography_label.value, agegrp, agegrp_label.value, 
         year, quarter, Emp, EmpEnd, EmpS, EmpSpv, EmpTotal, sEmp, sEmpEnd,
         sEmpS, sEmpSpv, sEmpTotal) |> 
  rename(geography_label = geography_label.value,
         agegrp_label = agegrp_label.value) |>
  filter(agegrp == "A01") 

# Create Key Variables for clear identification: county, state and year
qwi_po <- qwi_po |> 
  separate(col = geography_label, into = c("county", "state"), sep = ",", remove = TRUE, fill = "right", extra = "drop") |>
  mutate(state = str_trim(state, side = "both"),
         county = str_replace_all(county, " ", "_"),
         date_q = paste0(year, ": Q", quarter)) |>
  mutate(date_q = yq(date_q)) |>
  mutate(date_q = as.Date(date_q)) |>
  relocate(date_q, .after = "state") |>
  select(-c("geography", "agegrp", "agegrp_label")) |>
  filter(!state %in% c("DC", "PR")) |>
  arrange(state, county) |>
  mutate(county = str_replace_all(county, "-", "_"))

# Create Year-Indicator of Treatment
qwi_po <- qwi_po |>
  mutate(treat_g2004 = ifelse(state == "IL", 1, 0),
         treat_g2006 = ifelse(state %in% c("FL", "MN", "WI"), 1, 0),
         treat_g2007 = ifelse(state %in% c("CO", "MD", "MI", "MO", "MT", "NV" ,
                                           "NC", "OH", "WV" ), 1, 0))

# Filter for all relevant states and create a region indicator 
qwi_restricted <- qwi_po |> 
  filter(!state %in% c("AL", "AK", "AZ", "AR", "CA", "CT", "DE", "HI", "KY", 
                       "ME", "MA", "MS", "NH", "NJ", "NY", "OR", "PA", "RI",
                       "VT", "WA", "WY", "DC", "PR")) |>
  mutate(region = region_abbreviations[state]) |>
  relocate(region, .after = state)

# qwi_matched 
# qwi_po_matched <- 
qwi_po |>
  full_join(cdb_2000_restricted, by = c("state", "county")) |> #View()
  filter(date_q == "2001-01-01") |>
  distinct(county, state)

return(qwi_po_matched)

qwi_po |>
  filter(!state %in% above_fed_mw) |>
  filter(!sEmp == 5) |>
  distinct(county, state)

test <- qwi_po |> 
  filter((date_q >= "2003-04-01") & (date_q <= "2007-01-01")) |>
  mutate(treat_general = ifelse(state %in% above_fed_mw, 1, 0))

qwi_po |>
  filter(year == 2001) |>
  distinct(county)

test <-  qwi_po |> filter(str_detect(date_q, regex("-01-01"))) 

v <- data.frame(year = rep(NA, 8), county_nr = rep(NA, 8))

for(i in 2001:2007) {

  v[i - 2000, 1] <- i
    
  v[i - 2000, 2] <- qwi_po |> 
  filter(year == i) |>
  distinct(county, state) |>
  nrow()
}
  

v



################################################################################
################################# END ##########################################
################################################################################