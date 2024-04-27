rm(list = ls(all = TRUE))

library(tidyverse)
library(haven)
# library(httr)

data_path <- "01_Data"
table_path <- "02_Tables"

# Import dataset of QWI for the period 1995 to 2010
qwi_po_raw <- read_csv(paste0("./", data_path, "/", "qwi_all_privatownership.csv"))


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

# Restrict the dataset according to the procdure of Cattaway & Sant'Anna (2021)
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
  relocate(date_q, .after = "state") |>
  select(-c("geography", "agegrp", "agegrp_label"))

# Filter for all relevant states and create a region indicator 
qwi_po <- qwi_po |> 
  filter(!state %in% c("AL", "AK", "AZ", "AR", "CA", "CT", "DE", "HI", "KY", 
                       "ME", "MA", "MS", "NH", "NJ", "NY", "OR", "PA", "RI",
                       "VT", "WA", "WY", "DC", "PR")) |>
  mutate(region = region_abbreviations[state]) |>
  relocate(region, .after = state)

# Create Year-Indicator of Treatment
qwi_po <- qwi_po |>
  mutate(treat_g2004 = ifelse(state == "IL", 1, 0),
         treat_g2006 = ifelse(state %in% c("FL", "MN", "WI"), 1, 0),
         treat_g2007 = ifelse(state %in% c("CO", "MD", "MI", "MO", "MT", "NV" ,
                                           "NC", "OH", "WV" ), 1, 0))

qwi_po |>
  filter(sEmp == 5) |>
  distinct(Emp)






################################################################################
################################# END ##########################################
################################################################################