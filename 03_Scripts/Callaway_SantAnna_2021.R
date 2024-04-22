rm(list = ls(all = TRUE))

library(tidyverse)
library(haven)
# library(httr)

data_path <- "01_Data"
table_path <- "02_Tables"

# Import dataset of QWI for the period 1995 to 2010
qwi_po_raw <- read_csv(paste0("./", data_path, "/", "qwi_all_privateownership.csv"))

# Restrict the dataset according to the procdure of Cattaway & Sant'Anna (2021)
# Described in the Supplementary Appendix

View(qwi_raw)

# Data Cleaning - Firms with all private ownership

qwi_po <- qwi_po_raw |> 
  select(geography, geography_label.value, agegrp, agegrp_label.value, 
         year, quarter, Emp, EmpEnd, EmpS, EmpSpv, EmpTotal, sEmp, sEmpEnd,
         sEmpS, sEmpSpv, sEmpTotal) |> 
  rename(geography_label = geography_label.value,
         agegrp_label = agegrp_label.value) |>
  filter(agegrp == "A01")

# Create Key Variables for clear identificiation: county, state and year
qwi_po <- qwi_po |> 
  separate(col = geography_label, into = c("county", "state"), sep = ",", remove = TRUE, fill = "right", extra = "drop") |>
  mutate(state = str_trim(state, side = "both"),
         county = str_replace_all(county, " ", "_"),
         date_q = paste0(year, ": Q", quarter)) |>
  mutate(date_q = yq(date_q)) |>
  relocate(date_q, .after = "state") |>
  select(-c("geography", "agegrp", "agegrp_label"))







################################################################################
################################# END ##########################################
################################################################################