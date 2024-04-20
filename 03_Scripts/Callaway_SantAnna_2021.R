rm(list = ls())

library(tidyverse)
library(haven)
# library(httr)

data_path <- "01_Data"
table_path <- "02_Tables"


# Import the data of QWI as API

# Import dataset of QWI for the period 1995 to 2010
qwi_po_raw <- read_csv(paste0("./", data_path, "/", "qwi_all_privateownership.csv"))

# Restrict the dataset according to the procdure of Cattaway & Sant'Anna (2021)
# Described in the Supplementary Appendix

View(qwi_raw)

# Data Cleaning - Firms with all private ownership

qwi_pop_raw <- qwi_po_raw |> 
  select(geography, geography_label.value, agegrp, agegrp_label.value, 
         year, quarter, Emp, EmpEnd, EmpS, EmpSpv, EmpTotal, sEmp, sEmpEnd,
         sEmpS, sEmpSpv, sEmpTotal) |> 
  rename(geography_label = geography_label.value,
         agegrp_label = agegrp_label.value) |>
  filter(agegrp == "A01")


qwi_pop_raw |>
  str_split(geography_label, ",", simplify = TRUE) |>
  as_tibble() |>
  mutate(
    county = .[[1,1]],
    state  = .[[1,2]] 
  ) |> View()
  
  
unique(test["geography"])
  View()
