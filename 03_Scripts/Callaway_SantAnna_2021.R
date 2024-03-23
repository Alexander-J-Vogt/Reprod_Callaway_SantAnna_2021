rm(list = ls())

library(tidyverse)
library(haven)
# library(httr)

data_path <- "01 Data"
table_path <- "02 Tables"


# Import the data of QWI as API
# 
# https_qwi <- "api.census.gov/data/timeseries/qwi.html"
# qwi <- GET(https_qwi)

# Import dataset of QWI for the period 1995 to 2010
qwi <- read_csv(paste0("./", data_path, "/", "qwi_county_level.csv"))

# Restrict the dataset according to the procdure of Cattaway & Sant'Anna (2021)
# Described in the Supplementary Appendix
