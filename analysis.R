################################################################################
# Reproduction of Callaway & Sant'Anna (2021)
################################################################################

# libraries
library(tidyverse)
library(data.table)
library(haven)
library(DRDID)
library(did)


# laod data
qwi <- read_rds(paste0("./", "01_Data/qwi_matched.RDS"))
qwi <- data.table(qwi)





################################################################################
## Start with the replication of the group-time average treatment effect
################################################################################


# drdid for g = 2004

pre_outcome <- qwi |>
  filter(treat_g2004 == 0) |>
  select(lnEmp)

post_outcome <- qwi |>
  filter(treat_g2004 == 1) |>
  select(lnEmp)


cov <-  model.matrix()



DRDID:drdid_panel(y1 = lnEmp, )



















out1 <- did::att_gt(yname = "lnEmp",
                    tname = "date_y",
                    idname = "county_id",
                    gname = "group",
                    xformla = ~white_pop_2000_perc+poverty_allages_1997_perc+pop_2000_nr_1000s+median_income_1997_1000s+HS_1990_perc,
                    data = qwi,
                    est_method = "dr")
summary(out1)


ggdid((out1))