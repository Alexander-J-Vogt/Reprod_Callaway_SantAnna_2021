################################################################################
# Reproduction of Callaway & Sant'Anna (2021)
################################################################################

# Remove exisitng environment
rm(list = ls(all = TRUE))

# libraries
library(tidyverse)
library(data.table)
library(haven)
library(DRDID)
library(did)


# load data
qwi <- read_rds(paste0("./", "01_Data/qwi_matched.RDS"))
qwi <- data.table(qwi)
qwi <- qwi |> 
  select(-c("treat_g2004", "treat_g2006", "treat_g2007", "state_name",
            "Median_Inc_1997_USD", "pop_nr_2000", "Emp", "nr_white_2000")) |>
  relocate(group, treated, .after =county_id) |>
  relocate(lnEmp, .after = date_y)
          





################################################################################
## Start with the replication of the group-time average treatment effect
################################################################################

#' Idea:
#' 
#' Loop over the different groups and than different time periods in order to
#' yield the ATT for each year after the reference period. 
#' Core problem: What is the reference period? - Reference period is g-1 
#' How do you calculate the ATT for pre-treatment periods?
#' 







# drdid for g = 2004


data <- qwi
timelist <- distinct(qwi$date_y)
grouplist <- distinct(qwi$group)

# current group indicator (should get overwritten once we loop over groups)
data$cg <- ifelse(data$group == 2004, TRUE, FALSE)

# Subset data of group - post-treatment
data_current <- data |> filter(group == 2004 & date_y == 2005)

# Subset data of group
data_p



panel2cs2(qwi, yname = lnEmp, idname = county_id, tname = date_y, balance_panel =  FALSE )

cov <-  model.matrix()



DRDID::drdid_panel(y1 = eval_lalonde_cps$re78, 
                   y0 = eval_lalonde_cps$re75,
                   D = eval_lalonde_cps$experimental,
                   covariates = covX,)




















out1 <- did::att_gt(yname = "lnEmp",
                    tname = "date_y",
                    idname = "county_id",
                    gname = "group",
                    xformla = ~white_pop_2000_perc+poverty_allages_1997_perc+pop_2000_nr_1000s+median_income_1997_1000s+HS_1990_perc,
                    data = qwi,
                    est_method = "dr",
                    base_period = "varying")
summary(out1)


ggdid((out1))