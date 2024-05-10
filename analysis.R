################################################################################
# Reproduction of Callaway & Sant'Anna (2021)
################################################################################

rm(list = ls(all = TRUE))
# libraries
library(tidyverse)
library(data.table)
library(haven)
library(DRDID)
library(did)


# ladd data
qwi <- read_rds(paste0("./", "01_Data/qwi_matched.RDS"))
qwi <- data.table(qwi)





################################################################################
## Start with the replication of the group-time average treatment effect
################################################################################


# drdid for g = 2004


data <- qwi[, group == 2004]






pre_out <- data[, treated == )]
pre_out <- data$lnEmp

post_post <- data[, (group == 2004) & (treated == 1)]
post_outc <- data$lnEmp

cov <-  model.matrix()



DRDID::drdid_panel(y1 = eval_lalonde_cps$re78, 
                   y0 = eval_lalonde_cps$re75,
                   D = eval_lalonde_cps$experimental,
                   covariates = covX)




















out1 <- did::att_gt(yname = "lnEmp",
                    tname = "date_y",
                    idname = "county_id",
                    gname = "group",
                    xformla = ~white_pop_2000_perc+poverty_allages_1997_perc+pop_2000_nr_1000s+median_income_1997_1000s+HS_1990_perc,
                    data = qwi,
                    est_method = "dr")
summary(out1)


ggdid((out1))