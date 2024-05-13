rm(list = ls(all = TRUE))
# libraries
library(tidyverse)
library(data.table)
library(haven)
library(DRDID)
library(did)




# Form the Lalonde sample with CPS comparison group (data in wide format)
eval_lalonde_cps <- subset(nsw, nsw$treated == 0 | nsw$sample == 2)
# Further reduce sample to speed example
set.seed(123)
unit_random <- sample(1:nrow(eval_lalonde_cps), 5000)
eval_lalonde_cps <- eval_lalonde_cps[unit_random,]
# Select some covariates
covX = as.matrix(cbind(eval_lalonde_cps$age, eval_lalonde_cps$educ,
                       eval_lalonde_cps$black, eval_lalonde_cps$married,
                       eval_lalonde_cps$nodegree, eval_lalonde_cps$hisp,
                       eval_lalonde_cps$re74))

# Implement traditional DR locally efficient DiD with panel data
results <- drdid_panel(y1 = eval_lalonde_cps$re78, y0 = eval_lalonde_cps$re75,
                        D = eval_lalonde_cps$experimental,
                       covariates = covX,
                       inffunc = TRUE)

results$att.inf.func
