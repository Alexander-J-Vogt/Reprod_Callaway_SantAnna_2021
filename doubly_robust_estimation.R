################################################################################
#Reproduction of Callaway & Sant'Anna (2021)
################################################################################

# Remove exisitng environment
rm(list = ls(all = TRUE))

# set seed for replication
set.seed(1234)

# libraries
library(tidyverse)
library(data.table)
library(haven)
library(DRDID)
library(did)
library(BMisc)
library(stats)

# load data
qwi <- read_rds(paste0("./", "01_Data/qwi_matched.RDS"))
qwi <- data.table(qwi)
qwi <- qwi |> 
  select(-c("treat_g2004", "treat_g2006", "treat_g2007", "state_name",
            "Median_Inc_1997_USD", "pop_nr_2000", "Emp", "nr_white_2000")) |>
  relocate(group, treated, .after =county_id) |>
  relocate(lnEmp, .after = date_y) |>
  arrange(county_id, date_y) # if sth make problem this could be the reason!

################################################################################
## ---- Start with the replication of the group-time average treatment effect
################################################################################

#---- Start up & Define variables ----------------------------------------------

# covariates formula
spec_formula <- ~-1+white_pop_2000_perc+poverty_allages_1997_perc+pop_2000_nr_1000s+median_income_1997_1000s+HS_1990_perc


# relevant numbers
timelist <- unique(qwi$date_y)
grouplist <- sort(unique(qwi$group))
grouplist <- grouplist[grouplist != 0]
num_of_cases <- (length(timelist) - 1) * length(grouplist)

# create empty data frame and/or list
attgt.df <- as.data.frame(matrix(NA, nrow = num_of_cases, ncol = 3))
colnames(attgt.df) <- c("attgt", "group", "year")
att.gt.ls <- list()

# create empty matrix
number <- 1
n_unique <- length(unique(qwi$county_id))
nr_group <- length(grouplist)
nr_times <- length(timelist)


data <- qwi
g <- 2004
t <- 200

if(t >= g) {
  reference_year <- g - 1
} else {
  reference_year <- t
}

# current group indicator (should get overwritten once we loop over groups)
data$g_ <- ifelse(data$group == g, 1, 0) 
data$c_ <- ifelse(data$group == 0, 1, 0)

# Select data in pre- & post-treatment period for relevant group and never-treated
data <- subset(data, date_y %in% c(reference_year, t + 1))

# select group & nevertreated (control)
index_gc <- (data$g_ == 1) | (data$c_ == 1)
data_sel <- data[index_gc,]
n_subset <- length(unique(data_sel$county_id))

#
data_sel <- data_sel|> 
  mutate(date = paste0("y", date_y), treat = ifelse(group == g, 1, 0))

data_wide       <- arrange(data_sel, county_id, date_y)
data_wide$.y1   <- data_wide$lnEmp
data_wide$.y0   <- shift(data_wide$lnEmp, 1)
data_wide$index <- ifelse(data_wide$date_y == reference_year, 1, 0)  
data_wide       <- data_wide[data_wide$index == 0,]

# Save a matrix of covariates
covariates <- model.matrix(spec_formula, data_wide)

Y_post <- data_wide$.y1
Y_pre  <- data_wide$.y0
treat  <- data_wide$treat

# 1. Doubly Robust DID-Estimator - OWN -----------------------------------------

# Estimate propensity score 
prop_score <- glm(treat ~ covariates, family = binomial(link = "logit"))
# type = responses gives predicted probabilities
Y_post_predict <- predict(prop_score, type = "response")

## Estimation of outcome regression
# Calculate mu for nevertreated individuals
index_nevertreated <- which(treat == 0)
delta_Y <- Y_post - Y_pre

covariates_nt <- covariates[index_nevertreated,]

or_post <- coef(lm(Y_post[index_nevertreated] ~ -1 + covariates_nt))
or_pre  <- coef(lm(Y_pre[index_nevertreated] ~ -1 + covariates_nt))
delta_or <- as.vector(or_post - or_pre)

mu <- covariates %*% delta_or

# Calculation of weights for treated and untreated group
omega_1 <- treat / mean(treat)
omega_2 <- (Y_post_predict * (1 - treat) / (1 - Y_post_predict)) /
  mean(Y_post_predict * (1 - treat) / (1 - Y_post_predict))

att <- mean((omega_1 - omega_2) * (delta_Y - mu))
  
  
relevant_var <- c(omega_1, omega_2, delta_Y, mu)
  sapply(relevant_var, function(x) {
     #d <- dim(x)
     l <- length()
     
    return(l)
  })
  
  
  
  
  length(omega_1)
  length(omega_2)
  length(delta_Y)
  dim(delta_Y)
  length(mu)











