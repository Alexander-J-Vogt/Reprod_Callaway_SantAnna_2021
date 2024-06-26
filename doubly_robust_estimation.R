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


load(paste0("./", "01_Data/mw_data_ch2.RData"))
load(paste0("./", "01_Data/data2.RData"))
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
g <- 2006
t <- 2006

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

# #
data_sel <- data_sel|>
  mutate(date = paste0("y", date_y), treat = ifelse(group == g, 1, 0))

data_wide       <- arrange(data_sel, county_id, date_y)
data_wide$.y1   <- data_wide$lnEmp
data_wide$.y0   <- shift(data_wide$lnEmp, 1)
data_wide$index <- ifelse(data_wide$date_y == reference_year, 1, 0)  
data_wide       <- data_wide[data_wide$index == 0,]

test_1 <- data_sel |> 
  arrange(county_id, date_y) |>
  pivot_wider(id_cols = county_id,
              names_from = date_y,
              values_from = lnEmp
              ) |>
  left_join(data_sel, join_by(county_id))

data_test <- arrange(data_sel, county_id, date_y)
data_test <- select(data_test, county_id, date_y, lnEmp)
x <- reshape(data = data_test, 
                idvar = "county_id", 
                timevar = "date_y", 
                v.names = "lnEmp",
                direction = "wide")

test2 <- x |>
  left_join(data_sel, by = join_by(county_id))


# Save a matrix of covariates
covariates <- model.matrix(spec_formula, data_wide)

Y_post <- data_wide$.y1
Y_pre  <- data_wide$.y0
treat  <- data_wide$treat

# DID-estimator for the case of unconditional trends

treat_ind   <-  which(treat == 1)
delta_treat <- mean(Y_post[which(treat == 1)]) - mean(Y_pre[which(treat == 1)])
delta_nt    <- mean(Y_post[which(treat == 0)]) - mean(Y_pre[which(treat == 0)])
att <-  delta_treat - delta_nt



data_alternative <- data_sel |>
  select(lnEmp, treat, date_y)

# Influence function with lava -----
# Same ATT  
reg <- lm(lnEmp ~ treat + date_y + date_y * treat, data_alternative)


# Function to calculate the unconditional average treatment effect on the treated
# ATT is calculated by simply taking the difference of the treatment effects of
# the pre- and post-treatment assuming that the parallel trend assumption holds.
unconditional_att <- function(outcome_post, outcome_pre, treatment) {
  # Estimating the average treatment effect on the treated 
  results_post <- lm(outcome_post ~ treatment)
  results_pre  <- lm(outcome_pre ~ treatment)
  coef_post <- coef(results_post)[2]
  coef_pre  <- coef(results_pre)[2]
  att <- coef_post - coef_pre
  
  # Estimating the influence function for each observation in order to 
  # calculate the standard errors based on the estimates. Theoretically,
  # estimates() allows also to extract the unconditional ATT
  merged_estimates <- merge(results_post, results_pre)
  merged_coef <- lava::estimate(merged_estimates, cbind(0, 1, 0, -1))
  
  # Extracting the estimates from the given object of estimates(). Dealing
  # with the unsorted list and convert it to a data frame. Rearranging the 
  # data frame in order to have the right order of initial observations given
  # of outcome_post and outcome_pre. (Resource: lava package + explanation)
  inf  <- IC(merged_coef)
  inf_df  <-  as.data.frame(inf)
  rownames <- as.vector(rownames(inf))
  rownames <- as.double(rownames)
  inf_df <- cbind(inf_df, rownames)
  inf_df <- arrange(inf_df, rownames)
  colnames(inf_df) <- c("estimates", "index")
  inf_df <- inf_df$estimates
  
  # Saving the results in list
  results <- list()
  results <- list(ATT = att, att.inf.func = inf_df)
}

results <- unconditional_att(Y_post, Y_pre, treat)

results$att.inf.func
results$att


results$inf.func.att


inf <- as.matrix(inf)

cbind(inf, rownames)

inf <- rbind(inf)
cbind(row_ind = rownames(inf), inf)
inf <- rbind(inf)
inf$row_index <- rownames(inf)

inf.func_post <- IC(estimate(reg1))
inf.func_pre <- IC(estimate(reg2))

inf.func.att <- as.vector(inf.func_post[,2]) - as.vector(inf.func_pre[,2])
var_ic(inf.func.att)

r <- IC(estimate(reg))

identical(r[,2], inf.func.att)



# 1. Doubly Robust DID-Estimator - OWN -----------------------------------------

#' The Doubly Robust Estimator follows the three step procedure of 
#' Sant'Anna & Zhao (2020) for estimating the ATT  of the improved 
#' DR DID estimator for panel data.  

dr_att_estimator <-  function(outcome_post, outcome_pre, treatment, covariates) {

    # Redefining variables given to the function for standardization
    Y_post <- outcome_post
    Y_pre  <- outcome_pre
    treat  <- treatment
    covariates <- as.matrix(covariates)
    
    # Step 1: IPW
    # Estimating the propensity score with a logistic regressions and predicting 
    # the probability of treatment for each observation.
    prop_score <- glm(treat ~ covariates, family = binomial)
    Y_post_predict <- predict(prop_score, type = "response")
    
    # Step 2: Outcome Regression
    # Select nevertreated group in order to prepare the data for the estimation
    # of the linear coefficient 
    index_nevertreated <- which(treat == 0)
    covariates_nt <- covariates[index_nevertreated,]
    
    # Estimating the linear coefficients for the pre and post treatment period
    # in order to calculate the change in coefficient
    beta_post <- coef(lm(Y_post[index_nevertreated] ~ -1 + covariates_nt))
    beta_pre  <- coef(lm(Y_pre[index_nevertreated]  ~ -1 + covariates_nt))
    
    # Calculate the difference between the pre and post coefficients 
    beta_delta <- as.vector(beta_post - beta_pre)
    
    # Calculating the product of covariates matrix and mu_delta, which will be 
    # used in the final calculation of the  
    mu_delta <- covariates %*% beta_delta
    
    # Calculating the weights for the treated group according to the equation (3.2)
    omega_1 <- treat / mean(treat)
    
    # Calculating the weights for the nevertreated group according to the equation (3.2)
    omega_2 <- (Y_post_predict * (1 - treat) / (1 - Y_post_predict)) /
      mean(Y_post_predict * (1 - treat) / (1 - Y_post_predict))
    
    # Step 3: 
    # Calculating  the ATT according to equation (3.1) in Sant'Anna & Zhao (2020)
    delta_Y <- Y_post - Y_pre
    att <- mean((omega_1 - omega_2) * (delta_Y - mu_delta))
  
}

t <-  dr_att_estimator(outcome_post = Y_post,
                 outcome_pre  = Y_pre,
                 treatment    = treat,
                 covariates   = covariates )

# Use data mpdta.rda
data(mpdta)
View(mpdta)
length(countyreal)

