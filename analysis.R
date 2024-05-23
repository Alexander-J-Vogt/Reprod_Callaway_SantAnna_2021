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

#  DR Estimator for DiD ----
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

#---- Start up & Define variables ----------------------------------------------

# Working copy of original data set
data_origin <- qwi

# Specifying formula for ATT estimator
spec_formula <- ~ -1 + white_pop_2000_perc + poverty_allages_1997_perc + pop_2000_nr_1000s + median_income_1997_1000s + HS_1990_perc

# Determining unique time periods and groups
timelist <- unique(qwi$date_y)
grouplist <- sort(unique(qwi$group))
grouplist <- grouplist[grouplist != 0]

# Creating empty data frame and/or list for the different ATT estimators
num_of_cases <- (length(timelist) - 1) * length(grouplist)
attgt.df <- as.data.frame(matrix(NA, nrow = num_of_cases, ncol = 3))
colnames(attgt.df) <- c("attgt", "group", "year")
att.gt.ls <- list()

# Creating an empty matrix for influence functions
number <- 1
n_unique <- length(unique(qwi$county_id))
nr_group <- length(grouplist)
nr_times <- length(timelist)
if_matrix <- Matrix::Matrix(data = 0, 
                            nrow = n_unique, 
                            ncol = nr_group * (nr_times - 1), 
                            sparse = TRUE)



# 1. ATT(g,t) via double loop -------------------------------------------------


# Loop over all groups
for (g in grouplist) {

   # Loop over all time periods within the loop over all groups
   for (t in timelist) {
  
    # Defining data for every loop new for a non-manipulated dataset
    data <- data_origin
    
    # If the t is in post-treatment period than the reference year is the year
    # before the group year. Otherwise the reference year is equal to t.
    if (t >= g) {
      reference_year <- g - 1
    } else {
      reference_year <- t
    }
    
    # Selecting the relevant data of the reference year and t+1 for current 
    # calculation of the ATT
    data <- subset(data, date_y %in% c(reference_year, t + 1)) 
    
    # Determining the nevertreated and group of the current iteration 
    data$g_ <- ifelse(data$group == g, 1, 0) 
    data$c_ <- ifelse(data$group == 0, 1, 0)
    
    # Indicator for influence matrix in order to save the influence function
    # in the right row (corresponding identifier)
    index_data    <- data[!duplicated(data$county_id),] 
    index_inffunc <- (index_data$g_ == 1) | (index_data$c_ == 1)
    n_sample      <- length(unique(index_data$county_id))
    
    # Selecting the relevant group and nevertreated from the two year dataset
    index_gc <- (data$g_ == 1) | (data$c_ == 1)
    data_sel <- data[index_gc,]
    n_subset <- length(unique(data_sel$county_id))
    
    # # Create treatment indicator and date variables as character # Can be deleted 
     data_sel <- data_sel|> 
       mutate(#date = paste0("y", date_y), 
                treat = ifelse(group == g, 1, 0))
    
    # If condition to check if the dataset contians excately two year.
    # If this is not the case, than the code jumps into the next loop.
    if (length(unique(data$date_y)) == 2) {
    
    # Reshaping data_sel from long to wide format in order to have the outcome
    # variable split by year. Thus, each row represents one county.
    data_wide       <- arrange(data_sel, county_id, date_y)
    data_wide$.y1   <- data_wide$lnEmp
    data_wide$.y0   <- shift(data_wide$lnEmp, 1)
    data_wide$index <- ifelse(data_wide$date_y == reference_year, 1, 0)  
    data_wide       <- data_wide[data_wide$index == 0,]
    
    } else {
      print(paste0("[!!] End of group iteration: ", g))
      next
    }
    
    # Control output for iteration # Might get deleted 
    print(paste0("Iteration: ", number))
    print(paste0("Iteration over group ", g, " and period ", t + 1 , " with reference period ", reference_year, "."))
    
    # Can probably get deleted 
    # If the number of rows is odd, the BMisc::panel2cs2 function produces missing 
    # in either .y1 or .y0. Thus, the NA's need to be removed, otherwise we don't
    # yield any DiD-estimator
    # data_cs <- data_wide[!is.na(.y1),]
    # data_cs <- data_wide[!is.na(.y0),]
    
    # Saving the covariates as matrix for the calculation of the ATT
    covariates <- model.matrix(spec_formula, data_wide)
    
    # Estimating the ATT(g,t) for the current iteration
    att <- DRDID::drdid_panel(y1 = data_wide$.y1,
                              y0 = data_wide$.y0,
                              D  = data_wide$treat,
                              covariates = covariates,
                              inffunc    = TRUE,
                              boot       = FALSE)
    # att <- dr_att_estimator(outcome_post = data_wide$.y1, 
    #                         outcome_pre = data_wide$.y0,
    #                         treatment  = data_wide$treat,
    #                         covariates = covariates)
    # recover att
    #att.gt.ls[[number]]<- list(attgt = att$ATT, group = g, period = t + 1)
    
    # Filling the data frame with values of ATT(g,t) 
    attgt.df[number, 1] <- att$ATT 
    attgt.df[number, 2] <- g
    attgt.df[number, 3] <- t + 1
    
    # Saving the estimated values of the influence function for each observation
    # and saving it in a vector. Estimates are adjusted to account for the smaller
    # observation size compared to the main dataset. 
    if_vector <- rep(0, n_sample)
    if_vector[index_inffunc] <- (n_subset / n_sample) * att$att.inf.func

    # Filling the matrix with the influence function, where one column
    # represents the estimates of the influence function of one ATT(g,t)
    if_matrix[, number] <- if_vector
    
    # Add to the variable +1 as this is the row/column indicator for the 
    # ATT(g,t) data frame and influence function matrix
    number <- number + 1
    
  }

}



# 4. Aggregated ATT(g,t) -------------------------------------------------------

## 4.0 Preparation of probabilities & vectors ----------------------------------

# Set up of copy of orginial dataset and relevant min and max of 
data_agg <- qwi
time_min <- min(data_agg$date_y)
time_max <- max(data_agg$date_y)

## Index and probability for attgt.df format
# Data frame with size per 
weights <- as.data.frame(matrix(NA, nrow = length(grouplist), ncol = 4))
colnames(weights) <- c("group", "size", "probs", "theta_o_w")

for ( i in seq_along(grouplist) ) {
  # select group
  year <- grouplist[i]
  
  # calculate group size and prob. of being in the group, 
  # i.e., group size divided by unique population size
  weights[i, 1] <- year
  weights[i, 2] <- nrow(data_agg[group == year & date_y == year, ])
  weights[i, 3] <- weights[i, 2] / n_unique
}


# Preparing the attgt.gt and combining it with the prob. of being a specific group
aggte_df <- merge(attgt.df, weights, by.x = "group" )  
index_post <- which(aggte_df$year >= aggte_df$group)

## Index and probability in cluster format (for group-specific ATT(g,t)) in order
## select the right county-estimates of the influence function
prob_list <- data_agg |>
  filter(date_y == time_min) |>
  select(county_id, group, date_y)

prob_list <- prob_list |> 
  left_join(weights[, c("group", "probs")], by = "group", keep = NULL) |>
  select(-c("date_y"))

for ( g in grouplist ) {
  assign(as.vector(paste0("index_post_", g)), which(prob_list$group == g | prob_list$group == 0))
}

# Function: Selecting the right influence function estimators & calculating the SE

recover_se_from_if <- function(matrix, 
                               prob_df, 
                               version = c("overall", "group"), 
                               index_col, 
                               index_row) {
  
  # Step 1: Filter relevant rows and cols
  org_if <- as.matrix(matrix)
    
  # Two Cases: Overall vs Group-time effects
  # Depending on the case the influence function is filtered for the specific 
  # group (rows are excluded; index_row) or only the relevant ATT(g,t) in the
  # post-treatment are selected (index_col)
  if (version == "overall") {
    if_mat <- org_if[, index_col]
  } else {
    if_mat <- org_if[index_row, index_post]
  }
  
  # Weight each column depending on prob of row
  group_weights <- prob_df[index_col] / sum(prob_df[index_col])
  
  # Calculate for each county a weighted influence function
  weighted_if <- if_mat %*% group_weights
  
  # Calculate actual standard error of the aggregated ATT
  var <- 1 /( nrow(weighted_if) - 1 ) * ( sum( (weighted_if - mean(weighted_if) )^2 ) )
  se <- sqrt(var/nrow(weighted_if))
  
  #return se 
  return(se)
}


recover_se_from_if(if_matrix, 
                   prob_df = aggte_df$probs,
                   version = "overall",
                   index_col = index_post)


# Prepare influence function by selecting the relevant columns/ATT(g,t)
# simple_if      <- if_matrix[, relevant_att]
# simple_weights <- simple_aggte_df[relevant_att,]
# simple_weights <- simple_aggte_df$probs / sum(relevant_attgt$probs)
# simple_weights <- simple_weights[relevant_att]



## 4.1 Simple Weighted ATT(g,t) ------------------------------------------------

aggte_simple <- aggte_df[index_post, ]

# Calculate the sum of probabilites over *all relevant* ATT(g,t) (all groups & all time periods)
kappa <- sum(aggte_simple$probs)


# Actual Simple Weighted Overall Treatment Effect
# Nominator: ATT(g,t) weighted with corresponding probability
# Denominator: Sum of probability of each ATT(g,t) in post-treatment period 
# taken over all groups
simple_att_est <- sum(aggte_simple$attgt * aggte_simple$probs) / kappa

# Recovering the standard error for the overall ATT weighted by the relative 
# size of the group

# Prepare influence function by selecting the relevant columns/ATT(g,t)
simple_if      <- if_matrix[, relevant_att]
simple_weights <- simple_aggte_df[relevant_att,]
simple_weights <- simple_aggte_df$probs / sum(relevant_attgt$probs)
simple_weights <- simple_weights[relevant_att]

# Calculate for each county a weighted influence function
simple_weighted_if <- simple_if %*% simple_weights

# Calculate actual standard error of the aggregated ATT
var <- 1/(nrow(simple_weighted_if)-1) *(sum((simple_weighted_if - mean(simple_weighted_if))^2))
se <- sqrt(var/nrow(simple_weighted_if))

####### [WARNING]: Might need some adjustment as SE is too small: What about WIF?


## 4.2 Group-Time ATT(g,t) -----------------------------------------------------

time_max <- max(timelist)

# Group-treatment effects (gte)
gte_attgt_df <- attgt.df 
theta_sel    <- rep(NA, nrow(gte_attgt_df))
gte_attgt_df <- cbind(gte_attgt_df,theta_sel)
gte_attgt_df <- gte_attgt_df[relevant_att, ]

# Group specific ATT
gte_results <- data.frame(matrix(NA, nrow = length(grouplist), ncol = 3))
colnames(gte_results) <- c("group", "gte", "se")

for ( i in 1:nrow(gte_results) ) {
  grouptime <- grouplist[i]
  gte_results[i, "group"] <- grouptime
  gte_results[i, "gte" ]  <- mean(gte_attgt_df[gte_attgt_df$group == grouptime, "attgt"])
}

# Aggregated GTE by population weights
agg_gte_results <- sum(gte_results$gte * simple_weights$probs) / sum(simple_weights$probs)

min_time <- min(timelist)
# Recover Standard errors
data_gte <- arrange(qwi, date_y, county_id)
index_g2004_row <- which(data_gte$date_y == min_time & (data_gte$group == 2004 | data_gte$group == 0))
index_g2004_col <- which(attgt.df$group == 2004 & attgt.df$year >= 2004)
group_if <- if_matrix[index_g2004, index_g2004_col]

group_if_weighted <- group_if %*% as.vector(simple_aggte_df[index_g2004_col,"probs"])


recover_se_from_if(if_matrix, 
                     prob_df = aggte_df$probs,
                     version = "group",
                     index_row = index_post_2004,
                     index_col = index_post)



# Check if _matrix! Does each row consist of that what I think it does?
dim(if_matrix)

## 4.3 Calendar Time Effects ---------------------------------------------------

# Select for each group the post-treatment period
# When 
aggte_ct <- aggte_df[attgt.df$year >= group_min & attgt.df$year >= attgt.df$group,]
calendar_timelist <- unique(aggte_ct$year)
attgt_et
# Calculate the calendar time effect for each period since the first group was treated
att_gt <- sapply(calendar_timelist, function(t){
                      df         <- aggte_ct[aggte_ct$year == t,]
                      group_prob <- df$probs / sum(df$probs)
                      attte_ct   <- sum(df$attgt * group_prob)
                  }
                 )

# Calculate the aggregated calendar time effect over all different calendar time effects
agg_att_gt <- mean(att_gt)

## 4.4 Event Study Effect (with & w/o balanced groups) -------------------------

# Indicator on how many periods a group should have experienced a treatment 
# NULL if balance group is not asked for
balance_groups <- 1

# Copy of attgt_df results
attgt_et <- aggte_df

# Calculate eventtime 
attgt_et$eventtime <- attgt_et$year - attgt_et$group

# Exclude pre-treatment
attgt_et <- attgt_et[attgt_et$eventtime >= 0, ]

# List of unique eventtime
grouplist_et <- unique(attgt_et$group)

# Check if event study design is to be calculated with balanced groups or not
# aka how many periods should a group have been experiencing a treatment/policy
# -> If yes, than the ATT(g,g+t) will be balanced 
# -> if not, the event study effect are calculated for every available event time
if ( !is.null(balance_groups) ) {
  # Calculate the observed number of event time per group
  att_per_group <- sapply(grouplist_et, function(g){
    df <- attgt_et[attgt_et$group == g,]
    n <- nrow(df)
    n
  })
  att_per_group <- data.frame(grouplist_et, att_per_group)
  colnames(att_per_group) <- c("eventtime", "abs_nr_att")
  
  # Check if the observed number of event time equal or larger than
  # balance_groups + 1
  # groups_to_exclude inhibits all years, which are not observed longer than 
  # "balance_groups"
  # !!! More elegant solution
  groups_to_exclude <- c()
  for ( i in seq_along(att_per_group$eventtime) ) {
    if ( att_per_group[i, 2] < balance_groups + 1 ) {
      groups_to_exclude[i] <- att_per_group[i, 1]
    } else {
      next
    }
  }
  groups_to_exclude <- groups_to_exclude[!is.na(groups_to_exclude)]
  
  # Balanced data frame
  attgt_et <- attgt_et[!attgt_et$group %in% groups_to_exclude,]
}

# Calculation of the event study effect
eventime_timelist <- unique(attgt_et$eventtime)
att_et <- sapply(eventime_timelist, function(et) {
                      df         <- attgt_et[attgt_et$eventtime == et, ]
                      group_prob <- df$probs / sum(df$probs)
                      attte_et   <- sum(df$attgt * group_prob)
                  }
                 )

att_et <- data.frame(cbind(eventime_timelist, att_et))

# Calculate aggregated event study effects
aggte_et <- mean(att_et$att_et)




# 2. Standard Errors of ATT(g,t) -----------------------------------------------

#' Multiplier Bootstrapping is relevant for confidence intervals and
#' replacing values that are too small in the v
if_matrix <- as.matrix(if_matrix)
# How to get variance of inffun:
# https://cran.r-project.org/web/packages/lava/vignettes/influencefunction.

vcov_matrix <- t(if_matrix) %*% if_matrix * (1 / n_unique)

# Basic matrix calculation to get variance and standard error
var <- diag(vcov_matrix / n_unique)
se <-  sqrt(var)

se[se <= sqrt(.Machine$double.eps)*10] <- NA
identifier <-  unique(which(is.na(se))) 

# 3. Statistics via bootstrapping ----------------------------------------------

# Defined Values for 3. 
cluster <- length(unique(qwi$county_id))


## Step 3.1.1: Multiplier Bootstrap Function -----------------------------------
#' Program function for multiplier bootstrapping in order to create limiting 
#' distribution of the influence function for each ATT(g,t) estimator. 
#' Thereby, the function resembles the first two steps of the described 
#' Algorithm 1 in Callaway & Sant'Anna (2021). 
#' The bootstrapping_algorithm-function is commonly specified for 
#' all influence function in order to allow for universality in usage.

bootstrapping_algorithm <- function(inffunc_matrix, iter = 1000) {
  
  # Determine number of rows and columns for the multiplier bootstrap algorithm
  inffunc_matrix <- as.matrix(inffunc_matrix)
  n_row          <- nrow(inffunc_matrix)
  n_col          <- ncol(inffunc_matrix)
  
  # Empty matrix, which is later filled with the distribution of 
  boot_results <- Matrix::Matrix(0, nrow = iter, ncol = n_col)
  
  # (1) & (2) of Algorithm 1 in Callaway & Sant'Anna (2021)
  for ( i in 1:iter ) {
    
    # Calculate Bernoulli Variates, which are used to select the influence 
    # function values of each calculated ATT(g,t) f
    # Advantage to common Bootstrapping: No re-sampling!
    
    # Bernoulli Variates according to Mammen (1993) which are iid
    kappa <- ( sqrt(5) + 1 ) / 2
    p <- kappa / sqrt(5)
    bernoulli_variates <- rbinom(n_row, 1, p) # Is this right?
    
    # Sampling each column (aka influence function of each ATT(g,t)) with the 
    # Bernoulli Variates
    multiplied_if <- inffunc_matrix * bernoulli_variates
    
    # Bootstrap-Sampling-Distribution of influence function (n x (g*(t-1)) matrix)
    boot_results[i, ] <- colMeans(multiplied_if)
    
  } # end of loop
  
  # Return bootstrap matrix with influence function
  return(boot_results)
  
} # end of bootstrapping algorithm

mat <- as.matrix(if_matrix)
test <-  multiplier_bootstrap(mat, 1000)
test1 <- bootstrapping_algorithm(mat, iter = 1000)
delta <- test - test1
colMeans(as.matrix(delta))

## Step 3.1.2: Use of the MBoot Function ---------------------------------------

#' Use the function to calculate the limiting distribution and scale it 
#' with sqrt(n) [write reason for this here]. The limiting distribution equals 
#' R*(g,t) in the proposed Algorithm 1 in CS. The calculations are bootstrapped 
#' on county-level as the Bernoulli-Variates are cluster-specific.

# Own
data_boot <-  qwi

dist <-  sqrt(cluster) * bootstrapping_algorithm(if_matrix)
dist <- as.matrix(dist)

# Given Multiplier function
if_matrix <- as.matrix(if_matrix)
dist_given <-  sqrt(cluster) * BMisc::multiplier_bootstrap(if_matrix, biters = 1000)
dist_given <- as.matrix(dist_given)

## Step 3.2:  Bootstrap Estimator of the Standard Deviation --------------------
#' Calculate the bootstrap estimator of the standard deviation (thus,standard 
#' error) [Note to myself: I do not need to multiply the results of the by 
#' itself as each row already resembles a joint distribution of an estimator. 
#' Thus, I can simply calculate the standard estimator for each column.]

# Define function to calculate the the standard error for the distribution. This
# is done by calculating the IQR of the distribution and standardizing it by 
# dividing it by the IQR of the standard normal distribution. This is the
# non-parametric way to calculate the standard error, which is more robust than
# the parametric sd and mean.

calculate_boots_sigma <- function(x) {
  (quantile(x, probs = .75,  na.rm = TRUE) - quantile(x, probs = .25, na.rm = TRUE)) /
    (qnorm(.75) - qnorm(.25))
}

# Calculate standard estimator based on the bootstrap distribution
boots_sigma <- apply(dist, 2, calculate_boots_sigma) # [Find another way to compute this.]
boots_sigma_given <- apply(dist_given, 2, calculate_boots_sigma)

## Step 3.3: Calculate the t-test for each limiting distribution ---------------

# Divide each distribution through the corresponding bootstrap sigma and take
# the absolute via loop instead of apply()
dist_c <- matrix(0, nrow = nrow(dist), ncol = ncol(dist))

for (j in 1:ncol(dist)) {
  dist_c[, j] <- abs(dist[, j] / boots_sigma[j])
}

# Take the maximum value of each bootstrap in order to calculate the t-test
t_test <- matrix(0, nrow = nrow(dist_c), ncol = 1)

for ( k in 1:nrow(dist_c) ) {
  t_test[k, ] <- max(dist_c[k, ])  
}

# t_test <- function(column) { max( abs( column / boots_sigma ) ) }
# 
# 
# results_t_test       <- apply(dist, MARGIN = 1, FUN = t_test)
# results_t_test_given <- apply(dist_given, MARGIN = 1, FUN = t_test)

## Step 3.4:  Calculation of c_hat ---------------------------------------------

# Calculation of the c_hat empirical (1-alpha)-quantile of the B bootstrap 
# draws of t-test

alpha <- .05
c_hat <- quantile(t_test, 1 - alpha, na.rm = TRUE)


























  
  
params <- DIDparams(yname = "lnEmp",
                    tname = "date_y",
                    idname = "county_id",
                    gname = "group",
                    xformla = ~white_pop_2000_perc+poverty_allages_1997_perc+pop_2000_nr_1000s+median_income_1997_1000s+HS_1990_perc,
                    data = qwi,
                    panel = TRUE,
                    allow_unbalanced_panel = FALSE,
                    control_group = "nevertreated",
                    anticipation = 0,
                    weightsname = NULL,
                    alp = 0.05,
                    bstrap = TRUE,
                    cband = TRUE,
                    biters = 1000,
                    clustervars = NULL,
                    est_method = "dr",
                    base_period = "varying",
                    print_details = FALSE,
                    pl = FALSE,
                    cores = 1)



test <- mboot(if_matrix, DIDparams = params)

out1$
# filter

out1$


# ---- free space --------------------------------------------------------------
att$att.inf.func
out1 <- did::att_gt(yname = "lnEmp",
                    tname = "date_y",
                    idname = "county_id",
                    gname = "group",
                    xformla = ~white_pop_2000_perc+poverty_allages_1997_perc+pop_2000_nr_1000s+median_income_1997_1000s+HS_1990_perc,
                    data = qwi,
                    control_group = "nevertreated",
                    bstrap = TRUE,
                    est_method = "dr",
                    base_period = "varying")
summary(out1)


ggdid((out1))

aggte(out1, type = "dynamic", balance_e = 1)
