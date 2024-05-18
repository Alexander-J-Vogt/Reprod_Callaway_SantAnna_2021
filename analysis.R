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


if_matrix <- Matrix::Matrix(data = 0, nrow = n_unique, 
                            ncol = nr_group * (nr_times - 1), 
                            sparse = TRUE)



#  1. ATT(g,t) via double loop -------------------------------------------------

data_origin <- qwi

for (g in grouplist) {

   for (t in timelist) {
  
    data <- data_origin
    
    ###
    # Add weight if-condition (if time)
    ###
    
    # determine reference_year based on whether t lays within the post-treatment 
    # period or not
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
    
    # indicator for influence matrix
    index_data    <- data[!duplicated(data$county_id),] # loop 
    index_inffunc <- (index_data$g_ == 1) | (index_data$c_ == 1)
    n_sample      <- length(unique(index_data$county_id))
    
    # select group & nevertreated (control)
    index_gc <- (data$g_ == 1) | (data$c_ == 1)
    data_sel <- data[index_gc,]
    n_subset <- length(unique(data_sel$county_id))
    
    # Create treatment indicator and date variables as character
    data_sel <- data_sel|> 
      mutate(date = paste0("y", date_y), treat = ifelse(group == g, 1, 0))
    
    if(length(unique(data$date_y)) == 2) {
    
    # Change the two period data set from long to wide format in order to be
    # usable for the drdidpanel function. 
    # .y1: contains outcome values for current looping period t + 1
    # .y0: contains outcome values for reference period t
      
    data_wide       <- arrange(data_sel, county_id, date_y)
    data_wide$.y1   <- data_wide$lnEmp
    data_wide$.y0   <- shift(data_wide$lnEmp, 1)
    data_wide$index <- ifelse(data_wide$date_y == reference_year, 1, 0)  
    data_wide       <- data_wide[data_wide$index == 0,]
    
    } else {
      print(paste0("[!!] End of group iteration: ", g))
      next
    }
    
    # Consol output
    print(paste0("Iteration: ", number))
    print(paste0("Iteration over group ", g, " and period ", t + 1 , " with reference period ", reference_year, "."))
    
    # If the number of rows is odd, the BMisc::panel2cs2 function produces missing 
    # in either .y1 or .y0. Thus, the NA's need to be removed, otherwise we don't
    # yield any DiD-estimator
    data_cs <- data_wide[!is.na(.y1),]
    data_cs <- data_wide[!is.na(.y0),]
    
    # Save a matrix of covariates
    covariates <- model.matrix(spec_formula, data_wide)
    
    # att of group g at time point t
    att <- DRDID::drdid_panel(y1 = data_wide$.y1, 
                              y0 = data_wide$.y0,
                              D  = data_wide$treat,
                              covariates = covariates,
                              inffunc    = TRUE,
                              boot       = FALSE)
    
    # recover att
    att.gt.ls[[number]]<- list(attgt = att$ATT, group = g, period = t + 1)
    
    # Data Frame of ATTgt
    attgt.df[number, 1] <- att$ATT 
    attgt.df[number, 2] <- g
    attgt.df[number, 3] <- t + 1
    
    
    ## Recover influence function
    # Create a empty vector with the length of unique observations
    if_vector <- rep(0, n_sample)
    
    # estimate of influence function, weighted by relative sample size
    if_vector[index_inffunc] <- (n_subset / n_sample) * att$att.inf.func
    
    # save vector of round x into the column
    if_matrix[, number] <- if_vector
    
    # add 1 to number for index
    number <- number + 1
    
  }

}

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


# 4. Aggregated ATT(g,t) -------------------------------------------------------

## 4.0 Preparation of probabilities & vectors ----------------------------------

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
index_post <- which(simple_aggte_df$year >= simple_aggte_df$group)
# index_post <- aggte_df[index_post,]

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
simple_if      <- if_matrix[, relevant_att]
simple_weights <- simple_aggte_df[relevant_att,]
simple_weights <- simple_aggte_df$probs / sum(relevant_attgt$probs)
simple_weights <- simple_weights[relevant_att]



## 4.1 Simple Weighted ATT(g,t) ------------------------------------------------

# Calculate the sum of probabilites over *all relevant* ATT(g,t) (all groups & all time periods)
kappa <- sum(relevant_attgt$probs)


# Actual Simple Weighted Overall Treatment Effect
# Nominator: ATT(g,t) weighted with corresponding probability
# Denominator: Sum of probability of each ATT(g,t) in post-treatment period 
# taken over all groups
simple_att_est <- sum(relevant_attgt$attgt * relevant_attgt$probs) / kappa

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



## Understand data transformation
originalt <- qwi$date_y
originalgroup <- qwi$group
originalglist <- grouplist
originaltlist <- timelist

# function to switch from "new" t values to  original t values
t2orig <- function(t) {
  unique(c(originalgtlist,0))[which(c(uniquet,0)==t)]
}
# function to switch between "original"
#  t values and new t values
orig2t <- function(orig) {
  new_t <- c(uniquet,0)[which(unique(c(originalgtlist,0))==orig)]
  out <- ifelse(length(new_t) == 0, NA, new_t)
  out
}
t <- sapply(originalt, orig2t)
group <- sapply(originalgroup, orig2t)
glist <- sapply(originalglist, orig2t)
tlist <- unique(t)
maxT <- max(t)


# we can work in overall probabilities because conditioning will cancel out
# cause it shows up in numerator and denominator
pg <- sapply(originalglist, function(g) mean(1*(qwi[,group]==g)))

# length of this is equal to number of groups
pgg <- pg
max_e = Inf
# same but length is equal to the number of ATT(g,t)
pg <- pg[match(group, glist)]

keepers <- which(group <= t) #& t<= (group + max_e)) ### added second condition to allow for limit on longest period included in att

# n x 1 vector of group variable
G <-  unlist(lapply(qwi[,group], orig2t))



  
  
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

aggte(out1, type = "group")
