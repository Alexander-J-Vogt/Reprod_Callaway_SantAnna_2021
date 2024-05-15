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
          
# 
# qwi_test <- arrange(qwi, county_id, date_y)
# identical(qwi, qwi_test)

################################################################################
## ---- Start with the replication of the group-time average treatment effect
################################################################################

#' Idea:
#' 
#' Loop over the different groups and than different time periods in order to
#' yield the ATT for each year after the reference period. 
#' Core problem: What is the reference period? - Reference period is g-1 
#' How do you calculate the ATT for pre-treatment periods?
#'


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
nr_group <- length(grouplistl)
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

# 3. Statistics via bootstrapping ----------------------------------------------

# Step 1.1: Program function for multiplier bootstrapping in order to create
#           limiting distribution of the influence function for each ATT(g,t)
#           estimator


bootstrapping_algorithm <- function(inffunc_matrix, iter = 1000) {
  
  # Setup
  # iter <-  1000
  # Determine number of rows and columns for the multiplier bootstrap algorithm
  inffunc <- as.matrix(inffunc_matrix)
  n_row <- nrow(inffunc_matrix)
  n_col <- ncol(inffunc_matrix)
  
  # Empty matrix, which is later filled with the bootstrap 
  boot_results <- Matrix::Matrix(0, nrow = iter, ncol = n_col)
  
  # (1) & (2) of Algorithm 1 in CS
  for ( i in 1:iter ) {
    
    # Calculate Bernoulli Variates, which are used to select the influence 
    # function values of each calculated ATT(g,t) f
    # Advantage to common Bootstrapping: No re-sampling!
    
    #' iid Bernouli Variates $$${V_i}$$$ according to Mammen (1993)
    kappa <- ( sqrt(5) + 1 ) / 2
    p <- kappa / sqrt(5)
    bernoulli_weight <- rbinom(n_row, 1, p) # Is this right?
    
    # Sampling each column (aka influence function of each ATT(g,t)) with the 
    # Bernoulli Variates
    multiplied_if <- inffunc * bernoulli_weight
    
    # Bootstrap-Sampling-Distribution of influence function (nx(g*(t-1)) matrix)
    boot_results[i, ] <- colMeans(multiplied_if)
  
  } # end of loop

  # Return bootstrap matrix with influence function
  return(boot_results)
  
} # end of bootstrapping algorithm

mat <- as.matrix(if_matrix)
test <-  multiplier_bootstrap(mat, 1000)
test1 <- bootstrapping_algorithm(mat, iter = 1000)

#' Step 1.2: Use the function to calculate the limiting distribution and scale it 
#'           with sqrt(n) [write reason for this here]. The limiting distribution
#'           equals R*(g,t) in the proposed Algorithm 1 in CS. The calculations
#'           are bootstrapped on county-level as the Bernoulli-Variates are 
#'           cluster-specific.

# Own
data_boot <-  qwi
cluster_amount <- length(unique(data_boot$county_id))
dist <-  sqrt(cluster_amount) * bootstrapping_algorithm(if_matrix)
dist <- as.matrix(dist)

# Given Multiplier function
if_matrix <- as.matrix(if_matrix)
dist_given <-  sqrt(cluster_amount) * BMisc::multiplier_bootstrap(if_matrix, biters = 1000)
dist_given <- as.matrix(dist_given)

#' Step 2: Calculate the bootstrap estimator of the standard deviation (thus,
#'         standard error)
#'         [Note to myself: I do not need to multiply the results of the 
#'         by itself as each row already resembles a joint distribution of an
#'         estimator. Thus, I can simply calculate the standard estimator for
#'         each column.]

# Define function to calculate the the standard error for the limiting distribution
calculate_boots_sigma <- function(x) {
  (quantile(x, probs = .75,  na.rm = TRUE) - quantile(x, probs = .25, na.rm = TRUE)) /
    (qnorm(.75) - qnorm(.25))
}

# Calculate standard estimator based on the bootstrap distribution
boots_sigma <- apply(dist, 2, calculate_boots_sigma) # [Find another way to compute this.]
boots_sigma_given <- apply(dist_given, 2, calculate_boots_sigma)

#' Step 3: Calculate the t-test for each limiting distribution
#' 
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

# Step 4: Calculation of the empirical (1-alpha)-quantile of the B bootstrap
#         draws of t-test
alpha <- .05
c_hat <- quantile(t_test, 1 - alpha, na.rm = TRUE)





# ---- Calculation of standard error without bootstrap -------------------------



# filter

out1$c


# ---- free space --------------------------------------------------------------
att$att.inf.func
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

