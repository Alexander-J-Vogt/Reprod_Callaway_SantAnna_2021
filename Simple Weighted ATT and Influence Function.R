################################################################################
# Reproduction of Callaway & Sant'Anna (2021)
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
library(lava)

# Please set here the working directory

# load data
qwi <- read_rds(paste0("./", "qwi_matched.RDS"))
qwi <- data.table(qwi)
qwi <- qwi |> 
  relocate(group, treated, .after =county_id) |>
  relocate(lnEmp, .after = date_y) |>
  arrange(county_id, date_y) |>
  mutate(lwhite_pop = log(white_pop_2000_perc),
         lpoverty = log(poverty_allages_1997_perc),
         lpop_1000s = log(pop_2000_nr_1000s),
         lpop = log(pop_nr_2000),
         lmedian_income_1000s = log(median_income_1997_1000s),
         lmeduab_income = log(Median_Inc_1997_USD),
         leduc = log(HS_1990_perc)
  ) |>
  select(-c("treat_g2004", "treat_g2006", "treat_g2007", "state_name",
            "Emp", "Median_Inc_1997_USD", "pop_nr_2000", "nr_white_2000"))


################################################################################
## ---- Start with the replication of the group-time average treatment effect
################################################################################

#---- Start up & Define variables ----------------------------------------------

# Specifying formula for ATT estimator
spec_formula <- ~ -1 + white_pop_2000_perc + poverty_allages_1997_perc + pop_2000_nr_1000s + median_income_1997_1000s + HS_1990_perc
spec_formula_log <- ~ -1 + lwhite_pop + lpoverty + lpop_1000s + lmedian_income_1000s + leduc
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
    data <- qwi
    
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

    # Saving the covariates as matrix for the calculation of the ATT
    covariates <- model.matrix(spec_formula, data_wide)
    
    # Estimating the ATT(g,t) for the current iteration
    att <- DRDID::drdid_panel(y1 = data_wide$.y1,
                              y0 = data_wide$.y0,
                              D  = data_wide$treat,
                              covariates = covariates,
                              inffunc    = TRUE,
                              boot       = FALSE)

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

# attgt.df includes all calculated ATT(g,t)
# if_matrix includes all estimates of the influence function for each ATT(g,t)
# -> each column inhibits the estimates of one ATT(g,t)

# 4. Aggregated ATT(g,t) -------------------------------------------------------

## 4.0 Preparation of probabilities & vectors ----------------------------------

# Set up of copy of original dataset and relevant min and max of 
data <- qwi
time_min <- min(data$date_y)
time_max <- max(data$date_y)

# Calculation of the probability to belong to a treated group g.   
# Preparing an empty data frame, which will be filled with probability 
# to belong to group g.
weights <- as.data.frame(matrix(NA, nrow = length(grouplist), ncol = 3))
colnames(weights) <- c("group", "size", "probs")

# Calculate actual probabilities to belong to a group in a loop
for (i in seq_along(grouplist)) {
  
  # select group
  g <- grouplist[i]
  
  # calculate group size and prob. of being in the group, 
  # i.e., group size divided by unique population size
  weights[i, 1] <- g
  weights[i, 2] <- nrow(data[group == g & date_y == g, ])
  weights[i, 3] <- weights[i, 2] / n_unique
}

# Merging the data frame with all ATT(g,t) with the corresponding probability
# to the belong to a group g
attgt_probs_df <- merge(attgt.df, weights, by.x = "group" )  
index_post   <- which(attgt_probs_df$year >= attgt_probs_df$group)

  ## 4.1 Simple Weighted Average of ATT(g,t) -------------------------------------

# Selecting the ATT(g,t) of each group in the post-treatment period for further
# calculations. ATT(g,t) of a group before the period of treatment are dropped.
aggte_simple <- attgt_probs_df[index_post, ]

# Calculating the sum of probabilities corresponding to ATT(g,t) in the
# post-treatment period over all groups.
kappa <- sum(aggte_simple$probs)

# Simple Weighted Average of group-time average treatment effects:
# Taking the sum over all ATT(g,t) and multiplying by the corresponding 
# probability of being in group g. This sum is then divided by kappa.
simple_att_est <- sum(aggte_simple$attgt * aggte_simple$probs) / kappa


#### Recovering the standard error for the overall ATT weighted by the relative 
# size of the group

# attgt_probs_df: data frame of all ATT(g,t), which are calculated for the
# the period between 2001 and 2007. Thereby, attgt_probs_df$probs is a column
# of probabilities for each group

# n x 1 vector of group variable; inhibits which group a county belongs to
G <- qwi[qwi$date_y == time_min, "group"]

# Adjusted wif function of did package
# estimating the influence function of weights of the nominator
if_nominator <- sapply(index_post, function(k) {
  (1*BMisc::TorF(G==attgt_probs_df$group[k]) - attgt_probs_df$probs[k]) /
    sum(attgt_probs_df[index_post, "probs"])
})

# estimating the influence function of weights of the denominator 
if_denominator <- base::rowSums( sapply( index_post, function(k) {
  1*BMisc::TorF(G==attgt_probs_df$group[k]) - attgt_probs_df$probs[k]
})) %*%
  t(attgt_probs_df$probs[index_post]/(sum(attgt_probs_df$probs[index_post])^2))

# This influence function adjusts for the uncertainty introduced by the 
# estimated probability of belonging to a group g
# produces an nxk matrix with n = number of counties and k equal to ATT in the
# post-treatment period of each group
if_adjusted_weights <- if_nominator - if_denominator

# Prepare influence function by selecting the relevant columns/ATT(g,t)
simple_if      <- if_matrix[, index_post]
simple_weights <- aggte_simple$probs / sum(aggte_simple$probs)

# Calculate for each county a weighted influence function
simple_weighted_if <- simple_if %*% simple_weights
simple_weighted_if <- simple_weighted_if + if_adjusted_weights %*% as.matrix(aggte_simple$attgt)

# Calculate actual standard error of the aggregated ATT
var <- 1/(nrow(simple_weighted_if)-1) * (sum((simple_weighted_if - mean(as.vector(simple_weighted_if), ))^2))
se <- sqrt(var/nrow(simple_weighted_if))

# Compare to results of did-package:
# Calcualtes the standard erros without mutliplier bootstrap
out1 <- did::att_gt(yname = "lnEmp",
                    tname = "date_y",
                    idname = "county_id",
                    gname = "group",
                    xformla = ~white_pop_2000_perc+poverty_allages_1997_perc+pop_2000_nr_1000s+median_income_1997_1000s+HS_1990_perc,
                    data = qwi,
                    control_group = "nevertreated",
                    bstrap = FALSE,
                    est_method = "dr",
                    base_period = "varying")
aggte(out1, type = "simple", balance_e = 1, bstrap = FALSE)

# The effect-magnitude is correctly estimated but the standard error is half of 
# what is calculated by the did-package. 


