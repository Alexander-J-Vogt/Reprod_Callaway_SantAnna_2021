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
library(stargazer)

# Please set here the working directory

# load data
qwi <- read_rds(paste0("./", "01_Data/qwi_matched.RDS"))
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
            "Emp", "Median_Inc_1997_USD", "pop_nr_2000", "nr_white_2000")) |>
  mutate(post_treat = ifelse(date_y >= 2004, 1, 0)) |>
  mutate(post_treat = ifelse(date_y >= 2006, 1, post_treat)) |>
  mutate(post_treat = ifelse(date_y >= 2007, 1, post_treat))
  

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

# Unconditional DiD-ATT estimator  -----

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
  
  # Saving the results in list
  results <- list()
  results <- list(ATT = att)
}

# Specifying formula for ATT estimator
spec_formula <- ~ -1 + white_pop_2000_perc + poverty_allages_1997_perc + pop_2000_nr_1000s + median_income_1997_1000s + HS_1990_perc
spec_formula_log <- ~ -1 + lwhite_pop + lpoverty + lpop_1000s + lmedian_income_1000s + leduc

calculating_agg_att <- function(data,
                                year_input,
                                group_input,
                                outcome_input,
                                id_input,
                                treatment,
                                formula,
                                unconditional_ind = FALSE,
                                method,
                                balanced = NULL
                                ) {

#---- Start up & Define variables ----------------------------------------------

# Define data within the function
data_original <- data
data_original <- data_original |>
  rename(year    = !!sym(year_input),
         group   = !!sym(group_input),
         outcome = !!sym(outcome_input),
         id      = !!sym(id_input)
         )
# data_original <- data_original |>
#   rename(year    = date_y, #!!sym(year_input),
#          group   = group, #!!sym(group_input),
#          outcome = lnEmp, #!!sym(outcome_input),
#          id      = county_id# !!sym(id_input)
#   )
# Determining unique time periods and groups
timelist <- unique(data_original$year)
grouplist <- sort(unique(data_original$group))
grouplist <- grouplist[grouplist != 0]

# Creating empty data frame and/or list for the different ATT estimators
num_of_cases <- (length(timelist) - 1) * length(grouplist)
attgt.df <- as.data.frame(matrix(NA, nrow = num_of_cases, ncol = 3))
colnames(attgt.df) <- c("attgt", "group", "year")
att.gt.ls <- list()

# Creating an empty matrix for influence functions
number <- 1
n_unique <- length(unique(data_original$id))

# Define whether the conditional or unconditional parallel trend assumption is
# implemented.
unconditional <- unconditional_ind
# unconditional <- FALSE
# Define pre-covariates
covariate_formula <- formula
# covariate_formula <- spec_formula

# 1. ATT(g,t) via double loop -------------------------------------------------

# Loop over all groups
for (g in grouplist) {

   # Loop over all time periods within the loop over all groups
   for (t in timelist) {
    
    # Defining data for every loop new for a non-manipulated dataset
    data <- data_original
    
    # If the t is in post-treatment period than the reference year is the year
    # before the group year. Otherwise the reference year is equal to t.
    if (t >= g) {
      reference_year <- g - 1
    } else {
      reference_year <- t
    }
    
    # Selecting the relevant data of the reference year and t+1 for current 
    # calculation of the ATT
    data <- subset(data, year %in% c(reference_year, t + 1)) 
    
    # Determining the nevertreated and group of the current iteration 
    data$g_ <- ifelse(data$group == g, 1, 0) 
    data$c_ <- ifelse(data$group == 0, 1, 0)
    
    # Selecting the relevant group and nevertreated from the two year dataset
    index_gc <- (data$g_ == 1) | (data$c_ == 1)
    data_sel <- data[index_gc,]
    n_subset <- length(unique(data_sel$id))
    
    # # Create treatment indicator and date variables as character # Can be deleted 
     data_sel <- data_sel|> 
       mutate(treat = ifelse(group == g, 1, 0))
    
    # If condition to check if the dataset contians excately two year.
    # If this is not the case, than the code jumps into the next loop.
    if (length(unique(data$year)) == 2) {
    
    # Reshaping data_sel from long to wide format in order to have the outcome
    # variable split by year. Thus, each row represents one county.
    data_wide       <- arrange(data_sel, id, year)
    data_wide$.y1   <- data_wide$outcome
    data_wide$.y0   <- shift(data_wide$outcome, 1)
    data_wide$index <- ifelse(data_wide$year == reference_year, 1, 0)  
    data_wide       <- data_wide[data_wide$index == 0,]
    
    } else {
      print(paste0("[!!] End of group iteration: ", g))
      next
    }
    
    # Control output for iteration # Might get deleted 
    print(paste0("Iteration: ", number))
    print(paste0("Iteration over group ", g, " and period ", t + 1 , " with reference period ", reference_year, "."))
    
    # Saving the covariates as matrix for the calculation of the ATT
    covariates <- model.matrix(covariate_formula, data_wide)
    
    if (unconditional == TRUE) {
      
      att <- unconditional_att(outcome_post = data_wide$.y1,
                               outcome_pre  = data_wide$.y0,
                               treatment    = data_wide$treat)
      
    } else {
    
      # Estimating the ATT(g,t) for the current iteration
      att <- DRDID::drdid_panel(y1 = data_wide$.y1,
                                y0 = data_wide$.y0,
                                D  = data_wide$treat,
                                covariates = covariates,
                                inffunc    = FALSE,
                                boot       = FALSE)
    }
    
    # Filling the data frame with values of ATT(g,t) 
    attgt.df[number, 1] <- att$ATT 
    attgt.df[number, 2] <- g
    attgt.df[number, 3] <- t + 1
    
    # Add to the variable +1 as this is the row/column indicator for the 
    # ATT(g,t) data frame and influence function matrix
    number <- number + 1
  }
}

# Can be deleted in the end; only for test purposes
# list <- list(att = attgt.df)
# return(list)
# }

# 4. Aggregated ATT(g,t) -------------------------------------------------------

## 4.0 Preparation of probabilities & vectors ----------------------------------
  
  # Set up of copy of original dataset and relevant min and max of 
  data <- data_original
  time_min <- min(data$year)
  time_max <- max(data$year)
  
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
    weights[i, 2] <- nrow(data[data$group == g & data$year == g, ])
    weights[i, 3] <- weights[i, 2] / n_unique
  }
  
  # Merging the data frame with all ATT(g,t) with the corresponding probability
  # to the belong to a group g
  attgt_probs_df <- merge(attgt.df, weights, by.x = "group" )  
  index_post   <- which(attgt_probs_df$year >= attgt_probs_df$group)
  

## 4.1 Simple Weighted Average of ATT(g,t) -------------------------------------

  if (method == "simple_att") {
  
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
    
    # Save partially and overall ATT (NO parial effects)
    results <- list(overall_att = simple_att_est)
    return(results)
  }


## 4.2 Group-Time ATT(g,t) -----------------------------------------------------
  
  if (method == "group_specific_att") {
    
    # Selecting the ATT(g,t) of each group in the post-treatment period in 
    # order to have the relevant ATT(g,t) for the group-time ATT(g,t)
    gte_df <- attgt_probs_df[index_post, ]
    
    # Creating an empty data frame, in which the group-time ATT(g,t) are going to
    # be saved (COMMENT: Do we need the column SE?)
    gte_results <- data.frame(matrix(NA, nrow = length(grouplist), ncol = 3))
    colnames(gte_results) <- c("time", "coef", "se")
    
    # Calculating the group-time average treatment effect by taking the mean of 
    # of all ATT(g,t) of a group
    for (i in seq_along(grouplist)) {
      g <- grouplist[i]
      gte_results[i, "time"] <- g
      gte_results[i, "coef" ]  <- mean(gte_df[gte_df$group == g, "attgt"])
    }
    
    # Calculating the overall average treatment effect of the group-time 
    # average treatment effect. Thereby, each group-time average treatment
    # effect is weighted by the probability of being in group g.
    agg_gte_results <- sum(gte_results$coef * weights$probs) / sum(weights$probs)
    
    # Save group-specific effects
    results <- list(partial_att = gte_results, overall_att = agg_gte_results)
    return(results)
  } # End of group-specific effects if-clause


## 4.3 Calendar Time Effects ---------------------------------------------------
# Gets activated if method is equal to calendar_att
  if (method == "calendar_att") {
  
    # Select for each group the ATT(g,t)-effects of the post-treatment period for
    # in order to calculate heterogenous treatment effects w.r.t. to calendar time
    group_min <- min(grouplist)
    aggte_ct <- attgt_probs_df[attgt.df$year >= group_min & attgt.df$year >= attgt.df$group,]
     calendar_timelist <- unique(aggte_ct$year)
    
    # Calculating the calendar time effects for each period after the first group 
    # got treated based on the pre-selected ATT(g,t) in aggte_ct. Each ATT(g,t) 
    # is weighted with the probability of the being in the group when taking the
    # sum of all ATT(g,t) of a period.
    cte_results <- as.data.frame(matrix(NA, nrow = length(calendar_timelist), ncol = 3))
    colnames(cte_results) <- c("time", "coef", "se")
    
    for (i in seq_along(calendar_timelist)) {
      # Selecting the post-treatment year for which the calendar time effects will be calculated
      calendar_time <- calendar_timelist[i]
      df         <- aggte_ct[aggte_ct$year == calendar_time,]
      # Calcualte the 
      group_prob <- df$probs / sum(df$probs)
      cte_results[i, "time"] <- calendar_time
      cte_results[i, "coef"]   <- sum(df$attgt * group_prob)
    }
    
    # Calculate the aggregated calendar time effect over all different calendar time effects
    agg_att_ct <- mean(cte_results$coef)
    
    # Save final calendar time effects
    results <- list(partial_att = cte_results, overall_att = agg_att_ct)
    return(results)
 } # End of calendar-time effects if-clause


## 4.4 Event Study Design -------------------------
# Preparation of event time data in the case, the event study design is calculated
   if (method %in% c("unbalanced_eventstudy", "balanced_eventstudy")) {
      
    # Indicator on how many periods a group should have experienced a treatment 
    # NULL if balance group is not asked for
    # balance_groups <- 1
    
    # Copy of attgt_df results
    attgt_et <- attgt_probs_df
    
    # Calculate eventtime 
    attgt_et$eventtime <- attgt_et$year - attgt_et$group
    
    # Exclude pre-treatment
    attgt_et <- attgt_et[attgt_et$eventtime >= 0, ]
    
    # Setting the balanced group parameter. How many periods should a group have been
    # treated. E.g., minimum 1 period, all groups with a treatment less than 1 period
    # are excluded
    balance_groups <- 1

### 4.4.1 Event Study Effect ---------------------------------------------------
    # Calculation of the unbalanced ecent study effect
    if (method == "unbalanced_eventstudy") {
      # List of unique eventtime
      
      et_results <- as.data.frame(matrix(NA, nrow = length(eventime_timelist), ncol = 2))
      colnames(et_results) <- c("time", "coef")
      
      for (i in seq_along(eventime_timelist)) {
        # Selecting the post-treatment year for which the calendar time effects will be calculated
        eventime_time <- eventime_timelist[i]
        df         <- attgt_et[attgt_et$eventtime == eventime_time,]
        # Calcualte the 
        group_prob <- df$probs / sum(df$probs)
        et_results[i, "time"]   <- eventime_time
        et_results[i, "coef"]   <- sum(df$attgt * group_prob)
      }
      
      # Calculate aggregated event study effects
      agg_et <- mean(et_results$coef)
      
      # Save final calendar time effects
      results <- list(partial_att = et_results, overall_att = agg_et)
      return(results)
    } # End of unbalanced event-study effects if-clause

## 4.4.2 Eventstudy with balanced group ---------------------------------------
    if (method == "balanced_eventstudy" & !is.null(balance_groups)) {
      
      # Indicator on how many periods a group should have experienced a treatment 
      # NULL if balance group is not asked for
      
      # List of unique eventtime
      grouplist_et <- unique(attgt_et$group)
      
      # Check if event study design is to be calculated with balanced groups or not
      # aka how many periods should a group have been experiencing a treatment/policy
      # -> If yes, than the ATT(g,g+t) will be balanced 
      # -> if not, the event study effect are calculated for every available event time
      
      # Calculate the observed number of event time per group
      att_per_group <- sapply(grouplist_et, function(g){
        df <- attgt_et[attgt_et$group == g,]
        n  <- nrow(df)
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
      attgt_et_bal <- attgt_et[!attgt_et$group %in% groups_to_exclude,]
      
      eventime_timelist <- unique(attgt_et_bal$eventtime)
      
      # Calculation of the event study effect
      att_et_bal <- as.data.frame(matrix(NA, nrow = length(eventime_timelist), ncol = 2))
      colnames(att_et_bal) <- c("time", "coef")
        
      for (i in seq_along(eventime_timelist)) {
        # Selecting the post-treatment year for which the calendar time effects will be calculated
        eventime_time <- eventime_timelist[i]
        df         <- attgt_et[attgt_et_bal$eventtime == eventime_time,]
        # Calcualte the 
        group_prob <- df$probs / sum(df$probs)
        att_et_bal[i, "time"]   <- eventime_time
        att_et_bal[i, "coef"]   <- sum(df$attgt * group_prob)
        }  
        
        
      # att_et_bal <- sapply(eventime_timelist, function(et) {
      #   df         <- attgt_et[attgt_et_bal$eventtime == et, ]
      #   group_prob <- df$probs / sum(df$probs)
      #   atte_et   <- sum(df$attgt * group_prob)
      # }
      # )
      # 
      # att_et_bal <- data.frame(cbind(eventime_timelist, att_et_bal))
      et_bal_results <- att_et_bal[att_et_bal$time < balance_groups + 1,]
      # colnames(et_bal_results) <- c("time", "coef")
      
      # Calculate aggregated event study effects
      agg_bal_et <- mean(et_bal_results$coef)
      
      # Save final calendar time effects
      results <- list(partial_att = et_bal_results, overall_att = agg_bal_et)
      return(results)
    } # End of balanced event study effects if-clause

  } # End of data-preparation if-clause

} # End of calculating_agg_att




est <- calculating_agg_att(data = qwi,
                           year_input = "date_y",
                           group_input = "group",
                           outcome_input = "lnEmp",
                           id_input = "county_id",
                           treatment = treated,
                           formula = spec_formula_log,
                           unconditional_ind = FALSE,
                           method = "unbalanced_eventstudy",
                           balanced = 1)


est <- calculating_agg_att(data = qwi,
                             year_input = "date_y",
                             group_input = "group",
                             outcome_input = "lnEmp",
                             id_input = "county_id",
                             treatment = treated,
                             formula = spec_formula,
                             unconditional_ind = FALSE,
                             method = "calendar_att",
                             balanced = 1)

est 
  
calculate_att_se <- function(data = qwi,
                                 year_input = "date_y",
                                 group_input = "group",
                                 outcome_input = "lnEmp",
                                 id_input = "county_id",
                                 treatment = treated,
                                 formula = spec_formula,
                                 unconditional_ind = FALSE,
                                 method = "group_att",
                                 balanced = FALSE) {
  
  data <- qwi
  data_for_b <- data |>
    rename(id = !!sym(id_input)) |>
    as.data.frame()
  method <- method
  iter <- 10
  b_res_overall <- list()
  b_res_partial <- list()
  for (i in 1:iter) {
  
    n_row <- length(data_for_b$id)  
    # Bernoulli Variates according to Mammen (1993) which are iid
    kappa <- ( sqrt(5) + 1 ) / 2
    p <- kappa / sqrt(5)
    bernoulli_variates <- rbinom(n_row, 1, p) # Is this right?
    
    # Select counties
    county <- unique(data_for_b$id)
    county <- county[bernoulli_variates == 1]
    
    b_data <- data_for_b |> filter(id %in% county)
     
    b_est <- calculating_agg_att(data = b_data,
                                 year_input = year_input,
                                 group_input = group_input,
                                 outcome_input = outcome_input,
                                 id_input = "id",
                                 treatment = treated,
                                 formula = formula,
                                 unconditional_ind = unconditional_ind,
                                 method = method,
                                 balanced = balanced)
    
    
    if (method != "simple_att")  b_res_partial[[i]] <- t(as.matrix(b_est$partial_att$coef))
    b_res_overall[[i]] <- b_est$overall_att
  }
  
  # Calculation of se for partial effects
  b_res_partial <- map_dfr(b_res_partial, as.data.frame)
  se_partial <- apply(b_res_partial, 2, sd)
  
  # Calculation of se for overall effects
  b_res_overall <- unlist(b_res_overall)
  se_overall <- sd(b_res_overall)
  
  est <- calculating_agg_att(data = data,
                             year_input = year_input,
                             group_input = group_input,
                             outcome_input = outcome_input,
                             id_input = id_input,
                             treatment = treated,
                             formula = spec_formula,
                             unconditional_ind = FALSE,
                             method = method,
                             balanced = balanced)
  
  
  est$partial_att$b_se <- se_partial
  est$overall_att <- cbind(est$overall_att, se_overall)
  colnames(est$overall_att) <- c("coef", "b_se")
  
  return(est)
}
  
                calculate_att_se(data = qwi,
                                 year_input = "date_y",
                                 group_input = "group",
                                 outcome_input = "lnEmp",
                                 id_input = "county_id",
                                 treatment = treated,
                                 formula = spec_formula,
                                 unconditional_ind = FALSE,
                                 method = "simple_att",
                                 balanced = 1)
  

method_opt <- c("simple_att", "group_specific", "calendar_att", "unbalanced_eventstudy", 
                "balanced_eventstudy")
  start_time <- Sys.time()
for ( m in method_opt ) {
  paste0("Method: ", m)
  assign(paste0("cond_est_", m), calculate_att_se(data = qwi,
                                             year_input = "date_y",
                                             group_input = "group",
                                             outcome_input = "lnEmp",
                                             id_input = "county_id",
                                             treatment = treated,
                                             formula = spec_formula,
                                             unconditional_ind = FALSE,
                                             method = m,
                                             balanced = 1))
  
}
  end_time <- Sys.time()
  time.taken <- round(end_time - start_time,2)
  
  
  start_time <- Sys.time()
for ( m in method_opt ) {
  paste0("Method: ", m)
  assign(paste0("uncond_est_", m), calculate_att_se(data = qwi,
                                                  year_input = "date_y",
                                                  group_input = "group",
                                                  outcome_input = "lnEmp",
                                                  id_input = "county_id",
                                                  treatment = treated,
                                                  formula = spec_formula,
                                                  unconditional_ind = TRUE,
                                                  method = m,
                                                  balanced = 1))
  
}
  end_time <- Sys.time()
  time.taken <- round(end_time - start_time,2)  


cond_twfe <- plm(lnEmp ~ -1 + post_treat + treated + post_treat * treated + 
               + lwhite_pop + lpoverty + lpop_1000s + lmedian_income_1000s + 
                 leduc, data = qwi, index = c("county_id", "date_y"), model = "within") 

uncond_twfe <- plm(lnEmp ~ -1 + post_treat + treated + post_treat * treated 
               , data = qwi, index = c("county_id", "date_y"), model = "within") 


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
                    xformla = ~lwhite_pop + lpoverty + lpop_1000s + lmedian_income_1000s + leduc,
                    data = qwi,
                    control_group = "nevertreated",
                    bstrap = FALSE,
                    est_method = "dr",
                    base_period = "varying")
summary(out1)


ggdid((out1))

aggte(out1, type = "dynamic", balance_e = NULL)
