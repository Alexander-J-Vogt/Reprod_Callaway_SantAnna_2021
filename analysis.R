################################################################################
#Reproduction of Callaway & Sant'Anna (2021)
################################################################################

# Remove exisitng environment
rm(list = ls(all = TRUE))

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


#---- Start up & Define variables ----------------------------------------------

# covariates formula
spec_formula <- ~-1+white_pop_2000_perc+poverty_allages_1997_perc+pop_2000_nr_1000s+median_income_1997_1000s+HS_1990_perc

# copy of qwi
data <- qwi

# relevant numbers
timelist <- unique(qwi$date_y)
grouplist <- unique(qwi$group)
num_of_cases <- length(timelist) * length(grouplist)

# create empty data frame and/or list
attgt.df <- as.data.frame(matrix(NA, nrow = num_of_cases, ncol = 3))
colnames(attgt.df) <- c("attgt", "group", "year")
att.gt.ls <- ls()

# create empty matrix
number <- 1
n_unique <- length(unique(qwi$county_id))
nr_group <- length(grouplist)
nr_times <- length(timelist)


if_matrix <- Matrix::Matrix(data = 0, nrow = n_unique, 
                            ncol = nr_group * (nr_times - 1), 
                            sparse = TRUE)
nrow(if_matrix)


# ---- Calculation and later loop ----------------------------------------------

# 
# if(period >= group) {
#   reference_year <- group - 1
# } else {
#   reference_year <- period - 1 
# }



# current group indicator (should get overwritten once we loop over groups)
data$g <- ifelse(data$group == 2004, 1, 0)
data$c <- ifelse(data$group == 0   , 1, 0)

# Select data in pre- & post-treatment period for relevant group and never-treated
data <- subset(data, date_y %in% c(2003, 2004)) # must be replaced by reference year and current period in loop



# indicator for influence matrix
index_data <- data[data$date_y == 2004,]
index_inffunc <- (index_data$g == 1) | (index_data$c == 1)
n_sample <- length(unique(index_data$county_id))

# select group & nevertreated (control)
index_gc <- (data$g == 1) | (data$c == 1)
data_sel <- data[index_gc,]
n_subset <- length(unique(data_sel$county_id))

# Create treatment indicator and date variables as character
data_sel <- data_sel|> 
  mutate(date = paste0("y", date_y), treat = ifelse(group == 2004, 1, 0))

# Use BMisc::panel2cs2 to transform the two period panel dataset into a
# into a cross-sectional data set, which is required for the function, which
# calculates the Doubly Robust DiD-estimator
data_cs <- BMisc::panel2cs2(data_sel, yname = "lnEmp", idname = "county_id", 
                            tname = "date", balance_panel = FALSE)

# If the number of rows is odd, the BMisc::panel2cs2 function produces missing 
# in either .y1 or .y0. Thus, the NA's need to be removed, otherwise we don't
# yield any DiD-estimator
data_cs <- data_cs[!is.na(.y1),]
data_cs <- data_cs[!is.na(.y0),]

# Save a matrix of covariates
covariates <- model.matrix(spec_formula, data_cs)

# att of group g at time point t
att <- DRDID::drdid_panel(y1 = data_cs$.y1, 
                          y0 = data_cs$.y0,
                          D  = data_cs$treat,
                          covariates = covariates,
                          inffunc    = TRUE,
                          boot       = FALSE)

# recover att
att.gt.ls[[1]]<- list(attgt = att$ATT, group = 2004, period = 2003)

# Data Frame of ATTgt
# attgt.df[1, 1] <- att$ATT 
# attgt.df[1, 2] <- 2004
# attgt.df[1, 3] <- 2003

# recover influence function
if_vector <- rep(0, n_sample)
# estimate of influence function, weighted by relative sample size
if_vector[index_inffunc] <- (n_subset / n_sample) * att$att.inf.func
# save vector of round x into the column
if_matrix[,1] <- if_vector







att$att.inf.func
 

test_qwi <- qwi[date_y %in% c(2004, 2005)] 
test_qwi <- test_qwi |> 
  mutate(date = as.character(paste0("y", date_y)))

cov <-  model.matrix()












out2 <-  did::compute.aggte(yname = "lnEmp",
                       tname = "date_y",
                       idname = "county_id",
                       gname = "group",
                       xformla = ~white_pop_2000_perc+poverty_allages_1997_perc+pop_2000_nr_1000s+median_income_1997_1000s+HS_1990_perc,
                       data = qwi,
                       est_method = "dr",
                       base_period = "varying")










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
