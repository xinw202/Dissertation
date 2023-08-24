
library(dplyr)
InRe<- read.table("InterconnectionData_Rescaled.txt")
GBconv<- read.table("GB_anonymised_conv.txt")
Inpeak<- read.table("InterconnectionData_peak.txt")
Iconv<- read.table("I_conv.txt")
####################################################
cap_gb<-GBconv$Capacity
oprob_gb<- 1-GBconv$Availability

pmfXfcn_gb<-function(cap_gb, oprob_gb){ ##cap is X_u
  
  if(length(cap_gb)!=length(oprob_gb)){stop("Length of capacity vector not equal to length of availability probabilities")}
  
  if(all(cap_gb!= round(cap_gb))){stop("Capacities not integers")}
  
  MaxCap_gb<-sum(cap_gb) # sum X_u 从u=1到n
  
  pmf_gb<-rep(0, times=MaxCap_gb+1)
  
  pmf_gb[1]<-1
  
  for(i in 1:length(cap_gb)){
    
    pmfold_gb<-pmf_gb
    
    pmf_gb = pmfold_gb*oprob_gb[i]
    
    pmf_gb[(cap_gb[i]+1):(MaxCap_gb+1)] = pmf_gb[(cap_gb[i]+1):(MaxCap_gb+1)] + pmfold_gb[1:(MaxCap_gb+1-cap_gb[i])]*(1-oprob_gb[i])
    
    
    
  }
  
  list(freq_gb = pmf_gb, vals_gb = seq(from=0, to = MaxCap_gb, by = 1))
  
}

pmf_result_gb <- pmfXfcn_gb(cap_gb, oprob_gb)
freq_gb <- pmf_result_gb$freq_gb
vals_pmf_gb <- pmf_result_gb$vals_gb
#########################################
ex1_gb<- mean(cap_gb)
vx2_gb<- var(cap_gb)

ex2_gb<- sum(oprob_gb*cap_gb) #E(X)
vx2_gb<- sum(cap_gb^2*oprob_gb*(1-oprob_gb)) # Var(X)
pmfXfcn_gb(cap_gb, oprob_gb)

#########################################
# Construct CDF of X
cdfXfcn_gb <- function(cap_gb, oprob_gb) {
  pmf_result_gb <- pmfXfcn_gb(cap_gb, oprob_gb)
  freq_gb <- pmf_result_gb$freq_gb
  cdf_gb <- cumsum(freq_gb)
  list(cdf_gb = cdf_gb, vals_gb = pmf_result_gb$vals_gb)
}

cdf_result_gb <- cdfXfcn_gb(cap_gb, oprob_gb)
cdf_gb <- cdf_result_gb$cdf_gb
vals_gb <- cdf_result_gb$vals_gb

####################################################
cap_ire<-Iconv$Capacity
oprob_ire<- 1-Iconv$Availability

pmfXfcn_ire<-function(cap_ire, oprob_ire){ ##cap is X_u
  
  if(length(cap_ire)!=length(oprob_ire)){stop("Length of capacity vector not equal to length of availability probabilities")}
  
  if(all(cap_ire!= round(cap_ire))){stop("Capacities not integers")}
  
  MaxCap_ire<-sum(cap_ire) # sum X_u 从u=1到n
  
  pmf_ire<-rep(0, times = MaxCap_ire + 1)
  
  pmf_ire[1]<-1
  
  for(i in 1:length(cap_ire)){
    
    pmfold_ire<-pmf_ire
    
    pmf_ire = pmfold_ire*oprob_ire[i]
    
    pmf_ire[(cap_ire[i]+1):(MaxCap_ire+1)] = pmf_ire[(cap_ire[i]+1):(MaxCap_ire+1)] + pmfold_ire[1:(MaxCap_ire+1-cap_ire[i])]*(1-oprob_ire[i])
    
    
    
  }
  
  list(freq_ire = pmf_ire, vals_ire = seq(from=0, to = MaxCap_ire, by = 1))
  
}

pmf_result_ire <- pmfXfcn_ire(cap_ire, oprob_ire)
freq_ire <- pmf_result_ire$freq_ire
vals_pmf_ire <- pmf_result_ire$vals_ire
#########################################
# Construct CDF of X
cdfXfcn_ire <- function(cap_ire, oprob_ire) {
  pmf_result_ire <- pmfXfcn_ire(cap_ire, oprob_ire)
  freq_ire <- pmf_result_ire$freq_ire
  cdf_ire <- cumsum(freq_ire)
  list(cdf_ire = cdf_ire, vals_ire = pmf_result_ire$vals_ire)
}

cdf_result_ire <- cdfXfcn_ire(cap_ire, oprob_ire)
cdf_ire <- cdf_result_ire$cdf_ire
vals_ire <- cdf_result_ire$vals_ire

#########################################
# For each historic record, calculate use the CDF to calculate the LOLP given the value of (demand minus wind)
historic_records_gb <- InRe$GBdem_r-InRe$GBwind_r

calculate_LOLP_gb <- function(cdf_gb, vals_gb, historic_records_gb) {
  if (historic_records_gb < 0) {
    return(1)  # LOLP is 1 if demand minus wind is negative (indicating a deficit)
  } else {
    idx_gb <- min(which(vals_gb >= historic_records_gb))
    if (is.na(idx_gb)) {
      return(0)  # LOLP is 0 if demand minus wind exceeds the maximum capacity
    } else {
      return(cdf_gb[idx_gb])
    }
  }
}

lolp_values_gb <- vector(length = length(historic_records_gb))

for (i in 1:length(historic_records_gb)) {
  demand_minus_wind_gb <- historic_records_gb[i]  # Extract demand minus wind for the current record
  
  lolp_gb <- calculate_LOLP_gb(cdf_gb, vals_gb, demand_minus_wind_gb)  # Calculate LOLP using the CDF
  
  lolp_values_gb[i] <- lolp_gb  # Store the calculated LOLP value
}

###########
# Add these LOLPs together to get the LOLE. 
# LOLE can be conditional on a repeat of the wind/demand from a given historic year, 
conditional_lole_gb <- sum(lolp_values_gb)/7 # 7 is length(lolp_values), i.e 7 winters
print(conditional_lole_gb)
# or averaged over all of the historic years
averaged_lole_gb <- sum(lolp_values_gb) / length(lolp_values_gb)
print(averaged_lole_gb)

########################################
historic_records_ire <- InRe$Idem_r-InRe$Iwind_r

calculate_LOLP_ire <- function(cdf_ire, vals_ire, historic_records_ire) {
  if (historic_records_ire < 0) {
    return(1)  # LOLP is 1 if demand minus wind is negative (indicating a deficit)
  } else {
    idx_ire <- min(which(vals_ire >= historic_records_ire))
    if (is.na(idx_ire)) {
      return(0)  # LOLP is 0 if demand minus wind exceeds the maximum capacity
    } else {
      return(cdf_ire[idx_ire])
    }
  }
}

lolp_values_ire <- vector(length = length(historic_records_ire))

for (i in 1:length(historic_records_ire)) {
  demand_minus_wind_ire <- historic_records_ire[i]  # Extract demand minus wind for the current record
  
  lolp_ire <- calculate_LOLP_ire(cdf_ire, vals_ire, demand_minus_wind_ire)  # Calculate LOLP using the CDF
  
  lolp_values_ire[i] <- lolp_ire  # Store the calculated LOLP value
}

conditional_lole_ire <- sum(lolp_values_ire)/7 # 7 is length(lolp_values), i.e 7 winters
print(conditional_lole_ire)


########################################
# LOLEs conditional on 7 winters
# 2007-2008
InRe_2007 <- InRe %>% filter(Date >= "2007-06-01" & Date <= "2008-05-31")
historic_records_2007_gb <- InRe_2007$GBdem_r-InRe_2007$GBwind_r

lolp_values_2007_gb <- vector(length = length(historic_records_2007_gb))

for (i in 1:length(historic_records_2007_gb)) {
  demand_minus_wind_2007_gb <- historic_records_2007_gb[i]  # Extract demand minus wind for the current record
  
  lolp_2007_gb <- calculate_LOLP_gb(cdf_gb, vals_gb, demand_minus_wind_2007_gb)  # Calculate LOLP using the CDF
  
  lolp_values_2007_gb[i] <- lolp_2007_gb  # Store the calculated LOLP value
}
conditional_lole_2007_gb <- sum(lolp_values_2007_gb)
print(conditional_lole_2007_gb)

# 2008-2009
InRe_2008 <- InRe %>% filter(Date >= "2008-06-01" & Date <= "2009-05-31")
historic_records_2008_gb <- InRe_2008$GBdem_r-InRe_2008$GBwind_r

lolp_values_2008_gb <- vector(length = length(historic_records_2008_gb))

for (i in 1:length(historic_records_2008_gb)) {
  demand_minus_wind_2008_gb <- historic_records_2008_gb[i]  # Extract demand minus wind for the current record
  
  lolp_2008_gb <- calculate_LOLP_gb(cdf_gb, vals_gb, demand_minus_wind_2008_gb)  # Calculate LOLP using the CDF
  
  lolp_values_2008_gb[i] <- lolp_2008_gb  # Store the calculated LOLP value
}
conditional_lole_2008_gb <- sum(lolp_values_2008_gb)
print(conditional_lole_2008_gb)
#####################################
# Calculate the baseline LOLE for the system using the historic records.
baseline_LOLE_gb <- sum(lolp_values_gb) / length(historic_records_gb)

# Replace the wind values in the historic records with a single constant EFC value. 
# This means setting the demand minus wind for each historic record to the demand minus the EFC value.
efc <- 0  # Initial EFC value
demand_minus_efc_gb <- historic_records_gb - efc

# Calculate the LOLP values using the updated demand minus EFC values.
lolp_values_efc_gb <- vector(length = length(historic_records_gb))
for (i in 1:length(historic_records_gb)) {
  demand_minus_efc_gb <- historic_records_gb[i] - efc
  
  lolp_gb <- calculate_LOLP_gb(cdf_gb, vals_gb, demand_minus_efc_gb)
  
  lolp_values_efc_gb[i] <- lolp_gb
}

# Adjust the EFC value until the average LOLE matches the baseline LOLE.
tolerance <- 0.001  # Tolerance for matching the baseline LOLE
lower_bound_gb <- 0  # Lower bound of EFC search
upper_bound_gb <- max(historic_records_gb)  # Upper bound of EFC search

while (upper_bound_gb - lower_bound_gb > tolerance) {
  efc_gb = (lower_bound_gb + upper_bound_gb) / 2  # Midpoint EFC value
  
  lolp_values_efc_gb <- calculate_LOLP_gb(cdf_gb, vals_gb, historic_records_gb - efc_gb)
  calculated_LOLE_gb <- sum(lolp_values_efc_gb) / length(historic_records_gb)
  
  if (calculated_LOLE_gb < baseline_LOLE_gb) {
    upper_bound_gb <- efc_gb
  } else {
    lower_bound_gb <- efc_gb
  }
}

# efc value matches the baseline LOLE
efc_gb  

#########################################
bisection_dprime <- function(cdf, target_prob, lower_bound, upper_bound, tolerance) {
  while (upper_bound - lower_bound > tolerance) {
    mid_point <- (lower_bound + upper_bound) / 2
    mid_point_prob <- cdf[mid_point]
    if (mid_point_prob < target_prob) {
      lower_bound <- mid_point
    } else {
      upper_bound <- mid_point
    }
  }
  return((lower_bound + upper_bound) / 2)
}

# Use the bisection method to find d_prime[1] and d_prime[2]
target_prob <- 0.1
lower_bound_gb <- min(vals_gb)
upper_bound_gb <- max(vals_gb)
lower_bound_ire <- min(vals_ire)
upper_bound_ire <- max(vals_ire)
tolerance <- 0.001  

d_prime_gb <- bisection_dprime(cdf_gb, target_prob, lower_bound_gb, upper_bound_gb, tolerance)
d_prime_ire <- bisection_dprime(cdf_ire, target_prob, lower_bound_ire, upper_bound_ire, tolerance)

d_prime_all <- c(d_prime_gb, d_prime_ire)

# Evaluate the CDF at d_prime_gb and d_prime_ire
cdf_result_gb$cdf_gb[d_prime_gb]
cdf_result_ire$cdf_ire[d_prime_ire]

# Set the interconnector capacity
c <- 1000  # MW

# Evaluate the CDF at d_prime_gb - c and d_prime_ire + c
cdf_result_gb$cdf_gb[d_prime_all[1] - c]
cdf_result_ire$cdf_ire[d_prime_all[2] - c]



####################
# Choose values of d - w that result in an LOLP of 0.1 for each system without considering the interconnector
# The key to the method of bisection is, in each iteration, checking which half of the interval the equation root lies in.

calculate_lolp_gb <- function(d_prime_all) {

  p_x2_less_d2_prime_ire <- cdf_result_ire$cdf_ire[d_prime_all[2]]
  p_x1_less_d1_prime_gb <- cdf_result_gb$cdf_gb[d_prime_all[1]]
  p_x2_greater_d2_prime_c_gb <- 1 - cdf_result_ire$cdf_ire[d_prime_all[2] + c]
  #p_x2_greater_d2_prime_c_gb <- 1 - cdf_result_ire$cdf_ire[d_prime_all[2]]
  
  p_x1_less_d1_prime_c_gb <- cdf_result_gb$cdf_gb[d_prime_all[1] - c]
  
  middle_sum_gb <- 0
  for (x_2 in d_prime_all[2]:(d_prime_all[2] + c - 1)) {
    middle_sum_gb <- middle_sum_gb + freq_ire[x_2] * cdf_result_gb$cdf_gb[d_prime_all[1] + d_prime_all[2] - x_2]
  }
  
  lolp_gb <- p_x1_less_d1_prime_gb*p_x2_less_d2_prime_ire + middle_sum_gb + p_x2_greater_d2_prime_c_gb * p_x1_less_d1_prime_c_gb
  
  return(lolp_gb)
}



calculate_lolp_gb(d_prime_all)
cdf_upper_bound_gb <- cdf_result_gb$cdf_gb[d_prime_all[1]]
cdf_upper_bound_gb
cdf_lower_bound_gb <- cdf_result_gb$cdf_gb[d_prime_all[1] - c]
cdf_lower_bound_gb

calculate_lolp_ire <- function(d_prime_all) {

  p_x1_greater_d1_prime_c_ire <- 1 - cdf_result_gb$cdf_gb[d_prime_all[1] + c]
  p_x2_less_d2_prime_ire <- cdf_result_ire$cdf_ire[d_prime_all[2]]
  p_x1_less_d1_prime_gb <- cdf_result_gb$cdf_gb[d_prime_all[1]]
  p_x2_less_d2_prime_c_ire <- cdf_result_ire$cdf_ire[d_prime_all[2] - c]
  
  middle_sum_ire <- 0
  for (x_1 in d_prime_all[1]:(d_prime_all[1] + c - 1)) {
    middle_sum_ire <- middle_sum_ire + freq_gb[x_1] * cdf_result_ire$cdf_ire[d_prime_all[1] + d_prime_all[2] - x_1]
  }
  
  lolp_ire <- p_x2_less_d2_prime_ire*p_x1_less_d1_prime_gb + middle_sum_ire + p_x1_greater_d1_prime_c_ire * p_x2_less_d2_prime_c_ire
  
  return(lolp_ire)
}

calculate_lolp_ire(d_prime_all)
cdf_lower_bound_ire <- cdf_result_ire$cdf_ire[d_prime_all[2] - c]
cdf_lower_bound_ire


# Choose a specific point in time (index of the dataset)
time_index <- which.max(InRe$Idem_r)
#time_index <- which.max(InRe$GBdem_r)

# Extract the relevant data at the chosen time
d_gb_c1 <- InRe$GBdem_r[time_index]
d_ire_c1 <- InRe$Idem_r[time_index]
w_gb_c1 <- InRe$GBwind_r[time_index]
w_ire_1 <- InRe$Iwind_r[time_index]

# Calculate d_prime for each period
d_prime_c1 <- c(d_gb_c1 - w_gb_c1, d_ire_c1 - w_ire_1)

# Now call the calculate_lolp functions with the correct arguments
lolp_gb_c1 <- calculate_lolp_gb(d_prime_c1)
lolp_ire_c1 <- calculate_lolp_ire(d_prime_c1)

print(lolp_gb_c1)
print(lolp_ire_c1)

