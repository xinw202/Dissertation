library(dplyr)
library(ggplot2)
InRe<- read.table("InterconnectionData_Rescaled.txt")
GBconv<- read.table("GB_anonymised_conv.txt")
Iconv<- read.table("I_conv.txt")
####################################################
cap_gb<-GBconv$Capacity
oprob_gb<- 1-GBconv$Availability

pmfXfcn_gb<-function(cap_gb, oprob_gb){ ##cap is X_u
  
  if(length(cap_gb)!=length(oprob_gb)){stop("Length of capacity vector not equal to length of availability probabilities")}
  
  if(all(cap_gb!= round(cap_gb))){stop("Capacities not integers")}
  
  MaxCap_gb<-sum(cap_gb) # sum X_u from u=1 to n
  
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
  
  MaxCap_ire<-sum(cap_ire) # sum X_u from u=1 to n
  
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

ex1_ire<- mean(cap_ire)
vx2_ire<- var(cap_ire)

ex2_ire<- sum(oprob_ire*cap_ire) #E(X)
vx2_ire<- sum(cap_ire^2*oprob_ire*(1-oprob_ire)) # Var(X)


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
conditional_lole_gb

########################################
historic_records_ire <- InRe$Idem_r-InRe$Iwind_r

calculate_LOLP_ire <- function(cdf_ire, vals_ire, demand_minus_wind_ire) {
  if (demand_minus_wind_ire < 0) {
    return(0)  # LOLP is 0 if there's an excess of wind capacity
  } else {
    idx_ire <- min(which(vals_ire >= demand_minus_wind_ire))
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
d_prime_all

# Evaluate the CDF at d_prime_gb and d_prime_ire
cdf_result_gb$cdf_gb[d_prime_gb]
cdf_result_ire$cdf_ire[d_prime_ire]

# interconnector capacity
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


calculate_lolp_gb_1 <- function(d_prime_all) {
  # Add a boundary check to prevent indices going below 1
  d_prime_all[1] <- max(1, d_prime_all[1])
  d_prime_all[2] <- max(1, d_prime_all[2])
  
  # If c is less than 0, set it to 0
  c <- max(0, c)
  
  p_x2_less_d2_prime_ire <- cdf_result_ire$cdf_ire[d_prime_all[2]]
  p_x1_less_d1_prime_gb <- cdf_result_gb$cdf_gb[d_prime_all[1]]
  p_x2_greater_d2_prime_c_gb <- 1 - cdf_result_ire$cdf_ire[min(d_prime_all[2] + c, length(cdf_result_ire$cdf_ire))]
  
  p_x1_less_d1_prime_c_gb <- cdf_result_gb$cdf_gb[max(1, d_prime_all[1] - c)]
  
  middle_sum_gb <- 0
  for (x_2 in max(1, d_prime_all[2]):min(d_prime_all[2] + c - 1, length(freq_ire))) {
    middle_sum_gb <- middle_sum_gb + freq_ire[x_2] * cdf_result_gb$cdf_gb[max(1, d_prime_all[1] + d_prime_all[2] - x_2)]
  }
  
  lolp_gb <- p_x1_less_d1_prime_gb*p_x2_less_d2_prime_ire + middle_sum_gb + p_x2_greater_d2_prime_c_gb * p_x1_less_d1_prime_c_gb
  
  return(lolp_gb)
}



calculate_lolp_ire_1 <- function(d_prime_all) {
  # Add a boundary check to prevent indices going below 1
  d_prime_all[1] <- max(1, d_prime_all[1])
  d_prime_all[2] <- max(1, d_prime_all[2])
  
  # If c is less than 0, set it to 0
  c <- max(0, c)
  
  p_x1_greater_d1_prime_c_ire <- 1 - cdf_result_gb$cdf_gb[min(d_prime_all[1] + c, length(cdf_result_gb$cdf_gb))]
  p_x2_less_d2_prime_ire <- cdf_result_ire$cdf_ire[d_prime_all[2]]
  p_x1_less_d1_prime_gb <- cdf_result_gb$cdf_gb[d_prime_all[1]]
  p_x2_less_d2_prime_c_ire <- cdf_result_ire$cdf_ire[max(1, d_prime_all[2] - c)]
  
  middle_sum_ire <- 0
  for (x_1 in max(1, d_prime_all[1]):min(d_prime_all[1] + c - 1, length(freq_gb))) {
    middle_sum_ire <- middle_sum_ire + freq_gb[x_1] * cdf_result_ire$cdf_ire[max(1, d_prime_all[1] + d_prime_all[2] - x_1)]
  }
  
  lolp_ire <- p_x2_less_d2_prime_ire*p_x1_less_d1_prime_gb + middle_sum_ire + p_x1_greater_d1_prime_c_ire * p_x2_less_d2_prime_c_ire
  
  return(lolp_ire)
}




# calculate LOLE without interconnection
#########################################
# For GB
# For each historic record, calculate use the CDF to calculate the lolp given the value of (demand - wind)
historic_records_gb <- InRe$GBdem_r-InRe$GBwind_r

calculate_LOLP_gb <- function(cdf_gb, vals_gb, historic_records_gb) {
  if (historic_records_gb < 0) {
    return(1)  # LOLP is 1 if demand - wind is negative
  } else {
    idx_gb <- min(which(vals_gb >= historic_records_gb))
    if (is.na(idx_gb)) {
      return(0)  # LOLP is 0 if demand - wind exceeds the max capacity
    } else {
      return(cdf_gb[idx_gb])
    }
  }
}

lolp_values_gb <- vector(length = length(historic_records_gb))

for (i in 1:length(historic_records_gb)) {
  demand_minus_wind_gb <- historic_records_gb[i]  # Extract demand - wind for the current record
  
  lolp_gb <- calculate_LOLP_gb(cdf_gb, vals_gb, demand_minus_wind_gb)  # Calculate lolp using the CDF
  
  lolp_values_gb[i] <- lolp_gb  # Store the calculated lolp value
}

###########
# Add these lolps together to get the lole. 
# lole can be conditional on a repeat of the wind/demand from a given historic year, 
conditional_lole_gb <- sum(lolp_values_gb)/7 # 7 is length(lolp_values), i.e 7 winters
print(conditional_lole_gb)
# or averaged over all of the historic years
averaged_lole_gb <- sum(lolp_values_gb) / length(lolp_values_gb)
print(averaged_lole_gb)

########################################
# For Ire
historic_records_ire <- InRe$Idem_r-InRe$Iwind_r

calculate_LOLP_ire <- function(cdf_ire, vals_ire, historic_records_ire) {
  if (historic_records_ire < 0) {
    return(1)  # lolp is 1 if demand - wind is negative
  } else {
    idx_ire <- min(which(vals_ire >= historic_records_ire))
    if (is.na(idx_ire)) {
      return(0)  # lolp is 0 if demand - wind exceeds the maximum capacity
    } else {
      return(cdf_ire[idx_ire])
    }
  }
}

lolp_values_ire <- vector(length = length(historic_records_ire))

for (i in 1:length(historic_records_ire)) {
  demand_minus_wind_ire <- historic_records_ire[i]  # Extract - minus wind for the current record
  
  lolp_ire <- calculate_LOLP_ire(cdf_ire, vals_ire, demand_minus_wind_ire)  # Calculate lolp using the CDF
  
  lolp_values_ire[i] <- lolp_ire  # Store the calculated lolp value
}

conditional_lole_ire <- sum(lolp_values_ire)/7 # 7 is length(lolp_values), i.e 7 winters
print(conditional_lole_ire)


######################################################################
# calculate LOLE with interconnection
# The LOLP values for all historic records
lolp_values_gb_interconnected <- vector(length = length(InRe$GBdem_r))
lolp_values_ire_interconnected <- vector(length = length(InRe$Idem_r))

# Interconnector capacity
c <- 0  # MW

for (i in 1:nrow(InRe)) {
  d_gb <- InRe$GBdem_r[i]
  w_gb <- InRe$GBwind_r[i]
  d_ire <- InRe$Idem_r[i]
  w_ire <- InRe$Iwind_r[i]
  d_prime <- c(d_gb - w_gb, d_ire - w_ire)
  
  lolp_gb_interconnected <- calculate_lolp_gb_1(d_prime)  # Calculate lolp for GB
  lolp_ire_interconnected <- calculate_lolp_ire_1(d_prime)  # Calculate lolp for Ireland
  
  lolp_values_gb_interconnected[i] <- lolp_gb_interconnected  # Store the lolp value for GB
  lolp_values_ire_interconnected[i] <- lolp_ire_interconnected  # Store the lolp value for Ireland
}

# Calculate LOLE for GB and Ireland
conditional_lole_gb_interconnected <- sum(lolp_values_gb_interconnected)/7 # 7 is length(lolp_values), i.e 7 winters
conditional_lole_ire_interconnected <- sum(lolp_values_ire_interconnected)/7 # 7 is length(lolp_values), i.e 7 winters

print(conditional_lole_gb_interconnected)
print(conditional_lole_ire_interconnected)


##########################################################################
# calculate LOLE without interconnection but with demand reduced by c in each area 
c <- 0

# For GB
historic_records_gb_c <- InRe$GBdem_r - InRe$GBwind_r - c

calculate_LOLP_gb_c <- function(cdf_gb, vals_gb, historic_records_gb_c) {
  if (historic_records_gb_c < 0) {
    return(1)  # lolp is 1 if demand - wind is negative (indicating a deficit)
  } else {
    idx_gb <- min(which(vals_gb >= historic_records_gb_c))
    if (is.na(idx_gb)) {
      return(0)  # lolp is 0 if demand - wind exceeds the maximum capacity
    } else {
      return(cdf_gb[idx_gb])
    }
  }
}

lolp_values_gb_c <- vector(length = length(historic_records_gb_c))

for (i in 1:length(historic_records_gb_c)) {
  demand_minus_wind_gb_c <- historic_records_gb_c[i]  # Extract demand - wind for the current record
  
  lolp_gb_c <- calculate_LOLP_gb_c(cdf_gb, vals_gb, demand_minus_wind_gb_c)  # Calculate lolp using the CDF
  
  lolp_values_gb_c[i] <- lolp_gb_c  # Store the calculated lolp value
}

conditional_lole_gb_c <- sum(lolp_values_gb_c)/7 # 7 is length(lolp_values), i.e 7 winters
print(conditional_lole_gb_c)

########################################
# For Ire
historic_records_ire_c <- InRe$Idem_r - InRe$Iwind_r - c

calculate_LOLP_ire_c <- function(cdf_ire, vals_ire, historic_records_ire_c) {
  if (historic_records_ire_c < 0) {
    return(1)  # lolp is 1 if demand - wind is negative (indicating a deficit)
  } else {
    idx_ire <- min(which(vals_ire >= historic_records_ire_c))
    if (is.na(idx_ire)) {
      return(0)  # lolp is 0 if demand - wind exceeds the max capacity
    } else {
      return(cdf_ire[idx_ire])
    }
  }
}

lolp_values_ire_c <- vector(length = length(historic_records_ire_c))

for (i in 1:length(historic_records_ire_c)) {
  demand_minus_wind_ire_c <- historic_records_ire_c[i]  # Extract demand minus wind for the current record
  
  lolp_ire_c <- calculate_LOLP_ire_c(cdf_ire, vals_ire, demand_minus_wind_ire_c)  # Calculate lolp using the CDF
  
  lolp_values_ire_c[i] <- lolp_ire_c  # Store the calculated lolp value
}

conditional_lole_ire_c <- sum(lolp_values_ire_c)/7 # 7 is length(lolp_values), i.e 7 winters
print(conditional_lole_ire_c)



# maximum interconnector capacity
max_c <- 3000  

# Extend the length of the probability mass function and cumulative distribution function arrays for both GB and IRE

# For GB
cdf_result_gb$cdf_gb <- c(cdf_result_gb$cdf_gb, rep(1, max_c))
freq_gb <- c(freq_gb, rep(0, max_c))

# For IRE
cdf_result_ire$cdf_ire <- c(cdf_result_ire$cdf_ire, rep(1, max_c))
freq_ire <- c(freq_ire, rep(0, max_c))

# Initialize a data frame to store the results
results <- data.frame()

c_values <- seq(0, 1000, 50)  # Now the c_values ranges from 0 to 3000

# Calculate LOLE for each value of c
for (c in c_values) {
  # Initialize vectors to store the LOLP values for each time period
  lolp_values_gb_interconnected <- vector(length = length(InRe$GBdem_r[i]))
  lolp_values_ire_interconnected <- vector(length = length(InRe$Idem_r[i]))
  
  for (i in 1:nrow(InRe)) {
    d_gb <- InRe$GBdem_r[i]
    w_gb <- InRe$GBwind_r[i]
    d_ire <- InRe$Idem_r[i]
    w_ire <- InRe$Iwind_r[i]
    d_prime <- c(max(1, d_gb - w_gb), max(1, d_ire - w_ire))  
    
    lolp_gb_interconnected <- calculate_lolp_gb_1(d_prime)  
    lolp_ire_interconnected <- calculate_lolp_ire_1(d_prime)  
    
    lolp_values_gb_interconnected[i] <- lolp_gb_interconnected  
    lolp_values_ire_interconnected[i] <- lolp_ire_interconnected  
  }
  
  
  
  # Calculate lole for gb and ire
  lole_gb_interconnected <- sum(lolp_values_gb_interconnected) / 7
  lole_ire_interconnected <- sum(lolp_values_ire_interconnected) / 7
  
  # Add the results to the data frame
  results <- rbind(results, data.frame(c = c, LOLE_GB = lole_gb_interconnected, LOLE_Ire = lole_ire_interconnected))
}

# Plot the results
library(ggplot2)
ggplot(results, aes(x = c)) +
  geom_line(aes(y = LOLE_GB), color = "blue") +
  geom_line(aes(y = LOLE_Ire), color = "red") +
  labs(x = "Interconnector Capacity (MW)", y = "LOLE (hours/year)", 
       title = "LOLE for GB and Ireland against Interconnector Capacity", 
       subtitle = "Blue: GB, Red: Ireland") +
  coord_cartesian(xlim = c(0, max(c_values))) +  
  theme_minimal()

plot(c_values, results$LOLE_GB, type="l", col="blue", 
     ylim=c(0, max(max(results$LOLE_GB), max(results$LOLE_Ire))),
     xlab="Interconnector Capacity (MW)", ylab="LOLE (hours/year)", 
     xaxt="n", yaxt="n")
lines(c_values, results$LOLE_Ire, col="red")
legend("topright", legend=c("GB", "Ireland"), col=c("blue", "red"), lty=1, cex=0.8)

# Enhancing axes
axis(1, at=seq(0, max(c_values), by=500), las=0, cex.axis=0.9, tcl=-0.3)
axis(2, las=2, cex.axis=0.9, tcl=-0.3)



InRe_2007 <- InRe %>% filter(Date >= "2007-06-01" & Date <= "2008-05-31")
InRe_2008 <- InRe %>% filter(Date >= "2008-06-01" & Date <= "2009-05-31")
InRe_2009 <- InRe %>% filter(Date >= "2009-06-01" & Date <= "2010-05-31")
InRe_2010 <- InRe %>% filter(Date >= "2010-06-01" & Date <= "2011-05-31")
InRe_2011 <- InRe %>% filter(Date >= "2011-06-01" & Date <= "2012-05-31")
InRe_2012 <- InRe %>% filter(Date >= "2012-06-01" & Date <= "2013-05-31")
InRe_2013 <- InRe %>% filter(Date >= "2013-06-01" & Date <= "2014-05-31")


calculate_lolp_gb_1 <- function(d_prime_all) {
  # Add a boundary check to prevent indices going below 1
  d_prime_all[1] <- max(1, d_prime_all[1])
  d_prime_all[2] <- max(1, d_prime_all[2])
  
  # If c is less than 0, set it to 0
  c <- max(0, c)
  
  p_x2_less_d2_prime_ire <- cdf_result_ire$cdf_ire[d_prime_all[2]]
  p_x1_less_d1_prime_gb <- cdf_result_gb$cdf_gb[d_prime_all[1]]
  p_x2_greater_d2_prime_c_gb <- 1 - cdf_result_ire$cdf_ire[min(d_prime_all[2] + c, length(cdf_result_ire$cdf_ire))]
  
  p_x1_less_d1_prime_c_gb <- cdf_result_gb$cdf_gb[max(1, d_prime_all[1] - c)]
  
  middle_sum_gb <- 0
  for (x_2 in max(1, d_prime_all[2]):min(d_prime_all[2] + c - 1, length(freq_ire))) {
    middle_sum_gb <- middle_sum_gb + freq_ire[x_2] * cdf_result_gb$cdf_gb[max(1, d_prime_all[1] + d_prime_all[2] - x_2)]
  }
  
  lolp_gb <- p_x1_less_d1_prime_gb*p_x2_less_d2_prime_ire + middle_sum_gb + p_x2_greater_d2_prime_c_gb * p_x1_less_d1_prime_c_gb
  
  return(lolp_gb)
}



calculate_lolp_ire_1 <- function(d_prime_all) {
  # Add a boundary check to prevent indices going below 1
  d_prime_all[1] <- max(1, d_prime_all[1])
  d_prime_all[2] <- max(1, d_prime_all[2])
  
  # If c is less than 0, set it to 0
  c <- max(0, c)
  
  p_x1_greater_d1_prime_c_ire <- 1 - cdf_result_gb$cdf_gb[min(d_prime_all[1] + c, length(cdf_result_gb$cdf_gb))]
  p_x2_less_d2_prime_ire <- cdf_result_ire$cdf_ire[d_prime_all[2]]
  p_x1_less_d1_prime_gb <- cdf_result_gb$cdf_gb[d_prime_all[1]]
  p_x2_less_d2_prime_c_ire <- cdf_result_ire$cdf_ire[max(1, d_prime_all[2] - c)]
  
  middle_sum_ire <- 0
  for (x_1 in max(1, d_prime_all[1]):min(d_prime_all[1] + c - 1, length(freq_gb))) {
    middle_sum_ire <- middle_sum_ire + freq_gb[x_1] * cdf_result_ire$cdf_ire[max(1, d_prime_all[1] + d_prime_all[2] - x_1)]
  }
  
  lolp_ire <- p_x2_less_d2_prime_ire*p_x1_less_d1_prime_gb + middle_sum_ire + p_x1_greater_d1_prime_c_ire * p_x2_less_d2_prime_c_ire
  
  return(lolp_ire)
}


# Preparing the data subsets for each winter
winters <- list(
  "2007" = InRe %>% filter(Date >= "2007-06-01" & Date <= "2008-05-31"),
  "2008" = InRe %>% filter(Date >= "2008-06-01" & Date <= "2009-05-31"),
  "2009" = InRe %>% filter(Date >= "2009-06-01" & Date <= "2010-05-31"),
  "2010" = InRe %>% filter(Date >= "2010-06-01" & Date <= "2011-05-31"),
  "2011" = InRe %>% filter(Date >= "2011-06-01" & Date <= "2012-05-31"),
  "2012" = InRe %>% filter(Date >= "2012-06-01" & Date <= "2013-05-31"),
  "2013" = InRe %>% filter(Date >= "2013-06-01" & Date <= "2014-05-31")
)

# Function to calculate LOLE for a given winter and interconnection capacity
calculate_lole_for_year <- function(data_year) {
  historic_records_gb <- data_year$GBdem_r - data_year$GBwind_r
  lolp_values_gb <- vector(length = length(historic_records_gb))
  
  for (i in 1:length(historic_records_gb)) {
    demand_minus_wind_gb <- historic_records_gb[i]
    d_prime <- c(max(1, demand_minus_wind_gb), max(1, data_year$Idem_r[i] - data_year$Iwind_r[i])) 
    lolp_gb <- calculate_lolp_gb_1(d_prime)
    lolp_values_gb[i] <- lolp_gb
  }
  return(sum(lolp_values_gb))
}

# Calculate LOLE for each year and interconnector capacity
c_values <- seq(0, 1000, 50)  # Interconnection capacities

df_results <- data.frame()

for (year in names(winters)) {
  data_year <- winters[[year]]
  
  for (j in 1:length(c_values)) {
    c <- c_values[j]
    lole <- calculate_lole_for_year(data_year)
    
    # Append to the results dataframe
    df_results <- rbind(df_results, data.frame(Year = year, Capacity = c, LOLE = lole))
  }
}

# Plotting the results
ggplot(df_results, aes(x = Capacity, y = LOLE, color = Year)) +
  geom_line(size = 1) +  
  scale_x_continuous(breaks = seq(0, max(c_values), by = 100),   
                     limits = c(0, max(c_values)),  
                     expand = c(0, 0)) +  
  scale_y_continuous(breaks = seq(0, max(df_results$LOLE, na.rm = TRUE), by = 0.5),  
                     limits = c(0, max(df_results$LOLE, na.rm = TRUE)),  
                     expand = c(0, 0)) +  
  labs(x = "Interconnector Capacity (MW)", 
       y = "LOLE (hours/year)", 
       # title = "Impact of Interconnector Capacity on LOLE", 
       subtitle = "Based on 7 Winters") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black", size = 12),  
        axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, color = "black", size = 12),  
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),  
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),  
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  
        plot.subtitle = element_text(hjust = 0.5))  





# Function to calculate LOLE for a given winter and interconnection capacity
calculate_lole_for_year_ire <- function(data_year) {
  historic_records_ire <- data_year$Idem_r - data_year$Iwind_r
  lolp_values_ire <- vector(length = length(historic_records_ire))
  
  for (i in 1:length(historic_records_ire)) {
    demand_minus_wind_ire <- historic_records_ire[i]
    d_prime <- c(max(1, data_year$GBdem_r[i] - data_year$GBwind_r[i]), max(1, demand_minus_wind_ire)) 
    lolp_ire <- calculate_lolp_ire_1(d_prime)
    lolp_values_ire[i] <- lolp_ire
  }
  return(sum(lolp_values_ire))
}

# Calculate LOLE for each year and interconnector capacity
c_values <- seq(0, 1000, 50)  # Interconnection capacities

df_results_ire <- data.frame()

for (year in names(winters)) {
  data_year <- winters[[year]]
  
  for (j in 1:length(c_values)) {
    c <- c_values[j]
    lole <- calculate_lole_for_year_ire(data_year)
    
    # Append to the results dataframe
    df_results_ire <- rbind(df_results_ire, data.frame(Year = year, Capacity = c, LOLE = lole))
  }
}

# Plotting the results
ggplot(df_results_ire, aes(x = Capacity, y = LOLE, color = Year)) +
  geom_line(size = 1) +  
  scale_x_continuous(breaks = seq(0, max(c_values), by = 100),   
                     limits = c(0, max(c_values)),  
                     expand = c(0, 0)) +  
  scale_y_continuous(breaks = seq(0, max(df_results_ire$LOLE, na.rm = TRUE), by = 0.5),  
                     limits = c(0, max(df_results_ire$LOLE, na.rm = TRUE)),  
                     expand = c(0, 0)) +  
  labs(x = "Interconnector Capacity (MW)", 
       y = "LOLE (hours/year)", 
       # title = "Impact of Interconnector Capacity on LOLE (Ireland)", 
       subtitle = "Based on 7 Winters") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black", size = 12),  
        axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, color = "black", size = 12),  
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),  
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),  
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  
        plot.subtitle = element_text(hjust = 0.5))


# Extract the day with the maximum (demand - wind) for each year

max_demand_wind_days <- sapply(winters, function(data_year) {
  difference <- data_year$GBdem_r - data_year$GBwind_r
  index_max <- which.max(difference)
  return(list(date = as.character(data_year$Date[index_max]), max_difference = difference[index_max]))
})

# Display the results
print(max_demand_wind_days)


# Preparing the data subsets for each winter
winters <- list(
  "2007" = InRe %>% filter(Date >= "2007-06-01" & Date <= "2008-05-31"),
  "2008" = InRe %>% filter(Date >= "2008-06-01" & Date <= "2009-05-31"),
  "2009" = InRe %>% filter(Date >= "2009-06-01" & Date <= "2010-05-31"),
  "2010" = InRe %>% filter(Date >= "2010-06-01" & Date <= "2011-05-31"),
  "2011" = InRe %>% filter(Date >= "2011-06-01" & Date <= "2012-05-31"),
  "2012" = InRe %>% filter(Date >= "2012-06-01" & Date <= "2013-05-31"),
  "2013" = InRe %>% filter(Date >= "2013-06-01" & Date <= "2014-05-31")
)

# Modified function to calculate LOLE for GB
calculate_lole_for_year <- function(data, demand_col = "GBdem_r", wind_col = "GBwind_r") {
  historic_records_gb <- data[[demand_col]] - data[[wind_col]]
  lolp_values_gb <- vector(length = length(historic_records_gb))
  
  for (i in 1:length(historic_records_gb)) {
    demand_minus_wind_gb <- historic_records_gb[i]
    d_prime <- c(max(1, demand_minus_wind_gb), max(1, data$Idem_r[i] - data$Iwind_r[i])) 
    lolp_gb <- calculate_lolp_gb_1(d_prime)
    lolp_values_gb[i] <- lolp_gb
  }
  return(sum(lolp_values_gb))
}

# Modified function to calculate LOLE for Ireland
calculate_lole_for_year_ire <- function(data, demand_col = "Idem_r", wind_col = "Iwind_r") {
  historic_records_ire <- data[[demand_col]] - data[[wind_col]]
  lolp_values_ire <- vector(length = length(historic_records_ire))
  
  for (i in 1:length(historic_records_ire)) {
    demand_minus_wind_ire <- historic_records_ire[i]
    d_prime <- c(max(1, data$GBdem_r[i] - data$GBwind_r[i]), max(1, demand_minus_wind_ire)) 
    lolp_ire <- calculate_lolp_ire_1(d_prime)
    lolp_values_ire[i] <- lolp_ire
  }
  return(sum(lolp_values_ire))
}



# Function to compute LOLP for GB
calculate_LOLP_gb <- function(cdf_gb, vals_gb, historic_records_gb) {
  
  lolp_values_gb <- ifelse(historic_records_gb < 0, 1, 
                           ifelse(historic_records_gb > max(vals_gb), 0, 
                                  cdf_gb[pmin(length(cdf_gb), sapply(historic_records_gb, function(x) min(which(vals_gb >= x))))]))
  return(lolp_values_gb)
}

# Function to compute LOLP for Ireland
calculate_LOLP_ire <- function(cdf_ire, vals_ire, historic_records_ire) {
  
  lolp_values_ire <- ifelse(historic_records_ire < 0, 1, 
                            ifelse(historic_records_ire > max(vals_ire), 0, 
                                   cdf_ire[pmin(length(cdf_ire), sapply(historic_records_ire, function(x) min(which(vals_ire >= x))))]))
  return(lolp_values_ire)
}



# Adjust wind capacity by a factor of 1.5
factor <- 1.5
InRe$GBwind_r <- InRe$GBwind_r * factor
InRe$Iwind_r <- InRe$Iwind_r * factor

# Define a sequence of shifts
demand_shifts <- seq(-2000, 2000, by=50)

# For GB
best_shift_gb <- NULL
min_diff_gb <- Inf
for(shift in demand_shifts) {
  InRe_shifted <- InRe
  InRe_shifted$GBdem_r <- InRe_shifted$GBdem_r + shift
  historic_records_shifted_gb <- InRe_shifted$GBdem_r - InRe_shifted$GBwind_r
  lolp_values_shifted_gb <- sapply(historic_records_shifted_gb, function(x) calculate_LOLP_gb(cdf_gb, vals_gb, x))
  conditional_lole_shifted_gb <- sum(lolp_values_shifted_gb) / 7
  diff_gb <- abs(conditional_lole_shifted_gb - 3)
  if(diff_gb < min_diff_gb) {
    min_diff_gb <- diff_gb
    best_shift_gb <- shift
  }
}
print(paste("Best shift for GB: ", best_shift_gb))

# For Ireland
best_shift_ire <- NULL
min_diff_ire <- Inf
for(shift in demand_shifts) {
  InRe_shifted <- InRe
  InRe_shifted$Idem_r <- InRe_shifted$Idem_r + shift
  historic_records_shifted_ire <- InRe_shifted$Idem_r - InRe_shifted$Iwind_r
  lolp_values_shifted_ire <- sapply(historic_records_shifted_ire, function(x) calculate_LOLP_ire(cdf_ire, vals_ire, x))
  conditional_lole_shifted_ire <- sum(lolp_values_shifted_ire) / 7
  diff_ire <- abs(conditional_lole_shifted_ire - 3)
  if(diff_ire < min_diff_ire) {
    min_diff_ire <- diff_ire
    best_shift_ire <- shift
  }
}
print(paste("Best shift for Ireland: ", best_shift_ire))


historic_records_gb_s <- (InRe$GBdem_r) -InRe$GBwind_r*1.5

calculate_LOLP_gb_s <- function(cdf_gb, vals_gb, historic_records_gb_s) {
  if (historic_records_gb_s < 0) {
    return(1)  # LOLP is 1 if demand minus wind is negative (indicating a deficit)
  } else {
    idx_gb <- min(which(vals_gb >= historic_records_gb_s))
    if (is.na(idx_gb)) {
      return(0)  # LOLP is 0 if demand minus wind exceeds the maximum capacity
    } else {
      return(cdf_gb[idx_gb])
    }
  }
}

lolp_values_gb_s <- vector(length = length(historic_records_gb_s))

for (i in 1:length(historic_records_gb_s)) {
  demand_minus_wind_gb_s <- historic_records_gb_s[i]  # Extract demand minus wind for the current record
  
  lolp_gb_s <- calculate_LOLP_gb_s(cdf_gb, vals_gb, demand_minus_wind_gb_s)  # Calculate LOLP using the CDF
  
  lolp_values_gb_s[i] <- lolp_gb_s  # Store the calculated LOLP value
}


###########
# Add these LOLPs together to get the LOLE. 
# LOLE can be conditional on a repeat of the wind/demand from a given historic year, 
conditional_lole_gb_s <- sum(lolp_values_gb_s)/7 # 7 is length(lolp_values), i.e 7 winters
print(conditional_lole_gb_s)



########################################
historic_records_ire_s <- (InRe$Idem_r+265)-InRe$Iwind_r*1.5

calculate_LOLP_ire_s <- function(cdf_ire, vals_ire, historic_records_ire_s) {
  if (historic_records_ire_s < 0) {
    return(1)  # LOLP is 1 if demand minus wind is negative (indicating a deficit)
  } else {
    idx_ire <- min(which(vals_ire >= historic_records_ire_s))
    if (is.na(idx_ire)) {
      return(0)  # LOLP is 0 if demand minus wind exceeds the maximum capacity
    } else {
      return(cdf_ire[idx_ire])
    }
  }
}

lolp_values_ire_s <- vector(length = length(historic_records_ire_s))

for (i in 1:length(historic_records_ire_s)) {
  demand_minus_wind_ire_s <- historic_records_ire_s[i]  # Extract demand minus wind for the current record
  
  lolp_ire_s <- calculate_LOLP_ire_s(cdf_ire, vals_ire, demand_minus_wind_ire_s)  # Calculate LOLP using the CDF
  
  lolp_values_ire_s[i] <- lolp_ire_s  # Store the calculated LOLP value
}

conditional_lole_ire_s <- sum(lolp_values_ire_s)/7 # 7 is length(lolp_values), i.e 7 winters
print(conditional_lole_ire_s)



historic_records_ire_s <- (InRe$Idem_r) - InRe$Iwind_r*1.5

calculate_LOLP_ire_s <- function(cdf_ire, vals_ire, demand_minus_wind_ire_s) {
  if (demand_minus_wind_ire_s < 0) {
    return(0)  # LOLP is 0 if there's an excess of wind capacity
  } else {
    idx_ire <- min(which(vals_ire >= demand_minus_wind_ire_s))
    if (is.na(idx_ire)) {
      return(0)  # LOLP is 0 if demand minus wind exceeds the maximum capacity
    } else {
      return(cdf_ire[idx_ire])
    }
  }
}


lolp_values_ire_s <- vector(length = length(historic_records_ire_s))

for (i in 1:length(historic_records_ire_s)) {
  demand_minus_wind_ire_s <- historic_records_ire_s[i]  # Extract demand minus wind for the current record
  
  lolp_ire_s <- calculate_LOLP_ire_s(cdf_ire, vals_ire, demand_minus_wind_ire_s)  # Calculate LOLP using the CDF
  
  lolp_values_ire_s[i] <- lolp_ire_s  # Store the calculated LOLP value
}

conditional_lole_ire_s <- sum(lolp_values_ire_s) /7
print(conditional_lole_ire_s)


library(ggplot2)
demand_adjustments <- seq(0, 2000, by=50)
lole_values_original <- numeric(length(demand_adjustments))
lole_values_adjusted_wind <- numeric(length(demand_adjustments))

for (j in 1:length(demand_adjustments)) {
  adjustment <- demand_adjustments[j]
  
  # Compute LOLE for original wind with demand adjustment
  historic_records_gb_adjusted <- InRe$GBdem_r + adjustment - InRe$GBwind_r
  lolp_values_gb_adjusted <- sapply(historic_records_gb_adjusted, function(record) calculate_LOLP_gb(cdf_gb, vals_gb, record))
  lole_values_original[j] <- sum(lolp_values_gb_adjusted) / 7
  
  # Compute LOLE for 1.5x wind with demand adjustment
  historic_records_gb_s_adjusted <- (InRe$GBdem_r + adjustment) - 1.5 * InRe$GBwind_r
  lolp_values_gb_s_adjusted <- sapply(historic_records_gb_s_adjusted, function(record) calculate_LOLP_gb_s(cdf_gb, vals_gb, record))
  lole_values_adjusted_wind[j] <- sum(lolp_values_gb_s_adjusted) / 7
}


df_plot <- data.frame(DemandIncrease=demand_adjustments, 
                      LOLE_Original=lole_values_original, 
                      LOLE_AdjustedWind=lole_values_adjusted_wind)

ggplot(df_plot, aes(x=DemandIncrease)) +
  geom_line(aes(y=LOLE_Original, color="Original Wind")) +
  geom_line(aes(y=LOLE_AdjustedWind, color="1.5x Wind")) +
  geom_hline(yintercept=3, linetype="dashed", color="red") +
  labs(# title="Impact of Demand Increase on LOLE", 
    x="Increase in Demand", y="LOLE", 
    color="Scenario") +
  theme_minimal()




demand_adjustments <- seq(0, 2000, by=50)
lole_values_original <- numeric(length(demand_adjustments))
lole_values_adjusted_wind <- numeric(length(demand_adjustments))

for (j in 1:length(demand_adjustments)) {
  adjustment <- demand_adjustments[j]
  
  # Compute LOLE for original wind with demand adjustment
  historic_records_gb_adjusted <- InRe$GBdem_r + adjustment - InRe$GBwind_r
  lolp_values_gb_adjusted <- sapply(historic_records_gb_adjusted, function(record) calculate_LOLP_gb(cdf_gb, vals_gb, record))
  lole_values_original[j] <- sum(lolp_values_gb_adjusted) / 7
  
  # Compute LOLE for 1.5x wind with demand adjustment
  historic_records_gb_s_adjusted <- (InRe$GBdem_r + adjustment) - 1.5 * InRe$GBwind_r
  lolp_values_gb_s_adjusted <- sapply(historic_records_gb_s_adjusted, function(record) calculate_LOLP_gb_s(cdf_gb, vals_gb, record))
  lole_values_adjusted_wind[j] <- sum(lolp_values_gb_s_adjusted) / 7
}


df_plot <- data.frame(DemandIncrease=demand_adjustments, 
                      LOLE_Original=lole_values_original, 
                      LOLE_AdjustedWind=lole_values_adjusted_wind)

ggplot(df_plot, aes(x=DemandIncrease)) +
  geom_line(aes(y=LOLE_Original, color="Original Wind")) +
  geom_line(aes(y=LOLE_AdjustedWind, color="1.5x Wind")) +
  geom_hline(yintercept=3, linetype="dashed", color="red") +
  labs(# title="Impact of Demand Increase on LOLE", 
    x="Increase in Demand", y="LOLE", 
    color="Scenario") +
  theme_minimal()



ggplot(df_plot, aes(x=DemandIncrease)) +
  geom_line(aes(y=LOLE_Original, color="Original Wind")) +
  geom_line(aes(y=LOLE_AdjustedWind, color="1.5x Wind")) +
  geom_hline(yintercept=3, linetype="dashed", color="red") +
  labs(
    # title="Impact of Demand Increase on LOLE", 
    x="Increase in Demand (MW)", y="LOLE (hour/year)", 
    color="Scenario"
  ) +
  theme_bw() +  # Use black & white theme for clearer background
  theme(
    axis.line = element_line(size = 1.5, colour = "black"), # Enhance axis line
    axis.title.x = element_text(size=14),  
    axis.title.y = element_text(size=14), 
    axis.text.x = element_text(size=12),   
    axis.text.y = element_text(size=12)    
  )



# Compute PMF and CDF for Ireland
pmf_result_ire <- pmfXfcn_ire(cap_ire, oprob_ire)
freq_ire <- pmf_result_ire$freq_ire
vals_ire <- pmf_result_ire$vals_ire
cdf_ire <- cumsum(freq_ire)

# Compute LOLE values for Ireland with varying demand adjustments and wind capacity scenarios
demand_adjustments <- seq(0, 2000, by=50)
lole_values_original <- numeric(length(demand_adjustments))
lole_values_adjusted_wind <- numeric(length(demand_adjustments))

for (j in 1:length(demand_adjustments)) {
  adjustment <- demand_adjustments[j]
  
  # Original wind with demand adjustment
  historic_records_ire_adjusted <- InRe$Idem_r + adjustment - InRe$Iwind_r
  lolp_values_ire_adjusted <- sapply(historic_records_ire_adjusted, function(record) calculate_LOLP_ire(cdf_ire, vals_ire, record))
  lole_values_original[j] <- sum(lolp_values_ire_adjusted) / 7
  
  # 1.5x wind with demand adjustment
  historic_records_ire_s_adjusted <- (InRe$Idem_r + adjustment) - 1.5 * InRe$Iwind_r
  lolp_values_ire_s_adjusted <- sapply(historic_records_ire_s_adjusted, function(record) calculate_LOLP_ire(cdf_ire, vals_ire, record))
  lole_values_adjusted_wind[j] <- sum(lolp_values_ire_s_adjusted) / 7
}


# Plotting the results
df_plot <- data.frame(DemandIncrease=demand_adjustments, 
                      LOLE_Original=lole_values_original, 
                      LOLE_AdjustedWind=lole_values_adjusted_wind)

ggplot(df_plot, aes(x=DemandIncrease)) +
  geom_line(aes(y=LOLE_Original, color="Original Wind")) +
  geom_line(aes(y=LOLE_AdjustedWind, color="1.5x Wind")) +
  geom_hline(yintercept=3, linetype="dashed", color="red") +
  labs(x="Increase in Demand (MW)", y="LOLE (hour/year)", color="Scenario") +
  theme_minimal() +
  coord_cartesian(xlim=c(0, 200)) +
  theme(
    axis.line = element_line(size = 1.5, color = "black"),
    axis.text = element_text(size=12, color="black"),
    panel.grid.major = element_line(color="gray80"),
    panel.grid.minor = element_line(color="gray95")
  )

