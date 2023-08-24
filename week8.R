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

d_prime[1] <- bisection_dprime(cdf_gb, target_prob, lower_bound_gb, upper_bound_gb, tolerance)
d_prime[2] <- bisection_dprime(cdf_ire, target_prob, lower_bound_ire, upper_bound_ire, tolerance)

d_prime <- c(d_prime[1], d_prime[2])

# Evaluate the CDF at d_prime[1] and d_prime[2]
cdf_result_gb$cdf_gb[d_prime[1]]
cdf_result_ire$cdf_ire[d_prime[2]]

# Evaluate the CDF at d_prime[1] - c and d_prime[2] + c
cdf_result_gb$cdf_gb[d_prime[1] - c]
cdf_result_ire$cdf_ire[d_prime[2] + c]

####################
# Choose values of d - w that result in an LOLP of 0.1 for each system without considering the interconnector
# The key to the method of bisection is, in each iteration, checking which half of the interval the equation root lies in.
# Set the interconnector capacity
c <- 1000  # MW

calculate_lolp_gb <- function(d_prime) {
  p_x2_less_d2_prime_ire <- cdf_result_ire$cdf_ire[d_prime[2]]
  p_x1_less_d1_prime_gb <- cdf_result_gb$cdf_gb[d_prime[1]]
  p_x2_greater_d2_prime_c_gb <- 1 - cdf_result_ire$cdf_ire[d_prime[2] + c]
  p_x1_less_d1_prime_c_gb <- cdf_result_gb$cdf_gb[d_prime[1] - c]
  
  middle_sum_gb <- 0
  for (x_2 in d_prime[2]:(d_prime[2] + c - 1)) {
    middle_sum_gb <- middle_sum_gb + freq_ire[x_2] * cdf_result_gb$cdf_gb[d_prime[1] + d_prime[2] - x_2]
  }
  
  lolp_gb <- p_x1_less_d1_prime_gb*p_x2_less_d2_prime_ire + middle_sum_gb + p_x2_greater_d2_prime_c_gb * p_x1_less_d1_prime_c_gb
  
  return(lolp_gb)
}

calculate_lolp_gb(d_prime)
cdf_upper_bound_gb <- cdf_result_gb$cdf_gb[d_prime[1]]
cdf_lower_bound_gb <- cdf_result_gb$cdf_gb[d_prime[1] - c]

calculate_lolp_ire <- function(d_prime) {
  p_x1_greater_d1_prime_c_ire <- 1 - cdf_result_gb$cdf_gb[d_prime[1] + c]
  p_x2_less_d2_prime_ire <- cdf_result_ire$cdf_ire[d_prime[2]]
  p_x1_less_d1_prime_gb <- cdf_result_gb$cdf_gb[d_prime[1]]
  p_x2_less_d2_prime_c_ire <- cdf_result_ire$cdf_ire[d_prime[2] - c]
  
  middle_sum_ire <- 0
  for (x_1 in d_prime[1]:(d_prime[1] + c - 1)) {
    middle_sum_ire <- middle_sum_ire + freq_gb[x_1] * cdf_result_ire$cdf_ire[d_prime[1] + d_prime[2] - x_1]
  }
  
  lolp_ire <- p_x2_less_d2_prime_ire*p_x1_less_d1_prime_gb + middle_sum_ire + p_x1_greater_d1_prime_c_ire * p_x2_less_d2_prime_c_ire
  
  return(lolp_ire)
}

calculate_lolp_ire(d_prime)


# Checking if d_prime values are NA
is.na(d_prime[1])
is.na(d_prime[2])

# Checking if d_prime values are within valid range
1 <= d_prime[1] && d_prime[1] <= length(cdf_result_ire$cdf_ire)
1 <= d_prime[2] && d_prime[2] <= length(cdf_result_ire$cdf_ire)

# Checking if the modified d_prime values are within valid range
1 <= d_prime[1] + c && d_prime[1] + c <= length(cdf_result_ire$cdf_ire)
1 <= d_prime[2] - c && d_prime[2] - c <= length(cdf_result_ire$cdf_ire)

# Checking if cdf_result_ire$cdf_ire and freq_ire contain NA values
any(is.na(cdf_result_ire$cdf_ire))
any(is.na(freq_ire))


# peak demand periods
# filter data to only consider peak demand periods
# assume peak demand typically occurs between 6PM and 8PM
InRe$Time <- strptime(InRe$Time, "%H:%M:%S")
# create a new column that indicates whether a given row is in the peak demand period
InRe$IsPeak <- with(InRe, Time >= strptime("18:00:00", "%H:%M:%S") & Time <= strptime("20:00:00", "%H:%M:%S"))
InRePeak <- InRe[InRe$IsPeak == TRUE, ]

# Calculate LOLP for each peak period
lolp_gb_peak <- numeric(nrow(InRePeak))  # Initialize array to store lolp for each peak period in gb
lolp_ire_peak <- numeric(nrow(InRePeak))  # Initialize array to store lolp for each peak period in ire

for (i in seq_along(InRePeak$Time)) {
  d_gb <- InRePeak$GBdem_r[i]
  d_ire <- InRePeak$Idem_r[i]
  w_gb <- InRePeak$GBwind_r[i]
  w_ire <- InRePeak$Iwind_r[i]
  
  d_prime <- c(d_gb - w_gb, d_ire - w_ire)
  # checks whether the calculated d_prime values for GB (both the original and the one decreased by c) 
  # are within the valid range of indices for the cumulative distribution function cdf_result_gb$cdf_gb.
  if(1 <= d_prime[1] && d_prime[1] <= length(cdf_result_gb$cdf_gb) &&
     1 <= d_prime[1] - c && d_prime[1] - c <= length(cdf_result_gb$cdf_gb)) {
    lolp_gb_peak[i] <- calculate_lolp_gb(d_prime)
  } else {
    print(paste("d_prime values for GB are out of the expected range at time index", i))
  }
  
  if(1 <= d_prime[2] && d_prime[2] <= length(cdf_result_ire$cdf_ire) &&
     1 <= d_prime[2] - c && d_prime[2] - c <= length(cdf_result_ire$cdf_ire)) {
    lolp_ire_peak[i] <- calculate_lolp_ire(d_prime)
  } else {
    print(paste("d_prime values for IRE are out of the expected range at time index", i))
  }
}

# Calculate LOLE by summing up LOLP over all peak periods
lole_gb_peak <- sum(lolp_gb_peak, na.rm = TRUE)
lole_ire_peak <- sum(lolp_ire_peak, na.rm = TRUE)

print(lole_gb_peak)
print(lole_ire_peak)
# during peak demand periods, it is expected that GB will not be able to meet demand for approximately 2.2 hours per year, 
# and Ireland will not be able to meet demand for approximately 0.97 hours per year.



# renewable generation peaks
# we may interested in how the system operates when there's a lot of renewable generation
# choose time indices that correspond to high wind generation periods
# Calculate the sum of wind generation for each time index
wind_sum <- InRe$GBwind_r + InRe$Iwind_r

# Get the indices of the top n periods of highest wind generation
n <- 100  # Adjust this number based on your specific needs
highest_wind_indices <- order(-wind_sum)[1:n]

# Initialize vectors to store the LOLP values for the peak wind periods
lolp_gb_wind_peak <- numeric(length(highest_wind_indices))
lolp_ire_wind_peak <- numeric(length(highest_wind_indices))

# Calculate LOLP for each of the peak wind periods
for (i in seq_along(highest_wind_indices)) {
  time_index <- highest_wind_indices[i]
  
  d_gb <- InRe$GBdem_r[time_index]
  d_ire <- InRe$Idem_r[time_index]
  w_gb <- InRe$GBwind_r[time_index]
  w_ire <- InRe$Iwind_r[time_index]
  
  d_prime <- c(d_gb - w_gb, d_ire - w_ire)
  
  if (1 <= d_prime[1] && d_prime[1] <= length(cdf_result_gb$cdf_gb) &&
      1 <= d_prime[1] - c && d_prime[1] - c <= length(cdf_result_gb$cdf_gb)) {
    lolp_gb_wind_peak[i] <- calculate_lolp_gb(d_prime)
  } else {
    print(paste("d_prime values for GB are out of the expected range at time index", i))
  }
  
  if (1 <= d_prime[2] && d_prime[2] <= length(cdf_result_ire$cdf_ire) &&
      1 <= d_prime[2] + c && d_prime[2] + c <= length(cdf_result_ire$cdf_ire)) {
    lolp_ire_wind_peak[i] <- calculate_lolp_ire(d_prime)
  } else {
    print(paste("d_prime values for IRE are out of the expected range at time index", i))
  }
}

# Calculate LOLE for the peak wind periods
lole_gb_wind_peak <- sum(lolp_gb_wind_peak) / length(highest_wind_indices)
lole_ire_wind_peak <- sum(lolp_ire_wind_peak) / length(highest_wind_indices)

print(lole_gb_wind_peak)
print(lole_ire_wind_peak)
# lole for both gb and ire are extremely low during the periods of highest wind generation
# lole close to zero, it means that the system is expected to be able to meet demand almost always during those periods


# low demand periods
# can present challenges for grid operation
# First, identify the time indices corresponding to periods of low demand
low_demand_indices_gb <- which(InRe$GBdem_r < quantile(InRe$GBdem_r, 0.1))  # bottom 10% demand periods
low_demand_indices_ire <- which(InRe$Idem_r < quantile(InRe$Idem_r, 0.1))  # bottom 10% demand periods

# Now, calculate LOLE for these periods
lole_gb_low_demand <- 0
lole_ire_low_demand <- 0

for (i in low_demand_indices_gb) {
  d_gb <- InRe$GBdem_r[i]
  w_gb <- InRe$GBwind_r[i]
  d_prime <- c(d_gb - w_gb, d_ire - w_ire)
  
  if(1 <= d_prime[1] && d_prime[1] <= length(cdf_result_gb$cdf_gb) &&
     1 <= d_prime[1] - c && d_prime[1] - c <= length(cdf_result_gb$cdf_gb)) {
    lole_gb_low_demand <- lole_gb_low_demand + calculate_lolp_gb(d_prime)
  } else {
    print(paste("d_prime values for GB are out of the expected range at time index", i))
  }
}

for (i in low_demand_indices_ire) {
  d_ire <- InRe$Idem_r[i]
  w_ire <- InRe$Iwind_r[i]
  d_prime <- c(d_gb - w_gb, d_ire - w_ire)
  
  if(1 <= d_prime[2] && d_prime[2] <= length(cdf_result_ire$cdf_ire) &&
     1 <= d_prime[2] + c && d_prime[2] + c <= length(cdf_result_ire$cdf_ire)) {
    lole_ire_low_demand <- lole_ire_low_demand + calculate_lolp_ire(d_prime)
  } else {
    print(paste("d_prime values for IRE are out of the expected range at time index", i))
  }
}

print(lole_gb_low_demand) # essentially zero for all practical purposes, it suggests that during periods of low demand in GB, the system is highly unlikely to experience a loss of load
print(lole_ire_low_demand) # calculated LOLE is exactly zero, suggesting that during periods of low demand in ire, there is no expectation of load loss



InRe$Time <- strptime(InRe$Time, "%H:%M:%S")
InRe$IsPeak <- with(InRe, Time >= strptime("18:00:00", "%H:%M:%S") & Time <= strptime("20:00:00", "%H:%M:%S"))
InRePeak <- InRe[InRe$IsPeak == TRUE, ]

# Calculate LOLP for each peak period
lolp_gb_peak <- numeric(nrow(InRePeak))  # Initialize array to store LOLP for each peak period in GB
lolp_ire_peak <- numeric(nrow(InRePeak))  # Initialize array to store LOLP for each peak period in IRE

for (i in seq_along(InRePeak$Time)) {
  d_gb <- InRePeak$GBdem_r[i]
  d_ire <- InRePeak$Idem_r[i]
  w_gb <- InRePeak$GBwind_r[i]
  w_ire <- InRePeak$Iwind_r[i]
  
  # Check if any of the values are NA
  if (is.na(d_gb) | is.na(d_ire) | is.na(w_gb) | is.na(w_ire)) {
    print(paste("NA values detected at time index", i))
    next  # Skip to the next iteration
  }
  
  d_prime <- c(d_gb - w_gb, d_ire - w_ire)
  
  if(1 <= d_prime[1] && d_prime[1] <= length(cdf_result_gb$cdf_gb) &&
     1 <= d_prime[1] - c && d_prime[1] - c <= length(cdf_result_gb$cdf_gb)) {
    lolp_gb_peak[i] <- calculate_lolp_gb(d_prime)
  } else {
    print(paste("d_prime values for GB are out of the expected range at time index", i))
  }
  
  if(1 <= d_prime[2] && d_prime[2] <= length(cdf_result_ire$cdf_ire) &&
     1 <= d_prime[2] - c && d_prime[2] - c <= length(cdf_result_ire$cdf_ire)) {
    lolp_ire_peak[i] <- calculate_lolp_ire(d_prime)
  } else {
    print(paste("d_prime values for IRE are out of the expected range at time index", i))
  }
}

# Calculate LOLE by summing up LOLP over all peak periods
lole_gb_peak <- sum(lolp_gb_peak, na.rm = TRUE)
lole_ire_peak <- sum(lolp_ire_peak, na.rm = TRUE)

print(lole_gb_peak)
print(lole_ire_peak)


