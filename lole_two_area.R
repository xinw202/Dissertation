#########################################################
# peak demand periods
# Define peak demand hours - for instance, 5-9 pm
peak_hours <- c(17, 18, 19, 20, 21)

# Filter the InRe for peak demand periods
InRePeak <- InRe %>% 
  # converts the 'Time' column to a POSIXct format which allows for time-based operations
  mutate(Time = as.POSIXct(Time, format = "%H:%M:%S")) %>% 
  # filters the rows of the InRe where the hour of the 'Time' column is in peak_hours
  filter(hour(Time) %in% peak_hours) 
  
# initialize numeric vectors to store the LOLP values for each time point during peak hours
lolp_gb_peak <- numeric(nrow(InRePeak))
lolp_ire_peak <- numeric(nrow(InRePeak))

# Loop through the filtered InRe, calculate LOLP and LOLE for each time point
for (i in seq_along(InRePeak$Time)) {
  d_gb <- InRePeak$GBdem_r[i]
  d_ire <- InRePeak$Idem_r[i]
  w_gb <- InRePeak$GBwind_r[i]
  w_ire <- InRePeak$Iwind_r[i]
  
  d_prime <- c(d_gb - w_gb, d_ire - w_ire)
  
  lolp_gb_peak[i] <- calculate_lolp_gb(d_prime)
  lolp_ire_peak[i] <- calculate_lolp_ire(d_prime)
}

# Calculate LOLE
lole_gb_peak <- sum(lolp_gb_peak)
lole_ire_peak <- sum(lolp_ire_peak)

# Print LOLE
print(paste("LOLE for GB during peak demand periods:", lole_gb_peak))
print(paste("LOLE for Ireland during peak demand periods:", lole_ire_peak))


##############################################
# renewable generation peaks
# Identify the periods of high wind generation
# sorts the InRe in descending order by the sum of GB and Ire wind generation
InRe_wind_peak <- InRe[order(InRe$GBwind_r + InRe$Iwind_r, decreasing = TRUE), ]


# initializes a vector of NA values with the same length as the number of rows in InRe_wind_peak to store the lolp
lolp_gb_wind_peak <- rep(NA, nrow(InRe_wind_peak))
lolp_ire_wind_peak <- rep(NA, nrow(InRe_wind_peak))

# Calculate the LOLP for each period
for (i in seq_along(InRe_wind_peak$Time)) {
  d_gb <- InRe_wind_peak$GBdem_r[i]
  d_ire <- InRe_wind_peak$Idem_r[i]
  w_gb <- InRe_wind_peak$GBwind_r[i]
  w_ire <- InRe_wind_peak$Iwind_r[i]
  
  d_prime <- c(d_gb - w_gb, d_ire - w_ire)
  
  if(1 <= d_prime[1] && d_prime[1] <= length(cdf_result_gb$cdf_gb) &&
     1 <= d_prime[1] - c && d_prime[1] - c <= length(cdf_result_gb$cdf_gb)) {
    lolp_gb_wind_peak[i] <- calculate_lolp_gb(d_prime)
  } else {
    print(paste("d_prime values for GB are out of the expected range at time index", i))
  }
  
  if(1 <= d_prime[2] && d_prime[2] <= length(cdf_result_ire$cdf_ire) &&
     1 <= d_prime[2] - c && d_prime[2] - c <= length(cdf_result_ire$cdf_ire)) {
    lolp_ire_wind_peak[i] <- calculate_lolp_ire(d_prime)
  } else {
    print(paste("d_prime values for IRE are out of the expected range at time index", i))
  }
}

# Calculate LOLE
lole_gb_wind_peak <- sum(lolp_gb_wind_peak, na.rm = TRUE) 
lole_ire_wind_peak <- sum(lolp_ire_wind_peak, na.rm = TRUE) 

# Print LOLE
print(paste("LOLE for GB during high wind generation periods:", lole_gb_wind_peak))
print(paste("LOLE for Ireland during high wind generation periods:", lole_ire_wind_peak))

#############################################
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



