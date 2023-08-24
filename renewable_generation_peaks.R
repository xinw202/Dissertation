##############################################
# renewable generation peaks
# Identify the periods of high wind generation
# sorts the InRe in descending order by the sum of GB and Ire wind generation
InRe_wind_peak_2007 <- InRe_2007[order(InRe_2007$GBwind_r + InRe_2007$Iwind_r, decreasing = TRUE), ]

# initializes a vector of NA values with the same length as the number of rows in InRe_wind_peak to store the lolp
lolp_gb_wind_peak_2007 <- rep(NA, nrow(InRe_wind_peak_2007))
lolp_ire_wind_peak_2007 <- rep(NA, nrow(InRe_wind_peak_2007))

# Calculate the LOLP for each period
for (i in seq_along(InRe_wind_peak_2007$Time)) {
  d_gb <- InRe_wind_peak_2007$GBdem_r[i]
  d_ire <- InRe_wind_peak_2007$Idem_r[i]
  w_gb <- InRe_wind_peak_2007$GBwind_r[i]
  w_ire <- InRe_wind_peak_2007$Iwind_r[i]
  
  d_prime <- c(d_gb - w_gb, d_ire - w_ire)
  
  if(1 <= d_prime[1] && d_prime[1] <= length(cdf_result_gb$cdf_gb) &&
     1 <= d_prime[1] - c && d_prime[1] - c <= length(cdf_result_gb$cdf_gb)) {
    lolp_gb_wind_peak_2007[i] <- calculate_lolp_gb(d_prime)
  } else {
    print(paste("d_prime values for GB are out of the expected range at time index", i))
  }
  
  if(1 <= d_prime[2] && d_prime[2] <= length(cdf_result_ire$cdf_ire) &&
     1 <= d_prime[2] - c && d_prime[2] - c <= length(cdf_result_ire$cdf_ire)) {
    lolp_ire_wind_peak_2007[i] <- calculate_lolp_ire(d_prime)
  } else {
    print(paste("d_prime values for IRE are out of the expected range at time index", i))
  }
}

# Calculate LOLE
lole_gb_wind_peak_2007 <- sum(lolp_gb_wind_peak_2007, na.rm = TRUE) 
lole_ire_wind_peak_2007 <- sum(lolp_ire_wind_peak_2007, na.rm = TRUE) 

# Print LOLE
print(paste("LOLE for GB during high wind generation periods in 2007:", lole_gb_wind_peak_2007))
print(paste("LOLE for Ireland during high wind generation periods in 2007:", lole_ire_wind_peak_2007))

#############################################
