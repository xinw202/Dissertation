# low demand periods
InRe_2007 <- InRe %>% filter(Date >= "2007-06-01" & Date <= "2008-05-31")
# can present challenges for grid operation
# First, identify the time indices corresponding to periods of low demand
low_demand_indices_gb_2007 <- which(InRe_2007$GBdem_r < quantile(InRe_2007$GBdem_r, 0.1))  # bottom 10% demand periods
low_demand_indices_ire_2007 <- which(InRe_2007$Idem_r < quantile(InRe_2007$Idem_r, 0.1))  # bottom 10% demand periods

# Now, calculate LOLE for these periods
lole_gb_low_demand_2007 <- 0
lole_ire_low_demand_2007 <- 0

for (i in low_demand_indices_gb_2007) {
  d_gb <- InRe_2007$GBdem_r[i]
  w_gb <- InRe_2007$GBwind_r[i]
  d_prime <- c(d_gb - w_gb, d_ire - w_ire)
  
  if(1 <= d_prime[1] && d_prime[1] <= length(cdf_result_gb$cdf_gb) &&
     1 <= d_prime[1] - c && d_prime[1] - c <= length(cdf_result_gb$cdf_gb)) {
    lole_gb_low_demand_2007 <- lole_gb_low_demand_2007 + calculate_lolp_gb(d_prime)
  } else {
    print(paste("d_prime values for GB are out of the expected range at time index", i))
  }
}

for (i in low_demand_indices_ire_2007) {
  d_ire <- InRe_2007$Idem_r[i]
  w_ire <- InRe_2007$Iwind_r[i]
  d_prime <- c(d_gb - w_gb, d_ire - w_ire)
  
  if(1 <= d_prime[2] && d_prime[2] <= length(cdf_result_ire$cdf_ire) &&
     1 <= d_prime[2] + c && d_prime[2] + c <= length(cdf_result_ire$cdf_ire)) {
    lole_ire_low_demand_2007 <- lole_ire_low_demand_2007 + calculate_lolp_ire(d_prime)
  } else {
    print(paste("d_prime values for IRE are out of the expected range at time index", i))
  }
}

print(lole_gb_low_demand_2007) # essentially zero for all practical purposes, it suggests that during periods of low demand in GB, the system is highly unlikely to experience a loss of load
print(lole_ire_low_demand_2007) # calculated LOLE is exactly zero, suggesting that during periods of low demand in ire, there is no expectation of load loss

#################################################2008
InRe_2008 <- InRe %>% filter(Date >= "2008-06-01" & Date <= "2009-05-31")
low_demand_indices_gb_2008 <- which(InRe_2008$GBdem_r < quantile(InRe_2008$GBdem_r, 0.1))  
low_demand_indices_ire_2008 <- which(InRe_2008$Idem_r < quantile(InRe_2008$Idem_r, 0.1))  

lole_gb_low_demand_2008 <- 0
lole_ire_low_demand_2008 <- 0

for (i in low_demand_indices_gb_2008) {
  d_gb <- InRe_2008$GBdem_r[i]
  w_gb <- InRe_2008$GBwind_r[i]
  d_prime <- c(d_gb - w_gb, d_ire - w_ire)
  
  if(1 <= d_prime[1] && d_prime[1] <= length(cdf_result_gb$cdf_gb) &&
     1 <= d_prime[1] - c && d_prime[1] - c <= length(cdf_result_gb$cdf_gb)) {
    lole_gb_low_demand_2008 <- lole_gb_low_demand_2008 + calculate_lolp_gb(d_prime)
  } else {
    print(paste("d_prime values for GB are out of the expected range at time index", i))
  }
}

for (i in low_demand_indices_ire_2008) {
  d_ire <- InRe_2008$Idem_r[i]
  w_ire <- InRe_2008$Iwind_r[i]
  d_prime <- c(d_gb - w_gb, d_ire - w_ire)
  
  if(1 <= d_prime[2] && d_prime[2] <= length(cdf_result_ire$cdf_ire) &&
     1 <= d_prime[2] + c && d_prime[2] + c <= length(cdf_result_ire$cdf_ire)) {
    lole_ire_low_demand_2008 <- lole_ire_low_demand_2008 + calculate_lolp_ire(d_prime)
  } else {
    print(paste("d_prime values for IRE are out of the expected range at time index", i))
  }
}

print(lole_gb_low_demand_2008) # essentially zero for all practical purposes, it suggests that during periods of low demand in GB, the system is highly unlikely to experience a loss of load
print(lole_ire_low_demand_2008) # calculated LOLE is exactly zero, suggesting that during periods of low demand in ire, there is no expectation of load loss

################################################################
calculate_yearly_lole_low_demand <- function(year) {
  # Filter the data for the specific year
  InRe_year <- InRe %>% filter(Date >= paste(year,"-06-01", sep = "") & Date <= paste(year + 1,"-05-31", sep = ""))
  
  # Identify the time indices corresponding to periods of low demand
  low_demand_indices_gb <- which(InRe_year$GBdem_r < quantile(InRe_year$GBdem_r, 0.1)) 
  low_demand_indices_ire <- which(InRe_year$Idem_r < quantile(InRe_year$Idem_r, 0.1))
  
  # Initialize LOLE values
  lole_gb_low_demand <- 0
  lole_ire_low_demand <- 0
  
  # Calculate LOLE for GB
  for (i in low_demand_indices_gb) {
    d_gb <- InRe_year$GBdem_r[i]
    w_gb <- InRe_year$GBwind_r[i]
    d_prime <- c(d_gb - w_gb, d_ire - w_ire)
    
    if(1 <= d_prime[1] && d_prime[1] <= length(cdf_result_gb$cdf_gb) && 
       1 <= d_prime[1] - c && d_prime[1] - c <= length(cdf_result_gb$cdf_gb)) {
      lole_gb_low_demand <- lole_gb_low_demand + calculate_lolp_gb(d_prime)
    } else {
      print(paste("d_prime values for GB are out of the expected range at time index", i))
    }
  }
  
  # Calculate LOLE for IRE
  for (i in low_demand_indices_ire) {
    d_ire <- InRe_year$Idem_r[i]
    w_ire <- InRe_year$Iwind_r[i]
    d_prime <- c(d_gb - w_gb, d_ire - w_ire)
    
    if(1 <= d_prime[2] && d_prime[2] <= length(cdf_result_ire$cdf_ire) && 
       1 <= d_prime[2] + c && d_prime[2] + c <= length(cdf_result_ire$cdf_ire)) {
      lole_ire_low_demand <- lole_ire_low_demand + calculate_lolp_ire(d_prime)
    } else {
      print(paste("d_prime values for IRE are out of the expected range at time index", i))
    }
  }
  
  # Print the results
  print(paste("LOLE for GB during low demand periods in", year, "-", year+1, ": ", lole_gb_low_demand))
  print(paste("LOLE for IRE during low demand periods in", year, "-", year+1, ": ", lole_ire_low_demand))
}

# Loop through the years of interest
for (year in 2007:2013) {
  calculate_yearly_lole_low_demand(year)
}
