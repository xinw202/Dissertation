# calculate LOLE without interconnection
#########################################
# For GB
# For each historic record, calculate use the CDF to calculate the lolp given the value of (demand - wind)
historic_records_gb <- InRe$GBdem_r-InRe$GBwind_r

calculate_LOLP_gb <- function(cdf_gb, vals_gb, historic_records_gb) {
  if (historic_records_gb < 0) {
    return(1)  # LOLP is 1 if demand - wind is negative (indicating a deficit)
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
    return(1)  # lolp is 1 if demand - wind is negative (indicating a deficit)
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
c <- 400  # MW

for (i in 1:nrow(InRe)) {
  d_gb <- InRe$GBdem_r[i]
  w_gb <- InRe$GBwind_r[i]
  d_ire <- InRe$Idem_r[i]
  w_ire <- InRe$Iwind_r[i]
  d_prime <- c(d_gb - w_gb, d_ire - w_ire)
  
  lolp_gb_interconnected <- calculate_lolp_gb(d_prime)  # Calculate lolp for GB
  lolp_ire_interconnected <- calculate_lolp_ire(d_prime)  # Calculate lolp for Ireland
  
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
c <- 500

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


################################################################
# Let's say the maximum interconnector capacity you're going to consider is max_c
max_c <- 3000  # Adjust this value as needed

# Extend the length of the probability mass function and cumulative distribution function arrays for both GB and IRE

# For GB
cdf_result_gb$cdf_gb <- c(cdf_result_gb$cdf_gb, rep(1, max_c))
freq_gb <- c(freq_gb, rep(0, max_c))

# For IRE
cdf_result_ire$cdf_ire <- c(cdf_result_ire$cdf_ire, rep(1, max_c))
freq_ire <- c(freq_ire, rep(0, max_c))

# Initialize a data frame to store the results
results <- data.frame()

c_values <- seq(0, 1000, 50) # 6500 or 6000


# Calculate LOLE for each value of c
for (c in c_values) {
  # Initialize vectors to store the LOLP values for each time period
  lolp_values_gb_interconnected <- vector(length = length(InRe$GBdem_r[i]))
  lolp_values_ire_interconnected <- vector(length = length(InRe$Idem_r[i]))
  
  # Loop over all time periods
  for (i in 1:nrow(InRe)) {
    d_gb <- InRe$GBdem_r[i]
    w_gb <- InRe$GBwind_r[i]
    d_ire <- InRe$Idem_r[i]
    w_ire <- InRe$Iwind_r[i]
    d_prime <- c(d_gb - w_gb, d_ire - w_ire)
    
    lolp_gb_interconnected <- calculate_lolp_gb(d_prime)  # Calculate lolp for gb
    lolp_ire_interconnected <- calculate_lolp_ire(d_prime)  # Calculate lolp for ire
    
    lolp_values_gb_interconnected[i] <- lolp_gb_interconnected  # Store the lolp for gb
    lolp_values_ire_interconnected[i] <- lolp_ire_interconnected  # Store the lolp for ire
  }
  
  # Calculate lole for gb and ire
  lole_gb_interconnected <- sum(lolp_values_gb_interconnected) / 7
  lole_ire_interconnected <- sum(lolp_values_ire_interconnected) / 7
  
  # Add the results to the data frame
  results <- rbind(results, data.frame(c = c, LOLE_GB = lole_gb_interconnected, LOLE_Ire = lole_ire_interconnected))
}

# Plot the results
# Plot the results
library(ggplot2)
ggplot(results, aes(x = c)) +
  geom_line(aes(y = LOLE_GB), color = "blue") +
  geom_line(aes(y = LOLE_Ire), color = "red") +
  labs(x = "Interconnector Capacity (MW)", y = "LOLE (hours/year)", 
       title = "LOLE for GB and Ireland against Interconnector Capacity", 
       subtitle = "Blue: GB, Red: Ireland") +
  coord_cartesian(xlim = c(0, max(c_values))) +  # explicitly set the x-axis limits
  theme_minimal()













# Inside the for loop, before calculating the LOLP:
max_c <- max(max_c, max(d_prime) + c)
cdf_result_gb$cdf_gb <- c(cdf_result_gb$cdf_gb, rep(1, max_c - length(cdf_result_gb$cdf_gb)))
cdf_result_ire$cdf_ire <- c(cdf_result_ire$cdf_ire, rep(1, max_c - length(cdf_result_ire$cdf_ire)))
freq_gb <- c(freq_gb, rep(0, max_c - length(freq_gb)))
freq_ire <- c(freq_ire, rep(0, max_c - length(freq_ire)))





# Now, you can safely use d_prime (demand net of wind generation) to index these arrays without worrying about exceeding their lengths
# Extend the arrays
max_c <- 1000  # Set this to the maximum value of c you are considering
max_val <- max_c + max(d_prime) + 1  # This should be greater than the maximum possible value of c + d_prime

# Extend the PDF arrays with zeros
freq_gb <- c(freq_gb, rep(0, max_val - length(freq_gb)))
freq_ire <- c(freq_ire, rep(0, max_val - length(freq_ire)))

# Extend the CDF arrays with ones
cdf_result_gb$cdf_gb <- c(cdf_result_gb$cdf_gb, rep(1, max_val - length(cdf_result_gb$cdf_gb)))
cdf_result_ire$cdf_ire <- c(cdf_result_ire$cdf_ire, rep(1, max_val - length(cdf_result_ire$cdf_ire)))

print(max_val)
print(length(freq_gb))





max_d_gb <- max(InRe$GBdem_r)
max_d_ire <- max(InRe$Idem_r)
max_val <- max(max_d_gb, max_d_ire) + max_c  # max_c is the maximum interconnector capacity

print(max_val)  # Print the new max_val for verification

freq_gb <- c(freq_gb, rep(0, max_val - length(freq_gb)))
cdf_gb <- c(cdf_gb, rep(1, max_val - length(cdf_gb)))

freq_ire <- c(freq_ire, rep(0, max_val - length(freq_ire)))
cdf_ire <- c(cdf_ire, rep(1, max_val - length(cdf_ire)))

