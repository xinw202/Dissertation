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
    d_prime <- c(max(1, d_gb - w_gb), max(1, d_ire - w_ire))  # Add the max(1, ...) condition
    
    lolp_gb_interconnected <- calculate_lolp_gb_1(d_prime)  # Calculate lolp for gb
    lolp_ire_interconnected <- calculate_lolp_ire_1(d_prime)  # Calculate lolp for ire
    
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
library(ggplot2)
ggplot(results, aes(x = c)) +
  geom_line(aes(y = LOLE_GB), color = "blue") +
  geom_line(aes(y = LOLE_Ire), color = "red") +
  labs(x = "Interconnector Capacity (MW)", y = "LOLE (hours/year)", 
       title = "LOLE for GB and Ireland against Interconnector Capacity", 
       subtitle = "Blue: GB, Red: Ireland") +
  coord_cartesian(xlim = c(0, max(c_values))) +  # explicitly set the x-axis limits
  theme_minimal()








