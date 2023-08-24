# Set the interconnector capacity
c <- 1000  # MW

# Choose a specific point in time (index of the dataset)
time_index <- 1  # Adjust this index as needed

# Extract the relevant data at the chosen time
d_gb <- InRe$GBdem_r[time_index]
d_ire <- InRe$Idem_r[time_index]
w_gb <- InRe$GBwind_r[time_index]
w_ire <- InRe$Iwind_r[time_index]

# Calculate d' = d - w
d_prime <- c(d_gb, d_ire) - c(w_gb, w_ire)

calculate_lolp <- function(x1) {
  p_x2_less_d2_prime_gb <- cdf_result_gb$cdf[d_prime[2]]
  p_x1_less_d1_prime_gb <- cdf_result_gb$cdf[d_prime[1]]
  p_x2_greater_d2_prime_c_gb <- 1 - cdf_result_gb$cdf[d_prime[2] + c]
  p_x1_less_d1_prime_c_gb <- cdf_result_gb$cdf[d_prime[1] - c]
  
  p_x2_less_d2_prime_ire <- cdf_result_ire$cdf[d_prime[2]]
  p_x1_less_d1_prime_ire <- cdf_result_ire$cdf[x1 - d_prime[1]]
  p_x2_greater_d2_prime_c_ire <- 1 - cdf_result_ire$cdf[d_prime[2] + c]
  p_x1_less_d1_prime_c_ire <- cdf_result_ire$cdf[d_prime[1] - c]
  
  middle_sum_gb <- 0
  middle_sum_ire <- 0
  
  for (x2 in d_prime[2]:(d_prime[2] + c - 1)) {
    middle_sum_gb <- middle_sum_gb + freq_gb[x2] * cdf_result_gb$cdf[d_prime[1] + d_prime[2] - x2]
    middle_sum_ire <- middle_sum_ire + freq_ire[x2] * cdf_result_ire$cdf[d_prime[1] + d_prime[2] - x2]
  }
  
  lolp_gb <- p_x2_less_d2_prime_gb * p_x1_less_d1_prime_gb + middle_sum_gb + p_x2_greater_d2_prime_c_gb * p_x1_less_d1_prime_c_gb
  lolp_ire <- p_x2_less_d2_prime_ire * p_x1_less_d1_prime_ire + middle_sum_ire + p_x2_greater_d2_prime_c_ire * p_x1_less_d1_prime_c_ire
  
  return(list(lolp_gb, lolp_ire))
}

# Calculate LOLP in the two-area system
lolp_1_gb <- 0  # Initialize LOLP in area 1 (GB)
lolp_1_ire <- 0  # Initialize LOLP in area 2 (Ireland)

# Calculate LOLP for each value of x1 (X_1 capacity)
for (x1 in vals_pmf_gb) {
  lolp <- calculate_lolp(x1)
  lolp_1_gb <- lolp_1_gb + lolp[[1]]
  lolp_1_ire <- lolp_1_ire + lolp[[2]]
}

# Print the calculated LOLP values in each area
print(lolp_1_gb)
print(lolp_1_ire)
