# Define a range of interconnector capacities
capacities <- seq(0, 1000, by=50)

# Initialize vectors to store LOLP values
lolp_gb_values <- numeric(length(capacities))
lolp_ire_values <- numeric(length(capacities))

# Calculate LOLP for each interconnector capacity
for (i in 1:length(capacities)) {
  c <- capacities[i]
  
  # Here, adjust d_prime_all or compute LOLP based on the current value of c
  # You can use the previously defined functions to do this
  lolp_gb <- calculate_lolp_gb(d_prime_all)
  lolp_ire <- calculate_lolp_ire(d_prime_all)
  
  # Store the results
  lolp_gb_values[i] <- lolp_gb
  lolp_ire_values[i] <- lolp_ire
}

# Plot the results
plot(capacities, lolp_gb_values, type="l", col="blue", ylim=c(0, max(max(lolp_gb_values), max(lolp_ire_values))),
     xlab="Interconnector Capacity (MW)", ylab="LOLP", main="Impact of Interconnector Capacity on LOLP")
lines(capacities, lolp_ire_values, col="red")
legend("topright", legend=c("GB", "Ireland"), col=c("blue", "red"), lty=1)
