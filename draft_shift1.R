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
  labs(x="Increase in Demand", y="LOLE", color="Scenario") +
  theme_minimal() +
  coord_cartesian(xlim=c(0, 200)) # Set the X-axis limits


