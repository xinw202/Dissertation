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



library(dplyr)
library(ggplot2)
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
       title = "Impact of Interconnector Capacity on LOLE", 
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
       title = "Impact of Interconnector Capacity on LOLE (Ireland)", 
       subtitle = "Based on 7 Winters") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black", size = 12),  
        axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, color = "black", size = 12),  
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),  
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),  
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  
        plot.subtitle = element_text(hjust = 0.5))




# Import the necessary libraries
library(dplyr)
library(ggplot2)

# Assuming InRe is your dataset's name and has been loaded

# ... [your functions and dataset preparation code here] ...

# Calculate LOLE for each year and interconnector capacity for GB
calculate_lole_for_year_gb <- function(data_year) {
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

# Calculate LOLE for each year and selected interconnector capacities
c_values_selected <- c(0, 500, 1000)  # Selected Interconnection capacities

df_results <- data.frame()

for (year in names(winters)) {
  data_year <- winters[[year]]
  
  for (j in 1:length(c_values_selected)) {
    c <- c_values_selected[j]
    
    lole_gb <- calculate_lole_for_year_gb(data_year)
    lole_ire <- calculate_lole_for_year_ire(data_year)
    
    # Append to the results dataframe for GB and IRE
    df_results <- rbind(df_results, data.frame(Year = year, Region = "GB", Capacity = c, LOLE = lole_gb))
    df_results <- rbind(df_results, data.frame(Year = year, Region = "IRE", Capacity = c, LOLE = lole_ire))
  }
}

# Print out LOLE values for each combination
df_results
