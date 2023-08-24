InRe_2007 <- InRe %>% filter(Date >= "2007-06-01" & Date <= "2008-05-31")
InRe_2008 <- InRe %>% filter(Date >= "2008-06-01" & Date <= "2009-05-31")
InRe_2009 <- InRe %>% filter(Date >= "2009-06-01" & Date <= "2010-05-31")
InRe_2010 <- InRe %>% filter(Date >= "2010-06-01" & Date <= "2011-05-31")
InRe_2011 <- InRe %>% filter(Date >= "2011-06-01" & Date <= "2012-05-31")
InRe_2012 <- InRe %>% filter(Date >= "2012-06-01" & Date <= "2013-05-31")
InRe_2013 <- InRe %>% filter(Date >= "2013-06-01" & Date <= "2014-05-31")

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

# Define each winter data
winters <- list(
  "2007" = InRe %>% filter(Date >= "2007-06-01" & Date <= "2008-05-31"),
  "2008" = InRe %>% filter(Date >= "2008-06-01" & Date <= "2009-05-31"),
  "2009" = InRe %>% filter(Date >= "2009-06-01" & Date <= "2010-05-31"),
  "2010" = InRe %>% filter(Date >= "2010-06-01" & Date <= "2011-05-31"),
  "2011" = InRe %>% filter(Date >= "2011-06-01" & Date <= "2012-05-31"),
  "2012" = InRe %>% filter(Date >= "2012-06-01" & Date <= "2013-05-31"),
  "2013" = InRe %>% filter(Date >= "2013-06-01" & Date <= "2014-05-31")
)

# Loop over each winter and capacity
results <- list()
c_values <- seq(0, 1000, 50)  # Assuming you want to check these interconnection capacities

for (year in names(winters)) {
  data_year <- winters[[year]]
  loles <- vector(length = length(c_values))
  
  for (j in 1:length(c_values)) {
    c <- c_values[j]
    loles[j] <- calculate_lole_for_year(data_year)
  }
  results[[year]] <- loles
}

# Now, the 'results' list contains the LOLEs for each year over the interconnector capacities

library(dplyr)
library(ggplot2)

# ... [previous code definitions and the winters list] ...

# Data frame to store the results
df_results <- data.frame()

# Loop over each winter and capacity
for (year in names(winters)) {
  data_year <- winters[[year]]
  
  for (j in 1:length(c_values)) {
    c <- c_values[j]
    lole <- calculate_lole_for_year(data_year)
    
    # Append to the results dataframe
    df_results <- rbind(df_results, data.frame(Year = year, Capacity = c, LOLE = lole))
  }
}

# Plotting
ggplot(df_results, aes(x = Capacity, y = LOLE, color = Year)) +
  geom_line(size = 1) +  
  scale_x_continuous(breaks = seq(0, max(c_values), by = 100),   
                     limits = c(0, max(c_values)),  # Set limits explicitly
                     expand = c(0, 0)) +  
  scale_y_continuous(breaks = seq(0, max(df_results$LOLE, na.rm = TRUE), by = 0.5),  
                     limits = c(0, max(df_results$LOLE, na.rm = TRUE)),  
                     expand = c(0, 0)) +  
  labs(x = "Interconnector Capacity (MW)", 
       y = "LOLE (hours/year)", 
       title = "Impact of Interconnector Capacity on LOLE", 
       subtitle = "Based on 7 Winters") +
  theme_minimal() +
  theme(legend.title = element_blank(),  # Remove legend title
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black", size = 12),  
        axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, color = "black", size = 12),  
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),  
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),  
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  
        plot.subtitle = element_text(hjust = 0.5))  






# List of the winters to loop through
years <- 2007:2013
final_loles_ire <- vector(length = 7)

# Function to calculate LOLE for a specific winter
calculate_lole_for_year <- function(data_year, cdf, vals) {
  historic_records <- data_year$Idem_r - data_year$Iwind_r
  lolp_values <- vector(length = length(historic_records))
  
  for (i in 1:length(historic_records)) {
    demand_minus_wind <- historic_records[i]  # Extract demand minus wind for the current record
    lolp <- calculate_LOLP_gb(cdf, vals, demand_minus_wind)  # Calculate LOLP using the CDF
    lolp_values[i] <- lolp  
  }
  
  lole <- sum(lolp_values)
  return(lole)
}

# Loop through each winter and calculate LOLE for Ireland
for (j in 1:7) {
  year <- years[j]
  winter_data_ire <- get(paste0("InRe_", year)) 
  final_loles_ire[j] <- calculate_lole_for_year(winter_data_ire, cdf_ire, vals_ire)
}

print(final_loles_ire)


# Sample capacities to check
capacities <- seq(0, 1000, 50)  # Assuming an increment of 50MW, you can adjust this
lolp_ire_values_list <- vector("list", length(capacities))
years <- 2007:2013

# Adjusted function to account for varying interconnector capacity
calculate_lolp_ire_1 <- function(d_prime_all, c) {
  # ... (Keep the function body as you've provided but adjust for the input c)
}

# Calculate LOLE for a specific winter
calculate_lole_for_year <- function(data_year, cdf, vals, c) {
  historic_records <- data_year$Idem_r - data_year$Iwind_r
  lolp_values <- vector(length = length(historic_records))
  
  for (i in 1:length(historic_records)) {
    demand_minus_wind <- historic_records[i]
    lolp <- calculate_lolp_ire_1(list(demand_minus_wind, demand_minus_wind), c)  # Adjusted for function
    lolp_values[i] <- lolp  
  }
  
  lole <- sum(lolp_values)
  return(lole)
}

# Loop through each capacity
for (k in 1:length(capacities)) {
  c_val <- capacities[k]
  loles_for_capacity <- numeric(length(years))
  
  # Loop through each winter and calculate LOLE for Ireland
  for (j in 1:7) {
    year <- years[j]
    winter_data_ire <- get(paste0("InRe_", year))
    loles_for_capacity[j] <- calculate_lole_for_year(winter_data_ire, cdf_ire, vals_ire, c_val)
  }
  
  lolp_ire_values_list[[k]] <- loles_for_capacity
}

# Convert the list to matrix for easier plotting
lolp_ire_matrix <- matrix(unlist(lolp_ire_values_list), nrow=length(years), byrow=FALSE)

# Plotting the results for each winter
plot(capacities, lolp_ire_matrix[1,], type="l", ylim=c(0, max(lolp_ire_matrix)), 
     xlab="Interconnector Capacity (MW)", ylab="LOLE", main="LOLE for Ireland with varying Interconnector Capacity")
for (i in 2:7) {
  lines(capacities, lolp_ire_matrix[i,], col=i)  # Different colors for each winter
}

# Adding a legend
legend("topright", legend=as.character(years), col=1:7, lty=1, cex=0.8)
