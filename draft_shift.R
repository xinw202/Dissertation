library(dplyr)

# Create a new column for net demand
InRe$NetDemand_GB <- InRe$GBdem_r - InRe$GBwind_r

# Add a Year column to the InRe dataframe
InRe$Year <- as.numeric(format(as.Date(InRe$Date), "%Y"))

# If a date is before June, it should belong to the previous winter year
InRe$Year[format(as.Date(InRe$Date), "%m") < "06"] <- InRe$Year[format(as.Date(InRe$Date), "%m") < "06"] - 1

# Now, identify the top 10 critical days for each winter year
critical_days <- InRe %>%
  group_by(Year) %>%
  top_n(10, wt = NetDemand_GB) %>%
  ungroup()



# Merge the two dataframes based on Year and Capacity
df_combined <- left_join(df_results, df_results_no_intercon, by = c("Year", "Capacity"), suffix = c("_intercon", "_no_intercon"))

# Calculate LOLE drop for each year and capacity value
df_combined$LOLE_drop <- df_combined$LOLE_no_intercon - df_combined$LOLE_intercon

# Convert the Year column in critical_days to character type to ensure compatibility
critical_days$Year <- as.character(critical_days$Year)

# Now, merge the LOLE drop data with the critical days data
df_analysis <- left_join(df_combined, critical_days, by = "Year")

# Plot
library(ggplot2)
plot <- ggplot(df_analysis, aes(x = Capacity, y = LOLE_drop, color = Year)) +
  geom_line(size = 1) +  
  facet_wrap(~ Year, scales = "free") +
  labs(x = "Interconnector Capacity (MW)", 
       y = "LOLE Drop (hours/year)", 
       title = "Impact of Interconnector Capacity on LOLE Drop", 
       subtitle = "Based on 7 Winters and Top 10 Critical Days") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black", size = 10),  
        axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, color = "black", size = 10),  
        axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10)),  
        axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 10)),  
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),  
        plot.subtitle = element_text(hjust = 0.5))

print(plot)
