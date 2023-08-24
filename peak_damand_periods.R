#########################################################
# peak demand periods
# Define peak demand hours - for instance, 5-9 pm
peak_hours <- c(17, 18, 19, 20, 21)

# Filter the InRe_2007 for peak demand periods
InRe_2007 <- InRe %>% filter(Date >= "2007-06-01" & Date <= "2008-05-31")
InRePeak_2007 <- InRe_2007 %>% 
  # converts the 'Time' column to a POSIXct format which allows for time-based operations
  mutate(Time = as.POSIXct(Time, format = "%H:%M:%S")) %>% 
  # filters the rows of the InRe where the hour of the 'Time' column is in peak_hours
  filter(hour(Time) %in% peak_hours) 

# initialize numeric vectors to store the LOLP values for each time point during peak hours
lolp_gb_peak_2007 <- numeric(nrow(InRePeak_2007))
lolp_ire_peak_2007 <- numeric(nrow(InRePeak_2007))

# Loop through the filtered InRe, calculate LOLP and LOLE for each time point
for (i in seq_along(InRePeak_2007$Time)) {
  d_gb <- InRePeak_2007$GBdem_r[i]
  d_ire <- InRePeak_2007$Idem_r[i]
  w_gb <- InRePeak_2007$GBwind_r[i]
  w_ire <- InRePeak_2007$Iwind_r[i]
  
  d_prime <- c(d_gb - w_gb, d_ire - w_ire)
  
  lolp_gb_peak_2007[i] <- calculate_lolp_gb(d_prime)
  lolp_ire_peak_2007[i] <- calculate_lolp_ire(d_prime)
}

# Calculate LOLE
lole_gb_peak_2007 <- sum(lolp_gb_peak_2007)
lole_ire_peak_2007 <- sum(lolp_ire_peak_2007)

# Print LOLE
print(paste("LOLE for GB during peak demand periods in 2007:", lole_gb_peak_2007))
print(paste("LOLE for Ireland during peak demand periods in 2007:", lole_ire_peak_2007))


##############################################
InRe_2008 <- InRe %>% filter(Date >= "2008-06-01" & Date <= "2009-05-31")
InRePeak_2008 <- InRe_2008 %>% 
  mutate(Time = as.POSIXct(Time, format = "%H:%M:%S")) %>% 
  filter(hour(Time) %in% peak_hours) 

lolp_gb_peak_2008 <- numeric(nrow(InRePeak_2008))
lolp_ire_peak_2008 <- numeric(nrow(InRePeak_2008))

for (i in seq_along(InRePeak_2008$Time)) {
  d_gb <- InRePeak_2008$GBdem_r[i]
  d_ire <- InRePeak_2008$Idem_r[i]
  w_gb <- InRePeak_2008$GBwind_r[i]
  w_ire <- InRePeak_2008$Iwind_r[i]
  
  d_prime <- c(d_gb - w_gb, d_ire - w_ire)
  
  lolp_gb_peak_2008[i] <- calculate_lolp_gb(d_prime)
  lolp_ire_peak_2008[i] <- calculate_lolp_ire(d_prime)
}

lole_gb_peak_2008 <- sum(lolp_gb_peak_2008)
lole_ire_peak_2008 <- sum(lolp_ire_peak_2008)

print(paste("LOLE for GB during peak demand periods in 2008:", lole_gb_peak_2008))
print(paste("LOLE for Ireland during peak demand periods in 2008:", lole_ire_peak_2008))

##############################################
InRe_2009 <- InRe %>% filter(Date >= "2009-06-01" & Date <= "2010-05-31")
InRePeak_2009 <- InRe_2009 %>% 
  mutate(Time = as.POSIXct(Time, format = "%H:%M:%S")) %>% 
  filter(hour(Time) %in% peak_hours) 

lolp_gb_peak_2009 <- numeric(nrow(InRePeak_2009))
lolp_ire_peak_2009 <- numeric(nrow(InRePeak_2009))

for (i in seq_along(InRePeak_2009$Time)) {
  d_gb <- InRePeak_2009$GBdem_r[i]
  d_ire <- InRePeak_2009$Idem_r[i]
  w_gb <- InRePeak_2009$GBwind_r[i]
  w_ire <- InRePeak_2009$Iwind_r[i]
  
  d_prime <- c(d_gb - w_gb, d_ire - w_ire)
  
  lolp_gb_peak_2009[i] <- calculate_lolp_gb(d_prime)
  lolp_ire_peak_2009[i] <- calculate_lolp_ire(d_prime)
}

lole_gb_peak_2009 <- sum(lolp_gb_peak_2009)
lole_ire_peak_2009 <- sum(lolp_ire_peak_2009)

print(paste("LOLE for GB during peak demand periods in 2009:", lole_gb_peak_2009))
print(paste("LOLE for Ireland during peak demand periods in 2009:", lole_ire_peak_2009))

#####################################################################
calculate_yearly_lole_peak_demand <- function(start_year) {
  # Define the time period
  start_date <- as.Date(paste(start_year, "-06-01", sep=""))
  end_date <- as.Date(paste(start_year + 1, "-05-31", sep=""))
  
  # Filter data for the time period
  InRe_year <- InRe %>% filter(Date >= start_date & Date <= end_date)
  
  # Define peak demand hours - for instance, 5-9 pm
  peak_hours <- c(17, 18, 19, 20, 21)
  
  # Filter for peak demand periods
  InRePeak_year <- InRe_year %>% 
    mutate(Time = as.POSIXct(Time, format = "%H:%M:%S")) %>% 
    filter(hour(Time) %in% peak_hours)
  
  # Initialize numeric vectors to store the LOLP values
  lolp_gb_peak <- numeric(nrow(InRePeak_year))
  lolp_ire_peak <- numeric(nrow(InRePeak_year))
  
  # Calculate LOLP for each time point
  for (i in seq_along(InRePeak_year$Time)) {
    d_gb <- InRePeak_year$GBdem_r[i]
    d_ire <- InRePeak_year$Idem_r[i]
    w_gb <- InRePeak_year$GBwind_r[i]
    w_ire <- InRePeak_year$Iwind_r[i]
    
    d_prime <- c(d_gb - w_gb, d_ire - w_ire)
    
    lolp_gb_peak[i] <- calculate_lolp_gb(d_prime)
    lolp_ire_peak[i] <- calculate_lolp_ire(d_prime)
  }
  
  # Calculate LOLE
  lole_gb_peak <- sum(lolp_gb_peak)
  lole_ire_peak <- sum(lolp_ire_peak)
  
  # Print LOLE
  print(paste("LOLE for GB during peak demand periods in", start_year, "-", start_year + 1, ":", lole_gb_peak))
  print(paste("LOLE for Ireland during peak demand periods in", start_year, "-", start_year + 1, ":", lole_ire_peak))
}
for (year in 2007:2013) {
  calculate_yearly_lole_peak_demand(year)
}

