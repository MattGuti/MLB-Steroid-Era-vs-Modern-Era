install.packages('Lahman')

library(Lahman)

data(package = "Lahman")

data("Batting")

head(Batting)
summary(Batting)

library(dplyr)

steroid_era <- Batting %>% filter(yearID >= 1994 & yearID <= 2004)
modern_era <- Batting %>% filter(yearID >= 2013 & yearID <= 2023)

# Check the first few rows to confirm the data
head(steroid_era)
head(modern_era)

steroid_summary <- steroid_era %>%
  summarize(
    total_AB = sum(AB, na.rm = TRUE),
    total_H = sum(H, na.rm = TRUE),
    total_HR = sum(HR, na.rm = TRUE),
    total_RBI = sum(RBI, na.rm = TRUE),
    avg_BA = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE)
  )

# Summarize key statistics for the modern era
modern_summary <- modern_era %>%
  summarize(
    total_AB = sum(AB, na.rm = TRUE),
    total_H = sum(H, na.rm = TRUE),
    total_HR = sum(HR, na.rm = TRUE),
    total_RBI = sum(RBI, na.rm = TRUE),
    avg_BA = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE)
  )

comparison <- data.frame(
  Era = c("Steroid Era", "Modern Era"),
  Avg_BA = c(steroid_summary$avg_BA, modern_summary$avg_BA),
  Total_HR = c(steroid_summary$total_HR, modern_summary$total_HR),
  Total_RBI = c(steroid_summary$total_RBI, modern_summary$total_RBI)
)

# Create a bar plot for batting average comparison
bar_heights <- barplot(
  comparison$Avg_BA,
  names.arg = comparison$Era,
  main = "Comparison of Batting Average",
  ylab = "Batting Average",
  col = "lightblue"
)

# Add the text labels at the top of each bar
text(
  x = bar_heights, # The x-coordinates (the bar centers returned by barplot)
  y = comparison$Avg_BA, # The y-coordinates (the heights of the bars)
  labels = round(comparison$Avg_BA, 2), # The values to display (rounded to 2 decimal places)
  pos = 3 # Position the text above the bars
)

install.packages("ggplot2")
library(ggplot2)

comparison <- data.frame(
  Era = c("Steroid Era", "Modern Era"),
  Avg_BA = c(steroid_summary$avg_BA, modern_summary$avg_BA),
  Total_HR = c(steroid_summary$total_HR, modern_summary$total_HR),
  Total_RBI = c(steroid_summary$total_RBI, modern_summary$total_RBI)
)

# Melt the data frame for easier plotting with ggplot
library(reshape2)
comparison_melted <- melt(comparison, id.vars = "Era")

# Create a ggplot bar plot comparing stats across eras
ggplot(comparison_melted, aes(x = Era, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Comparison of Batting Statistics: Steroid Era vs. Modern Era",
    y = "Value",
    x = "Era"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Avg_BA" = "lightblue", "Total_HR" = "lightgreen", "Total_RBI" = "lightcoral")) +
  theme(legend.title = element_blank()) +
  geom_text(aes(label = round(value, 2)), position = position_dodge(width = 0.9), vjust = -0.25)

# How do batting averages in the steroid era compare to those in the modern era? Are players today hitting more or fewer home runs than during the steroid era? 
# Is there a significant difference in RBIs between the two eras? What might account for the change?

steroid_era_ba <- steroid_era %>%
  filter(AB > 0) %>%
  mutate(BA = H / AB) %>%
  summarise(Avg_BA = mean(BA, na.rm = TRUE))

modern_era_ba <- modern_era %>%
  filter(AB > 0) %>%
  mutate(BA = H / AB) %>%
  summarise(Avg_BA = mean(BA, na.rm = TRUE))

# Compare batting averages
steroid_era_ba
modern_era_ba
  
# Power Hitting:Did home run counts peak during the steroid era, and how do they compare to the modern era? What could explain any differences in home run production between these two periods?
# How does slugging percentage compare between the two eras? Have modern hitters become more focused on power hitting over time?

steroid_era_hr <- steroid_era %>%
  summarise(Total_HR = sum(HR, na.rm = TRUE))

modern_era_hr <- modern_era %>%
  summarise(Total_HR = sum(HR, na.rm = TRUE))

# Compare home runs
steroid_era_hr
modern_era_hr



#Slugging
steroid_era_slg <- steroid_era %>%
  filter(AB > 0) %>%
  mutate(SLG = ((H - X2B - X3B - HR) + 2*X2B + 3*X3B + 4*HR) / AB) %>%
  summarise(Avg_SLG = mean(SLG, na.rm = TRUE))

modern_era_slg <- modern_era %>%
  filter(AB > 0) %>%
  mutate(SLG = ((H - X2B - X3B - HR) + 2*X2B + 3*X3B + 4*HR) / AB) %>%
  summarise(Avg_SLG = mean(SLG, na.rm = TRUE))

# Compare slugging percentage
steroid_era_slg
modern_era_slg

#RBI's
steroid_era_rbi <- steroid_era %>%
  summarise(Total_RBI = sum(RBI, na.rm = TRUE))

modern_era_rbi <- modern_era %>%
  summarise(Total_RBI = sum(RBI, na.rm = TRUE))

# Compare RBIs
steroid_era_rbi
modern_era_rbi

# Calculate OBP for both eras
steroid_era_ops <- steroid_era %>%
  filter(AB > 0) %>%
  mutate(OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
         SLG = ((H - X2B - X3B - HR) + 2*X2B + 3*X3B + 4*HR) / AB,
         OPS = OBP + SLG) %>%
  summarise(Avg_OPS = mean(OPS, na.rm = TRUE))

modern_era_ops <- modern_era %>%
  filter(AB > 0) %>%
  mutate(OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
         SLG = ((H - X2B - X3B - HR) + 2*X2B + 3*X3B + 4*HR) / AB,
         OPS = OBP + SLG) %>%
  summarise(Avg_OPS = mean(OPS, na.rm = TRUE))

# Compare OPS
steroid_era_ops
modern_era_ops

#Strikeout
steroid_era_k_rate <- steroid_era %>%
  filter(AB > 0) %>%
  summarise(K_Rate = sum(SO, na.rm = TRUE) / sum(AB, na.rm = TRUE))

modern_era_k_rate <- modern_era %>%
  filter(AB > 0) %>%
  summarise(K_Rate = sum(SO, na.rm = TRUE) / sum(AB, na.rm = TRUE))

# Compare strikeout rates
steroid_era_k_rate
modern_era_k_rate

library(ggplot2)

batting_trends <- Batting %>%
  filter(yearID >= 1994 & yearID <= 2024) %>%
  group_by(yearID) %>%
  summarise(Avg_BA = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE))

ggplot(batting_trends, aes(x = yearID, y = Avg_BA)) +
  geom_line() +
  geom_point() +
  labs(title = "Batting Average Trends (1994-2024)", x = "Year", y = "Batting Average") +
  theme_minimal()
  
# Consistency:Are players in the modern era more consistent in their performance (in terms of batting average or home runs) compared to the steroid era?
# Are there notable fluctuations in key metrics (e.g., batting average, on-base percentage) across both eras?

# Calculate walk rate (BB / PA) for both eras
steroid_era_walk_rate <- steroid_era %>%
  filter(AB > 0) %>%
  summarise(Walk_Rate = sum(BB, na.rm = TRUE) / sum(AB + BB + HBP + SF, na.rm = TRUE))

modern_era_walk_rate <- modern_era %>%
  filter(AB > 0) %>%
  summarise(Walk_Rate = sum(BB, na.rm = TRUE) / sum(AB + BB + HBP + SF, na.rm = TRUE))

# Compare walk rates
steroid_era_walk_rate
modern_era_walk_rate


# Calculate extra-base hit percentage (2B + 3B + HR) / H
steroid_era_xbh_rate <- steroid_era %>%
  filter(H > 0) %>%
  summarise(XBH_Rate = sum(X2B + X3B + HR, na.rm = TRUE) / sum(H, na.rm = TRUE))

modern_era_xbh_rate <- modern_era %>%
  filter(H > 0) %>%
  summarise(XBH_Rate = sum(X2B + X3B + HR, na.rm = TRUE) / sum(H, na.rm = TRUE))

# Compare extra-base hit rates
steroid_era_xbh_rate
modern_era_xbh_rate


# Calculate standard deviation of batting average and home runs for both eras
steroid_era_sd <- steroid_era %>%
  filter(AB > 0) %>%
  mutate(BA = H / AB) %>%
  summarise(SD_BA = sd(BA, na.rm = TRUE),
            SD_HR = sd(HR, na.rm = TRUE))

modern_era_sd <- modern_era %>%
  filter(AB > 0) %>%
  mutate(BA = H / AB) %>%
  summarise(SD_BA = sd(BA, na.rm = TRUE),
            SD_HR = sd(HR, na.rm = TRUE))

# Compare standard deviation (consistency) of batting averages and home runs
steroid_era_sd
modern_era_sd


# Calculate HR per plate appearance (PA) and HR per at-bat (AB) for both eras
steroid_era_hr_rate <- steroid_era %>%
  summarise(HR_per_PA = sum(HR, na.rm = TRUE) / sum(AB + BB + HBP + SF, na.rm = TRUE),
            HR_per_AB = sum(HR, na.rm = TRUE) / sum(AB, na.rm = TRUE))

modern_era_hr_rate <- modern_era %>%
  summarise(HR_per_PA = sum(HR, na.rm = TRUE) / sum(AB + BB + HBP + SF, na.rm = TRUE),
            HR_per_AB = sum(HR, na.rm = TRUE) / sum(AB, na.rm = TRUE))

# Compare home run rates
steroid_era_hr_rate
modern_era_hr_rate

# Create a density plot of batting averages for both eras
steroid_era_ba_dist <- steroid_era %>%
  filter(AB > 0) %>%
  mutate(BA = H / AB)

modern_era_ba_dist <- modern_era %>%
  filter(AB > 0) %>%
  mutate(BA = H / AB)

# Combine for plotting
ba_combined <- rbind(
  data.frame(BA = steroid_era_ba_dist$BA, Era = "Steroid Era"),
  data.frame(BA = modern_era_ba_dist$BA, Era = "Modern Era")
)

# Plot density of batting averages
ggplot(ba_combined, aes(x = BA, fill = Era)) +
  geom_density(alpha = 0.5) +
  labs(title = "Batting Average Distribution: Steroid Era vs. Modern Era", x = "Batting Average", y = "Density") +
  theme_minimal()