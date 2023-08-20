library(rvest)
library(lubridate)
library(tidyverse)
library(stringr)
library(lubridate)
library(zoo)
# Add URL
urlHitting <- "https://www.frontierleague.com/sports/bsb/2022-23/teams?sort=avg&r=0&pos=h"

# Read the HTML content of the webpage
webpage <- read_html(urlHitting)

# Extract the table from the webpage
table_data <- html_table(html_nodes(webpage, "table")[1], fill = TRUE)

# Access the table
desired_table <- table_data[[1]]

# Add a new column "OPS"
desired_table$OPS <- with(desired_table, obp + slg)

# Add additional data set. Could not figure out how to webscrape
# the second page of the Frontier League website for pitching
pitchingTable <- read.csv("~/Downloads/FrontierLeague Pitching.csv")

# Merge the statistics into one dataframe
merged_table <- merge(desired_table, pitchingTable, by = "Name")

# Using subset function to select the two teams
selected_rows <- subset(merged_table, Name %in% c("Gateway Grizzlies", "Schaumburg Boomers"))

# Read the additional team stats on the Frontier League website
# and add it to the new data frame
gatewaySchaumburg <- read.csv("~/Downloads/Gatewayschaumburgstats.csv")
analysisDf <- merge(selected_rows, gatewaySchaumburg, by = "Name")

# Read the additional pitching stats on the Frontier League website
# and add it to the new data frame
gatewaySchaumburg2 <- read.csv("~/Downloads/advancedpitchingGWSC.csv")
analysisDf <- merge(analysisDf, gatewaySchaumburg2, by = "Name")

library(ggplot2)
library(tidyr)

# Select only the offensive columns
selected_columns <- c("Name", "BABIP",  "R.x",  "AVG.x", "OBP", "BB..x", "K..x")

#print(selected_columns)
# Create the new data frame of just the selected offensive columns
subset_analysisDf <- analysisDf[, selected_columns]

# Select only the defensive columns
selected_columns2 <- c("Name", "ERA", "vOPS", "whip", "AVG.y", "H.y", "FIP")
# Create the new data frame of just the selected defensive columns
subset_analysisDf2 <- analysisDf[, selected_columns2]

# Reshape both data to vertical long format
subset_analysisDf_long <- gather(subset_analysisDf, metric, value, -Name, na.rm = TRUE)
subset_analysisDf2_long <- gather(subset_analysisDf2, metric2, value, -Name, na.rm = TRUE)

# Create a custom label for each plot
custom_labels <- c(
  "BABIP" = "BABIP",
  "R.x" = "Runs Scored",
  "AVG.x" = "Batting Average",
  "OBP" = "On Base Percentage",
  "BB..x" = "Walk Rate",
  "K..x" = "Strikeout Rate"
)
custom_labels2 <- c("vOPS" = "OPS Against",
   "ERA" = "Team Earned Runs Average",
   "whip" = "WHIP",
   "AVG.y" = "Batting Average Against",
   "H.y" = "Hits Allowed",
   "FIP" = "FIP"
)

# Define the desired order of the facets
facet_order <- c("AVG.x", "BABIP", "OBP", "R.x", "BB..x" , "K..x")
facet_order2 <- c("ERA", "whip", "AVG.y", "vOPS", "H.y", "FIP")

# Convert the stats to a factor with the desired order
subset_analysisDf_long$metric <- factor(subset_analysisDf_long$metric, levels = facet_order)
subset_analysisDf2_long$metric2 <- factor(subset_analysisDf2_long$metric, levels = facet_order2)

# Implement colors for each team
team_colors <- c("Gateway Grizzlies" = "navy", "Schaumburg Boomers" = "darkorange")

#Add photos>

# Bar plot comparing the offensive metrics for the two teams
p1 <- ggplot(subset_analysisDf_long, aes(x = Name, y = value, fill = Name)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = value), position = position_dodge(width = 0.9), vjust = 1.5, color = "white") +
  facet_wrap(~ metric, scales = "free_y", ncol = 2, labeller = labeller(metric = custom_labels)) +
  labs(x = "Team Name", y = "Value", fill = "Metric",title = "Run Producing Stats") +
  scale_fill_manual(values = team_colors) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Decrease x-axis text size
        axis.text.y = element_text(size = 8),  # Decrease y-axis text size
        legend.text = element_text(size = 8),  # Decrease legend text size
        legend.title = element_text(size = 8),  
        legend.position = "none", plot.title = element_text(hjust = 0.5))
p1 <- p1 + annotation_custom(rasterGrob(logo_img, interpolate = TRUE), 
                             xmin = 1, xmax = 2, ymin = Inf, ymax = Inf)


# Bar plot comparing the defensive metrics for the two teams
ggplot(subset_analysisDf2_long, aes(x = Name, y = value, fill = Name)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = value), position = position_dodge(width = 0.9), vjust = 1.5, color = "white")+
  facet_wrap(~ metric2, scales = "free_y", ncol = 2, labeller = labeller(metric2 = custom_labels2)) +
  labs(x = "Team Name", y = "Value", fill = "Metric2", title = "Run Prevention Stats") +
  scale_fill_manual(values = team_colors) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Decrease x-axis text size
        axis.text.y = element_text(size = 8),  # Decrease y-axis text size
        legend.text = element_text(size = 8),  # Decrease legend text size
        legend.title = element_text(size = 8),  
        legend.position = "none", plot.title = element_text(hjust = 0.5))

