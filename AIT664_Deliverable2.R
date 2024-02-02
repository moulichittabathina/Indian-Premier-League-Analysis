library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)


getwd()
setwd("C:/Users/DELL/Downloads/AIT_664/Project/Project Part2- Effective Visualization/Deliverable2")
ipl_dataset <- read.csv("output.csv",header = TRUE)
head(ipl_dataset)
colnames(ipl_dataset)
dim(ipl_dataset)
ipl_dataset[["X"]]

#Does the dataset contain any NaN values?

colSums(is.na(ipl_dataset))
ipl_dataset <- subset(ipl_dataset, select = -X)
colSums(is.na(ipl_dataset))
# Check unique values in the 'team1' column
unique_team1 <- unique(ipl_dataset$team1)
print(unique_team1)

# Replace "Rising Pune Supergiant" with "Rising Pune Supergiants" in the 'name' column
ipl_dataset$team1 <- replace(ipl_dataset$team1, ipl_dataset$team1 == "Rising Pune Supergiant", "Rising Pune Supergiants")

dim(ipl_dataset)

# Check unique values in the 'team2' column
unique_team2 <- unique(ipl_dataset$team2)
print(unique_team2)

ipl_dataset$team2 <- replace(ipl_dataset$team2, ipl_dataset$team2 == "Rising Pune Supergiant", "Rising Pune Supergiants")
unique(ipl_dataset$team2)

unique_toss_winner <- unique(ipl_dataset$toss_winner)
print(unique_toss_winner)

ipl_dataset$toss_winner <- replace(ipl_dataset$toss_winner, ipl_dataset$toss_winner == "Rising Pune Supergiant", "Rising Pune Supergiants")
unique(ipl_dataset$toss_winner)
ipl_dataset[343,"team2"]

is.na(ipl_dataset$team2)

# Replace empty strings with NA in the 'team2' column
ipl_dataset$team2[ipl_dataset$team2 == ""] <- NA

colSums(is.na(ipl_dataset))

ipl_dataset[ipl_dataset== ""] <- NA

colSums(is.na(ipl_dataset))

ipl_dataset[is.na(ipl_dataset$team2), "margin" ]


ipl_dataset <- ipl_dataset[!is.na(ipl_dataset$team2), ]
dim(ipl_dataset)
colSums(is.na(ipl_dataset))

ipl_dataset <- ipl_dataset[complete.cases(ipl_dataset$team1_score), ]
colSums(is.na(ipl_dataset))

#non sensical values
# Remove the row where man_of_the_match is "16.00 start, First Session 16.00-17.30, Interval 17.30-17.50, Second Session 17.50-19.20"
ipl_dataset <- ipl_dataset[ipl_dataset$man_of_the_match != "16.00 start, First Session 16.00-17.30, Interval 17.30-17.50, Second Session 17.50-19.20", ]
dim(ipl_dataset)
unique(ipl_dataset$winner)

ipl_dataset$winner <- replace(ipl_dataset$winner, ipl_dataset$winner == "Supergiant", "Supergiants")
unique(ipl_dataset$winner)


ipl_dataset$winner[(ipl_dataset$winner == 'Super') & (ipl_dataset$team1 != 'Chennai Super Kings') & (ipl_dataset$team2 != 'Chennai Super Kings')] <- 'Lucknow Super Giants'
unique(ipl_dataset$winner)


abbreviatedTeamNamesList <- unique(ipl_dataset$winner)
abbreviatedTeamNamesList
fullTeamNamesList <- c('Kolkata Knight Riders', 'Delhi Capitals', 'Punjab Kings', 'Gujurat Titans',
                       'Rajasthan Royals', 'Royal Challengers Bangalore', 'Chennai Super Kings',
                       'Lucknow Super Giants', 'Sunrisers Hyderabad', 'Mumbai Indians', 'Tied',
                       'Kings XI Punjab', 'Delhi Daredevils',
                       'Rising Pune Supergiants', 'Gujarat Lions', 'Pune Warriors',
                       'Deccan Chargers', 'Kochi Tuskers Kerala')

dictTeamNames <- setNames(fullTeamNamesList, abbreviatedTeamNamesList)
dictTeamNames

ipl_dataset$winner <- dictTeamNames[ipl_dataset$winner]
unique(ipl_dataset$winner)



ipl_dataset[is.na(ipl_dataset$place), "stadium" ]
# Update values in the 'stadium' column based on conditions
ipl_dataset <- ipl_dataset %>%
  mutate(place = case_when(
    stadium == "Dubai International Cricket Stadium" ~ "Dubai",
    stadium == "Sharjah Cricket Stadium" ~ "Sharjah",
    TRUE ~ place  # Keep the original value if none of the conditions match
  ))

colSums(is.na(ipl_dataset))
dim(ipl_dataset)

ipl_dataset <- subset(ipl_dataset, select = -X)
colSums(is.na(ipl_dataset))


str(ipl_dataset)

write.csv(ipl_dataset, "ipl_dataset.csv", row.names = FALSE)

# Count the highest winning team
most_wins <- table(ipl_dataset$winner)
most_wins <- as.data.frame(most_wins)
names(most_wins) <- c("Team", "Win_Count")
most_wins <- most_wins[order(-most_wins$Win_Count), ]
most_wins
# Plot for most wins
library(ggplot2)

ggplot(most_wins, aes(x = reorder(Team, -Win_Count), y = Win_Count, fill = Win_Count)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(x = "Team", y = "Win Count", title = "Comparing The Win Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create correlation matrix
cor_matrix <- cor(ipl_dataset[, c("team1_score", "team2_score")])

# Print correlation matrix
print(cor_matrix)


# Plot the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust")

# Fit a data model using linear regression
model <- lm(team2_score ~ team1_score + toss_winner + toss_choice   , data = ipl_dataset)

# Print the summary of the model
summary(model)


library(stringr)

ipl_dataset1 <- read.csv("ipl_dataset.csv",header = TRUE)
# Define replacement patterns
stadium_replacements <- c("Zayed Cricket Stadium" = "Sheikh Zayed Stadium",
                          "Sardar Patel Stadium" = "Narendra Modi Stadium",
                          "Andhra Cricket Association-Visakhapatnam District Cricket Association Stadium" = "Dr. Y.S. Rajasekhara Reddy ACA-VDCA Cricket Stadium",
                          "Subrata Roy Sahara Stadium" = "Maharashtra Cricket Association Stadium",
                          "Punjab Cricket Association Stadium" = "Punjab Cricket Association IS Bindra Stadium",
                          "Feroz Shah Kotla" = "Arun Jaitley Stadium",
                          "Himachal Pradesh Cricket Association Stadium" = "Punjab Cricket Association IS Bindra Stadium")

place_replacements <- c("Bengaluru" = "Bangalore",
                        "Chepauk" = "Chennai",
                        "Motera" = "Ahmedabad",
                        "Uppal" = "Hyderabad")

team_replacements <- c("Deccan Chargers" = "Sunrisers Hyderabad",
                       "Delhi Daredevils" = "Delhi Capitals",
                       "Gujarat Lions" = "Gujarat Titans",
                       "Kings XI Punjab" = "Punjab Kings")

# Replace stadium names
ipl_dataset1$stadium <- str_replace_all(ipl_dataset1$stadium, stadium_replacements)


ipl_dataset1$place <- str_replace_all(ipl_dataset1$place, place_replacements)
ipl_dataset1$team1 <- str_replace_all(ipl_dataset1$team1, team_replacements)
ipl_dataset1$team2 <- str_replace_all(ipl_dataset1$team2, team_replacements)
ipl_dataset1$toss_winner <- str_replace_all(ipl_dataset1$toss_winner, team_replacements)
ipl_dataset1$winner <- str_replace_all(ipl_dataset1$winner, team_replacements)


ipl_dataset1

write.csv(ipl_dataset1, "ipl_dataset.csv", row.names = FALSE)

ipl_dataset2 <- read.csv("ipl_dataset.csv",header = TRUE)


# Define stadium-home team mappings
stadium_home_teams <- list(
  "MA Chidambaram Stadium" = "Chennai Super Kings",
  "Arun Jaitley Stadium" = "Delhi Capitals",
  "Narendra Modi Stadium" = "Gujarat Titans",
  "Nehru Stadium" = "Kochi Tuskers Kerala",
  "Eden Gardens" = "Kolkata Knight Riders",
  "Wankhede Stadium" = "Mumbai Indians",
  "Maharashtra Cricket Association Stadium" = c("Pune Warriors", "Rising Pune Supergiants"),
  "Himachal Pradesh Cricket Association Stadium" = "Punjab Kings",
  "Sawai Mansingh Stadium" = "Rajasthan Royals",
  "M Chinnaswamy Stadium" = "Royal Challengers Bangalore",
  "Rajiv Gandhi International Stadium" = "Sunrisers Hyderabad"
)

# Add the "Home Ground" column
ipl_dataset2$Home_Ground <- "Neutral"  # Set initial value as "Neutral"

# Update the "Home Ground" based on stadium-home team mappings
for (stadium in names(stadium_home_teams)) {
  home_teams <- stadium_home_teams[[stadium]]
  ipl_dataset2$Home_Ground[ipl_dataset2$stadium == stadium & (ipl_dataset2$team1 %in% home_teams | ipl_dataset2$team2 %in% home_teams)] <- home_teams
}

write.csv(ipl_dataset2, "ipl_dataset_final.csv", row.names = FALSE)

#------------------------------------------------------------------------------------------------------------------------

#Question 1 -How has the performance of teams differed when they play at home versus away?

# Load the dataset
df <- read.csv("ipl_dataset_final.csv")

# Get the unique teams
teams <- unique(c(df$team1, df$team2))

# Create a data frame to store team performances
team_performances <- data.frame(
  Team = character(),
  Home_Wins = integer(),
  Home_Matches = integer(),
  Away_Wins = integer(),
  Away_Matches = integer(),
  Neutral_Wins = integer(),
  Neutral_Matches = integer(),
  Home_Performance = numeric(),
  Away_Performance = numeric(),
  Neutral_Performance = numeric(),
  stringsAsFactors = FALSE
)

# Iterate over each team
for (team in teams) {
  # Filter records for the team's home matches
  home_matches <- subset(df, Home_Ground == team)
  
  # Filter records for the team's away matches
  away_matches <- subset(df, (team1 == team & Home_Ground != team & Home_Ground != "Neutral") | (team2 == team & Home_Ground != team & Home_Ground != "Neutral"))
  
  # Filter records for the team's neutral venue matches
  neutral_matches <- subset(df, (team1 == team & Home_Ground == "Neutral") | (team2 == team & Home_Ground == "Neutral"))
  
  # Calculate home performance
  home_matches_count <- nrow(home_matches)
  home_wins_count <- nrow(subset(home_matches, winner == team))
  home_performance <- home_wins_count / home_matches_count
  
  # Calculate away performance
  away_matches_count <- nrow(away_matches)
  away_wins_count <- nrow(subset(away_matches, winner == team))
  away_performance <- away_wins_count / away_matches_count
  
  # Calculate neutral performance
  neutral_matches_count <- nrow(neutral_matches)
  neutral_wins_count <- nrow(subset(neutral_matches, winner == team))
  neutral_performance <- neutral_wins_count / neutral_matches_count
  
  # Append team performances to the data frame
  team_performances <- rbind(team_performances, data.frame(
    Team = team,
    Home_Wins = home_wins_count,
    Home_Matches = home_matches_count,
    Away_Wins = away_wins_count,
    Away_Matches = away_matches_count,
    Neutral_Wins = neutral_wins_count,
    Neutral_Matches = neutral_matches_count,
    Home_Performance = home_performance,
    Away_Performance = away_performance,
    Neutral_Performance = neutral_performance
  ))
}

# Print the team performances table
print(team_performances)



library(ggplot2)

# Reshape the data from wide to long format
team_performances_long <- reshape2::melt(team_performances, id.vars = "Team", measure.vars = c("Home_Performance", "Away_Performance", "Neutral_Performance"))

# Calculate total matches played
team_performances_total_matches <- team_performances[, c("Team", "Total_Home_Matches", "Total_Away_Matches", "Total_Neutral_Matches")]
team_performances_total_matches_long <- reshape2::melt(team_performances_total_matches, id.vars = "Team", variable.name = "Match_Type", value.name = "Total_Matches")

# Plot performance graph
plot_performance <- ggplot() +
  geom_bar(data = team_performances_long, aes(x = Team, y = value, fill = variable),
           stat = "identity", position = "dodge", width = 0.8, color = "black") +
  labs(x = "Team", y = "Performance", title = "Team Performance: Home vs Away vs Neutral",
       fill = "Performance") +
  scale_fill_manual(values = c("blue", "orange", "green"), labels = c("Home", "Away", "Neutral")) +
  theme_minimal() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 35, hjust = 1))

# Calculate the position of text labels
position <- position_dodge(width = 0.8)

# Add performance ratio as text labels with increased size and bold
plot_performance <- plot_performance +
  geom_text(data = team_performances_long, aes(x = Team, y = value, label = paste0(round(value * 100), "%"),
                                               group = variable),
            position = position, vjust = -0.5, angle = 0, hjust = 0.5, color = "black", size = 5, fontface = "bold")

# Set the text properties for x-axis, y-axis, and plot title
plot_performance <- plot_performance +
  theme(axis.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"))

# Print the plot
print(plot_performance)




plot_total_matches <- ggplot() +
  geom_bar(data = team_performances_total_matches_long, aes(x = Team, y = Total_Matches, fill = Match_Type),
           stat = "identity", position = "dodge", width = 0.8, color = "black") +
  geom_text(data = team_performances_total_matches_long, aes(x = Team, y = Total_Matches, label = Total_Matches, group = Match_Type),
            position = position_dodge(width = 0.6), vjust = -0.5, color = "black", size = 3) +
  labs(x = "Team", y = "Total Matches", title = "Total Matches Played: Home vs Away vs Neutral",
       fill = "Match Type") +
  scale_fill_manual(values = c("blue", "orange", "green"), labels = c("Home Matches", "Away Matches", "Neutral Matches")) +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text.x = element_text(face = "bold",angle = 35, hjust = 1),
        axis.title = element_text(face = "bold"),  # Make axis labels bold
        plot.title = element_text(face = "bold"))  # Make plot title bold

print(plot_total_matches)


# Combine the plots using grid.arrange from the gridExtra package
gridExtra::grid.arrange(plot_performance, plot_total_matches, ncol = 1)


#-------------------------------------------------------------------------------------------------------------

# Question 2- Which IPL team has the highest winning percentage, and how does it compare to the others?
library(dplyr)

matches <- read.csv("ipl_dataset_final.csv")

# Create an empty data frame to store the results
team_winning_percentages <- data.frame(Team = character(), Winning_Percentage = numeric(), stringsAsFactors = FALSE)

# Iterate over each team
for (team in unique(c(matches$team1, matches$team2))) {
  team_matches <- matches %>%
    filter(team1 == team | team2 == team)  # Filter matches where the team is either team1 or team2
  
  total_matches <- nrow(team_matches)
  wins <- sum(team_matches$winner == team, na.rm = TRUE)
  winning_percentage <- (wins / total_matches) * 100
  
  team_winning_percentages <- rbind(team_winning_percentages, data.frame(Team = team, Winning_Percentage = winning_percentage))
}

# Sort the results in descending order of winning percentage
team_winning_percentages <- team_winning_percentages %>%
  arrange(desc(Winning_Percentage))

# Team with the highest winning percentage
highest_winning_team <- team_winning_percentages[1, ]

# Print the results
cat("Team with the highest winning percentage:", highest_winning_team$Team, "\n")
cat("\nWinning percentages of all teams:\n")
print(team_winning_percentages)

library(ggplot2)

# Sort the team_winning_percentages data frame by winning percentage in descending order
team_winning_percentages <- team_winning_percentages[order(-team_winning_percentages$Winning_Percentage), ]

# Create a bar plot of winning percentages
plot_winning_percentages <- ggplot(team_winning_percentages, aes(x = reorder(Team, -Winning_Percentage), y = Winning_Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  labs(x = "Team", y = "Winning Percentage", title = "IPL Team Winning Percentages") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = paste0(round(Winning_Percentage,1), "%")), vjust = -0.5, color = "black", size = 4)

# Set the text properties for x-axis, y-axis, and plot title
plot_winning_percentages <- plot_winning_percentages +
  theme(axis.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"))

# Print the plot
print(plot_winning_percentages)


#----------------------------------------------------------------------------------------------

#Question 3- Is there a link between who wins the coin toss and who wins the match? 

ipl_dataset_final <- read.csv("ipl_dataset_final.csv")
# Calculate the total number of matches
total_matches <- nrow(ipl_dataset_final)

# Calculate the number of matches won by the toss winner
matches_won_by_toss_winner <- sum(ipl_dataset_final$toss_winner == ipl_dataset_final$winner, na.rm = TRUE)

# Calculate the percentage of matches won by the toss winner
percentage_won_by_toss_winner <- (matches_won_by_toss_winner / total_matches) * 100

# Print the result
cat("Percentage of matches won by the toss winner:", round(percentage_won_by_toss_winner, 2), "%\n")

# Create a data frame for visualization
data <- data.frame(Variable = c("Toss Winner", "Other Team"),
                   Percentage = c(percentage_won_by_toss_winner, 100 - percentage_won_by_toss_winner),
                   Order = c(1, 2))
data
# Plot the bar chart
ggplot(data, aes(x = factor(Order), y = Percentage, fill = Variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Team", y = "Percentage", title = "Percentage of Matches Won by Toss Winner") +
  scale_x_discrete(labels = c("Toss Winner", "Other Team")) +
  scale_fill_manual(values = c("red", "green"), guide = FALSE) +
  geom_text(aes(label = paste0(round(Percentage,1), "%")), vjust = -0.5, color = "black", size = 6, fontface = "bold") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"))



#-------------------------------------------------------------------------------------------------
#Question 4-How has each team's win percentage changed when batting first versus bowling first?

library(ggplot2)
library(tidyverse)

# Calculate win percentage when batting first
win_percentage_batting_first <- ipl_dataset_final %>%
  filter(!is.na(winner)) %>%
  group_by(team1) %>%
  summarise(matches = n(), wins = sum(ifelse(team1 == winner, 1, 0))) %>%
  mutate(win_percentage_batting_first = (wins / matches) * 100) %>%
  select(team = team1, win_percentage_batting_first)

# Calculate win percentage when bowling first
win_percentage_bowling_first <- ipl_dataset_final %>%
  filter(!is.na(winner)) %>%
  group_by(team2) %>%
  summarise(matches = n(), wins = sum(ifelse(team2 == winner, 1, 0))) %>%
  mutate(win_percentage_bowling_first = (wins / matches) * 100) %>%
  select(team = team2, win_percentage_bowling_first)

# Merge win percentage data for batting first and bowling first
win_percentage_data <- merge(win_percentage_batting_first, win_percentage_bowling_first, by = "team", all = TRUE)
win_percentage_data
# Convert data from wide to long format
win_percentage_data_long <- pivot_longer(win_percentage_data, cols = c(win_percentage_batting_first, win_percentage_bowling_first),
                                         names_to = "condition", values_to = "win_percentage")
win_percentage_data_long
# Plot grouped bar chart
plot_win_percentage <- ggplot(win_percentage_data_long, aes(x = team, y = win_percentage, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8, color = "black") +
  geom_text(aes(label = paste0(round(win_percentage, 1), "%")), position = position_dodge(width = 0.8), vjust = -0.5, color = "black", size = 4) +  # Add percentage labels
  labs(x = "Team", y = "Win Percentage", title = "Win Percentage: Batting First vs Bowling First") +
  scale_fill_manual(values = c("blue", "orange"), labels = c("Batting First", "Bowling First")) +
  theme_minimal() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"))

# Print the plot
print(plot_win_percentage)



