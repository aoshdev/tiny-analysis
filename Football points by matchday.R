# Get libraries
library(readr)
library(tidyverse)
library(assertive)

# Get data
url_1920 <- "https://www.football-data.co.uk/mmz4281/1920/E0.csv"
# Data notes: https://www.football-data.co.uk/notes.txt

epl1920 <- read.csv(url_1920)

# Keep useful variables
epl1920a <- epl1920 %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

str(epl1920a)

# Get unique list of teams
teams1920 <- sort(unique(epl1920a$HomeTeam))


# Instantiate empty data frame
epl1920_stack <- data.frame()

for (i in teams1920) {
  
  # Create table of match day, cumulative points for each team
  individualteam1920 <- epl1920a %>% 
    filter(HomeTeam == i | AwayTeam == i) %>% 
    mutate(Team = i,
           Matchday = row_number(),
           Points = case_when(
             (HomeTeam == i & FTR == 'H') | (AwayTeam == i & FTR == 'A') ~ 3 ,
             (HomeTeam == i & FTR == 'A') | (AwayTeam == i & FTR == 'H') ~ 0 ,
             FTR == 'D' ~ 1),
           CumulativePoints = cumsum(Points))
  
  # Append to single table
  epl1920_stack <- rbind(epl1920_stack, individualteam1920)
  
}

# Check rows = 38 games in season * 20 teams
assert_all_are_true(nrow(epl1920_stack) == 38*20)

# Number of teams to show
n_teams = 4
epl1920_top <- epl1920_stack %>% 
  filter(Matchday == 38) %>% 
  top_n(wt = CumulativePoints, n = n_teams) %>% 
  pull(Team)

epl1920_top

epl1920_final <- epl1920_stack %>% 
  filter(Team %in% epl1920_top)

# Graph
matchday_label <- seq(from = min(epl1920_final$Matchday), to = max(epl1920_final$Matchday), by = 2)

ggplot(epl1920_final, aes(x = Matchday, y = CumulativePoints, color = Team)) + 
  geom_line() +
  scale_x_continuous(labels = matchday_label, breaks = matchday_label) + 
  ggtitle(paste('EPL 2019/20: Points by matchday (top', n_teams, 'EPL teams)')) +
  ylab("Points")


