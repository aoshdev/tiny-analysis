# How big of a role does luck play in our lives?
# Inspired by Veritasium's video 'Is Success Luck or Hard Work?

# Assign variables
skill_perc = 0.95 # Proportion of total score attributed to skill
luck_perc = 0.05 # Proportion of total score attributed to luck

n_users = 18300 # Total number of participants
n_win = 11 # Total number of winners

n_simul = 1000 # Number of simulations

# Check assigned variables
assertthat::assert_that(skill_perc + luck_perc == 1, msg = 'Skill_perc and luck_perc do not sum to 1')
assertthat::assert_that(n_win < n_users, msg = 'Can\'t have more winners than users')

# Instantiate vectors
winners_luck <- c()
number_overlap <- c()

for (i in 1:n_simul) {
  
  # Randomly generate skill and luck vector from 0 to 100
  skill <- runif(n_users, 0, 100)
  luck <- runif(n_users, 0, 100)
  
  overall_score <- skill*skill_perc + luck*luck_perc
  
  # Indices for top n_win based on skill alone
  skill_winners <- which(skill >= sort(skill, decreasing = TRUE)[n_win])
  
  # Indices for top n_win based on luck and skill
  overall_winners <- which(overall_score >= sort(overall_score, decreasing = TRUE)[n_win])
  
  
  
  # How many would have won if only skill was considered
  n_overlap <- length(intersect(skill_winners,overall_winners))
  number_overlap[i] <- n_overlap
  
  # Average luck score of winners
  avg_luck <- mean(luck[overall_winners])
  winners_luck[i] <- avg_luck
  
}

win_luck_score <- mean(winners_luck)
win_without_luck <- mean(number_overlap)


print(paste('Winners average luck score was: ',format(round(win_luck_score,2), nsmall=2),'out of 100.'))
print(paste('Of the 11 winners,', format(round(win_without_luck,2), nsmall = 2),'would have won if no luck was involved.'))












