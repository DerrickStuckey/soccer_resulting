library(tidyverse)

# from American Soccer Analysis MLS Interactive Tables
# https://app.americansocceranalysis.com/mls/
game.stats.2020 <- read_csv("american_soccer_analysis_mls_xgoals_games_2020-07-25.csv")

## Data prep

# clean up names
names(game.stats.2020)
names(game.stats.2020)[4] <- "HxGtTeam"
names(game.stats.2020)[8] <- "AxGtTeam"
names(game.stats.2020)[14] <- "HxPtsExpected"
names(game.stats.2020)[15] <- "AxPtsExpected"
names(game.stats.2020)
# View(game.stats.2020)

# split into separate dataframes for home and away
# to combine both into a single "long-form" dataframe
names(game.stats.2020)
home.stats.2020 <- data.frame("Date"=game.stats.2020$Date,
                              "Location"="Home",
                              "Team"=game.stats.2020$Home,
                              "xG"=game.stats.2020$HxGtTeam,
                              "xG.Against"=game.stats.2020$AxGtTeam,
                              "Goals"=game.stats.2020$HG,
                              "Goals.Against"=game.stats.2020$AG,
                              "Points"=game.stats.2020$HPtsActual)
away.stats.2020 <- data.frame("Date"=game.stats.2020$Date,
                              "Location"="Away",
                              "Team"=game.stats.2020$Away,
                              "xG"=game.stats.2020$AxGtTeam,
                              "xG.Against"=game.stats.2020$HxGtTeam,
                              "Goals"=game.stats.2020$AG,
                              "Goals.Against"=game.stats.2020$HG,
                              "Points"=game.stats.2020$APtsActual)

full.stats.2020 <- bind_rows(home.stats.2020,away.stats.2020)
dim(home.stats.2020)
dim(away.stats.2020)
dim(full.stats.2020)
head(full.stats.2020)
tail(full.stats.2020)

# calculate actual points earned
full.stats.2020$Points <- 1
full.stats.2020$Points[full.stats.2020$Goals > full.stats.2020$Goals.Against] <- 3
full.stats.2020$Points[full.stats.2020$Goals < full.stats.2020$Goals.Against] <- 0

# sort the season by date, listing Home team first
full.stats.2020 <- full.stats.2020 %>%
  arrange(
    Date, desc(Location)
  )
# View(full.stats.2020)

# add a game number (e.g. it is Houston's Nth game of the season)
full.stats.2020 <- full.stats.2020 %>%
  group_by(Team) %>% 
  mutate(Game.Number=row_number()) %>%
  ungroup()

## Test predictive performance
# use each game number as a cutoff, measure predictive power of past performance for future
# measure average performance over all teams using leave-one-out cross-validation
# compare xG-based past performance metrics to actual goals



