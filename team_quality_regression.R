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
                              "Points"=game.stats.2020$HPtsActual,
                              "Points.xG.Simul"=game.stats.2020$HxPtsExpected)
away.stats.2020 <- data.frame("Date"=game.stats.2020$Date,
                              "Location"="Away",
                              "Team"=game.stats.2020$Away,
                              "xG"=game.stats.2020$AxGtTeam,
                              "xG.Against"=game.stats.2020$HxGtTeam,
                              "Goals"=game.stats.2020$AG,
                              "Goals.Against"=game.stats.2020$HG,
                              "Points"=game.stats.2020$APtsActual,
                              "Points.xG.Simul"=game.stats.2020$AxPtsExpected)

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
# compare xG-based past performance metrics to actual goals

gameday.cutoffs <- 1:30

holdout.mean.error.base.cutoffs <- c()
holdout.mean.error.xg.cutoffs <- c()
# holdout.mean.error.combined.cutoffs <- c()

for (gameday.cutoff in gameday.cutoffs) {
  training.data <- full.stats.2020 %>% filter(Game.Number <= gameday.cutoff)
  test.data <- full.stats.2020 %>% filter(Game.Number > gameday.cutoff)
  
  # a simple model which assumes points per game continue unchanged for each team
  ppg.before <- training.data %>% group_by(Team) %>% summarise(ppg.before = mean(Points))
  ppg.after <- test.data %>% group_by(Team) %>% summarise(ppg.after = mean(Points))
  ppg.merged <- ppg.before %>% inner_join(ppg.after, by=c("Team"="Team"))
  holdout.mean.error.base <- mean(abs(ppg.merged$ppg.after - ppg.merged$ppg.before))
  holdout.mean.error.base.cutoffs <- c(holdout.mean.error.base.cutoffs, holdout.mean.error.base)
  
  # a model which assumes points per game based on xG simulation continue
  # unchanged for each team
  ppg.before.xg <- training.data %>% group_by(Team) %>% summarise(ppg.before.simul = mean(Points.xG.Simul))
  ppg.after <- test.data %>% group_by(Team) %>% summarise(ppg.after = mean(Points))
  ppg.merged.xg <- ppg.before.xg %>% inner_join(ppg.after, by=c("Team"="Team"))
  holdout.mean.error.xg <- mean(abs(ppg.merged.xg$ppg.after - ppg.merged.xg$ppg.before.simul))
  holdout.mean.error.xg.cutoffs <- c(holdout.mean.error.xg.cutoffs, holdout.mean.error.xg)
  
  # a model using a weighted combination of actual and xG-simulated points per game
}

basic.model.performance <- data.frame(
  "GameDay"=gameday.cutoffs,
  "Model"="Actual Results",
  "Mean Error"=holdout.mean.error.base.cutoffs
)
xg.model.performance <- data.frame(
  "GameDay"=gameday.cutoffs,
  "Model"="xG-Simulated Results",
  "Mean Error"=holdout.mean.error.xg.cutoffs
)

model.performance.comp <- bind_rows(basic.model.performance,
                                    xg.model.performance)

# plot Mean Error of extrapolating previous results forward
# using actual results vs. xG-simulated results
ggplot(data=model.performance.comp) + 
  geom_line(mapping=aes(x=GameDay,y=Mean.Error,col=Model)) + 
  scale_y_reverse()


## measure average performance over all teams using leave-one-out cross-validation

# teams <- unique(full.stats.2020$Team)

# for (team in teams) {
#   training.data <- full.stats.2020 %>% filter(Team != team)
#   test.data <- full.stats.2020 %>% filter(Team == team)
# }

