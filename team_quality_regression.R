library(tidyverse)

# from American Soccer Analysis MLS Interactive Tables
# https://app.americansocceranalysis.com/mls/
game.stats.2019 <- read_csv("./xg_data/american_soccer_analysis_mls_xgoals_games_2019.csv")

## Data prep

# clean up names
names(game.stats.2019)
names(game.stats.2019)[4] <- "HxGtTeam"
names(game.stats.2019)[8] <- "AxGtTeam"
names(game.stats.2019)[14] <- "HxPtsExpected"
names(game.stats.2019)[15] <- "AxPtsExpected"
names(game.stats.2019)
# View(game.stats.2019)

# split into separate dataframes for home and away
# to combine both into a single "long-form" dataframe
names(game.stats.2019)
home.stats.2019 <- data.frame("Date"=game.stats.2019$Date,
                              "Location"="Home",
                              "Team"=game.stats.2019$Home,
                              "Opponent"=game.stats.2019$Away,
                              "xG"=game.stats.2019$HxGtTeam,
                              "xG.Against"=game.stats.2019$AxGtTeam,
                              "Goals"=game.stats.2019$HG,
                              "Goals.Against"=game.stats.2019$AG,
                              "Points.xG.Simul"=game.stats.2019$HxPtsExpected)
away.stats.2019 <- data.frame("Date"=game.stats.2019$Date,
                              "Location"="Away",
                              "Team"=game.stats.2019$Away,
                              "Opponent"=game.stats.2019$Home,
                              "xG"=game.stats.2019$AxGtTeam,
                              "xG.Against"=game.stats.2019$HxGtTeam,
                              "Goals"=game.stats.2019$AG,
                              "Goals.Against"=game.stats.2019$HG,
                              "Points.xG.Simul"=game.stats.2019$AxPtsExpected)

full.stats.2019 <- bind_rows(home.stats.2019,away.stats.2019)
dim(home.stats.2019)
dim(away.stats.2019)
dim(full.stats.2019)
head(full.stats.2019)
tail(full.stats.2019)

# calculate actual points earned
full.stats.2019$Points <- 1
full.stats.2019$Points[full.stats.2019$Goals > full.stats.2019$Goals.Against] <- 3
full.stats.2019$Points[full.stats.2019$Goals < full.stats.2019$Goals.Against] <- 0

# sort the season by date, listing Home team first
full.stats.2019 <- full.stats.2019 %>%
  arrange(
    Date, desc(Location)
  )
# View(full.stats.2019)

# add a game number (e.g. it is Houston's Nth game of the season)
full.stats.2019 <- full.stats.2019 %>%
  group_by(Team) %>% 
  mutate(Game.Number=row_number()) %>%
  ungroup()
# View(full.stats.2019)

## Test predictive performance
# use each game number as a cutoff, measure predictive power of past performance for future
# compare xG-based past performance metrics to actual goals

gameday.cutoffs <- 1:30

holdout.mean.error.base.values <- c()
holdout.mean.error.xg.values <- c()

for (gameday.cutoff in gameday.cutoffs) {
  training.data <- full.stats.2019 %>% filter(Game.Number <= gameday.cutoff)
  test.data <- full.stats.2019 %>% filter(Game.Number > gameday.cutoff)
  
  # a simple model which assumes points per game continue unchanged for each team
  ppg.before <- training.data %>% group_by(Team) %>% summarise(ppg.before = mean(Points))
  ppg.after <- test.data %>% group_by(Team) %>% summarise(ppg.after = mean(Points))
  ppg.merged <- ppg.before %>% inner_join(ppg.after, by=c("Team"="Team"))
  holdout.mean.error.base <- mean(abs(ppg.merged$ppg.after - ppg.merged$ppg.before))
  holdout.mean.error.base.values <- c(holdout.mean.error.base.values, holdout.mean.error.base)
  
  # a model which assumes points per game based on xG simulation continue
  # unchanged for each team
  ppg.before.xg <- training.data %>% group_by(Team) %>% summarise(ppg.before.simul = mean(Points.xG.Simul))
  ppg.after <- test.data %>% group_by(Team) %>% summarise(ppg.after = mean(Points))
  ppg.merged.xg <- ppg.before.xg %>% inner_join(ppg.after, by=c("Team"="Team"))
  holdout.mean.error.xg <- mean(abs(ppg.merged.xg$ppg.after - ppg.merged.xg$ppg.before.simul))
  holdout.mean.error.xg.values <- c(holdout.mean.error.xg.values, holdout.mean.error.xg)
  
  # a model using a weighted combination of actual and xG-simulated points per game
}

basic.model.performance <- data.frame(
  "GameDay"=gameday.cutoffs,
  "Extrapolation"="Actual Results",
  "Mean Error"=holdout.mean.error.base.values
)
xg.model.performance <- data.frame(
  "GameDay"=gameday.cutoffs,
  "Extrapolation"="xG-Simulated Results",
  "Mean Error"=holdout.mean.error.xg.values
)

model.performance.comp <- bind_rows(basic.model.performance,
                                    xg.model.performance)

# plot Mean Error of extrapolating previous results forward
# using actual results vs. xG-simulated results
ggplot(data=model.performance.comp) + 
  geom_line(mapping=aes(x=GameDay,y=Mean.Error,col=Extrapolation)) + 
  scale_y_reverse() + 
  ggtitle("Simple PPG Extrapolation Models")


## build a linear model to predict points per game based on xG, and one based on actual goals
## and a model based on team results

holdout.lm.goals.mean.error.avg.values <- c()
holdout.lm.xg.mean.error.avg.values <- c()
holdout.lm.results.mean.error.avg.values <- c()

gameday.cutoffs <- 1:30
teams <- unique(full.stats.2019$Team)

# measure performance of the linear models at each gameday cutoff
for (gameday.cutoff in gameday.cutoffs) {
  
  # use pre-cutoff data to predict post-cutoff performance on a per-team basis
  predictor.data <- full.stats.2019 %>% filter(Game.Number <= gameday.cutoff) %>%
    group_by(Team) %>%
    summarize(avg.xG=mean(xG),
              avg.xG.Against=mean(xG.Against),
              avg.Goals=mean(Goals),
              avg.Goals.Against=mean(Goals.Against),
              avg.Points.Before=mean(Points))
  label.data <- full.stats.2019 %>% filter(Game.Number > gameday.cutoff) %>%
    group_by(Team) %>%
    summarize(avg.Points.After=mean(Points))
  
  lm.data <- predictor.data %>% inner_join(label.data, by="Team")
  # head(lm.data)
  
  # measure model performance using leave-one-out cross-validation with each team as a holdout
  for (holdout.team in teams) {
    holdout.lm.goals.mean.error.crossval.values <- c()
    holdout.lm.xg.mean.error.crossval.values <- c()
    holdout.lm.results.mean.error.crossval.values <- c()
    
    # Note: the training / test split is not totally clean because the "other side" of the holdout 
    # team's results are included in the training data, but that shouldn't cause major problems
    # TODO fix this by removing all games involving the holdout team from training, even their opponent's results
    training.data <- lm.data %>% filter(Team != holdout.team)
    test.data <- lm.data %>% filter(Team == holdout.team)
    
    # goals-based lm
    goals.lm <- lm(data=training.data, avg.Points.After ~ avg.Goals + avg.Goals.Against)
    goals.lm.pred <- predict(goals.lm, newdata = test.data)
    holdout.lm.goals.mean.error.crossval.val <- mean(abs(test.data$avg.Points.After - goals.lm.pred))
    holdout.lm.goals.mean.error.crossval.values <- c(holdout.lm.goals.mean.error.crossval.values, holdout.lm.goals.mean.error.crossval.val)
    
    # xg-based lm
    xg.lm <- lm(data=training.data, avg.Points.After ~ avg.xG + avg.xG.Against)
    xg.lm.pred <- predict(xg.lm, newdata = test.data)
    holdout.lm.xg.mean.error.crossval.val <- mean(abs(test.data$avg.Points.After - xg.lm.pred))
    holdout.lm.xg.mean.error.crossval.values <- c(holdout.lm.xg.mean.error.crossval.values, holdout.lm.xg.mean.error.crossval.val)
    
    # previous results-based lm
    results.lm <- lm(data=training.data, avg.Points.After ~ avg.Points.Before)
    results.lm.pred <- predict(results.lm, newdata = test.data)
    holdout.lm.results.mean.error.crossval.val <- mean(abs(test.data$avg.Points.After - results.lm.pred))
    holdout.lm.results.mean.error.crossval.values <- c(holdout.lm.results.mean.error.crossval.values, holdout.lm.results.mean.error.crossval.val)
    
    # TODO add "kitchen sink" linear model
  }
  
  # aggregate the crossval mean errors for each holdout team into a single value per gameday cutoff
  holdout.lm.goals.mean.error.avg.values <- c(holdout.lm.goals.mean.error.avg.values,
                                              mean(abs(holdout.lm.goals.mean.error.crossval.values)))
  holdout.lm.xg.mean.error.avg.values <- c(holdout.lm.xg.mean.error.avg.values,
                                           mean(abs(holdout.lm.xg.mean.error.crossval.values)))
  holdout.lm.results.mean.error.avg.values <- c(holdout.lm.results.mean.error.avg.values,
                                                mean(abs(holdout.lm.results.mean.error.crossval.values)))
}

goals.lm.performance <- data.frame(
  "GameDay"=gameday.cutoffs,
  "Model"="Goals-Based LM",
  "Mean Error"=holdout.lm.goals.mean.error.avg.values
)
xg.lm.performance <- data.frame(
  "GameDay"=gameday.cutoffs,
  "Model"="xG-Based LM",
  "Mean Error"=holdout.lm.xg.mean.error.avg.values
)
results.lm.performance <- data.frame(
  "GameDay"=gameday.cutoffs,
  "Model"="Results-Based LM",
  "Mean Error"=holdout.lm.results.mean.error.avg.values
)

linear.model.performance.comp <- bind_rows(goals.lm.performance,
                                    xg.lm.performance,
                                    results.lm.performance)

# plot Mean Error of extrapolating previous results forward
# using actual results vs. xG-simulated results
ggplot(data=linear.model.performance.comp) + 
  geom_line(mapping=aes(x=GameDay,y=Mean.Error,col=Model)) + 
  scale_y_reverse() + 
  ggtitle("Linear Models")
# TODO what accounts for the steep performance drop around gameday 25?


## TODO incorporate multiple years of data

## TODO does counting a draw as 1.5 points for predicting future results help?
## or using wins and draws as separate predictors?

