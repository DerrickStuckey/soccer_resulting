library(tidyverse)
library(lubridate)

# from American Soccer Analysis MLS Interactive Tables
# https://app.americansocceranalysis.com/mls/
# "Games", "Regular Season", 2013-2019
game.stats <- read_csv("./xg_data/american_soccer_analysis_mls_xgoals_games_2013_2019.csv")


## Data prep

# clean up names
names(game.stats)
names(game.stats)[4] <- "HxGtTeam"
names(game.stats)[8] <- "AxGtTeam"
names(game.stats)[14] <- "HxPtsExpected"
names(game.stats)[15] <- "AxPtsExpected"
names(game.stats)
# View(game.stats)

# split into separate dataframes for home and away
# to combine both into a single "long-form" dataframe
names(game.stats)
home.stats <- data.frame("Date"=game.stats$Date,
                              "Location"="Home",
                              "Team"=game.stats$Home,
                              "Opponent"=game.stats$Away,
                              "xG"=game.stats$HxGtTeam,
                              "xG.Against"=game.stats$AxGtTeam,
                              "Goals"=game.stats$HG,
                              "Goals.Against"=game.stats$AG,
                              "Points.xG.Simul"=game.stats$HxPtsExpected)
away.stats <- data.frame("Date"=game.stats$Date,
                              "Location"="Away",
                              "Team"=game.stats$Away,
                              "Opponent"=game.stats$Home,
                              "xG"=game.stats$AxGtTeam,
                              "xG.Against"=game.stats$HxGtTeam,
                              "Goals"=game.stats$AG,
                              "Goals.Against"=game.stats$HG,
                              "Points.xG.Simul"=game.stats$AxPtsExpected)

full.stats <- bind_rows(home.stats,away.stats)
dim(home.stats)
dim(away.stats)
dim(full.stats)
head(full.stats)
tail(full.stats)

# calculate actual points earned
full.stats$Points <- 1
full.stats$Points[full.stats$Goals > full.stats$Goals.Against] <- 3
full.stats$Points[full.stats$Goals < full.stats$Goals.Against] <- 0

# sort the season by date, listing Home team first
full.stats <- full.stats %>%
  arrange(
    Date, desc(Location)
  )
# View(full.stats)

# split by year
full.stats$Year <- lubridate::year(full.stats$Date)

# add a game number (e.g. it is Houston's Nth game of the season)
full.stats <- full.stats %>%
  group_by(Team, Year) %>% 
  mutate(Game.Number=row_number()) %>%
  ungroup()
# View(full.stats %>% filter(Team=="FCD"))


## Test predictive performance
# use each game number as a cutoff, measure predictive power of past performance for future
# compare xG-based past performance metrics to actual goals

years <- unique(full.stats$Year)
years

# initialize a dataframe to hold all the results
model.performance.comp <- data.frame("GameDay"=c(),
                                     "Extrapolation"=c(),
                                     "Mean Error"=c(),
                                     "Year"=c())

for (year in years) {
  
  gameday.cutoffs <- 1:30
  
  holdout.mean.error.base.values <- c()
  holdout.mean.error.xg.values <- c()
  
  for (gameday.cutoff in gameday.cutoffs) {
    training.data <- full.stats %>% filter(Game.Number <= gameday.cutoff,
                                           Year == year)
    test.data <- full.stats %>% filter(Game.Number > gameday.cutoff,
                                       Year == year)
    
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
    "Mean Error"=holdout.mean.error.base.values,
    "Year"=year
  )
  xg.model.performance <- data.frame(
    "GameDay"=gameday.cutoffs,
    "Extrapolation"="xG-Simulated Results",
    "Mean Error"=holdout.mean.error.xg.values,
    "Year"=year
  )
  
  # append both models' results to the comp dataframe
  model.performance.comp <- bind_rows(model.performance.comp,
                                      basic.model.performance,
                                      xg.model.performance)
}

model.performance.comp$Year <- as.factor(model.performance.comp$Year)

# plot Mean Error of extrapolating previous results forward
# using actual results vs. xG-simulated results
ggplot(data=model.performance.comp) + 
  geom_line(mapping=aes(x=GameDay,y=Mean.Error,col=Year,linetype=Extrapolation)) + 
  # scale_y_reverse() + 
  ggtitle("Simple PPG Extrapolation Models")

# plot results aggregated across years
model.performance.comp.year.agg <- model.performance.comp %>%
  group_by(GameDay,Extrapolation) %>% 
  summarise(Mean.Error=mean(Mean.Error))
ggplot(data=model.performance.comp.year.agg) + 
  geom_line(mapping=aes(x=GameDay,y=Mean.Error,col=Extrapolation)) + 
  # scale_y_reverse() + 
  ggtitle("Simple PPG Extrapolation Models")


## build a linear model to predict points per game based on xG, and one based on actual goals
## and a model based on team results
years <- unique(full.stats$Year)
years

# initialize a dataframe to hold all the results
linear.model.performance.comp <- data.frame("GameDay"=c(),
                                     "Model"=c(),
                                     "Mean Error"=c(),
                                     "Year"=c())

for (year in years) {
  
  holdout.lm.goals.mean.error.avg.values <- c()
  holdout.lm.xg.mean.error.avg.values <- c()
  holdout.lm.results.mean.error.avg.values <- c()
  
  gameday.cutoffs <- 1:30
  
  # measure performance of the linear models at each gameday cutoff
  for (gameday.cutoff in gameday.cutoffs) {
    
    # use pre-cutoff data to predict post-cutoff performance on a per-team basis
    predictor.data <- full.stats %>% 
      filter(Game.Number <= gameday.cutoff,
             Year == year) %>%
      group_by(Team) %>%
      summarize(avg.xG=mean(xG),
                avg.xG.Against=mean(xG.Against),
                avg.Goals=mean(Goals),
                avg.Goals.Against=mean(Goals.Against),
                avg.Points.Before=mean(Points))
    label.data <- full.stats %>% 
      filter(Game.Number > gameday.cutoff,
             Year == year) %>%
      group_by(Team) %>%
      summarize(avg.Points.After=mean(Points))
    
    lm.data <- predictor.data %>% inner_join(label.data, by="Team")
    # head(lm.data)
    
    # measure model performance using leave-one-out cross-validation with each team as a holdout
    teams <- unique(lm.data$Team)
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
    "Mean Error"=holdout.lm.goals.mean.error.avg.values,
    "Year"=year
  )
  xg.lm.performance <- data.frame(
    "GameDay"=gameday.cutoffs,
    "Model"="xG-Based LM",
    "Mean Error"=holdout.lm.xg.mean.error.avg.values,
    "Year"=year
  )
  results.lm.performance <- data.frame(
    "GameDay"=gameday.cutoffs,
    "Model"="Results-Based LM",
    "Mean Error"=holdout.lm.results.mean.error.avg.values,
    "Year"=year
  )
  
  linear.model.performance.comp <- bind_rows(linear.model.performance.comp,
                                             goals.lm.performance,
                                             xg.lm.performance,
                                             results.lm.performance)
}

linear.model.performance.comp$Year <- as.factor(linear.model.performance.comp$Year)

# plot Mean Error of extrapolating previous results forward
# using actual results vs. xG-simulated results
ggplot(data=linear.model.performance.comp) + 
  geom_line(mapping=aes(x=GameDay,y=Mean.Error,col=Year,linetype=Model)) + 
  scale_y_reverse() + 
  ggtitle("Linear Models")

# plot results aggregated across years
linear.model.performance.comp.year.agg <- linear.model.performance.comp %>%
  group_by(GameDay,Model) %>% 
  summarise(Mean.Error=mean(Mean.Error))
ggplot(data=linear.model.performance.comp.year.agg) + 
  geom_line(mapping=aes(x=GameDay,y=Mean.Error,col=Model)) + 
  # scale_y_reverse() + 
  ggtitle("Linear Models")
# TODO does future performance really peak this early (around week 9)?
# probably so, but only because the test dataset is a moving target
# try fixing test evaluation to weeks 18-34 for all models


## TODO does counting a draw as 1.5 points for predicting future results help?
## or using wins and draws as separate predictors?

