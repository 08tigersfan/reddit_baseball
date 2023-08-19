library(baseballr)
library(tidyverse)

# this data originated from the following website: umpscorecards.com/single_umpire/?name=CB%20Bucknor
cb <- read.csv("cb_bucknor_games.csv")

# grab the dates and teams from all games that CB Bucknor was the home plate umpire for
cb <- cb %>%
  mutate(game_date = ymd(Date),
         home_team = Home,
         away_team = Away) %>%
  select(game_date, home_team, away_team)

# this dataset contains pitches from all days where CB Bucknor was the home plate umpire, from the start of 2015 through 2023-07-04
dat <- read.csv("statcast_pitches_cb_bucknor.csv")

# it is then filtered to only games that CB was a part of
dat <- dat %>%
  mutate(game_date = ymd(game_date))

dat <- dat %>%
  inner_join(cb, by = c("game_date", "home_team", "away_team"))

# these are games that were parts of doubleheaders that CB Bucknor was not the home plate umpire for
doubleheaders <- dat %>%
  distinct(game_pk, game_date, home_team, away_team) %>%
  group_by(game_date) %>%
  filter(n() == 2)
print(doubleheaders)

no_cb <- dat %>%
  filter(game_pk %in% doubleheaders$game_pk) %>%
  filter(player_name %in% c("Giolito, Lucas", "Eflin, Zach", "Peterson, David", "Cobb, Alex", "Sparkman, Glenn")) %>%
  distinct(game_pk) %>%
  pull(game_pk)

dat <- dat %>%
  filter(!(game_pk %in% no_cb))

# filter the data to only called strikes that have valid location data
dat <- dat %>%
  filter(description == "called_strike" & !is.na(plate_x) & !is.na(plate_z))

# calculate how many feet all called strikes missed the strike zone by, both vertically and horizontally, if at all
dat <- dat %>%
  mutate(hmiss = ifelse(plate_x < -17/24, -17/24 - plate_x, ifelse(plate_x > 17/24, plate_x - 17/24, 0)),
         vmiss = ifelse(plate_z > sz_top, plate_z - sz_top, ifelse(plate_z < sz_bot, sz_bot - plate_z, 0)),
         totmiss = sqrt(hmiss^2 + vmiss^2))

# find the top 10 worst calls, just since 2020
dat <- dat %>%
  arrange(desc(totmiss))

worst = dat %>%
  filter(game_year >= 2020) %>%
  mutate(des = paste0(substr(des, 1, 30), "...")) %>%
  head(10) %>%
  select(game_date, player_name, pitch_type, balls, strikes, hmiss, vmiss, totmiss, des)
print(worst)