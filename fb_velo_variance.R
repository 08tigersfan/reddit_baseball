library(baseballr)
library(paletteer)
library(tidyverse)

dat <- read.csv("statcast_pitches_2021_04_01_to_2023_05_09.csv")

# types of fastballs:  FA = Fastball,  FF = 4-Seam,  FT = 2-Seam,  FC = Cutter,  SI = Sinker
fastballs <- c("FA", "FF", "FT", "FC", "SI")

# reduce the data to one row for each pitcher_x_pitch combination
pitches <- dat %>%
  group_by(player_name, pitcher, pitch_type) %>%
  summarize(n = n()) %>%
  arrange(desc(n), .by_group = TRUE)

# subset the pitcher_x_pitch data to just fastballs
pitches_fastballs <- pitches %>%
  filter(pitch_type %in% fastballs)

# reduce the data to one row for each pitcher_x_game combination
# create a variable "innings" storing which innings that pitcher pitched in for that game
# create a dummy variable "start" that equals 1 if that pitcher pitched in at least 3 innings,
# starting no later than the second inning
starters_games <- dat %>%
  group_by(player_name, pitcher, game_pk) %>%
  summarize(innings = paste(sort(unique(inning)), collapse = "")) %>%
  mutate(start = ifelse(grepl("123", innings, fixed = TRUE) | grepl("234", innings, fixed = TRUE), 1, 0)) %>%
  filter(start == 1)

# count the number of "starts" each pitcher has had since the beginning of 2021
# if the number of "starts" is at least 15, consider that pitcher to be a qualified starter
starters <- starters_games %>%
  group_by(player_name, pitcher) %>%
  summarize(starts = n()) %>%
  filter(starts >= 15) %>%
  pull(pitcher)

# subset the pitcher_x_fastball data to just qualified starters
pitches_fastballs_starters <- pitches_fastballs %>%
  filter(pitcher %in% starters) %>%
  ungroup()

# subset the data to just fastballs thrown by qualified starters in the games in which they pitched
# at least 3 innings, starting no later than the second inning
dat_sub <- dat %>%
  right_join(pitches_fastballs_starters %>% select(pitcher, pitch_type),
             by = c("pitcher", "pitch_type")) %>%
  filter(game_pk %in% unique(starters_games$game_pk))

# using the subsetted data, for each game_x_pitcher_x_pitch combination,
# find the number of pitches thrown, the mean velocity, and the standard deviation of the velocity
# only consider instances where the pitcher threw at least 10 of that pitch in the start
# (with valid release_speed measurements)
mean_sd_fastball_velo_by_start <- dat_sub %>%
  group_by(game_pk, player_name, pitcher, pitch_type) %>%
  filter(sum(!is.na(release_speed)) >= 10) %>%
  summarize(n = sum(!is.na(release_speed)),
            mean_velo = mean(release_speed, na.rm = TRUE),
            sd_velo = sd(release_speed, na.rm = TRUE))

# find the average velocity standard deviation across all starts for each pitcher_x_pitch combination
# there should be at least 150 of that type of pitch thrown by that pitcher since the beginning of 2021
# (with valid release_speed measurements)
avg_sd_fastball_velo <- mean_sd_fastball_velo_by_start %>%
  group_by(pitch_type, player_name, pitcher) %>%
  summarize(n = sum(n),
            avg_velo_sd = mean(sd_velo, na.rm = TRUE)) %>%
  filter(n >= 150) %>%
  arrange(desc(avg_velo_sd))

# create a player_pitch variable that stores the pitcher's name and pitch type in the format:
# FirstName LastName's PitchType (ex. Justin Verlander's 4-Seam)
avg_sd_fastball_velo_plot <- avg_sd_fastball_velo %>%
  mutate(pitch_type = ifelse(pitch_type == "SI", "Sinker", pitch_type)) %>%
  mutate(pitch_type = ifelse(pitch_type == "FC", "Cutter", pitch_type)) %>%
  mutate(pitch_type = ifelse(pitch_type == "FF", "4-Seam", pitch_type)) %>%
  mutate(pitch_type = ifelse(pitch_type == "FT", "2-Seam", pitch_type)) %>%
  mutate(pitch_type = ifelse(pitch_type == "FA", "Fastball", pitch_type)) %>%
  mutate(player_name = sub("(\\w+),\\s(\\w+)", "\\2 \\1", player_name)) %>%
  mutate(player_pitch = paste0(player_name, "'s ", pitch_type))

# plot the top and bottom 15 pitches (with regards to average standard deviation of velocity)
avg_sd_fastball_velo_plot %>%
  head(15) %>%
  ggplot() +
  geom_col(aes(y = reorder(player_pitch, avg_velo_sd), x = avg_velo_sd, fill = avg_velo_sd)) +
  theme_bw() +
  labs(title = "Average Standard Deviation of Fastball Velocity within Starts (Top 15 out of 405 Qualified Pitches)",
       subtitle = "Data from Opening Day 2021 through May 9th, 2023",
       x = "Average Standard Deviation (MPH)",
       y = "") +
  guides(fill = "none") +
  theme(axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 16)) +
  scale_fill_paletteer_c("ggthemes::Green") +
  scale_x_continuous(expand = c(0, 0.02))

avg_sd_fastball_velo_plot %>%
  tail(15) %>%
  ggplot() +
  geom_col(aes(y = reorder(player_pitch, -avg_velo_sd), x = avg_velo_sd, fill = avg_velo_sd)) +
  theme_bw() +
  labs(title = "Average Standard Deviation of Fastball Velocity within Starts (Bottom 15 out of 405 Qualified Pitches)",
       subtitle = "Data from Opening Day 2021 through May 9th, 2023",
       x = "Average Standard Deviation (MPH)",
       y = "") +
  guides(fill = "none") +
  theme(axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 16)) +
  scale_fill_paletteer_c("ggthemes::Red", direction = -1) +
  scale_x_continuous(expand = c(0, 0.02))