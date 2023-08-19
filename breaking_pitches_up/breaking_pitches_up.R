library(baseballr)
library(tidyverse)

dat <- read.csv("statcast_pitches_2022_04_07_to_2023_05_17.csv")

# codes and names of the different types of breaking balls
breaking <- data.frame(pitch_type = c("SL", "KC", "CU", "ST",
                                      "EP", "CS", "KN", "SV"),
                       pitch_name = c("Slider", "Knuckle Curve", "Curveball", "Sweeper",
                                      "Eephus", "Slow Curve", "Knuckleball", "Slurve"))

# filter the pitches to only include breaking balls
# filter the pitches to only include those with valid vertical measurements
dat_breaking <- dat %>%
  filter(pitch_type %in% breaking$pitch_type &
         !is.na(plate_z) & !is.na(sz_top) & !is.na(sz_bot))

# filter the data to only pitchers who have thrown at least 150 breaking balls since the start of 2022
# if a breaking ball is above the middle of the strike zone, classify it as "up"
# find the percentage of breaking balls that are thrown "up" by each pitcher
# re-format the player names to "First Last"
pitcher_pct_up <- dat_breaking %>%
  group_by(pitcher, player_name) %>%
  filter(n() >= 150) %>%
  mutate(up = ifelse(plate_z > (sz_top+sz_bot)/2, 1, 0),
         player_name = sub("(\\w+),\\s(\\w+)", "\\2 \\1", player_name)) %>%
  summarize(pct_up = mean(up)) %>%
  arrange(desc(pct_up))

# remove Kody Clemens, a position player
pitcher_pct_up <- pitcher_pct_up %>% filter(player_name != "Kody Clemens")

# grab the data from the top 10 pitchers with regard to % of breaking balls thrown "up"
dat_top_10 <- dat_breaking %>%
  filter(pitcher %in% pitcher_pct_up$pitcher[1:10]) %>%
  mutate(player_name = sub("(\\w+),\\s(\\w+)", "\\2 \\1", player_name))

# get all pitchers' ERA and IP since the beginning of 2022
top_10_era <- bref_daily_pitcher("2022-04-07", "2023-05-17") %>%
  mutate(player_name = Name) %>%
  select(player_name, IP, ERA)

# join the breaking ball data with the ERA and IP data
# only grab the pitcher's name, pitch type, pitch coordinates, IP, and ERA
top_10_plot <- inner_join(dat_top_10, top_10_era, by = "player_name") %>%
  select(pitch_type, player_name, plate_x, plate_z, IP, ERA)

# create labels for the plot that show the pitcher's name, % of breaking balls thrown up in the zone,
# ERA, and IP
labels <- pitcher_pct_up %>%
  head(10) %>%
  inner_join(top_10_era, by = "player_name") %>%
  mutate(label = paste0(player_name, ": ", round(pct_up * 100, 2), "%\n", ERA, " ERA across ", IP, " IP")) %>%
  pull(label)
names(labels) = pitcher_pct_up$player_name[1:10]

# order the pitchers by decreasing % of breaking balls thrown up in the zone
top_10_plot %>%
  mutate(player_name = factor(player_name, levels = pitcher_pct_up$player_name[1:10])) %>%
  inner_join(breaking, by = "pitch_type") %>%
  ggplot() +
  # plot each pitch's coordinates and color by the type of breaking ball
  geom_point(aes(x = plate_x, y = plate_z, color = pitch_name), size = 2, alpha = 0.25) +
  # draw the strike zone based on the width of home plate and the average top and bottom strike zone measurements from the full dataset
  geom_rect(aes(xmin = -17/24, xmax = 17/24, ymin = mean(dat$sz_bot, na.rm = TRUE), ymax = mean(dat$sz_top, na.rm = TRUE)), color = "black", alpha = 0) +
  facet_wrap(~player_name, labeller = labeller(player_name = labels)) +
  theme_minimal() +
  labs(title = "Which Pitchers Throw Their Breaking Balls Up in the Zone the Most?",
       x = "Catcher Perspective (Measurements Shown in Feet)",
       subtitle = "Opening Day 2022 - 5/17/2023: Min. 150 Breaking Balls Thrown",
       color = "Pitch Type") +
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 16),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12, face = "bold")) +
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 0.5))) +
  scale_x_continuous(limits = c(-4, 4)) +
  scale_y_continuous(limits = c(-2.2, 6.2)) +
  scale_color_manual(values = c("#30ACEC", "#80C34F", "#D64787", "#D64A3B", "#A666E1"))