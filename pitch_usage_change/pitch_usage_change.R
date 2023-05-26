library(baseballr)
library(tidyverse)

pitch_labels <- data.frame(pitch_type = c("FF", "SL", "ST", "SI", "CH", "FC", "CU", "FS", "KC", "CS", "SV", "FA", "EP", "KN", "FO", "SC", "FT"),
                           pitch_label = c("4-Seam", "Slider", "Sweeper", "Sinker", "Changeup", "Cutter", "Curveball", "Splitter", "Knuckle Curve", "Slow Curve", "Slurve", "Fastball", "Eephus", "Knuckleball", "Forkball", "Screwball", "2-Seam"))

dat <- read.csv("statcast_pitches_2022_04_07_to_2023_05_22.csv")

# remove unidentified pitches and pitchouts
dat <- dat %>% filter(!(pitch_type %in% c("", "PO")))

# create separate dataframes for 2022 and 2023
# for each year, remove pitchers that did not throw at least 200 pitches
dat2022 <- dat %>%
  filter(game_year == 2022) %>%
  group_by(pitcher) %>%
  filter(n() >= 200)
  
dat2023 <- dat %>%
  filter(game_year == 2023) %>%
  group_by(pitcher) %>%
  filter(n() >= 200)

# only keep pitchers that threw at least 200 pitches in both years
dat2022 <- dat2022 %>%
  filter(pitcher %in% dat2023$pitcher)

dat2023 <- dat2023 %>%
  filter(pitcher %in% dat2022$pitcher)

# summarize data by calculating number of each type of pitch thrown by each pitcher
pitchers2022 <- data.frame(table(dat2022$pitcher, dat2022$pitch_type)) %>%
  pivot_wider(id_cols = "Var1",
              names_from = "Var2",
              values_from = "Freq") %>%
  rename(pitcher = Var1) %>%
  inner_join(dat %>% distinct(pitcher, player_name) %>% mutate(pitcher = as.factor(pitcher)), by = "pitcher")

pitchers2023 <- data.frame(table(dat2023$pitcher, dat2023$pitch_type)) %>%
  pivot_wider(id_cols = "Var1",
              names_from = "Var2",
              values_from = "Freq") %>%
  rename(pitcher = Var1) %>%
  inner_join(dat %>% distinct(pitcher, player_name) %>% mutate(pitcher = as.factor(pitcher)), by = "pitcher")

# taylor rogers had some of his pitches classified as two-seam fastballs (FT) in 2023
# these are the only instances of two-seam fastballs in the whole dataset
# change these pitches to sinkers to be consistent
pitchers2023[pitchers2023$FT > 0, ]
pitchers2023 <- pitchers2023 %>%
  mutate(SI = ifelse(pitcher == 573124, SI + FT, SI)) %>%
  select(-FT)

# clayton kershaw had one of his pitches classified as a fastball (FA) in 2023
# this is the only instance of a fastball in the whole dataset
# change this pitch to a four-seam fastball to be consistent
pitchers2023[pitchers2023$FA > 0, ]
pitchers2023 <- pitchers2023 %>%
  mutate(FF = ifelse(pitcher == 477132, FF + FA, FF)) %>%
  select(-FA)

# convert pitch counts to percentages
pitchers2022pct <- pitchers2022 %>%
  mutate(across(where(is.numeric), function(.x) {.x / rowSums(.[2:12])}))

pitchers2023pct <- pitchers2023 %>%
  mutate(across(where(is.numeric), function(.x) {.x / rowSums(.[2:12])}))

# sort dataframes for performing algebra
pitchers2022pct <- pitchers2022pct %>% arrange(pitcher)
pitchers2023pct <- pitchers2023pct %>% arrange(pitcher)
sum(pitchers2022pct$pitcher == pitchers2023pct$pitcher)

# calculate pitch usage change in terms of change in percentage points for each pitch type
usage_change <- bind_cols(pitchers2022pct[, c(1, 13)],
                          pitchers2023pct[, -c(1, 13)] - pitchers2022pct[, -c(1, 13)])

# select the 10 pitchers with the greatest change in their pitch usages
usage_change$total_change <- rowSums(abs(usage_change[, 3:13]))
top10 <- usage_change %>%
  arrange(desc(total_change)) %>%
  mutate(player_name = sub("(\\w+),\\s(\\w+)", "\\2 \\1", player_name)) %>%
  mutate(player_name = ifelse(player_name == "A Puk.J.", "A.J. Puk", player_name)) %>%
  head(10)

# get the original counts by pitch type for the top 10
top10_2022 <- pitchers2022 %>% filter(pitcher %in% top10$pitcher) %>% mutate(year = 2022)
top10_2023 <- pitchers2023 %>% filter(pitcher %in% top10$pitcher) %>% mutate(year = 2023)

# combine the data from both years
# change the player name format to "First Last"
top10_plot <- bind_rows(top10_2022, top10_2023) %>%
  mutate(player_name = sub("(\\w+),\\s(\\w+)", "\\2 \\1", player_name)) %>%
  mutate(player_name = ifelse(player_name == "A Puk.J.", "A.J. Puk", player_name)) %>%
  pivot_longer(cols = c("CH", "CS", "CU", "FC", "FF", "FS", "KC", "SI", "SL", "ST", "SV")) %>%
  rename(pitch_type = name,
         count = value) %>%
  inner_join(pitch_labels, by = "pitch_type") %>%
  filter(count != 0) %>%
  mutate(player_name = factor(player_name, levels = top10$player_name))

# create a stacked bar plot of the top 10 pitchers whose repertoires changed the most
top10_plot %>%
  ggplot(aes(fill = pitch_label, y = count, x = as.factor(year))) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(~player_name) +
  labs(title = "Which Pitchers Changed their Pitch Repertoire/Usage the Most?",
       subtitle = "Opening Day 2022 - 5/22/2023: Min. 200 Pitches Thrown Each Year",
       fill = "Pitch Type") +
  theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = rev(c("#607D8B",
                                   "#9C27B0",
                                   "#3F51B5",
                                   "#29B6F6",
                                   "#009688",
                                   "#8BC34A",
                                   "#FFEB3B",
                                   "#FF9800",
                                   "#C62828")))