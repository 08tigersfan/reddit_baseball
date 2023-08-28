library(paletteer)
library(scales)
library(tidyverse)

# get the path/file_name for the yearly attendance datasets
path <- ""
files <- paste0(path, list.files(path))

# read in all the datasets and bind them together
dat <- lapply(files, read.csv) %>% bind_rows()

# add the year as a column in the dataset
years <- as.numeric(str_sub(files, -8, -5))
dat <- dat %>%
  mutate(Year = rep(years, each = 30)) %>%
  select(-X)

# clean up the franchise name changes
dat30 <- dat %>%
  mutate(Tm = ifelse(str_detect(Tm, "Angels"), "Los Angeles Angels", Tm),
         Tm = ifelse(str_detect(Tm, "Cleveland"), "Cleveland Guardians", Tm),
         Tm = ifelse(str_detect(Tm, "Marlins"), "Miami Marlins", Tm),
         Tm = ifelse(str_detect(Tm, "Expos"), "Washington Nationals", Tm),
         Tm = ifelse(str_detect(Tm, "Rays"), "Tampa Bay Rays", Tm))

# read in the stadium change dataset for annotating the plot
changes <- read.csv("stadium_changes.csv") %>%
  filter(!is.na(year_change))

# export as 2880px * 1620px
dat30 %>%
  ggplot(aes(x = Year, y = Tm, fill = Attend.G, label = Attend.G)) +
  geom_tile(color = "black") +
  geom_text(size = 7) +
  labs(title = "MLB Average Game Attendance (1998 through 8/23/2023)",
       subtitle = "Stadium/City Changes Shown in Bold") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 24),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 24),
        plot.title = element_text(size = 34, face = "bold"),
        plot.subtitle = element_text(size = 30),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 22),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm")) +
  labs(fill = "Average\nAttendance   ") +
  scale_x_continuous(expand = c(0, 0), breaks = unique(dat30$Year)) +
  scale_y_discrete(expand = c(0, 0), limits = rev) +
  scale_fill_paletteer_c("ggthemes::Red-Blue Diverging", direction = -1, na.value = "grey70", labels = label_number_si()) +
  annotate("rect",
           xmin = changes$xmin,
           xmax = changes$xmax,
           ymin = changes$ymin,
           ymax = changes$ymax,
           colour = "black",
           fill = "transparent", 
           linewidth = 2.5)