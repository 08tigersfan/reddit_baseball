library(baseballr)
library(tidyverse)

dates <- as_date(c(seq(as.numeric(ymd("2021-04-01")),
                       as.numeric(ymd("2021-10-03"))),
                   seq(as.numeric(ymd("2022-04-07")),
                       as.numeric(ymd("2022-10-05"))),
                   seq(as.numeric(ymd("2023-03-30")),
                       as.numeric(ymd("2023-05-25")))))

get_pitch_data <- function(date) {

  return(statcast_search(start_date = date,
                         end_date = date,
                         player_type = "pitcher"))

}

dat <- map(dates, get_pitch_data)

dat_unlist <- bind_rows(dat)

dat <- dat_unlist

rm(dat_unlist, dates, get_pitch_data)
