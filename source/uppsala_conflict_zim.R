library(tidyverse)
library(readr)
library(readxl)
library(dplyr)


up_df <- load("data/ucdp-prio-acd-201.RData")

up_zim <- up_df %>% 
    filter(location == "Zimbabwe (Rhodesia)")

## ACLED DATABASE 

acled <- read_csv("data/conflict_data_zwe.csv")
acled <- acled[-1,]

acled$event_date <- lubridate::ymd(acled$event_date)

acled$year <- as.integer(acled$year)

acled$event_type_dummy <- NA

acled$event_type_dummy[acled$event_type == "Violence against civilians"] <- 1
acled$event_type_dummy[acled$event_type == "Protests"] <- 2
acled$event_type_dummy[acled$event_type == "Riots"] <- 3
acled$event_type_dummy[acled$event_type == "Strategic developments"] <- 4
acled$event_type_dummy[acled$event_type == "Battles"] <- 5
acled$event_type_dummy[acled$event_type == "Explosions/Remote violence"] <- 6

## barplot frequency of conflict since 1997-2020

count_plot <- acled %>%
    group_by(year) %>%
    summarise(count = n())%>%
    arrange(desc(count))

count_plot  %>% 
    ggplot(aes(x = year, y = count, fill = -count)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(name = "Total number of Conflicts in Zimbabwe", labels = scales::comma) +
    labs(title = "Frequency of conflict in Zimbabwe by the ACLED dataset 1997-2020") +
    theme(panel.background = element_blank(),
          text = element_text(family = "Helvetica", size = 13),
          panel.grid.major = element_line(colour = "#f0f0f0"),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          strip.background = element_rect(fill = "#f5f5f5"),
          plot.background = element_blank(),
          strip.text.x = element_text(hjust = 0),
          plot.caption = element_text(hjust = 0),
          plot.title = element_text(size = 18), 
          plot.title.position = "plot") 

## types of conflict


