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

## types of conflict "Violence against citizens" main graph

breaks <- c(1998, 2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020)

acled %>% 
    filter(event_type_dummy == 1) %>% 
ggplot(aes(x = year, y = event_type_dummy, fill = sub_event_type)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = breaks, labels = breaks) +
    scale_fill_viridis_d() +
    labs(title = "Frequency of violence against citizens in Zimbabwe ",
         subtitle = "The ACLED dataset 1997-2020",
         y = "", x = "") +
    annotate("text", x = 1998, y = 150, label = "Economic crisis") + #economic
    annotate("segment", x = 1998, y = 140, xend = 1998, yend = 20,
             arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
    annotate("text", x = 2000, y = 350, label = "Farm invasions") + # farm
    annotate("segment", x = 2000, y = 340, xend = 2000, yend = 280,
             arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
    annotate("text", x = 2000, y = 700, label = "Mugabe wins six-year term in election") + # mugabe election
    annotate("curve", x = 2000, y = 680, xend = 2001.5, yend = 650,
             arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
    annotate("text", x = 2005, y = 400, 
             label = paste(strwrap("Tsvangirai and MDC officials assaulted by police in a pro-democracy march.", 30), collapse = "\n")) +
    annotate("segment", x = 2005, y = 350, xend = 2007, yend = 160,
             arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
    annotate("text", x = 2011, y = 700, 
             label = paste(strwrap("Mugabe declared winner of run-off presidential election", 40), collapse = "\n")) +
    annotate("curve", x = 2011, y = 660, xend = 2008.5, yend = 600,
             arrow = arrow(length = unit(0.2, "cm"), type = "closed"), curvature = -0.2) +
    annotate("text", x = 2015, y = 280, 
             label = paste(strwrap("Mugabe resigns days after the military takes control", 40), collapse = "\n")) +
    annotate("segment", x = 2015, y = 250, xend = 2017, yend = 170,
             arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
    annotate("text", x = 2018, y = 430, 
             label = paste(strwrap("Mnangagwa narrowly wins presidential election over Nelson Chamisa", 40), collapse = "\n")) +
    annotate("segment", x = 2019, y = 400, xend = 2018, yend = 170,
             arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
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
          legend.position = "bottom") +
    guides(fill = guide_legend(title = "Type of violence"))

ggsave("barplot_zim_violence.png", dpi = 600, width = 12, height = 8)

## violence by monthly 2008

acled %>% 
    filter(event_type_dummy == 1) %>%
    filter(year == 2008) %>% 
    ggplot(aes(x = event_date, y = event_type_dummy, fill = sub_event_type)) +
    geom_bar(stat = "identity") +
    #geom_vline(xintercept = as.Date(""))
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(date_labels = "%B", date_breaks = "1 month") +
    scale_fill_viridis_d() +
    labs(title = "Frequency of violence against citizens in Zimbabwe 2008",
         subtitle = "The ACLED dataset 1997-2020",
         y = "", x = "") +
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
          legend.position = "bottom",
          axis.text.x = element_text(angle=45, hjust = 1))

## 2000-2010

acled %>% 
    filter(event_type_dummy == 1) %>%
    filter(year %in% c(2010, 2009, 2008, 2007, 2006, 2005, 2004, 2003, 2002, 2001, 2000)) %>% 
    ggplot(aes(x = event_date, y = event_type_dummy, fill = sub_event_type)) +
    geom_bar(stat = "identity") +
    geom_vline(xintercept = as.Date("2000-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    geom_vline(xintercept = as.Date("2001-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    geom_vline(xintercept = as.Date("2002-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    geom_vline(xintercept = as.Date("2003-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    geom_vline(xintercept = as.Date("2004-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    geom_vline(xintercept = as.Date("2005-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    geom_vline(xintercept = as.Date("2006-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    geom_vline(xintercept = as.Date("2007-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    geom_vline(xintercept = as.Date("2008-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    geom_vline(xintercept = as.Date("2009-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    geom_vline(xintercept = as.Date("2010-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    scale_fill_viridis_d() +
    labs(title = "Frequency of violence against citizens in Zimbabwe",
         subtitle = "The ACLED dataset 2000-2010",
         y = "", x = "") +
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
          legend.position = "bottom",
          axis.text.x = element_text(angle=45, hjust = 1))+
    guides(fill = guide_legend(title = "Type of violence"))

ggsave("monthly_zim_violence2000.png", dpi = 600, width = 10, height = 8)

## 2011-2020

acled %>% 
    filter(event_type_dummy == 1) %>%
    filter(year %in% c(2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011)) %>% 
    ggplot(aes(x = event_date, y = event_type_dummy, fill = sub_event_type)) +
    geom_bar(stat = "identity") +
    geom_vline(xintercept = as.Date("2011-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    geom_vline(xintercept = as.Date("2012-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    geom_vline(xintercept = as.Date("2013-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    geom_vline(xintercept = as.Date("2014-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    geom_vline(xintercept = as.Date("2015-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    geom_vline(xintercept = as.Date("2016-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    geom_vline(xintercept = as.Date("2017-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    geom_vline(xintercept = as.Date("2018-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    geom_vline(xintercept = as.Date("2019-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dashed", size = 0.5, alpha = 0.3) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(date_labels = "%B", date_breaks = "6 months") +
    scale_fill_viridis_d() +
    labs(title = "Frequency of violence against citizens in Zimbabwe",
         subtitle = "The ACLED dataset 2011-2020",
         y = "", x = "") +
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
          legend.position = "bottom",
          axis.text.x = element_text(angle=45, hjust = 1))+
    guides(fill = guide_legend(title = "Type of violence"))

ggsave("monthly_zim_violence2011.png", dpi = 600, width = 10, height = 8)



## violence against citizens actors involved 

acled %>% 
    filter(event_type_dummy == 1) %>% 
    ggplot(aes(x = year, y = event_type_dummy, fill = actor1)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_viridis_d(option = "A") +
    labs(title = "Frequency of violence against citizens in Zimbabwe",
         subtitle = "The ACLED dataset 1997-2020") +
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
          plot.title.position = "plot",
          legend.position = "right") +
    guides(fill = "none")

ggsave("barplot_zim_violence2.png", dpi = 600, width = 12, height = 8)

## Strategic developments

acled %>% 
    filter(event_type_dummy == 4) %>% 
    ggplot(aes(x = year, y = event_type_dummy)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Frequency of strategic developments in Zimbabwe by the ACLED dataset 1997-2020",
         subtitle = " ") +
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




