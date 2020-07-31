library(tidyverse)
library(readr)
library(readxl)
library(dplyr)
library(wbstats)

zim <- wb(country = "ZWE", indicator = c("NY.ADJ.NNTY.KD.ZG", "NY.GDP.PCAP.KD.ZG"),
          startdate = 1996, enddate = 2018, return_wide = T)

zim$year <- as.numeric(zim$date)

rl <- read_excel("data/wgi_zwe_1.xlsx")

zim <- zim[c(-2, -4, -6),]

merge <- merge(zim, rl, all.y = T)

## line plot

merge$RL.PER

ggplot(merge, aes(x = year)) +
    geom_line(aes(y = NY.GDP.PCAP.KD.ZG, colour = "#7FBC41"), size = 1.5) +
    geom_line(aes(y = RL.PER, colour = "#4393C3"), size = 1.5) +
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
          legend.position = "top", legend.title = element_blank()) +
    scale_color_identity(guide="legend", labels = c("NY.GDP.PCAP.KD.ZG", "RL.PER")) +
    labs(x = "", y = "", title = "GDP per capita growth (annual %) and Rule of Law Percentile Rank")


### upper and lower RL 

ggplot(merge, aes(x = year)) +
    geom_line(aes(y = NY.GDP.PCAP.KD.ZG, colour = "#3a4971"), size = 1.5) +
    geom_line(aes(y = RL.PER.RNK.LOWER, colour = "#c35959"), size = 1.5, linetype = "dashed") +
    geom_line(aes(y = RL.PER.RNK.UPPER, colour = "#781e52"), size = 1.5, linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.5, alpha = 0.7) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    theme(panel.background = element_blank(),
          text = element_text(family = "Helvetica", size = 13),
          panel.grid.major = element_line(colour = "#f0f0f0"),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          strip.background = element_rect(fill = "#f5f5f5"),
          plot.background = element_blank(),
          strip.text.x = element_text(hjust = 0),
          plot.caption = element_text(hjust = 0),
          plot.title = element_text(size = 14),
          legend.position = "bottom", legend.title = element_blank()) +
    scale_color_identity(guide="legend", labels = c("GDP per capita growth","Rule of Law (Upper estimate)","Rule of Law (Lower estimate)")) +
    labs(x = "", y = "", title = "Zimbabwe's GDP per capita growth (annual %) and Rule of Law (Percentile Rank)",
         caption = "Source: World Bank, July 2020")

ggsave("zim_gdp_rl.png", dpi = 600, width = 8, height = 8) 

### corruption

cc <- readr::read_csv("data/wgi_all_10.csv")

colnames(cc) <- c("country", "iso3c", "series_name", "series_code", 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009)

cc_tidy <- cc %>% 
    gather(year, value, `2018`:`2009`) # create a column for year 

cc_tidy <- cc_tidy %>% 
    pivot_longer()   



