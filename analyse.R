library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(imputeTS)

deaths_raw <- read.csv("/Users/pkryger/gh/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

deaths <- deaths_raw %>%
    pivot_longer(names_to="date", values_to="cumDeaths", cols=starts_with("X")) %>%
    filter(cumDeaths != 0) %>%
    select(-(Lat:Long)) %>%
    mutate(date=as.Date(date, format="X%m.%d.%y")) %>%
    rename(country=Country.Region, province=Province.State)

first_deaths <- deaths %>%
    group_by(country, province) %>%
    summarize(firstDeath=min(date))

pk.revcumsum <- function(x) {
    return(x -
           shift(x, n=1, type="lag", fill=0))
}

pk.inter <- function(x) {
    if (length(x) > 2) {
        if (length(x) - length(which(x == 0)) > 2) {
            return(x %>%
                   na_if(0) %>%
                   na_interpolation(option="linear") %>%
                   replace_na(0))
        }
        else return(x)
    }
    else return(x)
}


deaths <- left_join(deaths, first_deaths) %>%
    mutate(offset=date-firstDeath)

deaths <- deaths %>%
    mutate(dailyDeaths=ave(deaths$cumDeaths,
                           deaths$country, deaths$province,
                           FUN = pk.revcumsum))

deaths <- deaths %>%
    mutate(dailyDeathsInter=ave(deaths$dailyDeaths,
                                 deaths$country, deaths$province,
                                 FUN = pk.inter))

df <- deaths %>%
    filter((country == "United Kingdom" & province == "")
           | country %in% c("Germany", "Italy", "Spain")
           | (country == "China" & province == "Hubei"))

cumDeaths <- ggplot(df, aes(x=offset, y=cumDeaths, color=country)) +
    geom_point() +
    geom_line() +
    scale_y_log10()

dailyDeaths <- ggplot(df, aes(x=offset, y=dailyDeaths, color=country)) +
    geom_density(stat="identity")

dailyDeathsInter <- ggplot(df, aes(x=offset, y=dailyDeathsInter, color=country)) +
    geom_density(stat="identity")

ggsave("cumDeaths.png", plot=cumDeaths, dpi=720, width=7, height=7)
ggsave("dailyDeaths.png", plot=dailyDeaths, dpi=720, width=7, height=7)
ggsave("dailyDeathsInter.png", plot=dailyDeathsInter, dpi=720, width=7, height=7)
