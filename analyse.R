library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)

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

deaths <- left_join(deaths, first_deaths) %>%
    mutate(offset=date-firstDeath) %>%
    mutate(dailyDeaths=ave(deaths$cumDeaths,
                           deaths$country, deaths$province,
                           FUN = pk.revcumsum))

df <- deaths %>%
    filter((country == "United Kingdom" & province == "")
           | country == c("Germany", "Italy", "Spain")
           | (country == "China" & province == "Hubei"))

cumDeaths <- ggplot(df, aes(x=offset, y=cumDeaths, color=country)) +
    geom_point() +
    geom_line() +
    scale_y_log10()

dailyDeaths <- ggplot(df, aes(x=offset, y=dailyDeaths, color=country)) +
    geom_density(stat="identity")

ggsave("cumDeaths.png", plot=cumDeaths, dpi=720, width=7, height=7)
ggsave("dailyDeaths.png", plot=dailyDeaths, dpi=720, width=7, height=7)
