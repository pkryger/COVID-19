library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(imputeTS)

deaths_raw <- read.csv("/Users/pkryger/gh/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

deaths <- deaths_raw %>%
    pivot_longer(names_to="date", values_to="cumDeaths", cols=starts_with("X")) %>%
    filter(cumDeaths >= 50) %>%
    select(-(Lat:Long)) %>%
    mutate(date=as.Date(date, format="X%m.%d.%y")) %>%
    rename(country=Country.Region, province=Province.State)

first_deaths <- deaths %>%
    group_by(country, province) %>%
    summarize(firstDeath=min(date))

pk.revcumsum <- function(x) {
    return(x -
           shift(x, n=1, type="lag", fill=x[1]))
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

## Input missing values with the next known value divided by the number of
## adjacent missing values plus 1. The known value is replaced as well.
pk.inter2 <- function(x) {
    if (length(x) < 3 |
        length(x) - length(which(x == 0)) < 3) {
        return(x)
    }
    y <- x
    idx <- which(x == 0)
    # while there are elements that are 0
    while (0 < length(idx)) {
        # from first element that is 0
        first <- idx[1]
        idx <- idx[-1]

        # find last adjacent element that is 0
        last <- first
        while (0 < length(idx) & idx[1] == first + 1) {
            last <- idx[1]
            idx <- idx[-1]
        }
        # and go one past that
        last <- last + 1

        # only in case this is not a series of 0's in the end
        if (last <= length(x)) {
            # replace 0's with an average value
            value <- (x[last] - x[first]) / (last - first + 1)
            for (i in first:last) {
                y[i] <- value
            }
        }
    }
    return(y)
}

deaths <- left_join(deaths, first_deaths) %>%
    mutate(day=date-firstDeath)

deaths <- deaths %>%
    mutate(dailyDeaths=ave(deaths$cumDeaths,
                           deaths$country, deaths$province,
                           FUN = pk.revcumsum))

deaths <- deaths %>%
    mutate(dailyDeathsInter=ave(deaths$dailyDeaths,
                                 deaths$country, deaths$province,
                                 FUN = pk.inter2))

df <- deaths %>%
    filter((country == "United Kingdom" & province == "")
           | (country == "France" & province == "")
           | country %in% c("Sweden", "Germany", "Italy", "Spain", "US"))
#           | (country == "China" & province == "Hubei"))

cumDeaths <- ggplot(df, aes(x=day, y=cumDeaths, color=country)) +
    geom_point() +
    geom_line()

dailyDeaths <- ggplot(df, aes(x=day, y=dailyDeaths, color=country)) +
    geom_line()

dailyDeathsInter <- ggplot(df, aes(x=day, y=dailyDeathsInter, color=country)) +
    geom_line()

cumDeathsLog10 <- cumDeaths + scale_y_log10()
dailyDeathsLog10 <- dailyDeaths + scale_y_log10()
dailyDeathsInterLog10 <- dailyDeathsInter + scale_y_log10()

ggsave("cumDeaths.png", plot=cumDeaths, dpi=720, width=7, height=7)
ggsave("dailyDeaths.png", plot=dailyDeaths, dpi=720, width=7, height=7)
ggsave("dailyDeathsInter.png", plot=dailyDeathsInter, dpi=720, width=7, height=7)
ggsave("cumDeathsLog10.png", plot=cumDeathsLog10, dpi=720, width=7, height=7)
ggsave("dailyDeathsLog10.png", plot=dailyDeathsLog10, dpi=720, width=7, height=7)
ggsave("dailyDeathsInterLog10.png", plot=dailyDeathsInterLog10, dpi=720, width=7, height=7)
