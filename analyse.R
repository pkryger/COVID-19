library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(imputeTS)

lookup_raw <- read.csv("csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv")

lookup <- lookup_raw %>%
    select(-(Lat:Long_)) %>%
    filter(Admin2 == "") %>%
    rename(country=Country_Region, province=Province_State, population=Population) %>%
    mutate(population10M=population/10000000)

deaths_raw <- read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

deaths <- deaths_raw %>%
    pivot_longer(names_to="date", values_to="cumDeaths", cols=starts_with("X")) %>%
    filter(cumDeaths >= 50) %>%
    select(-(Lat:Long)) %>%
    mutate(date=as.Date(date, format="X%m.%d.%y")) %>%
    rename(country=Country.Region, province=Province.State) %>%
    group_by(country, province) %>%
    arrange(date)

first_deaths <- deaths %>%
    summarize(firstDeath=min(date))

pk.revcumsum <- function(x) {
    x <- x - lag(x, default=x[1])
    x[x < 0] = 0
    return(x)
}

pk.ratio <- function(x) {
    return(x / lag(x, default=x[1]))
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
    mutate(dailyDeaths=pk.revcumsum(cumDeaths),
           dailyDeathsInter=pk.inter2(dailyDeaths),
           cumDeathsRatio=pk.ratio(cumDeaths),
           dailyDeathsRatio=pk.ratio(dailyDeathsInter))

deaths <- left_join(deaths, lookup) %>%
    mutate(cumDeathsNorm=cumDeaths/population10M,
           dailyDeathsNorm=dailyDeaths/population10M,
           dailyDeathsInterNorm=dailyDeathsInter/population10M)

df <- deaths %>%
    filter((country == "United Kingdom" & province == "")
           | (country == "France" & province == "")
           | country %in% c("Sweden", "Italy", "Spain", "US"))
#           | (country == "China" & province == "Hubei"))

cumDeaths <- ggplot(df, aes(x=day, y=cumDeaths, color=country)) +
    geom_point() +
    geom_line()

cumDeathsNorm <- ggplot(df, aes(x=day, y=cumDeathsNorm, color=country)) +
    geom_point() +
    geom_line()

dailyDeaths <- ggplot(df, aes(x=day, y=dailyDeaths, color=country)) +
    geom_point() +
    geom_line()

dailyDeathsNorm <- ggplot(df, aes(x=day, y=dailyDeathsNorm, color=country)) +
    geom_point() +
    geom_line()

dailyDeathsInter <- ggplot(df, aes(x=day, y=dailyDeathsInter, color=country)) +
    geom_point() +
    geom_line()

dailyDeathsInterNorm <- ggplot(df, aes(x=day, y=dailyDeathsInterNorm, color=country)) +
    geom_point() +
    geom_line()


ggsave("cumDeaths.png", plot=cumDeaths, dpi=720, width=7, height=7)
ggsave("dailyDeaths.png", plot=dailyDeaths, dpi=720, width=7, height=7)
ggsave("dailyDeathsInter.png", plot=dailyDeathsInter, dpi=720, width=7, height=7)
ggsave("cumDeathsNorm.png", plot=cumDeathsNorm, dpi=720, width=7, height=7)
ggsave("dailyDeathsNorm.png", plot=dailyDeathsNorm, dpi=720, width=7, height=7)
ggsave("dailyDeathsInterNorm.png", plot=dailyDeathsInterNorm, dpi=720, width=7, height=7)


## Log10 scale

cumDeathsLog10 <- cumDeaths + scale_y_log10()
dailyDeathsLog10 <- dailyDeaths + scale_y_log10()
dailyDeathsInterLog10 <- dailyDeathsInter + scale_y_log10()
ggsave("cumDeathsLog10.png", plot=cumDeathsLog10, dpi=720, width=7, height=7)
ggsave("dailyDeathsLog10.png", plot=dailyDeathsLog10, dpi=720, width=7, height=7)
ggsave("dailyDeathsInterLog10.png", plot=dailyDeathsInterLog10, dpi=720, width=7, height=7)

# Smoothing

dff <- df %>% filter((country == "France" & day >= 10) |
                     (country == "Italy" & day >= 12) |
                     (country == "Spain" & day >= 8) |
                     (country == "Sweden" & day >= 13) |
                     (country == "United Kingdom" & day >= 16) |
                     (country == "US" & day >= 8))

cumDeathsRatio <- ggplot(df,
                         aes(x=day, y=cumDeathsRatio, color=country)) +
    geom_point() +
    geom_line() +
    geom_smooth(data=dff,
                size=0.3,
                colour="black",
                linetype="dashed",
                method=lm,
                fullrange=TRUE) +
    ylim(1, 2) +
    facet_wrap(~country)
ggsave("cumDeathsRatio.png", plot=cumDeathsRatio, dpi=720, width=12, height=7)

dailyDeathsRatio <- ggplot(df,
                           aes(x=day, y=dailyDeathsRatio, color=country)) +
    geom_point() +
    geom_line() +
    geom_smooth(data=dff,
                size=0.3,
                colour="black",
                linetype="dashed",
                method=lm,
                fullrange=TRUE) +
    ylim(0, 2) +
    facet_wrap(~country)
ggsave("dailyDeathsRatio.png", plot=dailyDeathsRatio, dpi=720, width=12, height=7)

# Modelling

models <- left_join(dff %>% do(model = lm(cumDeathsRatio ~ day, .)),
                    dff %>% summarize(nextDay = max(day) + 1))

models <- dff %>% do(model = lm(cumDeathsRatio ~ day, .),
                     nextDays = seq(max(.$day) + 1, max(.$day) + 8))
