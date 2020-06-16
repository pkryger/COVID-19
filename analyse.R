library(tidyverse)
library(data.table)
library(ggplot2)
library(imputeTS)
library(zoo)

lookup_raw <- read.csv("csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv")

lookup <- lookup_raw %>%
    filter(Admin2 == "") %>%
    select(c(Country_Region, Province_State, Population)) %>%
    rename(
        country = Country_Region,
        province = Province_State,
        population = Population
    ) %>%
    mutate(population10M = population / 10000000)

deaths_raw <- read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

deaths <- deaths_raw %>%
    pivot_longer(
        names_to = "date",
        values_to = "cumDeaths",
        cols = starts_with("X")
    ) %>%
    select(-(Lat:Long)) %>%
    mutate(date = as.Date(date, format = "X%m.%d.%y")) %>%
    rename(country = Country.Region, province = Province.State) %>%
    group_by(country, province) %>%
    arrange(date)

first_deaths <- deaths %>%
    filter(cumDeaths >= 50) %>%
    summarize(firstDeath = min(date))

pk_revcumsum <- function(x) {
    x <- x - lag(x, default = x[1])
    x[x < 0] <- 0
    return(x)
}

pk_ratio <- function(x) {
    return(x / lag(x, default = x[1]))
}

pk_inter <- function(x) {
    if (length(x) > 2) {
        if (length(x) - length(which(x == 0)) > 2) {
            return(x %>%
                na_if(0) %>%
                na_interpolation(option = "linear") %>%
                replace_na(0))
        }
        else {
            return(x)
        }
    }
    else {
        return(x)
    }
}

## Input missing values with the next known value divided by the number of
## adjacent missing values plus 1. The known value is replaced as well.
pk_inter2 <- function(x) {
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

df <- left_join(
    deaths %>% filter(cumDeaths >= 50),
    first_deaths
) %>%
    mutate(day = as.integer(date - firstDeath))

df <- df %>%
    mutate(
        dailyDeaths = pk_revcumsum(cumDeaths),
        dailyDeaths = pk_inter2(dailyDeaths),
        cumDeathsRatio = pk_ratio(cumDeaths),
        dailyDeathsRatio = pk_ratio(dailyDeaths),
        rollmeanDeaths = rollmeanr(dailyDeaths, 7, fill = NA)
    )

df <- left_join(df, lookup) %>%
    mutate(
        cumDeathsNorm = cumDeaths / population10M,
        dailyDeathsNorm = dailyDeaths / population10M,
        dailyDeathsNorm = dailyDeaths / population10M,
        rollmeanDeathsNorm = rollmeanr(dailyDeathsNorm, 7, fill = NA)
    )

df <- df %>%
    filter((country == "United Kingdom" & province == "")
           | (country == "France" & province == "")
           | country %in% c("Sweden", "Italy", "Spain", "US"))
#   | (country == "China" & province == "Hubei"))

cumDeaths <- ggplot(df, aes(x = day, y = cumDeaths, color = country)) +
    geom_point() +
    geom_line()

cumDeathsNorm <- ggplot(df, aes(x = day, y = cumDeathsNorm, color = country)) +
    geom_point() +
    geom_line()


ggsave("cumDeaths.png",
       plot = cumDeaths, dpi = 720, width = 7, height = 7
)
ggsave("cumDeathsNorm.png",
        plot = cumDeathsNorm, dpi = 720, width = 7, height = 7
)


dailyDeathsFct <- ggplot(data = df, mapping = aes(x = day)) +
    geom_col(mapping = aes(y = dailyDeaths, fill = country)) +
    geom_line(mapping = aes(y = rollmeanDeaths)) +
    facet_wrap(~country)

dailyDeathsNormFct <- ggplot(data = df, mapping = aes(x = day)) +
    geom_col(mapping = aes(y = dailyDeathsNorm, fill = country)) +
    geom_line(mapping = aes(y = rollmeanDeathsNorm)) +
    facet_wrap(~country)

ggsave("dailyDeathsFct.png",
       plot = dailyDeathsFct, dpi = 720, width = 12, height = 7
       )

ggsave("dailyDeathsNormFct.png",
       plot = dailyDeathsNormFct, dpi = 720, width = 12, height = 7
)


## Log10 scale

cumDeathsLog10 <- cumDeaths + scale_y_log10()
ggsave("cumDeathsLog10.png",
       plot = cumDeathsLog10, dpi = 720, width = 7, height = 7
)


dff <- df %>% filter(day > max(day) - 14)

# Modelling
# see also: https://aosmith.rbind.io/2018/11/16/plot-fitted-lines/
models <- df %>%
    filter(day > max(day) - 14) %>%
    do(
        model = lm(cumDeathsRatio ~ day, .),
        day = seq(min(.$day), max(df$day) + 8)
    )

predictions <- models %>% do(data.frame(
    country = .$country,
    province = .$province,
    day = .$day,
    pred = predict(.$model, newdata = .)
))
predictions <- full_join(df, predictions)

cumDeathsRatioModel <- ggplot(
    predictions,
    aes(x = day, y = cumDeathsRatio, color = country)
) +
    geom_point() +
    geom_line() +
    geom_line(
        mapping = aes(x = day, y = pred),
        size = 0.3,
        linetype = "dashed"
    ) +
    ylim(1, 1.4) +
    geom_hline(
        yintercept = 1.055,
        colour = "black",
        size = 0.3,
        linetype = "dashed"
    ) +
    facet_wrap(~country)
ggsave("cumDeathsRatioModel.png",
       plot = cumDeathsRatioModel, dpi = 720, width = 12, height = 7
)


# Death ratio
# Province/State,Country/Region,Lat,Long
confirmed_raw <- read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
confirmed <- confirmed_raw %>%
    pivot_longer(
        names_to = "date",
        values_to = "cumConfirmed",
        cols = starts_with("X")
    ) %>%
    select(-(Lat:Long)) %>%
    mutate(date = as.Date(date, format = "X%m.%d.%y")) %>%
    rename(country = Country.Region, province = Province.State) %>%
    group_by(country, province) %>%
    arrange(date)

df <- full_join(
    deaths %>% group_by(date) %>% summarize(deaths = sum(cumDeaths)),
    confirmed %>% group_by(date) %>% summarize(confirmed = sum(cumConfirmed))
)

deathsRatio <- ggplot(df, aes(x = date, y = deaths / confirmed)) +
    geom_point() +
    geom_line()

ggsave("deathsRatio.png",
       plot = deathsRatio, dpi = 720, width = 7, height = 7
)
