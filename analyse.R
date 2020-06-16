library(tidyverse)
library(data.table)
library(ggplot2)
library(imputeTS)
library(zoo)

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

lookup_raw <- read.csv("csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv")

lookup <- lookup_raw %>%
    filter(Admin2 == "") %>%
    select(c(Country_Region, Province_State, Population)) %>%
    rename(
        country = Country_Region,
        province = Province_State,
        population = Population
    ) %>%
    mutate(population1M = population / 1000000)

pk_get_clean_df <- function(path) {
    raw <- read.csv(path)

    df <- raw %>%
        pivot_longer(
            names_to = "date",
            values_to = "cumulative",
            cols = starts_with("X")
        ) %>%
        select(-(Lat:Long)) %>%
        mutate(date = as.Date(date, format = "X%m.%d.%y")) %>%
        rename(country = Country.Region, province = Province.State) %>%
        group_by(country, province) %>%
        arrange(date)
    return(df)
}

pk_enrich_and_filter_df <- function(df) {
    firsts <- df %>%
        filter(cumulative >= 50) %>%
        summarize(first = min(date))

    df <- left_join(
        df %>% filter(cumulative >= 50),
        firsts
    ) %>%
        mutate(day = as.integer(date - first))

    df <- df %>%
        mutate(
            daily = pk_revcumsum(cumulative),
            daily = pk_inter2(daily),
            cumulativeRatio = pk_ratio(cumulative),
            rollmean = rollmeanr(daily, 7, fill = NA)
        )

    df <- left_join(df, lookup) %>%
        mutate(
            cumulativeNorm = cumulative / population1M,
            dailyNorm = daily / population1M,
            rollmeanNorm = rollmeanr(dailyNorm, 7, fill = NA)
        )

    df <- df %>%
        filter((country == "United Kingdom" & province == "")
               | (country == "France" & province == "")
               | country %in% c("Sweden", "Italy", "Spain", "US"))
# Todo:-Spain, France, +Brazil, Russia, Mexico, India
    return(df)
}

deaths <- pk_get_clean_df("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
df <- pk_enrich_and_filter_df(deaths)

cumDeaths <- ggplot(df, aes(x = day, y = cumulative, color = country)) +
    geom_point() +
    geom_line()

cumDeathsNorm <- ggplot(df, aes(x = day, y = cumulativeNorm, color = country)) +
    geom_point() +
    geom_line()


ggsave("cumDeaths.png",
       plot = cumDeaths, dpi = 720, width = 7, height = 7
)
ggsave("cumDeathsNorm.png",
        plot = cumDeathsNorm, dpi = 720, width = 7, height = 7
)


dailyDeathsFct <- ggplot(data = df, mapping = aes(x = day)) +
    geom_col(mapping = aes(y = daily, fill = country)) +
    geom_line(mapping = aes(y = rollmean)) +
    facet_wrap(~country)

dailyDeathsNormFct <- ggplot(data = df, mapping = aes(x = day)) +
    geom_col(mapping = aes(y = dailyNorm, fill = country)) +
    geom_line(mapping = aes(y = rollmeanNorm)) +
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
        model = lm(cumulativeRatio ~ day, .),
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
    aes(x = day, y = cumulativeRatio, color = country)
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
confirmed <- pk_get_clean_df("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

df <- full_join(
    deaths %>% group_by(date) %>% summarize(deaths = sum(cumulative)),
    confirmed %>% group_by(date) %>% summarize(confirmed = sum(cumulative))
)

deathsRatio <- ggplot(df, aes(x = date, y = deaths / confirmed)) +
    geom_point() +
    geom_line()

ggsave("deathsRatio.png",
       plot = deathsRatio, dpi = 720, width = 7, height = 7
)
