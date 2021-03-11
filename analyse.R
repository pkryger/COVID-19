library(tidyverse)
library(data.table)
library(ggplot2)
library(zoo)

pk_revcumsum <- function(x) {
    x <- x - lag(x, default = x[1])
    x[x < 0] <- 0
    return(x)
}

pk_ratio <- function(x) {
    return(x / lag(x, default = x[1]))
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

pk_enrich_and_filter_df <- function(df, cutoff) {
    firsts <- df %>%
        filter(cumulative >= cutoff) %>%
        summarize(first = min(date))

    df <- left_join(
        df %>% filter(cumulative >= cutoff),
        firsts
    ) %>%
        mutate(day = as.integer(date - first))

    df <- df %>%
        mutate(
            daily = pk_revcumsum(cumulative),
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
        filter((province == ""
                & country %in% c("United Kingdom"))
               | country %in% c("Sweden", "Italy", "US",
                                "Brazil", "Russia", "Mexico",
                                "India", "Czechia", "South Africa",
                                "Poland", "Belgium", "Germany", "Spain",
                                "Chile", "Iran"))

    return(df)
}

pk_generate_charts <- function(df, name, log10min) {
    cumulativePlot <- ggplot(df, aes(x = day, y = cumulative, color = country)) +
        geom_line() +
        geom_text(data = filter(df, day == max(day)),
                  aes(label = country),
                  hjust = 0, nudge_x = 0.1)
    ggsave(paste("cum", name, ".png", sep=""),
           plot = cumulativePlot, dpi = 720, width = 7, height = 7
           )

    cumulativeNormPlot <- ggplot(df, aes(x = day, y = cumulativeNorm, color = country)) +
        geom_line() +
        geom_text(data = filter(df, day == max(day)),
                  aes(label = country),
                  hjust = 0, nudge_x = 0.1)
    ggsave(paste("cum", name, "Norm.png", sep=""),
           plot = cumulativeNormPlot, dpi = 720, width = 7, height = 7
           )

    dailyFctPlot <- ggplot(data = df, mapping = aes(x = day)) +
        geom_col(mapping = aes(y = daily, fill = country)) +
        geom_line(mapping = aes(y = rollmean)) +
        facet_wrap(~country)
    ggsave(paste("daily", name, "Fct.png", sep=""),
           plot = dailyFctPlot, dpi = 720, width = 12, height = 7
           )

    dailyNormFctPlot <- ggplot(data = df, mapping = aes(x = day)) +
        geom_col(mapping = aes(y = dailyNorm, fill = country)) +
        geom_line(mapping = aes(y = rollmeanNorm)) +
        ylim(0, if (name == "Deaths") 20 else 1500) +
        facet_wrap(~country)
    ggsave(paste("daily", name, "NormFct.png", sep=""),
           plot = dailyNormFctPlot, dpi = 720, width = 12, height = 7
           )

    cumulativeLog10Plot <- cumulativePlot +
        scale_y_log10(limits = c(log10min, NA))
    ggsave(paste("cum", name, "Log10.png", sep=""),
           plot = cumulativeLog10Plot, dpi = 720, width = 7, height = 7
           )
    return(df)
}

pk_model_cumulativeRatio <- function(df, name) {
    # see also: https://aosmith.rbind.io/2018/11/16/plot-fitted-lines/
    models <- df %>%
        filter(day > max(day) - 21) %>%
        do(
            model = lm(cumulativeRatio ~ day, .),
            day = seq(min(.$day), max(df$day) + 14)
        )

    predictions <- models %>% do(data.frame(
                                  country = .$country,
                                  province = .$province,
                                  day = .$day,
                                  pred = predict(.$model, newdata = .)
                              ))
    predictions <- full_join(df %>% filter(day > max(day) - 180),
                             predictions)

    cumulativeRatioModelPlot <- ggplot(
        predictions,
        aes(x = day, y = cumulativeRatio, color = country)
    ) +
        geom_line() +
        geom_line(
            mapping = aes(x = day, y = pred),
            size = 0.3,
            linetype = "dashed"
        ) +
        ylim(1, 1.10) +
        geom_hline(
            yintercept = 1.055,
            colour = "black",
            size = 0.3,
            linetype = "dashed"
        ) +
        facet_wrap(~country)
    ggsave(paste("cum", name, "RatioModel.png", sep=""),
           plot = cumulativeRatioModelPlot, dpi = 720, width = 12, height = 7
           )
    return(df)
}

deaths <- pk_get_clean_df(paste("csse_covid_19_data",
                                "csse_covid_19_time_series",
                                "time_series_covid19_deaths_global.csv",
                                sep="/"))
deaths %>%
    pk_enrich_and_filter_df(cutoff=50) %>%
    pk_generate_charts(name="Deaths", log10min=5000) %>%
    pk_model_cumulativeRatio(name="Deaths")

# Province/State,Country/Region,Lat,Long
confirmed <- pk_get_clean_df(paste("csse_covid_19_data",
                                   "csse_covid_19_time_series",
                                   "time_series_covid19_confirmed_global.csv",
                                   sep="/"))
confirmed %>%
    pk_enrich_and_filter_df(cutoff=100) %>%
    pk_generate_charts(name="Confirmed", log10min=50000) %>%
    pk_model_cumulativeRatio(name="Confirmed")


# Death ratio
df <- full_join(
    deaths %>% group_by(date) %>% summarize(deaths = sum(cumulative)),
    confirmed %>% group_by(date) %>% summarize(confirmed = sum(cumulative))
)

deathsRatioPlot <- ggplot(df, aes(x = date, y = deaths / confirmed)) +
    geom_line()

ggsave("deathsRatio.png",
       plot = deathsRatioPlot, dpi = 720, width = 7, height = 7
)
