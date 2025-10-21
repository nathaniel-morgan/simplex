{
    if (is.null(title)) 
        title <- glue("weather at ", station, " in ", yr)
    url <- glue("https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/access/", 
        station, ".csv")
    weather_all <- read_csv(url, show_col_types = FALSE)
    weather_all <- pivot_longer(select(mutate(janitor::clean_names(mutate(weather_all, 
        date = as.Date(DATE), across(c(TMAX, TMIN), ~(.x/10) * 
            1.8 + 32), PRCP = PRCP * 0.0393701/10)), rain = prcp, 
        max = tmax, min = tmin), date, max, min, rain), cols = c("max", 
        "min"), names_to = "type", values_to = "temperature")
    summary_all <- ungroup(summarise(group_by(weather_all, month(date), 
        day(date), type), mean_temperature = mean(temperature, 
        na.rm = TRUE), q5 = quantile(temperature, probs = 0.05, 
        na.rm = TRUE), q10 = quantile(temperature, probs = 0.1, 
        na.rm = TRUE), q25 = quantile(temperature, probs = 0.25, 
        na.rm = TRUE), median = median(temperature, na.rm = TRUE), 
        q75 = quantile(temperature, probs = 0.75, na.rm = TRUE), 
        q90 = quantile(temperature, probs = 0.9, na.rm = TRUE), 
        q95 = quantile(temperature, probs = 0.95, na.rm = TRUE), 
        date = median(yday(date)), rain = mean(rain), date_fake = as.Date(date - 
            1, origin = "2000-01-01")))
    weather_year <- mutate(filter(weather_all, year(date) == 
        yr), date = yday(date), date_fake = as.Date(date - 1, 
        origin = "2000-01-01"))
    print(ggplot(summary_all, aes(x = date_fake, fill = type)) + 
        geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.2) + 
        geom_line(aes(y = mean_temperature, col = type)) + scale_x_date(date_labels = "%b", 
        date_breaks = "month") + geom_point(data = weather_year, 
        size = 0.2, aes(y = temperature, col = type)) + xlab(NULL) + 
        ylab("temperature °F") + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)))
}
