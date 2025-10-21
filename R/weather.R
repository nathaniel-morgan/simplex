#' Basic weather plotter.
#'
#' @param station The station you would like to plot weather data for
#' @param year the year you would like to plot
#' @return a ggplot object
#'
#' @examples
#'
#' @export
#'
weather <- function(station = "USW00013959", yr = 2025, title = NULL){
  if( is.null(title) ) title <- glue("weather at ", station, " in ", yr)
  url <- glue("https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/access/",station,".csv")

  weather_all <- read_csv(url, show_col_types = FALSE)

  weather_all <- weather_all |>
    mutate(
      date = as.Date(DATE),
      across(c(TMAX, TMIN), ~ (.x / 10) * 1.8 + 32),  # convert tenths of °C to °C
      PRCP = PRCP * 0.0393701 / 10                         # convert tenths of mm to mm
    ) |> janitor::clean_names() |> mutate(rain = prcp, max = tmax, min = tmin) |>
    select(date, max, min, rain) |>
    pivot_longer(cols = c("max", "min"), names_to = "type", values_to = "temperature")

  summary_all <- weather_all |> group_by(month(date),day(date),type) |>
    summarise(mean_temperature = mean(temperature, na.rm = TRUE),
              q5 = quantile(temperature, probs = .05, na.rm = TRUE),
              q10 = quantile(temperature, probs = .1, na.rm = TRUE),
              q25 = quantile(temperature, probs = .25, na.rm = TRUE),
              median = median(temperature, na.rm = TRUE),
              q75 = quantile(temperature, probs = .75, na.rm = TRUE),
              q90 = quantile(temperature, probs = .9, na.rm = TRUE),
              q95 = quantile(temperature, probs = .95, na.rm = TRUE),
              date = median(yday(date)),
              rain = mean(rain),
              date_fake = as.Date(date - 1, origin = "2000-01-01")) |>
    ungroup()

  weather_year <- weather_all |> filter(year(date) == yr) |>
    mutate(date = yday(date), date_fake = as.Date(date - 1, origin = "2000-01-01"))

  summary_all |>
    ggplot(aes(x = date_fake, fill = type))+
    geom_ribbon(aes(ymin = q10, ymax = q90), alpha = .2)+
    geom_line(aes(y = mean_temperature, col = type))+
    scale_x_date(date_labels = "%b", date_breaks = "month")+
    geom_point(data = weather_year, size = .2, aes(y = temperature, col = type))+
    xlab(NULL)+
    ylab("temperature °F")+
    ggtitle(title)+
    theme(plot.title = element_text(hjust = 0.5))
}

#' @export
#'
weather_template <- function(
    station = "USW00013959",
    yr = 2025,
    title = NULL,
    template = NULL
) {
  if(is.null(template)){
    if( is.null(title) ) title <- glue("weather at ", station, " in ", yr)
    url <- glue("https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/access/",station,".csv")

    weather_all <- read_csv(url, show_col_types = FALSE)

    weather_all <- weather_all |>
      mutate(
        date = as.Date(DATE),
        across(c(TMAX, TMIN), ~ (.x / 10) * 1.8 + 32),  # convert tenths of °C to °C
        PRCP = PRCP * 0.0393701 / 10                         # convert tenths of mm to mm
      ) |> janitor::clean_names() |> mutate(rain = prcp, max = tmax, min = tmin) |>
      select(date, max, min, rain) |>
      pivot_longer(cols = c("max", "min"), names_to = "type", values_to = "temperature")

    summary_all <- weather_all |> group_by(month(date),day(date),type) |>
      summarise(mean_temperature = mean(temperature, na.rm = TRUE),
                q5 = quantile(temperature, probs = .05, na.rm = TRUE),
                q10 = quantile(temperature, probs = .1, na.rm = TRUE),
                q25 = quantile(temperature, probs = .25, na.rm = TRUE),
                median = median(temperature, na.rm = TRUE),
                q75 = quantile(temperature, probs = .75, na.rm = TRUE),
                q90 = quantile(temperature, probs = .9, na.rm = TRUE),
                q95 = quantile(temperature, probs = .95, na.rm = TRUE),
                date = median(yday(date)),
                rain = mean(rain),
                date_fake = as.Date(date - 1, origin = "2000-01-01")) |>
      ungroup()

    weather_year <- weather_all |> filter(year(date) == yr) |>
      mutate(date = yday(date), date_fake = as.Date(date - 1, origin = "2000-01-01"))

    summary_all |>
      ggplot(aes(x = date_fake, fill = type))+
      geom_ribbon(aes(ymin = q10, ymax = q90), alpha = .2)+
      geom_line(aes(y = mean_temperature, col = type))+
      scale_x_date(date_labels = "%b", date_breaks = "month")+
      geom_point(data = weather_year, size = .2, aes(y = temperature, col = type))+
      xlab(NULL)+
      ylab("temperature °F")+
      ggtitle(title)+
      theme(plot.title = element_text(hjust = 0.5))
  }
  else {
    source(template, local = TRUE)
  }
}

