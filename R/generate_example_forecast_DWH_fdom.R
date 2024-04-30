source('forecast_code/load_packages.R')
# load the forecast generation function - include at least a forecast_date argument
source('R/generate_example_forecast.R')
source('R/get_weather.R') # wrapper around the RopenMeteo package to get weather covariates



#DWH testing inputs
forecast_date <- ymd("2024-04-24")
model_id <- "example_fDOM_AR_dwh"
targets_url <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"
var <- "fDOM_QSU_mean"
site <- "fcre"
forecast_depths <- 1.6
project_id <- "vera4cast"

site_list <- read_csv("https://raw.githubusercontent.com/LTREB-reservoirs/vera4cast/main/vera4cast_field_site_metadata.csv",
                      show_col_types = FALSE)

water_temp_4cast_url <- "s3://anonymous@bio230121-bucket01/vera4cast/forecasts/parquet/project_id=vera4cast/duration=P1D/variable=Temp_C_mean?endpoint_override=renc.osn.xsede.org"

# generate_example_forecast
generate_example_forecast <- function(forecast_date, # a recommended argument so you can pass the date to the function
                                      model_id,
                                      targets_url, # where are the targets you are forecasting?
                                      water_temp_4cast_url, #get url for water temp used as covariate
                                      weather_forecast,
                                      var, # what variable(s)?
                                      site, # what site(s)
                                      forecast_depths = 'focal',
                                      project_id = 'vera4cast') {

  # Put your forecast generating code in here, and add/remove arguments as needed.
  # Forecast date should not be hard coded
  # This is an example function that also grabs weather forecast information to be used as co-variates

  if (site == 'fcre' & forecast_depths == 'focal') {
    forecast_depths <- 1.6
  }

  if (site == 'bvre' & forecast_depths == 'focal') {
    forecast_depths <- 1.5
  }
  #-------------------------------------

  # Get targets
  message('Getting targets')
  targets <- readr::read_csv(targets_url, show_col_types = F) |>
    filter(variable %in% var,
           site_id %in% site,
           depth_m %in% forecast_depths,
           datetime < forecast_date)
  #-------------------------------------

  # Get the weather data


  if (weather_forecast == "RopenMeteo") {

    message('Getting weather')
    # uses the RopenMeteo function to grab weather from the sites
    # and you can specify the length of the future period and number of days in the past
    # you can modify the data that are collected in the get_daily_weather function
    # or if you want to generate an hourly forecast, you can use get_hourly_weather
    weather_dat_Z <- site |>
      map_dfr(get_daily_weather, site_list = site_list, past = 60, future = 30, vars = c("precipitation", "shortwave_radiation"))

  }

  if (weather_forecast == "vera_noaa_S3") {

    message('update noaa input weather function')
    # noaa_present_daily <- arrow::open_dataset("s3://anonymous@bio230121-bucket01/flare/drivers/met/gefs-v12/stage2?endpoint_override=renc.osn.xsede.org") |>
    #   dplyr::filter(
    #     ymd(reference_datetime) >= ymd("2024-04-28"),
    #     site_id == site,
    #     variable %in% c("surface_downwelling_shortwave_flux_in_air", "precipitation_flux")) |>
    #   mutate(datetime_date = as.Date(datetime)) |>
    #   group_by(reference_datetime, datetime_date, variable, parameter) |>
    #   summarise(prediction = mean(prediction, na.rm = T), .groups = "drop")
    #
    # df_noaa_present_daily <- noaa_present_daily |> dplyr::collect()

  }

  #-------------------------------------

  # split it into historic and future
   historic_weather <- weather_dat |>
    filter(datetime < forecast_date) |>
    # calculate a daily mean (remove ensemble)
    group_by(datetime, variable, site_id) |>
    summarise(prediction = mean(prediction)) |>
    pivot_wider(names_from = variable, values_from = prediction)

  forecast_weather <- weather_dat |>
    filter(datetime >= forecast_date) |>
    pivot_wider(names_from = variable, values_from = prediction)
  #-------------------------------------

  #Get water temp forecasts
  message('Getting water temp forecast')

  water_temp_past <- ymd(forecast_date - 60)

  water_temp_4casts <- arrow::open_dataset(water_temp_4cast_url) |>
    dplyr::filter(site_id == site,
                  depth_m == forecast_depths,
                  reference_date >= water_temp_past,
                  model_id %in% c("glm_aed_v1", "gfs_seamless")
                  ) |>
    mutate(parameter = as.numeric(parameter)) |>
    dplyr::collect()

  water_temp_4casts <- water_temp_4casts |>
    mutate(parameter = ifelse(model_id == "gfs_seamless", parameter + 1, parameter)) |>
    filter(parameter <= 31)

  # split it into historic and future
  historic_watertemp <- water_temp_4casts |>
    filter(datetime == ymd(reference_datetime)) |> #can add + 86400 to get just forecast of the next day
    # filter(datetime < forecast_date) |>
    mutate(variable = "temperature") |>
    # calculate a daily mean (remove ensemble)
    group_by(datetime, variable, site_id) |>
    summarise(prediction = mean(prediction, na.rm = T)) |>
    pivot_wider(names_from = variable, values_from = prediction)


  forecast_watertemp <- water_temp_4casts |>
    mutate(variable = "temperature") |>
    filter(reference_datetime >= forecast_date) |>
    pivot_wider(names_from = variable, values_from = prediction)


  #-------------------------------------



  # Fit model
  message('Fitting model')
  fit_df <- targets |>
    filter(datetime > ymd("2024-01-01")) |>
    pivot_wider(names_from = variable, values_from = observation) |>
    left_join(historic_weather) |>
    left_join(historic_watertemp) |>
    mutate(fDOM_lag1 = lag(fDOM_QSU_mean, 1),
           precip_lag1 = lag(precipitation_flux, 1))

  model_fit <- lm(fit_df$fDOM_QSU_mean ~ fit_df$fDOM_lag1 + fit_df$surface_downwelling_shortwave_flux_in_air +
                    fit_df$precipitation_flux + fit_df$precip_lag1 + fit_df$temperature)

  # fdom_model_summary <- summary(fdom_model)
  #
  # coeffs <- round(fdom_model_summary$coefficients[,1], 2)
  # params_se <- fdom_model_summary$coefficients[,2]
  #
  # #### get param uncertainty
  # #get param distribtuions for parameter uncertainity
  # param_df <- data.frame(beta1 = rnorm(30, coeffs[1], params_se[1]),
  #                        beta2 = rnorm(30, coeffs[2], params_se[2]),
  #                        beta3 = rnorm(30, coeffs[3], params_se[3]),
  #                        beta4 = rnorm(30, coeffs[4], params_se[4]),
  #                        beta5 = rnorm(30, coeffs[5], params_se[5]),
  #                        beta6 = rnorm(30, coeffs[6], params_se[6])
  #)

  #-------------------------------------

  # Generate forecasts
  message('Generating forecast')
  forecast <- (forecast_weather$surface_downwelling_shortwave_flux_in_air * model_fit$coefficients[2]) + model_fit$coefficients[1]

  forecast_df <- data.frame(datetime = forecast_weather$datetime,
                            reference_datetime = forecast_date,
                            model_id = model_id,
                            site_id = forecast_weather$site_id,
                            parameter = forecast_weather$parameter,
                            family = 'ensemble',
                            prediction = forecast,
                            variable = var,
                            depth_m = forecast_depths,
                            duration = targets$duration[1],
                            project_id = project_id)
  #-------------------------------------

  return(forecast_df)

}
