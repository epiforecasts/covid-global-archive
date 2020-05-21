
# Packages -----------------------------------------------------------------
require(data.table, quietly = TRUE) 
require(future, quietly = TRUE)
require(forecastHybrid, quietly = TRUE)
## Require for data and nowcasting
# require(EpiNow, quietly = TRUE)
# require(NCoVUtils, quietly = TRUE)

## Required for forecasting
# require(forecastHybrid, quietly = TRUE)

# Get cases ---------------------------------------------------------------

NCoVUtils::reset_cache()

cases <- NCoVUtils::get_ecdc_cases()

cases <-  NCoVUtils::format_ecdc_data(cases) 
cases <- data.table::setDT(cases)[!is.na(region)][, 
            `:=`(local = cases, imported = 0)][, cases := NULL]

cases <- data.table::melt(cases, measure.vars = c("local", "imported"),
                          variable.name = "import_status",
                          value.name = "confirm")

## Remove regions with data issues
cases <- cases[!region %in% c("Faroe Islands", "Sao Tome and Principe", "Tajikistan")]

# Get linelist ------------------------------------------------------------

linelist <- 
  data.table::fread("https://raw.githubusercontent.com/epiforecasts/NCoVUtils/master/data-raw/linelist.csv")


delays <- linelist[!is.na(date_onset_symptoms)][, 
                   .(report_delay = as.numeric(lubridate::dmy(date_confirmation) - 
                                                 as.Date(lubridate::dmy(date_onset_symptoms))))]

delays <- delays$report_delay

# Set up cores -----------------------------------------------------
if (!interactive()){
  options(future.fork.enable = TRUE)
}

future::plan("multiprocess", gc = TRUE, earlySignal = TRUE, workers = 60)

# Fit the reporting delay -------------------------------------------------

delay_defs <- EpiNow::get_dist_def(delays,
                                    bootstraps = 100, 
                                    samples = 1000)

# Fit the incubation period -----------------------------------------------

## Mean delay
exp(EpiNow::covid_incubation_period[1, ]$mean)

## Get incubation defs
incubation_defs <- EpiNow::lognorm_dist_def(mean = EpiNow::covid_incubation_period[1, ]$mean,
                                            mean_sd = EpiNow::covid_incubation_period[1, ]$mean_sd,
                                            sd = EpiNow::covid_incubation_period[1, ]$sd,
                                            sd_sd = EpiNow::covid_incubation_period[1, ]$sd_sd,
                                            max_value = 30, samples = 1000)


# Run regions nested ------------------------------------------------------

cores_per_region <- 1
future::plan(list(tweak("multiprocess", 
                        workers = floor(future::availableCores() / cores_per_region)),
                  tweak("multiprocess", workers = cores_per_region)),
                  gc = TRUE, earlySignal = TRUE)

# Run pipeline ----------------------------------------------------

EpiNow::regional_rt_pipeline(
  cases = cases,
  delay_defs = delay_defs,
  incubation_defs = incubation_defs,
  target_folder = "national",
  case_limit = 60,
  horizon = 14,
  nowcast_lag = 9,
  approx_delay = TRUE,
  report_forecast = TRUE, 
  forecast_model = function(y, ...){EpiSoon::forecastHybrid_model(
    y = y[max(1, length(y) - 21):length(y)],
    model_params = list(models = "aefz", weights = "equal"),
    forecast_params = list(PI.combination = "mean"), ...)}
)


future::plan("sequential")

# Summarise results -------------------------------------------------------

EpiNow::regional_summary(results_dir = "national",
                         summary_dir = "national-summary",
                         target_date = "latest",
                         region_scale = "Country",
                         csv_region_label = "country",
                         log_cases = TRUE)


