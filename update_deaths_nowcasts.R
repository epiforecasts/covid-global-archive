
# Packages -----------------------------------------------------------------
require(data.table, quietly = TRUE) 
require(future, quietly = TRUE)
require(stringr, quietly = TRUE)
## Require for data and nowcasting
# require(EpiNow, quietly = TRUE)
# require(NCoVUtils, quietly = TRUE)

## Required for forecasting
# require(forecastHybrid, quietly = TRUE)

# Get deaths ---------------------------------------------------------------

NCoVUtils::reset_cache()

cases <- NCoVUtils::get_ecdc_cases()

deaths <- data.table::setDT(cases)[!is.na(country)]
deaths <- deaths[country != "Cases_on_an_international_conveyance_Japan"]
deaths <- deaths[, .(date, region = stringr::str_replace_all(country, "_", " "), local = deaths, imported = 0)]


deaths  <- data.table::melt(deaths , measure.vars = c("local", "imported"),
                          variable.name = "import_status",
                          value.name = "confirm")

## Remove regions with data issues
deaths <- deaths[!region %in% c("Faroe Islands", "Sao Tome and Principe")]

# Get delay to death -----------------------------------------------------

delay_defs <- readRDS("data/onset_to_death_delay.rds")

# Get the incubation period -----------------------------------------------

## Mean delay
exp(EpiNow::covid_incubation_period[1, ]$mean)

## Get incubation defs
incubation_defs <- EpiNow::lognorm_dist_def(mean = EpiNow::covid_incubation_period[1, ]$mean,
                                            mean_sd = EpiNow::covid_incubation_period[1, ]$mean_sd,
                                            sd = EpiNow::covid_incubation_period[1, ]$sd,
                                            sd_sd = EpiNow::covid_incubation_period[1, ]$sd_sd,
                                            max_value = 30, samples = 1000)

# Set up cores -----------------------------------------------------
if (!interactive()){
  options(future.fork.enable = TRUE)
}

cores_per_region <- 4
future::plan(list(future::tweak("multiprocess", workers = round(future::availableCores() / cores_per_region)),
                  future::tweak("multiprocess", workers = cores_per_region)), gc = TRUE, earlySignal = TRUE)

# Run regions nested ------------------------------------------------------

future::plan("multiprocess", gc = TRUE, earlySignal = TRUE)

# Run pipeline ----------------------------------------------------

EpiNow::regional_rt_pipeline(
  cases = deaths,
  delay_defs = delay_defs,
  incubation_defs = incubation_defs,
  target_folder = "deaths/national",
  case_limit = 20,
  min_forecast_cases = 100,
  horizon = 14,
  nowcast_lag = 9 + 5, # Delay from death -> onset + onset -> infection
  approx_delay = TRUE,
  report_forecast = TRUE, 
  forecast_model = function(...){EpiSoon::forecastHybrid_model(
          model_params = list(models = "aeftz", weights = "equal"),
          forecast_params = list(PI.combination = "mean"), ...)}
)

future::plan("sequential")

# Summarise results -------------------------------------------------------

EpiNow::regional_summary(results_dir = "deaths/national",
                         summary_dir = "deaths/national-summary",
                         target_date = "latest",
                         region_scale = "Country",
                         csv_region_label = "country",
                         log_cases = TRUE)


