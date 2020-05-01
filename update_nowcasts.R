
# Packages -----------------------------------------------------------------
require(EpiNow, quietly = TRUE)
require(NCoVUtils, quietly = TRUE)
require(furrr, quietly = TRUE)
require(future, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(tidyr, quietly = TRUE)
require(magrittr, quietly = TRUE)
require(future.apply, quietly = TRUE)
require(fable, quietly = TRUE)
require(fabletools, quietly = TRUE)
require(feasts, quietly = TRUE)
require(urca, quietly = TRUE)



# Get cases ---------------------------------------------------------------

NCoVUtils::reset_cache()

cases <- NCoVUtils::get_ecdc_cases() %>%
  NCoVUtils::format_ecdc_data()


## Arrange to run countries with most cases first
total_cases_order <- cases %>% 
  dplyr::count(region, wt = cases) %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::mutate(n = 1:dplyr::n())

cases <- cases %>% 
  dplyr::left_join(total_cases_order, by = "region") %>% 
  dplyr::arrange(n, date) %>% 
  dplyr::select(-n)

cases <- cases %>%
  dplyr::rename(local = cases) %>%
  dplyr::mutate(imported = 0) %>%
  tidyr::gather(key = "import_status", value = "cases", local, imported) %>% 
  tidyr::drop_na(region) %>% 
  dplyr::filter(!region %in% "Faroe Islands")

# Get linelist ------------------------------------------------------------

linelist <-  NCoVUtils::get_international_linelist() 

# Set up cores -----------------------------------------------------
if (!interactive()){
  options(future.fork.enable = TRUE)
}

## Allocating cores per parallel process helps with RAM requirements 
## and speeds up long running regions.
cores_per_job <- 1
jobs <- round(future::availableCores() / cores_per_job)

plan(list(tweak(multisession, workers = jobs),
          tweak(multisession, workers = cores_per_job)))

data.table::setDTthreads(threads = 1)

# Run pipeline ----------------------------------------------------

EpiNow::regional_rt_pipeline(
  cases = cases,
  linelist = linelist,
  target_folder = "national",
  case_limit = 60,
  horizon = 14,
  report_forecast = TRUE,
  forecast_model = function(...) {
    EpiSoon::fable_model(model = fabletools::combination_model(fable::RW(y ~ drift()), fable::ETS(y), 
                                                               fable::NAIVE(y),
                                                               cmbn_args = list(weights = "inv_var")), ...)
  }
)


# Summarise results -------------------------------------------------------

EpiNow::regional_summary(results_dir = "national",
                         summary_dir = "national-summary",
                         target_date = "latest",
                         region_scale = "Country/Region",
                         csv_region_label = "country",
                         log_cases = TRUE)


