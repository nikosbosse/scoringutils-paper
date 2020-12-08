# load libraries ---------------------------------------------------------------
# remotes::install_github("sbfnk/covid19.forecasts.uk")
library(scoringutils)
library(dplyr)
library(ggplot2)
library(covid19.forecasts.uk)


# load data --------------------------------------------------------------------
data(covid_uk_data)
data(uk_forecasts)
data(ensembles)

# reformat data for scoringutils -----------------------------------------------

# all_forecasts <- dplyr::bind_rows(uk_forecasts, ensembles) %>%
#   dplyr::rename(prediction = value) %>%
#   dplyr::select(-nmodels)

combined <- dplyr::inner_join(uk_forecasts %>%
                                dplyr::rename(prediction = value), 
                              covid_uk_data %>%
                                dplyr::rename(true_value = value))

example_subset <- combined %>%
  dplyr::filter(model == "SIRCOVID", 
                creation_date == "2020-06-22")

# plot predictions -------------------------------------------------------------

additional_observations <- covid_uk_data %>%
  dplyr::rename(true_value = value) %>%
  dplyr::filter(value_date <= "2020-06-22", 
                value_date > "2020-06-01", 
                value_type %in% unique(example_subset$value_type))

scoringutils::plot_predictions(example_subset, 
                               additional_observations,
                               x = "value_date",
                               facet_formula = geography ~ value_type)
ggplot2::ggsave("plots/forecast_visualistion.png")



# create scoring table ---------------------------------------------------------
scores <- combined %>%
  scoringutils::eval_forecasts(summarise_by = c("model", "value_type")) %>%
  dplyr::select(-coverage, -quantile_coverage)

scoringutils::score_table(scores, y = "model", facet_formula = ~ value_type)
ggplot2::ggsave("plots/score_table.png")

# create rank plot -------------------------------------------------------------


# create WIS components plot

scores <- combined %>%
  scoringutils::eval_forecasts(summarise_by = c("model")) %>%
  dplyr::select(-coverage, -quantile_coverage)

scoringutils::wis_components(scores, y = "model") + 
  ggplot2::scale_y_log10()
ggplot2::ggsave("plots/score_table.png")


combined$creation_date %>%
  min()
