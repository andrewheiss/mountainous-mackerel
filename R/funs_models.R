coef_map <- c(
  "b_derogation_ineffect" = "Derogation in effect",
  "b_v2csreprss" = "Civil society repression",
  "b_derogation_ineffect:v2csreprss" = "Derogation × Civil society represstion",
  "b_new_cases_z" = "New cases (standardized)",
  "b_cumulative_cases_z" = "Cumulative cases (standardized)",
  "b_new_deaths_z" = "New deaths (standardized)",
  "b_cumulative_deaths_z" = "Cumulative deaths (standardized)",
  "b_year_week_num" = "Year-week",
  "b_v2x_rule" = "Rule of law",
  "b_v2x_civlib" = "Civil liberties",
  "b_v2xcs_ccsi" = "Core civil society index",
  "b_Intercept" = "Constant",
  "b_Intercept[1]" = "Cut 1",
  "b_Intercept[2]" = "Cut 2",
  "b_Intercept[3]" = "Cut 3",
  "sd_country_name__Intercept" = "Country random effects σ",
  "sd_who_region__Intercept" = "Region random effects σ"
)

gof_map <- tribble(
  ~raw,        ~clean,     ~fmt, ~omit,
  "nobs",      "N",        0,    TRUE
)

# This is how to get other stats like ELPD (using metrics = "LOOIC" is required
# for that; metrics = "R2" provides nobs). I would do this for all the summary
# tables, but for whatever reason, it takes forever to calculate ELPD/LOO stuff
# on ordered logit models, so we just show N instead
#
# gm <- tribble(
#   ~raw,        ~clean,      ~fmt, ~omit,
#   "nobs",      "N",         0,    FALSE,
#   "r.squared", "R2",        3,    FALSE,
#   "elpd",      "ELPD",      1,    FALSE,
#   "elpd.se",   "ELPD (SE)", 1,    FALSE
# )
# 
# modelsummary(models_prelim,
#              estimate = "{estimate}",
#              statistic = "conf.int",
#              gof_map = gm,
#              metrics = c("R2", "LOOIC"))


f_prelim_derog <- function(panel) {
  BAYES_SEED <- 1757  # From random.org
  
  panel <- panel %>% 
    mutate(across(c(new_cases, new_deaths, cumulative_cases, cumulative_deaths),
                  list(z = ~as.numeric(scale(.)))))
  
  # Use .data[[blah]] for tidy evaluation with strings
  # https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/#other-simple-tidy-evaluation-patterns
  never_filter <- function(x) {
    panel %>% 
      filter(.data[[x]] == 0)
  }
  
  logit_priors <- c(prior(student_t(1, 0, 3), class = Intercept),
                    prior(student_t(1, 0, 3), class = b),
                    prior(cauchy(0, 1), class = sd, lb = 0))
  
  prelim_derog_model <- function(y, data) {
    form <- glue::glue(y, " ~ derogation_ineffect + new_cases_z + cumulative_cases_z + ",
                       "new_deaths_z + cumulative_deaths_z + year_week_num + (1 | country_name)") %>% 
      as.formula()
    
    # Use rlang::inject() to evaluate the formula object before running the model
    # so that the formula shows up correctly in summary(). 
    # See https://community.rstudio.com/t/tidy-evaluation-and-formulae/4561/17
    rlang::inject(
      brm(
        bf(!!form), 
        data = data,
        family = bernoulli(),
        prior = logit_priors,
        chains = 4, seed = BAYES_SEED,
        threads = threading(2)  # Two CPUs per chain to speed things up
      )
    )
  }
  
  preliminary_models <- tribble(
    ~nice, ~y, ~never,
    "Cancel Public Events", "c3_cancel_events_bin", "c3_cancel_events_never",
    "Gathering Restrictions", "c4_gatherings_bin", "c4_gatherings_never",
    "Close Public Transit", "c5_public_transport_bin", "c5_public_transport_never",
    "Movement", "c7_internal_movement_bin", "c7_internal_movement_never",
    "International Travel", "c8_intl_travel_bin", "c8_intl_travel_never"
  ) %>% 
    mutate(data = map(never, ~never_filter(.))) %>% 
    mutate(model = map2(y, data, ~prelim_derog_model(.x, .y)))
  
  return(preliminary_models)
}


f_h1 <- function(panel) {
  BAYES_SEED <- 6440  # From random.org
  
  panel <- panel %>% 
    mutate(across(c(new_cases, new_deaths, cumulative_cases, cumulative_deaths),
                  list(z = ~as.numeric(scale(.)))))
  
  logit_priors <- c(prior(student_t(1, 0, 3), class = Intercept),
                    prior(student_t(1, 0, 3), class = b),
                    prior(cauchy(0, 1), class = sd, lb = 0))
  
  ologit_priors <- c(prior(student_t(1, 0, 3), class = Intercept),
                     prior(student_t(1, 0, 3), class = b),
                     prior(cauchy(0, 1), class = sd, lb = 0))
  
  # It would be neat to include country random effects but there's not enough
  # variation within lots of the countries to pick up any of the effect. When 
  # (1 | country_name) is included, the model predicts a 100% chance of 
  # pandem_discrim == 1 and a 0% chance of pandem_discrim == 0, which is
  # annoying (and it takes 45 minutes to run, ugh)
  
  # So instead we use region random effects instead? That only takes 10 minutes.
  # Or no random effects at all?
  h1_model <- function(y, family, prior) {
    form <- glue::glue(y, " ~ derogation_ineffect + new_cases_z + cumulative_cases_z + ",
                       "new_deaths_z + cumulative_deaths_z + ", 
                       "v2x_rule + v2x_civlib + v2xcs_ccsi + ",
                       "year_week_num + (1 | who_region)") %>% 
      as.formula()
    
    # Use rlang::inject() to evaluate the formula object before running the model
    # so that the formula shows up correctly in summary(). 
    # See https://community.rstudio.com/t/tidy-evaluation-and-formulae/4561/17
    rlang::inject(
      brm(
        bf(!!form), 
        data = panel,
        family = family,
        prior = prior,
        chains = 4, seed = BAYES_SEED,
        threads = threading(2)  # Two CPUs per chain to speed things up
      )
    )
  }
  
  h1_models <- tribble(
    ~nice, ~y, ~family, ~prior,
    "Discriminatory Policy", "pandem_discrim", "cumulative", ologit_priors,
    "Non-Derogable Rights", "pandem_ndrights", "bernoulli", logit_priors,
    "No Time Limit Measures", "pandem_nolimit", "cumulative", ologit_priors,
    "Abusive Enforcement", "pandem_abusive", "cumulative", ologit_priors
  ) %>% 
    # Neat pattern for using named list elements in pmap instead of ..1, ..2, etc.
    # https://stackoverflow.com/a/66147672/120898
    # But we can't use that here because of ... issues nested in a function like
    # this. And there are similar issues when using ..1, ..2, etc. when using
    # the formula syntax like ~h1_model(..1, ..2, ..3). Everything works fine
    # without the anonymous lambda ~ syntax though
    mutate(model = pmap(lst(y, family, prior), h1_model))
  
  return(h1_models)
}


f_h2 <- function(panel) {
  BAYES_SEED <- 7973  # From random.org
  
  panel <- panel %>% 
    mutate(across(c(new_cases, new_deaths, cumulative_cases, cumulative_deaths),
                  list(z = ~as.numeric(scale(.)))))
  
  logit_priors <- c(prior(student_t(1, 0, 3), class = Intercept),
                    prior(student_t(1, 0, 3), class = b),
                    prior(cauchy(0, 1), class = sd, lb = 0))
  
  ologit_priors <- c(prior(student_t(1, 0, 3), class = Intercept),
                     prior(student_t(1, 0, 3), class = b),
                     prior(cauchy(0, 1), class = sd, lb = 0))
  
  h2_model <- function(y, family, prior) {
    form <- glue::glue(y, " ~ derogation_ineffect*v2csreprss + ",
                       "new_cases_z + cumulative_cases_z + ",
                       "new_deaths_z + cumulative_deaths_z + ", 
                       "v2x_rule + v2x_civlib + ",
                       "year_week_num + (1 | who_region)") %>% 
      as.formula()
    
    # Use rlang::inject() to evaluate the formula object before running the model
    # so that the formula shows up correctly in summary(). 
    # See https://community.rstudio.com/t/tidy-evaluation-and-formulae/4561/17
    rlang::inject(
      brm(
        bf(!!form), 
        data = panel,
        family = family,
        prior = prior,
        chains = 4, seed = BAYES_SEED,
        threads = threading(2)  # Two CPUs per chain to speed things up
      )
    )
  }
  
  h2_models <- tribble(
    ~nice, ~y, ~family, ~prior,
    "Discriminatory Policy", "pandem_discrim", "cumulative", ologit_priors,
    "Non-Derogable Rights", "pandem_ndrights", "bernoulli", logit_priors,
    "Limiting Media", "pandem_media", "cumulative", ologit_priors,
    "Abusive Enforcement", "pandem_abusive", "cumulative", ologit_priors
  ) %>% 
    mutate(model = pmap(lst(y, family, prior), h2_model))
  
  return(h2_models)
}
