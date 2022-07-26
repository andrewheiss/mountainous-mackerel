library(targets)
library(tarchetypes)
suppressPackageStartupMessages(library(dplyr))

options(tidyverse.quiet = TRUE,
        dplyr.summarise.inform = FALSE)

# Bayesian stuff
suppressPackageStartupMessages(library(brms))
options(mc.cores = 4,
        brms.backend = "cmdstanr")

set.seed(8588)  # From random.org

tar_option_set(
  packages = c("tidyverse"),
  format = "qs",
  workspace_on_error = TRUE,
  workspaces = c()
)

# here::here() returns an absolute path, which then gets stored in tar_meta and
# becomes computer-specific (i.e. /Users/andrew/Research/blah/thing.Rmd).
# There's no way to get a relative path directly out of here::here(), but
# fs::path_rel() works fine with it (see
# https://github.com/r-lib/here/issues/36#issuecomment-530894167)
here_rel <- function(...) {fs::path_rel(here::here(...))}

# Load R scripts with functions to use in the pipeline
lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)

# Pipeline ----------------------------------------------------------------
list(
  ## Raw data files ----
  tar_target(iccpr_who_raw_file,
             here_rel("data", "raw_data", "ICCPR Derogation and WHO case data 1 3 2020 to 6 30 2021.xlsx"),
             format = "file"),
  tar_target(oxford_raw_file,
             here_rel("data", "raw_data", "Oxford Covid Response Data 1 3 2020 to 6 20 2021.xlsx"),
             format = "file"),
  tar_target(pandem_raw_file,
             here_rel("data", "raw_data", "pandem", "datasets", "pandem_TS_v6.xlsx"),
             format = "file"),
  tar_target(vdem_raw_file,
             here_rel("data", "raw_data", "Country_Year_V-Dem_Full+others_R_v12",
                      "V-Dem-CY-Full+Others-v12.rds"),
             format = "file"),
  
  ## Process and clean data ----
  tar_target(iccpr_who_clean, clean_iccpr_who(iccpr_who_raw_file)),
  tar_target(oxford_clean, clean_oxford(oxford_raw_file)),
  tar_target(pandem_clean, clean_pandem(pandem_raw_file)),
  tar_target(vdem_clean, clean_vdem(vdem_raw_file)),
  
  tar_target(skeleton, 
             create_daily_skeleton(iccpr_who_clean, oxford_clean, 
                                   pandem_clean, vdem_clean)),
  
  tar_target(daily_panel, 
             make_final_data(skeleton, iccpr_who_clean, oxford_clean, 
                             pandem_clean, vdem_clean)),
  
  tar_target(weekly_panel, make_weekly_data(daily_panel)),
  
  tar_target(year_week_lookup, make_year_week_lookup(weekly_panel)),
  
  ## Save data ----
  ### Daily ----
  tar_target(data_daily_stata, 
             save_dta(daily_panel, here_rel("data", "derived_data", "daily_panel.dta")),
             format = "file"),
  tar_target(data_daily_stata_website, 
             save_dta(daily_panel, here_rel("analysis", "data", "daily_panel.dta")),
             format = "file"),
  tar_target(data_daily_csv, 
             save_csv(daily_panel, here_rel("data", "derived_data", "daily_panel.csv")),
             format = "file"),
  tar_target(data_daily_csv_website, 
             save_csv(daily_panel, here_rel("analysis", "data", "daily_panel.csv")),
             format = "file"),
  tar_target(data_daily_rds, 
             save_r(daily_panel, here_rel("data", "derived_data", "daily_panel.rds")),
             format = "file"),
  tar_target(data_daily_rds_website, 
             save_r(daily_panel, here_rel("analysis", "data", "daily_panel.rds")),
             format = "file"),
  
  ### Weekly ----
  tar_target(data_weekly_stata, 
             save_dta(weekly_panel, here_rel("data", "derived_data", "weekly_panel.dta")),
             format = "file"),
  tar_target(data_weekly_stata_website, 
             save_dta(weekly_panel, here_rel("analysis", "data", "weekly_panel.dta")),
             format = "file"),
  tar_target(data_weekly_csv, 
             save_csv(weekly_panel, here_rel("data", "derived_data", "weekly_panel.csv")),
             format = "file"),
  tar_target(data_weekly_csv_website, 
             save_csv(weekly_panel, here_rel("analysis", "data", "weekly_panel.csv")),
             format = "file"),
  tar_target(data_weekly_rds, 
             save_r(weekly_panel, here_rel("data", "derived_data", "weekly_panel.rds")),
             format = "file"),
  tar_target(data_weekly_rds_website, 
             save_r(weekly_panel, here_rel("analysis", "data", "weekly_panel.rds")),
             format = "file"),
  
  ## Models ----
  tar_target(m_prelim_derog, f_prelim_derog(weekly_panel)),
  tar_target(m_h1, f_h1(weekly_panel)),
  tar_target(m_h2, f_h2(weekly_panel)),
  tar_target(m_h3, f_h3(weekly_panel)),
  
  tar_target(m_coef_gof, lst(coef_map, gof_map)),
  
  ## Graphics ----
  tar_target(graphic_functions, lst(theme_pandem, set_annotation_fonts, clrs)),
  
  ## Analysis notebook ----
  tar_quarto(analysis_notebook, path = "analysis"),
  tar_target(deploy_script, here_rel("deploy.sh"), format = "file"),
  tar_target(deploy_notebook, {
    # Force a dependency
    analysis_notebook
    # Run the deploy script
    if (Sys.getenv("UPLOAD_WEBSITES") == "TRUE") processx::run(paste0("./", deploy_script))
  })
)
