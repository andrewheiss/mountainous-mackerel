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
  tar_target(iccpr_treaty_action_file,
             here_rel("data", "raw_data", "ICCPR Treaty Action Variables.xlsx"),
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
  tar_target(naturalearth_raw_file,
             here_rel("data", "raw_data", "ne_110m_admin_0_countries",
                      "ne_110m_admin_0_countries.shp"),
             format = "file"),
  
  ## Process and clean data ----
  tar_target(iccpr_who_clean, clean_iccpr_who(iccpr_who_raw_file)),
  tar_target(iccpr_action_clean, clean_iccpr_action(iccpr_treaty_action_file)),
  tar_target(oxford_clean, clean_oxford(oxford_raw_file)),
  tar_target(pandem_clean, clean_pandem(pandem_raw_file)),
  tar_target(vdem_clean, clean_vdem(vdem_raw_file)),
  
  tar_target(skeleton, 
             create_daily_skeleton(iccpr_who_clean, oxford_clean, 
                                   pandem_clean, vdem_clean)),
  
  tar_target(daily_panel, 
             make_final_data(skeleton, iccpr_who_clean, iccpr_action_clean, 
                             oxford_clean, pandem_clean, vdem_clean)),
  
  tar_target(weekly_panel, make_weekly_data(daily_panel)),
  
  tar_target(year_week_lookup, make_year_week_lookup(weekly_panel)),
  
  tar_target(world_map, load_world_map(naturalearth_raw_file)),
  tar_target(derogation_count, make_derogation_count(iccpr_who_clean)),
  tar_target(derogation_map_data, make_derogation_map_data(derogation_count, world_map)),

  
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
  tar_target(m_policies, f_policies(weekly_panel)),
  tar_target(m_human_rights, f_human_rights(weekly_panel)),

  ## Model tables ----
  # Build tables here because they take a while
  tar_target(modelsummary_functions, lst(coef_map, gof_map)),
  tar_target(models_tbl_policies, build_modelsummary(m_policies)),
  tar_target(models_tbl_human_rights, build_modelsummary(m_human_rights)),

  ## Graphics ----
  tar_target(graphic_functions, lst(theme_pandem, set_annotation_fonts, clrs)),
  tar_target(diagnostic_functions, lst(plot_trace, plot_trank, plot_pp)),
  
  ## Plots and tables ----
  tar_target(policies_plot_data, build_policies_plot_data(m_policies, year_week_lookup)),
  tar_target(policies_table_data, build_policies_table_data(policies_plot_data)),
  tar_target(human_rights_plot_data, build_human_rights_plot_data(m_human_rights, year_week_lookup)),
  tar_target(hr_table_data, build_hr_table_data(human_rights_plot_data)),

  ## Manuscript and analysis notebook ----
  tar_quarto(manuscript_nice, path = "manuscript", quiet = FALSE, profile = "nice"),
  tar_quarto(appendix_nice, path = "manuscript", quiet = FALSE, profile = "appendix-nice"),
  tar_quarto(manuscript_manuscripty, path = "manuscript", quiet = FALSE, profile = "ms"),
  tar_quarto(appendix_manuscripty, path = "manuscript", quiet = FALSE, profile = "appendix-ms"),

  tar_quarto(website, path = ".", quiet = FALSE),
  tar_target(deploy_script, here_rel("deploy.sh"), format = "file"),
  tar_target(deploy, {
    # Force a dependency
    website
    # Run the deploy script
    if (Sys.getenv("UPLOAD_WEBSITES") == "TRUE") processx::run(paste0("./", deploy_script))
  }),
  
  ## Render the README ----
  tar_quarto(readme, here_rel("README.qmd"))
)
