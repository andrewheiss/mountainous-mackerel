library(targets)
library(tarchetypes)

options(tidyverse.quiet = TRUE,
        dplyr.summarise.inform = FALSE)

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
  tar_target(iccpr_who_raw,
             here_rel("data", "raw_data", "ICCPR Derogation and WHO case data 1 3 2020 to 6 30 2021.xlsx"),
             format = "file"),
  tar_target(oxford_raw,
             here_rel("data", "raw_data", "Oxford Covid Response Data 1 3 2020 to 6 20 2021.xlsx"),
             format = "file"),
  
  ## Process and clean data ----
  tar_target(oxford_clean, clean_oxford(oxford_raw))
)
