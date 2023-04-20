library(readxl)
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(clock))
library(countrycode)
library(jsonlite)
suppressPackageStartupMessages(library(sf))

clean_iccpr_who <- function(path) {
  who_regions <- tribble(
    ~who_region, ~who_region_long,
    "AFRO", "Regional Office for Africa",
    "AMRO", "Regional Office for the Americas",
    "SEARO", "Regional Office for South-East Asia",
    "EURO", "Regional Office for Europe",
    "EMRO", "Regional Office for the Eastern Mediterranean",
    "WPRO", "Regional Office for the Western Pacific"
  )
  
  x <- read_excel(path) %>% 
    janitor::clean_names() %>% 
    # Make this a date instead of PosixCT
    mutate(date_reported = as.Date(date_reported)) %>% 
    # All NAs here are actually 0s
    replace_na(list(iccpr_derogation_filed = 0,
                    derogation_start = 0,
                    derogation_ineffect = 0,
                    derogation_end = 0)) %>% 
    # Country names and codes fun times
    mutate(
      country_name = countrycode(
        country_code, origin = "iso2c", destination = "country.name",
        custom_match = c("XK" = "Kosovo", "TR" = "Türkiye")),
      iso3 = countrycode(
        country_code, origin = "iso2c", destination = "iso3c",
        custom_match = c("XK" = "XKX")
      )
    ) %>% 
    left_join(who_regions, by = "who_region") %>% 
    # Final column order
    select(-c(country_code, country, cow_code)) %>% 
    select(country_name, iso3, who_region, who_region_long,
           day = date_reported, everything())
  
  return(x)
}

clean_iccpr_action <- function(path) {
  x <- read_excel(path) %>% 
    janitor::clean_names() %>% 
    rename(prior_iccpr_other_action = other_prior_iccpr_post_commitment_treaty_actions,
           country_name = country) %>% 
    mutate(across(starts_with("prior_"), ~case_when(
      . == 1 ~ TRUE,
      is.na(.) ~ FALSE
    )))
  
  return(x)
}

clean_oxford <- function(path) {
  x <- tibble(
    # Get a list of all the sheets in the Excel file
    index_name = excel_sheets(path)
  ) %>% 
    # Read each sheet
    mutate(data = map(index_name, ~read_excel(path, sheet = .x))) %>% 
    # Standardize the index name based on the sheet name
    mutate(index_name = janitor::make_clean_names(index_name)) %>% 
    # Make each data frame cell in the list column long and a little cleaner
    mutate(clean = map(data, ~{
      .x %>% 
        pivot_longer(cols = -c(country_code, country_name),
                     names_to = "day", values_to = "value") %>% 
        mutate(day = dmy(day))
    })) %>% 
    # Get rid of the original wide data frame and unnest the long clean data
    select(-data) %>% 
    unnest(clean) %>% 
    # Make data wide so that there's a column for each index and row for each
    # country-day
    pivot_wider(names_from = "index_name", values_from = "value") %>% 
    # Country names and codes fun times
    mutate(iso3 = recode(country_code, "RKS" = "XKX")) %>% 
    mutate(country_name = countrycode(
      iso3, origin = "iso3c", destination = "country.name",
      custom_match = c("XKX" = "Kosovo", "TUR" = "Türkiye")
    )) %>% 
    # Get rid of countries with all missing data
    group_by(country_name) %>% 
    filter(!all(is.na(stringency_index))) %>% 
    ungroup() %>% 
    # Shorten these long column names
    rename(
      c3_cancel_events = c3_cancel_public_events, 
      c4_gatherings = c4_restrictions_on_gaterings,
      c5_public_transport = c5_close_public_transport, 
      c6_stay_at_home = c6_stay_at_home_requirements,
      c7_internal_movement = c7_movement_restrictions_intern, 
      c8_intl_travel = c8_international_travel_control
    ) %>% 
    # Make binary versions of these columns
    mutate(across(c(c3_cancel_events, c4_gatherings,
                    c5_public_transport, c6_stay_at_home,
                    c7_internal_movement, c8_intl_travel,
                    e1_income_support, e2_debt_relief),
                  list(bin = ~ ifelse(. > 0, 1, 0)))) %>% 
    # Create indicators for whether policies were added, removed, or never changed
    group_by(country_name) %>% 
    mutate(across(c(c3_cancel_events, c4_gatherings,
                    c5_public_transport, c6_stay_at_home,
                    c7_internal_movement, c8_intl_travel,
                    e1_income_support, e2_debt_relief),
                  list(added = ~any(c(NA, diff(.)) >= 1, na.rm = TRUE),
                       removed = ~any(c(NA, diff(.)) <= -1, na.rm = TRUE),
                       never = ~all(c(NA, diff(.)) == 0, na.rm = TRUE)))) %>% 
    ungroup() %>% 
    # Final column order
    select(-country_code) %>% 
    select(country_name, iso3, day, everything())
  
  return(x)
}

clean_pandem <- function(path) {
  pandem_raw <- read_excel(path)
  
  pandem_levels <- c("None" = "0", "Minor" = "1", "Moderate" = "2", "Major" = "3")
  
  pandem_clean <- pandem_raw %>% 
    mutate(quarter_numeric = parse_number(quarter) / 10) %>% 
    mutate(year_quarter = year + quarter_numeric) %>% 
    mutate(iso3 = countrycode(country_name, 
                              origin = "country.name", 
                              destination = "iso3c"),
           country_name = countrycode(iso3, origin = "iso3c", 
                                      destination = "country.name",
                                      custom_match = c("TUR" = "Türkiye"))) %>% 
    select(country_name, iso3, year, year_quarter, 
           pandem, panback, 
           pandem_discrim = type1, 
           pandem_ndrights = type2,
           pandem_abusive = type3,
           pandem_nolimit = type4,
           pandem_media = type7) %>% 
    # Make these 0-3 columns factors
    mutate(across(starts_with("pandem_"), ~factor(., levels = pandem_levels, ordered = TRUE))) %>% 
    # Add labels
    mutate(across(starts_with("pandem_"), ~fct_recode(., !!!pandem_levels))) %>% 
    # Drop unused levels
    mutate(across(starts_with("pandem_"), ~fct_drop(.)))
  
  return(pandem_clean)
}

clean_vdem <- function(path) {
  vdem_raw <- read_rds(path) %>% as_tibble()
  
  vdem_clean <- vdem_raw %>% 
    filter(year >= 2020) %>% 
    mutate(country_name = countrycode(
      country_text_id, origin = "iso3c", destination = "country.name",
      custom_match = c("XKX" = "Kosovo", "ZZB" = "Zanzibar", 
                       "PSG" = "Palestine (Gaza)", "SML" = "Somaliland", 
                       "TUR" = "Türkiye")
    )) %>% 
    select(country_name, iso3 = country_text_id, year,
           
           # Civil society stuff
           v2csreprss,  # CSO repression
           v2xcs_ccsi,  # Core civil society index (entry/exit, repression, participatory env)
           
           # Human rights and politics
           # Political corruption index (less to more, 0-1) (public sector +
           # executive + legislative + judicial corruption)
           v2x_corr,
           v2x_rule,  # Rule of law index
           
           # Rights indexes
           v2x_civlib,  # Civil liberties index
           v2x_clphy,  # Physical violence index
           v2x_clpriv,  # Private civil liberties index
           v2x_clpol,  # Political civil liberties index
           
           # Democracy
           v2x_polyarchy, v2x_libdem, v2x_regime_amb
    )
  
  return(vdem_clean)
}

create_daily_skeleton <- function(iccpr_who, oxford, pandem, vdem) {
  all_countries <- list(unique(iccpr_who$iso3), 
                        unique(oxford$iso3), 
                        unique(pandem$iso3),
                        unique(vdem$iso3))
  
  countries_in_all_data <- reduce(all_countries, intersect)
  
  # first_day <- min(oxford$day)
  first_day <- ymd("2020-03-11")
  last_day <- max(oxford$day)
  
  daily_skeleton <- expand_grid(
    iso3 = countries_in_all_data,
    day = seq(first_day, last_day, by = "1 day")
  ) %>% 
    mutate(year = year(day),
           year_quarter = quarter(day, type = "year.quarter"),
           day_num = as.numeric(day) - as.numeric(ymd("2020-03-10")),
           year_week = calendar_narrow(as_iso_year_week_day(day), "week"),
           year_week_day = as_year_month_day(calendar_narrow(set_day(year_week, 1), "day"))) %>% 
    # Force the year_week column to be text since it's a weird {clock} class
    mutate(year_week = as.character(year_week),
           year_week_day = as.character(year_week_day)) %>% 
    # Pandem starts Q2 2020 on March 11 instead of April 1
    mutate(year_quarter = ifelse(year_quarter == 2020.1, 2020.2, year_quarter)) %>% 
    mutate(country_name = countrycode(
      iso3, origin = "iso3c", destination = "country.name",
      custom_match = c("XKX" = "Kosovo", "TUR" = "Türkiye")
    )) %>% 
    select(country_name, iso3, day, day_num, year, year_quarter, year_week, year_week_day)
  
  return(daily_skeleton)
}

make_final_data <- function(skeleton, iccpr_who, iccpr_action, oxford, pandem, vdem) {
  daily_final <- skeleton %>% 
    left_join(select(iccpr_who, -country_name), by = c("iso3", "day")) %>% 
    left_join(select(iccpr_action, -country_name), by = c("iso3")) %>% 
    left_join(select(oxford, -country_name), by = c("iso3", "day")) %>% 
    left_join(select(pandem, -c(country_name, year)), by = c("iso3", "year_quarter")) %>% 
    left_join(select(vdem, -country_name), by = c("iso3", "year"))
  
  return(daily_final)
}

make_weekly_data <- function(daily_final) {
  weekly_final <- daily_final %>% 
    group_by(year_week, year_week_day, country_name, iso3, who_region, who_region_long, 
             prior_iccpr_derogations, prior_iccpr_other_action) %>% 
    summarize(across(c(new_cases, new_deaths), ~sum(., na.rm = TRUE)),
              across(c(iccpr_derogation_filed, derogation_start, 
                       derogation_ineffect, derogation_end), ~max(., na.rm = TRUE)),
              across(matches("[ce]\\d_"), ~max(., na.rm = TRUE)),
              across(c(pandem, panback, starts_with("pandem_"), starts_with("v2")), ~max(., na.rm = TRUE))) %>% 
    group_by(country_name) %>% 
    mutate(cumulative_cases = cumsum(new_cases),
           cumulative_deaths = cumsum(new_deaths)) %>% 
    mutate(year_week_num = 1:n(), .after = "year_week") %>% 
    ungroup() %>% 
    arrange(country_name)
  
  return(weekly_final)
}

make_year_week_lookup <- function(weekly_final) {
  year_week_lookup <- weekly_final %>% 
    distinct(year_week, year_week_num, year_week_day) %>% 
    mutate(year_week_day = ymd(year_week_day))
  
  return(year_week_lookup)
}

load_world_map <- function(path) {
  world_map <- read_sf(path) %>%
    filter(ISO_A3 != "ATA")
  
  return(world_map)
}

# When using a file-based target, {targets} requires that the function that
# saves the file returns a path to the file. write_csv() and write_dta() both
# invisibly return the data frame being written, and saveRDS() returns NULL, so
# we need some wrapper functions to save the files and return the paths.
save_csv <- function(df, path) {
  readr::write_csv(df, path)
  return(path)
}

save_r <- function(df, path) {
  saveRDS(df, path)
  return(path)
}

save_dta <- function(df, path) {
  haven::write_dta(df, path)
  return(path)
}
