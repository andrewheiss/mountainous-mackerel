library(readxl)
suppressPackageStartupMessages(library(lubridate))
library(countrycode)

clean_iccpr_who <- function(path) {
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
    # Final column order
    select(-c(country_code, country, cow_code, who_region)) %>% 
    select(country_name, iso3, day = date_reported, everything())
  
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
    # Final column order
    select(-country_code) %>% 
    select(country_name, iso3, day, everything())
  
  return(x)
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
           v2x_polyarchy, v2x_regime_amb
    )
  
  return(vdem_clean)
}

create_daily_skeleton <- function(iccpr_who, oxford, vdem) {
  all_countries <- list(unique(iccpr_who$iso3), 
                        unique(oxford$iso3), 
                        unique(vdem$iso3))
  
  countries_in_all_data <- reduce(all_countries, intersect)
  
  first_day <- min(oxford$day)
  last_day <- max(oxford$day)
  
  daily_skeleton <- expand_grid(
    iso3 = countries_in_all_data,
    day = seq(first_day, last_day, by = "1 day")
  ) %>% 
    mutate(year = year(day)) %>% 
    mutate(country_name = countrycode(
      iso3, origin = "iso3c", destination = "country.name",
      custom_match = c("XKX" = "Kosovo", "TUR" = "Türkiye")
    )) %>% 
    select(country_name, iso3, day, year)
  
  return(daily_skeleton)
}
