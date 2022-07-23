library(readxl)
library(lubridate)

clean_oxford <- function(df) {
  x <- tibble(
    # Get a list of all the sheets in the Excel file
    index_name = excel_sheets(df)
  ) %>% 
    # Read each sheet
    mutate(data = map(index_name, ~read_excel(df, sheet = .x))) %>% 
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
    mutate(iso3 = recode(country_code, "RKS" = "XKK")) %>% 
    mutate(country_name = countrycode(
      iso3, origin = "iso3c", destination = "country.name",
      custom_match = c("XKK" = "Kosovo", "TUR" = "Türkiye")
    )) %>% 
    # Final column order
    select(-country_code) %>% 
    select(country_name, iso3, day, everything())
  
  return(x)
}
