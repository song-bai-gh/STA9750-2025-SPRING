ensure_package <- function(pkg){
  pkg <- as.character(substitute(pkg))
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  if(!require(pkg, character.only=TRUE)) install.packages(pkg)
  stopifnot(require(pkg, character.only=TRUE))
}

ensure_package(httr2)
ensure_package(rvest)
ensure_package(datasets)
ensure_package(purrr)
ensure_package(DT)
ensure_package(stringr)
ensure_package(dplyr)

get_eia_sep <- function(state, abbr){
  state_formatted <- str_to_lower(state) |> str_replace_all("\\s", "")
  
  dir_name <- file.path("data", "mp02")
  file_name <- file.path(dir_name, state_formatted)
  
  dir.create(dir_name, showWarnings=FALSE, recursive=TRUE)
  
  if(!file.exists(file_name)){
    BASE_URL <- "https://www.eia.gov"
    REQUEST <- request(BASE_URL) |> 
      req_url_path("electricity", "state", state_formatted)
    
    RESPONSE <- req_perform(REQUEST)
    
    resp_check_status(RESPONSE)
    
    writeLines(resp_body_string(RESPONSE), file_name)
  }
  
  TABLE <- read_html(file_name) |> 
    html_element("table") |> 
    html_table() |>
    mutate(Item = str_to_lower(Item))
  
  if("U.S. rank" %in% colnames(TABLE)){
    TABLE <- TABLE |> rename(Rank = `U.S. rank`)
  }
  
  CO2_MWh <- TABLE |> 
    filter(Item == "carbon dioxide (lbs/mwh)") |>
    pull(Value) |> 
    str_replace_all(",", "") |>
    as.numeric()
  
  PRIMARY <- TABLE |> 
    filter(Item == "primary energy source") |> 
    pull(Rank)
  
  RATE <- TABLE |>
    filter(Item == "average retail price (cents/kwh)") |>
    pull(Value) |>
    as.numeric()
  
  GENERATION_MWh <- TABLE |>
    filter(Item == "net generation (megawatthours)") |>
    pull(Value) |>
    str_replace_all(",", "") |>
    as.numeric()
  
  data.frame(CO2_MWh               = CO2_MWh, 
             primary_source        = PRIMARY,
             electricity_price_MWh = RATE * 10, # / 100 cents to dollars &
             # * 1000 kWh to MWH 
             generation_MWh        = GENERATION_MWh, 
             state                 = state, 
             abbreviation          = abbr
  )
}

EIA_SEP_REPORT <- map2(state.name, state.abb, get_eia_sep) |> list_rbind()

library(knitr)

# Which state has the most expensive retail electricity?
state_highest_price <- EIA_SEP_REPORT |>
  select(electricity_price_MWh, state) |>
  slice_max(electricity_price_MWh, n=1)
kable(state_highest_price)

# Which state has the 'dirtiest' electricity mix?
state_dirtiest_electricity <- EIA_SEP_REPORT |>
  select(CO2_MWh, primary_source, state) |>
  slice_max(CO2_MWh, n=1)
state_dirtiest_electricity

# On average, how many pounds of CO2 are emitted per MWh of electricity produced in the US? (Note that you will need to use a suitably weighted average here.)
average_co2_emitted <- EIA_SEP_REPORT |>
  summarize(
    average_co2 = sum(CO2_MWh * generation_MWh) / sum(generation_MWh)
  )
average_co2_emitted

# What is the rarest primary energy source in the US? 
rarest_source <- EIA_SEP_REPORT |>
  group_by(primary_source) |>
  summarize(num_state_source = n()) |>
  slice_min(num_state_source, n = 1)
rarest_source
# What is the associated cost of electricity and where is it used?
rarest_source_table <- EIA_SEP_REPORT |>
  select(primary_source, electricity_price_MWh, state) |>
  filter(primary_source == "Petroleum")
rarest_source_table

# How many times cleaner is NY’s energy mix than that of Texas?
co2_emitted_NY <- EIA_SEP_REPORT |>
  filter(state == "New York") |>
  summarize(CO2_MWh)

co2_emitted_Texas <- EIA_SEP_REPORT |>
  filter(state == "Texas") |>
  summarize(CO2_MWh)

times_cleaner <- co2_emitted_Texas / co2_emitted_NY
times_cleaner

###
install.packages("readxl")
library(readxl)

###
###
ensure_package(readxl)
# Create 'data/mp02' directory if not already present
DATA_DIR <- file.path("data", "mp02")
dir.create(DATA_DIR, showWarnings=FALSE, recursive=TRUE)

NTD_ENERGY_FILE <- file.path(DATA_DIR, "2023_ntd_energy.xlsx")

if(!file.exists(NTD_ENERGY_FILE)){
  DS <- download.file("https://www.transit.dot.gov/sites/fta.dot.gov/files/2024-10/2023%20Energy%20Consumption.xlsx", 
                      destfile=NTD_ENERGY_FILE, 
                      method="curl")
  
  if(DS | (file.info(NTD_ENERGY_FILE)$size == 0)){
    cat("I was unable to download the NTD Energy File. Please try again.\n")
    stop("Download failed")
  }
}

NTD_ENERGY_RAW <- read_xlsx(NTD_ENERGY_FILE)

library(tidyr)
library(dplyr)

to_numeric_fill_0 <- function(x){
  replace_na(as.numeric(x), 0)
}

NTD_ENERGY <- NTD_ENERGY_RAW |> 
  select(-c(`Reporter Type`, 
            `Reporting Module`, 
            `Other Fuel`, 
            `Other Fuel Description`)) |>
  mutate(across(-c(`Agency Name`, 
                   `Mode`,
                   `TOS`), 
                to_numeric_fill_0)) |>
  group_by(`NTD ID`, `Mode`, `Agency Name`) |>
  summarize(across(where(is.numeric), sum), 
            .groups = "keep") |>
  mutate(ENERGY = sum(c_across(c(where(is.numeric))))) |>
  filter(ENERGY > 0) |>
  select(-ENERGY) |>
  ungroup()

# Display 10 random rows
slice_sample(NTD_ENERGY , n=10)


# clean up mode entries in ntd energy
NTD_ENERGY <- NTD_ENERGY |>
  mutate(Mode=case_when(
    Mode == "HR" ~ "Heavy Rail",
    Mode == "AR" ~ "Alaska Railroad",
    Mode == "CB" ~ "Commuter Bus",
    Mode == "CC" ~ "Cable Car",
    Mode == "CR" ~ "Commuter Rail",
    Mode == "DR" ~ "Demand Response",
    Mode == "FB" ~ "Ferryboat",
    Mode == "IP" ~ "Inclined Plane",
    Mode == "LR" ~ "Light Rail",
    Mode == "MB" ~ "Bus",
    Mode == "MG" ~ "Monorail and Automated Guideway modes",
    Mode == "PB" ~ "Publico",
    Mode == "RB" ~ "Bus Rapid Transit",
    Mode == "SR" ~ "Streetcar Rail",
    Mode == "TB" ~ "Trolleybus",
    Mode == "TR" ~ "Aerial Tramways",
    Mode == "VP" ~ "Vanpool",
    Mode == "YR" ~ "Hybrid Rail",
    TRUE ~ "Unknown"))

NTD_SERVICE_FILE <- file.path(DATA_DIR, "2023_service.csv")
if(!file.exists(NTD_SERVICE_FILE)){
  DS <- download.file("https://data.transportation.gov/resource/6y83-7vuw.csv", 
                      destfile=NTD_SERVICE_FILE, 
                      method="curl")
  
  if(DS | (file.info(NTD_SERVICE_FILE)$size == 0)){
    cat("I was unable to download the NTD Service File. Please try again.\n")
    stop("Download failed")
  }
}

NTD_SERVICE_RAW <- read_csv(NTD_SERVICE_FILE)

NTD_SERVICE <- NTD_SERVICE_RAW |>
  mutate(`NTD ID` = as.numeric(`_5_digit_ntd_id`)) |> 
  rename(Agency = agency, 
         City   = max_city, 
         State  = max_state,
         UPT    = sum_unlinked_passenger_trips_upt, 
         MILES  = sum_passenger_miles) |>
  select(matches("^[A-Z]", ignore.case=FALSE)) |>
  filter(MILES > 0)

NTD_SERVICE <- NTD_SERVICE_RAW |>
  mutate(`NTD ID` = as.numeric(`_5_digit_ntd_id`)) |> 
  rename(Agency = agency, 
         City   = max_city, 
         State  = max_state,
         UPT    = sum_unlinked_passenger_trips_upt, 
         MILES  = sum_passenger_miles) |>
  select(matches("^[A-Z]", ignore.case=FALSE)) |>
  filter(MILES > 0)