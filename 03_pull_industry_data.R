library(tidyverse)

state_to_fed <- tribble(
  ~state_fips, ~fed,
  "01", "Atlanta",       "02", "San Francisco", "04", "San Francisco",
  "05", "St. Louis",     "06", "San Francisco", "08", "Kansas City",
  "09", "Boston",        "10", "Philadelphia",  "11", "Richmond",
  "12", "Atlanta",       "13", "Atlanta",        "15", "San Francisco",
  "16", "San Francisco", "17", "Chicago",        "18", "Chicago",
  "19", "Chicago",       "20", "Kansas City",    "21", "St. Louis",
  "22", "Atlanta",       "23", "Boston",         "24", "Richmond",
  "25", "Boston",        "26", "Chicago",        "27", "Minneapolis",
  "28", "Atlanta",       "29", NA,               "30", "Minneapolis",
  "31", "Kansas City",   "32", "San Francisco",  "33", "Boston",
  "34", "New York",      "35", "Dallas",         "36", "New York",
  "37", "Richmond",      "38", "Minneapolis",    "39", "Cleveland",
  "40", "Kansas City",   "41", "San Francisco",  "42", "Philadelphia",
  "44", "Boston",        "45", "Richmond",       "46", "Minneapolis",
  "47", "Atlanta",       "48", "Dallas",         "49", "San Francisco",
  "50", "Boston",        "51", "Richmond",       "53", "San Francisco",
  "54", "Richmond",      "55", "Chicago",        "56", "Kansas City"
)

# Missouri KC Fed counties (western MO)
mo_kc_fips <- paste0("29", c(
  "003","009","011","013","015","021","023","025","029","031","033","037",
  "039","041","043","045","047","049","051","053","055","057","059","063",
  "065","073","075","083","085","087","093","095","101","103","107","111",
  "113","115","117","121","123","131","133","135","137","139","141","145",
  "159","163","165","167","177","195","197","201","209","211","213","215",
  "217","221","225"
))

naics_to_sector <- tribble(
  ~industry_code, ~sector,
  "11",    "Agriculture",
  "21",    "Energy",
  "22",    "Energy",
  "23",    "Housing",
  "31-33", "Manufacturing",
  "42",    "Trade",
  "44-45", "Trade",
  "48-49", "Trade",
  "51",    "Technology",
  "52",    "Finance",
  "53",    "Housing",
  "54",    "Technology",
  "55",    "Finance",
  "56",    "Manufacturing",
  "61",    "Labor",
  "62",    "Labor",
  "71",    "Labor",
  "72",    "Labor",
  "81",    "Labor",
  "92",    "Labor"
)

message("Reading QCEW singlefile (~30s)...")
qcew_raw <- read_csv(
  "data/2022.annual.singlefile.csv",
  col_types = cols(
    area_fips         = col_character(),
    own_code          = col_double(),
    industry_code     = col_character(),
    agglvl_code       = col_double(),
    annual_avg_emplvl = col_double(),
    .default          = col_skip()
  )
)
message("Rows: ", nrow(qcew_raw))

county_industry <- qcew_raw |>
  filter(
    agglvl_code == 74,
    nchar(area_fips) == 5,
    !str_ends(area_fips, "000"),
    !is.na(annual_avg_emplvl),
    annual_avg_emplvl > 0
  ) |>
  group_by(area_fips, industry_code) |>
  summarise(employment = sum(annual_avg_emplvl, na.rm = TRUE), .groups = "drop")

message("County-industry rows: ", nrow(county_industry))
message("Unique counties: ", n_distinct(county_industry$area_fips))

county_fed <- county_industry |>
  mutate(state_fips = substr(area_fips, 1, 2)) |>
  left_join(state_to_fed, by = "state_fips") |>
  mutate(fed = case_when(
    area_fips %in% mo_kc_fips ~ "Kansas City",
    state_fips == "29"        ~ "St. Louis",
    TRUE                      ~ fed
  )) |>
  filter(!is.na(fed))

fed_industry <- county_fed |>
  left_join(naics_to_sector, by = "industry_code") |>
  filter(!is.na(sector)) |>
  group_by(fed, sector) |>
  summarise(employment = sum(employment, na.rm = TRUE), .groups = "drop") |>
  group_by(fed) |>
  mutate(emp_share = employment / sum(employment)) |>
  ungroup()

message("\nFed industry shares (wide):")
print(
  fed_industry |>
    select(fed, sector, emp_share) |>
    pivot_wider(names_from = sector, values_from = emp_share) |>
    arrange(fed)
)

write_csv(fed_industry, "data/fed_industry_shares.csv")
message("Saved to data/fed_industry_shares.csv")
