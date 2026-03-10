# Clear workspace and set work directory
# rm(list = ls())
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# working_dir <- getwd()


# devtools::install_github("JGCRI/rgcam)
library(rgcam)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(patchwork)
library(RColorBrewer)
library(gcamdata)
library(stringr)
#library(tidyverse)

# read mapping files ----
gas_consumption_mapping <- read_csv("./mappings/gas_consumption_mapping.csv")
price_mapping <- read_csv("./mappings/price_mapping.csv")
state_mapping <- read_csv("./mappings/state_mapping.csv")

# define constants ----
ANALYSIS_YEARS <- 2021:2050
CONV_NG_EJ_QBTU <- 1/(1.0551 * .905)    # conversion from EJ LHV of natural gas to quadrillion btu HHV
CONV_NG_EJ_TBTU <- CONV_NG_EJ_QBTU * 1000    # conversion from EJ LHV of natural gas to trillion btu HHV
CONV_elec_EJ_TWh <- 1/.0036             # conversion from EJ of electricity to TWh
gdp_deflator_1975_2021 <- 3.964         # conversion from 1975$ in GCAM output to 2021$ for reporting
EJ_Tcf <- .981
EJ_Mmcf <- 981000                       # conversion from million cubic feet of natural gas to EJ (LHV)
EXCLUDED_STATES <- c("AK", "HI")        # Only CONUS states are tracked at this point
REPORTING_DIGITS_TWH <- 3               # number of decimal places to report quantities in TWh
REPORTING_DIGITS_TBTU <- 1               # number of decimal places to report quantities in Tbtu
REPORTING_DIGITS_QBTU <- 4               # number of decimal places to report quantities in Qbtu

# read rgcam proj file----
gcam_output.proj <- loadProject("rgcam/gcam_output.proj")

REQUERY_DATA <- FALSE
if(REQUERY_DATA){
  scenarios <- c("FutureGrid_Ref")

  for(i in scenarios){
    conn <- localDBConn("/Users/d3p747/Desktop/stash/decarb/output", "database_basexdb")
    gcam_output.proj <- addScenario(conn, gcam_output.proj, i, "rgcam/BatchQueries_futuregrid.xml", clobber = TRUE)
  }
}

# List available queries:
listQueries(gcam_output.proj, 'FutureGrid_Ref')

# Key outputs from this script
# Electricity consumption (TWh/yr) by scenario and year: state, nation
# Electricity generation by scenario, technology, and year: by state, nation
# Gas prices by scenario, consumption sector, and year: state, nation
# Gas consumption by scenario, state, reporting category, and year
# Gas production by scenario, state, and year
# Gas trade:
#   pipeline imports from Mexico and Canada
#   pipeline exports to Mexico and Canada
#   LNG imports: nation
#   LNG exports: state, nation

# Electricity consumption
elec_consumption_state <- getQuery(gcam_output.proj, "outputs by tech") %>%
  filter(grepl("elect_td", sector),
         year %in% ANALYSIS_YEARS,
         !region %in% EXCLUDED_STATES) %>%
  group_by(scenario, region, year) %>%
  summarise(consumption_TWh = round(sum(value) * CONV_elec_EJ_TWh, digits = REPORTING_DIGITS_TWH)) %>%
  ungroup()

write_csv(elec_consumption_state, "outputs/elec_consumption_state.csv")

elec_consumption_nation <- elec_consumption_state %>%
  group_by(scenario, year) %>%
  summarise(consumption_TWh = sum(consumption_TWh)) %>%
  ungroup()

write_csv(elec_consumption_nation, "outputs/elec_consumption_nation.csv")

# Electricity consumption comparison with EER Reference scenario
EER_load_by_scenario <- read_csv("data/EER_load_by_scenario.csv")

ref_elec_EER <- filter(EER_load_by_scenario, scenario == "reference") %>%
  mutate(elec_EER = load_MWh / 1e6 / CONV_elec_EJ_TWh) %>%
  select(region = state, year, elec_EER)

ref_elec_GCAM <- getQuery(gcam_output.proj, "outputs by tech") %>%
  filter(grepl("elect_td", sector),
         year %in% ANALYSIS_YEARS,
         scenario == "FutureGrid_Ref") %>%
  group_by(region, year) %>%
  summarise(elec_GCAM = sum(value)) %>%
  ungroup()

ref_elec_comparison <- left_join(ref_elec_EER, ref_elec_GCAM, by = c("region", "year")) %>%
  group_by(region) %>%
  mutate(EER_growth = elec_EER - elec_EER[year == 2021],
         GCAM_growth = elec_GCAM - elec_GCAM[year == 2021],
         target_increase = EER_growth - GCAM_growth)

ref_elec_comparison_for_plot <- filter(ref_elec_comparison, year > 2021) %>%
  mutate(highlight = if_else(abs(target_increase) < 0.09, FALSE, TRUE))
  
ggplot(ref_elec_comparison_for_plot, aes(x = elec_EER * CONV_elec_EJ_TWh, y = elec_GCAM * CONV_elec_EJ_TWh, color = highlight)) +
  geom_point() +
  facet_wrap(~year) +
  geom_abline(slope = 1, intercept = 0) +
  xlab("EER TWh") +
  ylab("GCAM TWh") +
  geom_text(data = filter(ref_elec_comparison_for_plot, abs(target_increase) > 0.09),
            aes(label = region),
            vjust = -0.5) +
  scale_color_manual(values = c("black", "red")) +
  theme(legend.position = "none")

ggsave("figures/EER_GCAM_ref_comparison.png", width = 5, height = 5, units = "in")

# Electricity generation
elec_gen_tech_state <- getQuery(gcam_output.proj, "outputs by nested tech") %>%
  filter(grepl("generation", sector) | technology == "rooftop_pv",
         year %in% ANALYSIS_YEARS,
         !region %in% EXCLUDED_STATES) %>%
  mutate(technology = str_to_title(subsector...5)) %>%
  group_by(scenario, region, technology, year) %>%
  summarise(generation_TWh = round(sum(value) * CONV_elec_EJ_TWh, digits = REPORTING_DIGITS_TWH)) %>%
  ungroup()

write_csv(elec_gen_tech_state, "outputs/elec_gen_tech_state.csv")

elec_gen_tech_nation <- elec_gen_tech_state %>%
  group_by(scenario, technology, year) %>%
  summarise(generation_TWh = sum(generation_TWh)) %>%
  ungroup()

write_csv(elec_gen_tech_nation, "outputs/elec_gen_tech_nation.csv")

# Gas prices
gas_prices <- getQuery(gcam_output.proj, "prices by sector") %>%
  filter(sector %in% c("regional natural gas", "wholesale gas", "delivered gas"),
         year %in% ANALYSIS_YEARS) %>%
  mutate(price_USDperMMBTU = value / CONV_NG_EJ_QBTU * gdp_deflator_1975_2021) %>%
  left_join(price_mapping, by = "sector") %>%
  select(scenario, region, rate_class, year, price_USDperMMBTU)

gas_prices_state <- inner_join(gas_prices, state_mapping, by = c(region = "grid.region"), relationship = "many-to-many") %>%
  mutate(region = state.code) %>%
  filter(!region %in% EXCLUDED_STATES) %>%
  arrange(scenario, region, rate_class, year) %>%
  select(scenario, region, rate_class, year, price_USDperMMBTU)
gas_prices_nation <- filter(gas_prices, region == "USA", rate_class == "Henry Hub")

write_csv(gas_prices_state, "outputs/gas_prices_state.csv")
write_csv(gas_prices_nation, "outputs/gas_prices_nation.csv")

# Compare Henry Hub prices
AEO_tab13 <- read_csv("data/AEO_Table13.csv") %>%
  gather_years()
AEO_henryhub <- filter(AEO_tab13, `full name` == "Natural Gas: Henry Hub Spot Price: Reference case") %>%
  mutate(scenario = "AEO 2025 Reference") %>%
  select(scenario, year, price_USDperMMBTU = value)
HH_gasprice_comparison <- gas_prices_nation %>%
  filter(scenario == "FutureGrid_Ref") %>%
  select(scenario, year, price_USDperMMBTU) %>%
  bind_rows(AEO_henryhub)
ggplot(HH_gasprice_comparison, aes(x = year, y = price_USDperMMBTU, color = scenario)) +
  geom_line() +
  xlab("") +
  ylab("2024$ per mmbtu") +
  theme_bw() +
  ylim(c(0,NA))
ggsave("figures/gas_prices_HenryHub.png",
       width = 5, height = 3.5, units = "in", dpi = 300)

# Gas consumption by reporting category and state
gas_consumption_nonelec <- getQuery(gcam_output.proj, "inputs by tech") %>%
  filter(!region %in% EXCLUDED_STATES,
         !grepl("grid", region),
         sector != "delivered gas",
         input %in% c("delivered gas", "wholesale gas"),
         year %in% ANALYSIS_YEARS) %>%
  left_join(gas_consumption_mapping, by = c("sector", "subsector", "technology")) %>%
  group_by(scenario, region, category, year) %>%
  summarise(consumption_Tbtu = round(sum(value) * CONV_NG_EJ_TBTU, digits = REPORTING_DIGITS_TBTU)) %>%
  ungroup()
  
gas_consumption_elec <- getQuery(gcam_output.proj, "inputs by nested tech") %>%
  filter(grepl("generation", sector),
         !region %in% EXCLUDED_STATES,
         input == "wholesale gas",
         year %in% ANALYSIS_YEARS) %>%
  mutate(category = "Electricity Generation") %>%
  group_by(scenario, region, category, year) %>%
  summarise(consumption_Tbtu = round(sum(value) * CONV_NG_EJ_TBTU, digits = REPORTING_DIGITS_TBTU)) %>%
  ungroup()

gas_consumption_state_category <- bind_rows(gas_consumption_elec, gas_consumption_nonelec)

write_csv(gas_consumption_state_category, "outputs/gas_consumption_state_category.csv")

# Gas production by state
gas_production_state <- getQuery(gcam_output.proj, "outputs by tech") %>%
  filter(sector == "natural gas production",
         year %in% ANALYSIS_YEARS,
         !region %in% c("USA", EXCLUDED_STATES)) %>%
  group_by(scenario, region, year) %>%
  summarise(production_Tbtu = round(sum(value) * CONV_NG_EJ_TBTU, digits = REPORTING_DIGITS_TBTU)) %>%
  ungroup()

write_csv(gas_production_state, "outputs/gas_production_state.csv")

# Gas trade
# Imports from Canada: equal to exports from Canada to N Amer pipeline market
Imports_Pipeline <- getQuery(gcam_output.proj, "outputs by tech") %>%
  filter(subsector %in% c("Canada traded N.Amer pipeline gas", "Mexico traded N.Amer pipeline gas"),
         year %in% ANALYSIS_YEARS) %>%
  mutate(Commodity = "Pipeline gas",
         To.Country = "USA",
         From.Country = substr(subsector, 1, 6),
         Flow_Tbtu = round(value * CONV_NG_EJ_TBTU, digits = REPORTING_DIGITS_TBTU)) %>%
  select(scenario, Commodity, From.Country, To.Country, year, Flow_Tbtu)

Exports_Pipeline <- getQuery(gcam_output.proj, "Canada and Mexico pipeline gas imports") %>%
  mutate(Commodity = "Pipeline gas",
         From.Country = "USA",
         To.Country = region,
         Flow_Tbtu = round(value * CONV_NG_EJ_TBTU, digits = REPORTING_DIGITS_TBTU)) %>%
  select(scenario, Commodity, From.Country, To.Country, year, Flow_Tbtu)

Imports_LNG <- getQuery(gcam_output.proj, "outputs by nested tech") %>%
  filter(technology == "imported LNG",
         year %in% ANALYSIS_YEARS) %>%
  mutate(Commodity = "LNG",
         From.Country = "World",
         To.Country = "USA",
         Flow_Tbtu = round(value * CONV_NG_EJ_TBTU, digits = REPORTING_DIGITS_TBTU)) %>%
  select(scenario, Commodity, From.Country, To.Country, year, Flow_Tbtu)

Exports_LNG <- getQuery(gcam_output.proj, "outputs by tech") %>%
  filter(subsector == "USA traded LNG",
         year %in% ANALYSIS_YEARS) %>%
  mutate(Commodity = "LNG",
         From.Country = "USA",
         To.Country = "World",
         Flow_Tbtu = round(value * CONV_NG_EJ_TBTU, digits = REPORTING_DIGITS_TBTU)) %>%
  select(scenario, Commodity, From.Country, To.Country, year, Flow_Tbtu)

gas_trade_nation <- bind_rows(Imports_Pipeline, Exports_Pipeline, Imports_LNG, Exports_LNG)

write_csv(gas_trade_nation, "outputs/gas_trade_nation.csv")

# LNG exports by state
# read in the data
US_LNG_export_shares <- read.csv("data/US_LNG_exports_by_state.csv") 

# calculate LNG export share by state for 2021 and 2025
shares <- US_LNG_export_shares %>%
  filter(year %in% c(2021, 2025)) %>%
  group_by(year, State) %>%
  summarise(
    state_share = mean(state_share, na.rm = TRUE),
    .groups = "drop"
  )

# add remaining states and assign them a zero share
shares_complete <- shares %>%
  right_join(
    expand_grid(
      State = gcamdata:::gcamusa.STATES,
      year  = c(2021, 2025)
    ),
    by = c("State", "year")
  ) %>%
  mutate(
    state_share = replace_na(state_share, 0)
  )

# read in the LNG export query from GCAM-USA
lng_exports_by_region <- getQuery(gcam_output.proj, "outputs by tech") %>%
  filter(subsector == "USA traded LNG") 

# copy 2021 and 2025 shares to GCAM-USA data
lng_exports_with_shares <- lng_exports_by_region %>%
  # create all combinations of export rows × states
  crossing(State = shares_complete$State) %>%
  # join the state_share from shares_complete
  left_join(
    shares_complete %>%
      select(State, year, state_share),
    by = c("State", "year")
  ) %>%
  # fill missing shares with 0
  mutate(state_share = replace_na(state_share, 0))

# Fill all years > 2025 with the state's 2025 share
lng_exports_with_shares <- lng_exports_with_shares %>%
  group_by(State) %>%
  arrange(State, year) %>%
  mutate(
    state_share = if_else(year > 2025, state_share[year == 2025], state_share)
  ) %>%
  ungroup()


# calculate absolute state values as national values multiplied by state shares:
lng_exports_with_shares <- lng_exports_with_shares %>%
  mutate(
    value = value * state_share
  )
  
# Plot result (only plot states with non-zero values):
plot_df <- lng_exports_with_shares %>%
  group_by(State) %>%
  filter(sum(value, na.rm = TRUE) > 0) %>%
  ungroup()

p <- ggplot(plot_df,
       aes(x = year, y = value, fill = State)) +
  geom_col() +
  labs(
    title = "LNG exports by state",
    x = NULL,
    y = "EJ"
  )
p

# Save plot:
ggsave("figures/lng_exports_by_state.png", plot = p,
       width = 5, height = 3.5, units = "in", dpi = 300)









