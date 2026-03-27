
# If the rgcam package is not installed locally, use:
#  devtools::install_github("JGCRI/rgcam")
library(rgcam)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(patchwork)
library(RColorBrewer)
library(devtools)
library(gcamdata)
library(stringr)

# Set working directory to this file's location (fails on Mac)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Output filenames will be stamped with the commit hashes of both the model and the post-process reporting script.
# The model commit hash needs to be specified manually here, whereas the post-processing commit hash is available here
GCAM_COMMIT_HASH <- "27792b"
THIS_COMMIT_HASH <- substr(system("git rev-parse HEAD", intern = TRUE), 1, 6)

# Make a function to write out data files, appending commit hashes to the filename
write_stamp_csv <- function(data_table, base_filename){
  full_filename <- paste0("outputs/", base_filename, "_", GCAM_COMMIT_HASH, "_", THIS_COMMIT_HASH, ".csv")
  write_csv(data_table, full_filename)
}

# read mapping files ----
fuel_LHV_HHV <- read_csv("./data/fuel_LHV_HHV.csv")
elec_tech_mapping <- read_csv("./mappings/elec_tech_mapping.csv")
price_mapping <- read_csv("./mappings/price_mapping.csv")
state_mapping <- read_csv("./mappings/state_mapping.csv")

# define constants ----
ANALYSIS_YEARS <- 2021:2050
CONV_NG_EJ_QBTU <- 1/(1.0551 * .905)    # conversion from EJ LHV of natural gas to quadrillion btu HHV
CONV_NG_EJ_TBTU <- CONV_NG_EJ_QBTU * 1000    # conversion from EJ LHV of natural gas to trillion btu HHV
CONV_GJ_MMBTU <- 1/1.0551
CONV_IO_HEATRATE <- 3.412                    # conversion from input-output coefficient to heat rate (btu per mwh)
CONV_MWH_GJ <- 3.6                    # conversion from megawatt-hours to gigajoules
TWH_PER_EJ <- 1/.0036
EJ_PER_TWH <- 0.0036
GW_PER_TW <- 1000
HOURS_PER_YEAR <- 8766
gdp_deflator_1975_2021 <- 3.964         # conversion from 1975$ in GCAM output to 2021$ for reporting
EJ_Tcf <- .981
EJ_Mmcf <- 981000                       # conversion from million cubic feet of natural gas to EJ (LHV)
EXCLUDED_STATES <- c("AK", "HI")        # Only CONUS states are tracked at this point
REPORTING_STATES <- state_mapping$state.code[!state_mapping$state.code %in% EXCLUDED_STATES]
REPORTING_DIGITS_TWH <- 3               # number of decimals for energy in TWh
REPORTING_DIGITS_TBTU <- 1               # number of decimals for energy in Tbtu
REPORTING_DIGITS_GW <- 3               # number of decimals for capacity in gigawatts
REPORTING_DIGITS_CF <- 3               # number of decimals for capacity factors
REPORTING_DIGITS_HEATRATE <- 4               # number of decimals for heat rates
REPORTING_DIGITS_OMVAR <- 3               # number of decimal places for variable O&M costs

# read rgcam proj file----
gcam_output.proj <- loadProject("rgcam/gcam_output.proj")

REQUERY_DATA <- FALSE
if(REQUERY_DATA){
  scenarios <- c("FutureGrid_Ref", "FutureGrid_LimitGas", "FutureGrid_HighGas")

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
  summarise(consumption_TWh = round(sum(value) * TWH_PER_EJ, digits = REPORTING_DIGITS_TWH)) %>%
  ungroup()

write_stamp_csv(elec_consumption_state, "elec_consumption_state")

elec_consumption_nation <- elec_consumption_state %>%
  group_by(scenario, year) %>%
  summarise(consumption_TWh = sum(consumption_TWh)) %>%
  ungroup()

write_stamp_csv(elec_consumption_nation, "elec_consumption_nation")

# Electricity consumption comparison with EER Reference scenario
EER_load_by_scenario <- read_csv("data/EER_load_by_scenario.csv")

ref_elec_EER <- filter(EER_load_by_scenario, scenario == "reference") %>%
  mutate(elec_EER = load_MWh / 1e6) %>%
  select(region = state, year, elec_EER)

ref_elec_GCAM <- getQuery(gcam_output.proj, "outputs by tech") %>%
  filter(grepl("elect_td", sector),
         year %in% ANALYSIS_YEARS,
         scenario == "FutureGrid_Ref") %>%
  group_by(region, year) %>%
  summarise(elec_GCAM = sum(value) * TWH_PER_EJ) %>%
  ungroup()

ref_elec_comparison <- left_join(ref_elec_EER, ref_elec_GCAM, by = c("region", "year")) %>%
  group_by(region) %>%
  mutate(EER_growth = elec_EER - elec_EER[year == 2021],
         GCAM_growth = elec_GCAM - elec_GCAM[year == 2021],
         target_increase = EER_growth - GCAM_growth)

ref_elec_comparison_nation <- ref_elec_comparison %>%
  group_by(year) %>%
  summarise(elec_EER = sum(elec_EER),
            elec_GCAM = sum(elec_GCAM),
            target_increase = sum(target_increase)) %>%
  ungroup()
  
ref_elec_comparison_for_plot <- filter(ref_elec_comparison, year > 2021) %>%
  mutate(highlight = if_else(abs(target_increase) < 25, FALSE, TRUE))
  
ggplot(ref_elec_comparison_for_plot, aes(x = elec_EER, y = elec_GCAM, color = highlight)) +
  geom_point() +
  facet_wrap(~year) +
  geom_abline(slope = 1, intercept = 0) +
  xlab("EER TWh") +
  ylab("GCAM TWh") +
  geom_text(data = filter(ref_elec_comparison_for_plot, abs(target_increase) > 25),
            aes(label = region),
            vjust = -0.5) +
  scale_color_manual(values = c("black", "red")) +
  theme(legend.position = "none")

ggsave("figures/EER_GCAM_ref_comparison.png", width = 5, height = 5, units = "in")

# Data center electricity consumption comparison with EER Reference scenario
EER_data_center_projections_annual <- read_csv("data/EER_data_center_projections_annual.csv")

ref_data_center_EER <- EER_data_center_projections_annual %>%
  mutate(state.name = str_to_title(state),
         state.name = sub(" Of ", " of ", state.name),
         dc_TWh_EER = load_MWh / 1e6) %>%
  left_join(state_mapping, by = "state.name") %>%
  filter(!state.code %in% EXCLUDED_STATES) %>%
  select(region = state.code, year, dc_TWh_EER)

ref_data_center_GCAM <- getQuery(gcam_output.proj, "inputs by tech") %>%
  filter(grepl("data center", sector),
         year %in% ANALYSIS_YEARS,
         scenario == "FutureGrid_Ref") %>%
  group_by(region, year) %>%
  summarise(dc_TWh_GCAM = sum(value) * TWH_PER_EJ) %>%
  ungroup()

ref_data_center_comparison <- left_join(ref_data_center_EER, ref_data_center_GCAM,
                                        by = c("region", "year")) %>%
  replace_na(list(dc_TWh_GCAM = 0)) %>%
  group_by(region) %>%
  mutate(diff = dc_TWh_EER - dc_TWh_GCAM)

ref_data_center_comparison_nation <- ref_data_center_comparison %>%
  group_by(year) %>%
  summarise(dc_TWh_EER = sum(dc_TWh_EER),
            dc_TWh_GCAM = sum(dc_TWh_GCAM),
            diff = sum(diff)) %>%
  ungroup()

ref_data_center_comparison_for_plot <- filter(ref_data_center_comparison, year > 2021) %>%
  mutate(highlight = if_else(abs(diff) < 15, FALSE, TRUE))

ggplot(ref_data_center_comparison_for_plot, aes(x = dc_TWh_EER, y = dc_TWh_GCAM, color = highlight)) +
  geom_point() +
  facet_wrap(~year) +
  geom_abline(slope = 1, intercept = 0) +
  xlab("EER TWh") +
  ylab("GCAM TWh") +
  geom_text(data = filter(ref_data_center_comparison_for_plot, abs(diff) > 15),
            aes(label = region),
            vjust = -0.5) +
  scale_color_manual(values = c("black", "red")) +
  theme(legend.position = "none")

ggsave("figures/EER_GCAM_ref_data_center_comparison.png", width = 5, height = 5, units = "in")


# Electricity generation
elec_gen_fuel_state <- getQuery(gcam_output.proj, "outputs by nested tech") %>%
  filter(grepl("generation", sector) | technology == "rooftop_pv",
         year %in% ANALYSIS_YEARS,
         !region %in% EXCLUDED_STATES) %>%
  mutate(technology = str_to_title(subsector...5)) %>%
  group_by(scenario, region, technology, year) %>%
  summarise(generation_TWh = round(sum(value) * TWH_PER_EJ, digits = REPORTING_DIGITS_TWH)) %>%
  ungroup()

write_stamp_csv(elec_gen_fuel_state, "elec_gen_fuel_state")

elec_gen_fuel_nation <- elec_gen_fuel_state %>%
  group_by(scenario, technology, year) %>%
  summarise(generation_TWh = sum(generation_TWh)) %>%
  ungroup()

write_stamp_csv(elec_gen_fuel_nation, "elec_gen_fuel_nation")

# Granular data for Task 4: output, capacity, capacity factors, fuel prices, and variable O&M costs by state, technology, and vintage
# First compile elec gen by state, technology, and vintage. The output quantities are the weighting factors for all per-unit variables
elec_gen_state_tech_vintage <- getQuery(gcam_output.proj, "elec gen by tech and vintage") %>%
  separate(technology, into = c("technology", "vintage"), sep = ",year=") %>%
  mutate(vintage = as.numeric(vintage)) %>%
  rename(gen_EJ = value) %>%
  select(-Units) %>%
  filter(!region %in% EXCLUDED_STATES,
         year %in% ANALYSIS_YEARS)

# Capacity factors, IOcoefficients, and OMvar assumptions are assigned to each cohort (vintage) and remain constant for the lifetime
# For joining with generation by tech and vintage, the reported "year" should be renamed to "vintage"
elec_capfac_state_tech <- getQuery(gcam_output.proj, "elec capacity factors") %>%
  rename(vintage = year,
         CF = value) %>%
  select(-Units)

elec_IOcoef_state_tech <- getQuery(gcam_output.proj, "electricity IOcoefs by tech") %>%
  rename(vintage = year,
         IOcoef = value,
         energy_input = input) %>%
  filter(!grepl("water|credits", energy_input)) %>%
  select(-Units)

elec_OMvar_state_tech <- getQuery(gcam_output.proj, "electricity OMvar costs") %>%
  rename(vintage = year) %>%
  mutate(OMvar_USDperMWh = value * gdp_deflator_1975_2021) %>%
  select(-Units, -input, -value)

# Fuel prices can be queried for selected fuels at the state level, grid region level, or USA level
fuelprices <- getQuery(gcam_output.proj, "prices by sector") %>%
  filter(sector %in% elec_IOcoef_state_tech$energy_input,
         year %in% ANALYSIS_YEARS) %>%
  mutate(fuelprice_USDperGJ = value * gdp_deflator_1975_2021) %>%
  select(scenario, region, energy_input = sector, year, fuelprice_USDperGJ)

fuelprices_fromstate <- filter(fuelprices, region %in% REPORTING_STATES)

fuelprices_fromgridregion <- filter(fuelprices, grepl("grid", region),
                                  !energy_input %in% fuelprices_fromstate$energy_input) %>%
  left_join(select(state_mapping, state.code, grid.region), by = c("region" = "grid.region"), relationship = "many-to-many") %>%
  mutate(region = state.code) %>%
  select(-state.code) %>%
  filter(!region %in% EXCLUDED_STATES)

fuelprices_fromusa <- filter(fuelprices, region == "USA",
                             !energy_input %in% c(fuelprices_fromstate$energy_input, fuelprices_fromgridregion$energy_input)) %>%
  select(-region) %>%
  repeat_add_columns(tibble(region = REPORTING_STATES))

fuelprices <- bind_rows(fuelprices_fromstate, fuelprices_fromgridregion, fuelprices_fromusa) %>%
  left_join(fuel_LHV_HHV, by = "energy_input") %>%
  replace_na(list(LHV_HHV = 1)) %>%
  mutate(fuelprice_USDperMMbtu = fuelprice_USDperGJ * LHV_HHV / CONV_GJ_MMBTU) %>%
  select(scenario, region, energy_input, year, fuelprice_USDperMMbtu)
  

# join in capacity factors, IOcoefficients (and heat rates), fuel prices (and fuel costs), OMvar
# Manually fill in capacity factors for hydro and geothermal (not sure why geothermal doesn't appear in the capacity factor query)
elec_allvars_state_tech_vintage <- elec_gen_state_tech_vintage %>%
  left_join(elec_capfac_state_tech, by = c("scenario", "region", "sector", "subsector...5", "subsector...6", "technology", "vintage")) %>%
  left_join(elec_IOcoef_state_tech, by = c("scenario", "region", "sector", "subsector...5", "subsector...6", "technology", "vintage")) %>%
  left_join(elec_OMvar_state_tech, by = c("scenario", "region", "sector", "subsector...5", "subsector...6", "technology", "vintage")) %>%
  replace_na(list(IOcoef = 1, OMvar_USDperMWh = 0)) %>%
  mutate(CF = if_else(subsector...5 == "hydro", 0.44, CF),
         CF = if_else(subsector...5 == "geothermal", 0.9, CF),
         energy_input = if_else(is.na(energy_input), subsector...5, energy_input),
         capacity_GW = gen_EJ * GW_PER_TW / (HOURS_PER_YEAR * EJ_PER_TWH * CF),
         energy_input_EJ = gen_EJ * IOcoef,
         OMvar_weighted = gen_EJ * OMvar_USDperMWh) %>%   # not bothering to convert to meaningful units (e.g. billion $) as it's just a weight
  left_join(elec_tech_mapping, by = "technology") %>%
  group_by(scenario, region, reporting_technology, energy_input, vintage, year) %>%
  summarise(gen_EJ = sum(gen_EJ),
            capacity_GW = sum(capacity_GW),
            energy_input_EJ = sum(energy_input_EJ),
            OMvar_weighted = sum(OMvar_weighted)) %>%
  ungroup() %>%
  left_join(fuelprices, by = c("scenario", "region", "energy_input", "year")) %>%
  left_join(fuel_LHV_HHV, by = "energy_input") %>%
  replace_na(list(fuelprice_USDperMMbtu = 0, LHV_HHV = 1)) %>%
  mutate(CF = round(gen_EJ * GW_PER_TW / (HOURS_PER_YEAR * EJ_PER_TWH * capacity_GW), digits = REPORTING_DIGITS_CF),
         capacity_GW = round(capacity_GW, REPORTING_DIGITS_GW),
         gen_TWh = round(gen_EJ * TWH_PER_EJ, digits = REPORTING_DIGITS_TWH),
         IOcoef = energy_input_EJ / gen_EJ,
         heatrate_BTUperMWh = round(IOcoef * CONV_IO_HEATRATE / LHV_HHV, digits = REPORTING_DIGITS_HEATRATE),
         OMvar_USDperMWh = round(OMvar_weighted / gen_EJ, digits = REPORTING_DIGITS_OMVAR)) %>%
  select(scenario, region, technology = reporting_technology, energy_input, vintage, year, gen_TWh, capacity_GW, CF, heatrate_BTUperMWh, fuelprice_USDperMMbtu, OMvar_USDperMWh)

write_stamp_csv(elec_allvars_state_tech_vintage, "elec_allvars_state_tech_vintage")

# Write out the data center electricity consumption
elec_consumption_datacenters_state <- getQuery(gcam_output.proj, "inputs by tech") %>%
  filter(region %in% REPORTING_STATES,
         sector == "data center",
         year %in% ANALYSIS_YEARS) %>%
  mutate(consumption_TWh = value * TWH_PER_EJ) %>%
  select(scenario, region, sector, year, consumption_TWh)

write_stamp_csv(elec_consumption_datacenters_state, "elec_consumption_datacenters_state")

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
gas_prices_nation <- filter(gas_prices, region == "USA", rate_class %in% c( "Henry Hub",
                                                                            "Residential and Commercial",
                                                                            "Power and Industry" ) )

write_stamp_csv(gas_prices_state, "gas_prices_state")
write_stamp_csv(gas_prices_nation, "gas_prices_nation")

# Compare Henry Hub prices
AEO_henryhub <- read_csv("data/AEO_Table13.csv") %>%
  bind_rows(read_csv("data/AEO_Table13_HighOGS.csv"),
            read_csv("data/AEO_Table13_LowOGS.csv")) %>%
  filter(grepl("Henry Hub Spot Price", `full name`))  %>%
  separate(`full name`, into = c("fuel", "variable", "scenario"), sep = ": ") %>%
  mutate(scenario = if_else(scenario == "Reference case", "Reference",
                            if_else(scenario == "High Oil and Gas Supply", "High Gas Supply", "Low Gas Supply"))) %>%
  gather_years() %>%
  mutate(model = "AEO2025") %>%
  select(model, scenario, year, price_USDperMMBTU = value)
HH_gasprice_comparison <- gas_prices_nation %>%
  mutate(model = "GCAM-USA",
         scenario = if_else(scenario == "FutureGrid_Ref", "Reference",
                            if_else(scenario == "FutureGrid_HighGas", "High Gas Supply", "Low Gas Supply"))) %>%
  filter(rate_class == "Henry Hub") %>%
  select(model, scenario, year, price_USDperMMBTU) %>%
  bind_rows(AEO_henryhub)
ggplot(HH_gasprice_comparison, aes(x = year, y = price_USDperMMBTU, color = scenario, linetype = model)) +
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
  group_by(scenario, region, sector, year) %>%
  summarise(consumption_Tbtu = round(sum(value) * CONV_NG_EJ_TBTU, digits = REPORTING_DIGITS_TBTU)) %>%
  ungroup()
  
gas_consumption_elec <- getQuery(gcam_output.proj, "inputs by nested tech") %>%
  filter(grepl("generation", sector),
         !region %in% EXCLUDED_STATES,
         input == "wholesale gas",
         year %in% ANALYSIS_YEARS) %>%
  group_by(scenario, region, sector, year) %>%
  summarise(consumption_Tbtu = round(sum(value) * CONV_NG_EJ_TBTU, digits = REPORTING_DIGITS_TBTU)) %>%
  ungroup()

# This file is written out in wide format, with years as columns and the units defined in a specific column
gas_consumption_sector_state <- bind_rows(gas_consumption_elec, gas_consumption_nonelec) %>%
  mutate(unit = "trillion btu") %>%
  spread(key = year, value = consumption_Tbtu, fill = 0)

write_stamp_csv(gas_consumption_sector_state, "gas_consumption_sector_state")

# Gas production by state
gas_production_state <- getQuery(gcam_output.proj, "outputs by tech") %>%
  filter(sector == "natural gas production",
         year %in% ANALYSIS_YEARS,
         !region %in% c("USA", EXCLUDED_STATES)) %>%
  group_by(scenario, region, year) %>%
  summarise(production_Tbtu = round(sum(value) * CONV_NG_EJ_TBTU, digits = REPORTING_DIGITS_TBTU)) %>%
  ungroup()

write_stamp_csv(gas_production_state, "gas_production_state")

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

write_stamp_csv(gas_trade_nation, "gas_trade_nation")

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
  group_by(scenario, State) %>%
  arrange(scenario, State, year) %>%
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
  group_by(scenario, State) %>%
  filter(sum(value, na.rm = TRUE) > 0) %>%
  ungroup()

p <- ggplot(plot_df,
       aes(x = year, y = value, fill = State)) +
  facet_wrap(~scenario) +
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









