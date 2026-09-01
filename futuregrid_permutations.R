###################################################################################################
###################################################################################################

# User settings to be specified by before running script

# If the rgcam package is not installed locally, use:
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
library(readxl)

# read mapping files ----
fuel_LHV_HHV <- read_csv("./data/fuel_LHV_HHV.csv")
elec_tech_mapping <- read_csv("./mappings/elec_tech_mapping.csv")
price_mapping <- read_csv("./mappings/price_mapping.csv")
state_mapping <- read_csv("./mappings/state_mapping.csv")
resource_mapping<-read.csv("./mappings/resource_mapping.csv")
EIAform860_techs <- read.csv("./mappings/EIAform860_techs.csv", na.strings = "")

# define constants ----
ANALYSIS_YEARS <- 2025:2050
CONV_NG_EJ_QBTU <- 1/(1.0551 * .905)    # conversion from EJ LHV of natural gas to quadrillion btu HHV
CONV_NG_EJ_TBTU <- CONV_NG_EJ_QBTU * 1000    # conversion from EJ LHV of natural gas to trillion btu HHV
CONV_GJ_MMBTU <- 1/1.0551
CONV_IO_HEATRATE <- 3.412                    # conversion from input-output coefficient to heat rate (btu per mwh)
CONV_MWH_GJ <- 3.6                    # conversion from megawatt-hours to gigajoules
TWH_PER_EJ <- 1/.0036
EJ_PER_TWH <- 0.0036
GW_PER_TW <- 1000
MW_PER_GW <- 1000
MW_PER_TW <- 1000000
MMBTU_PER_EJ <- 947817120
HOURS_PER_YEAR <- 8766
gdp_deflator_1975_2021 <- 3.964  # conversion from 1975$ in GCAM output to 2021$ for reporting
gdp_deflator_2021_2025<-1.17055 # conversion from 2021$ in reporting to 2025$ for comparison template
EJ_Tcf <- .981
EJ_Mmcf <- 981000                       # conversion from million cubic feet of natural gas to EJ (LHV)
EXCLUDED_STATES <- c("AK", "HI", "DC")        # Only CONUS states are tracked at this point. DC is omitted from EER scenarios
REPORTING_STATES <- state_mapping$state.code[!state_mapping$state.code %in% EXCLUDED_STATES]
REPORTING_DIGITS_TWH <- 3               # number of decimals for energy in TWh
REPORTING_DIGITS_TBTU <- 1               # number of decimals for energy in Tbtu
REPORTING_DIGITS_GW <- 3               # number of decimals for capacity in gigawatts
REPORTING_DIGITS_CF <- 3               # number of decimals for capacity factors
REPORTING_DIGITS_HEATRATE <- 4               # number of decimals for heat rates
REPORTING_DIGITS_OMVAR <- 3               # number of decimal places for variable O&M costs
WECC_states <- c("WA", "ID", "MT", "OR", "WY", "CA", "NV", "UT", "CO", "AZ", "NM", "NE", "SD", "TX") # Based on data shared by Kostas on April 6, 2026

# read rgcam proj file----
query_out_1.proj <- loadProject("rgcam/query_out_1.proj")
query_out_2.proj <- loadProject("rgcam/query_out_2.proj")
query_out_3.proj <- loadProject("rgcam/query_out_3.proj")
query_out_4.proj <- loadProject("rgcam/query_out_4.proj")
query_out_5.proj <- loadProject("rgcam/query_out_5.proj")
query_out_6.proj <- loadProject("rgcam/query_out_6.proj")

# make a combined project file
query_out_all.proj <- c(query_out_1.proj, query_out_2.proj, query_out_3.proj, query_out_4.proj, query_out_5.proj, query_out_6.proj)

###################################################################################################
###################################################################################################

# Key outputs from this script
###################################################################################################

# Granular data for Task 4: output, capacity, capacity factors, fuel prices, and variable O&M costs by state, technology, and vintage
# First compile elec gen by state, technology, and vintage. The output quantities are the weighting factors for all per-unit variables
elec_gen_state_tech_vintage <- getQuery(query_out_all.proj, "elec gen by tech and vintage") %>%
  separate(technology, into = c("technology", "vintage"), sep = ",year=") %>%
  mutate(vintage = as.numeric(vintage)) %>%
  rename(gen_EJ = value) %>%
  select(-Units) %>%
  filter(!region %in% EXCLUDED_STATES,
         year %in% ANALYSIS_YEARS)

# Capacity factors, IOcoefficients, and OMvar assumptions are assigned to each cohort (vintage) and remain constant for the lifetime
# For joining with generation by tech and vintage, the reported "year" should be renamed to "vintage"
elec_capfac_state_tech <- getQuery(query_out_all.proj, "elec capacity factors") %>%
  rename(vintage = year,
         CF = value) %>%
  select(-Units)

elec_IOcoef_state_tech <- getQuery(query_out_all.proj, "electricity IOcoefs by tech") %>%
  rename(vintage = year,
         IOcoef = value,
         energy_input = input) %>%
  filter(!grepl("water|credits", energy_input)) %>%
  select(-Units)

elec_OMvar_state_tech <- getQuery(query_out_all.proj, "electricity OMvar costs") %>%
  rename(vintage = year) %>%
  mutate(OMvar_USDperMWh = value * gdp_deflator_1975_2021) %>%
  select(-Units, -input, -value)

# Fuel prices can be queried for selected fuels at the state level, grid region level, or USA level
fuelprices <- getQuery(query_out_all.proj, "prices by sector") %>%
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

write_csv(elec_allvars_state_tech_vintage, "outputs_permutations/elec_allvars_state_tech_vintage.csv")

# Intercomparison template
#resource tab

resource_comp<-elec_allvars_state_tech_vintage %>%
  left_join_error_no_match(resource_mapping, by = join_by(technology))

resource_comp_gen_cap<- resource_comp %>%
  group_by(scenario, year, region, resource_type) %>%
  summarize(generation = sum(gen_TWh),
            installed_capacity_total = sum(capacity_GW), .groups = "drop") %>%
  mutate(generation = generation * MW_PER_TW,
         installed_capacity_total = installed_capacity_total * MW_PER_GW)

resource_comp_new_cap<-resource_comp %>%
  filter(vintage==year) %>%
  group_by(scenario, year, region, resource_type) %>%
  summarize(installed_capacity_new=sum(capacity_GW), .groups = "drop") %>%
  mutate(installed_capacity_new= installed_capacity_new*MW_PER_GW/5)

resource_comp_retirements<-resource_comp %>%
  group_by(scenario, year, vintage, region, technology) %>%
  summarize(capacity_GW=sum(capacity_GW), .groups = "drop") %>%
  arrange(scenario, region, technology, vintage, year) %>%
  group_by(scenario, region, technology, vintage) %>%
  mutate(capacity_prev = lag(capacity_GW, n = 1, order_by = year),# Previous period capacity within same vintage/region/resource group
         retired_capacity = pmax(0, capacity_prev - capacity_GW, na.rm = FALSE))  %>%
  ungroup() %>%
  left_join(resource_mapping, by = join_by(technology)) %>%
  group_by(scenario, year, region, resource_type) %>%
  summarize(retired_capacity=sum(retired_capacity, na.rm = TRUE), .groups = "drop") %>%
  mutate(retired_capacity= retired_capacity*MW_PER_GW/5)

resource_fuel_input <- getQuery(query_out_all.proj, "inputs by nested tech") %>%
  bind_rows(getQuery(query_out_all.proj, "inputs by tech")) %>%
  filter(grepl("generation", sector) | technology == "rooftop_pv" | (grepl("cogen", technology)),
         !grepl("water",input),
         !grepl("oil-credits",input),
         technology != "hydrogen cogen",
         year %in% ANALYSIS_YEARS,
         region %in% REPORTING_STATES) %>%
  mutate(fuel_consumption=value*MMBTU_PER_EJ,
         subsector...5 = ifelse(is.na(subsector...5), subsector, subsector...5))%>%
  left_join(resource_mapping, by = c("subsector...5"="technology")) %>%
  group_by(scenario, year, region, resource_type) %>%
  summarize(fuel_consumption=sum(fuel_consumption), .groups = "drop")

  resource_tab<-left_join(resource_comp_gen_cap, resource_comp_new_cap, by = join_by(scenario, year, region, resource_type)) %>%
    left_join(resource_comp_retirements, by = join_by(scenario, year, region, resource_type)) %>%
    left_join(resource_fuel_input, by = join_by(scenario, year, region, resource_type)) %>%
    mutate(model="GCAM",
           resource_id = resource_type) %>%
    select(model,scenario,year,region,resource_id, resource_type, installed_capacity_total, installed_capacity_new,
           retired_capacity, generation, fuel_consumption) %>%
  rename(zone_name=region,
         scenario=scenario)

# 5/11/2026 GPK - add in GW of capacity, based on each state's TWh of VRE (wind + solar)
  # The factor of 0.03 is consistent with AEO2026, but is somewhat higher than A-Leaf (0.015) or ReEDS (0.041)
  # Assume 5-year lifetime of storage for simplicity
  VRE_TWh_to_Storage_GW <- 0.041
  storage_tab <- filter(resource_tab, resource_type %in% c("Solar", "Wind")) %>%
    group_by(model, scenario, year, zone_name) %>%
    summarise(VRE_generation = sum(generation), .groups = "drop") %>%
    mutate(resource_id = "Storage",
           resource_type = "Storage",
           installed_capacity_total = VRE_generation * VRE_TWh_to_Storage_GW / MW_PER_GW,
           installed_capacity_new = installed_capacity_total / 5,
           generation = 0,
           fuel_consumption = 0) %>%
    group_by(model, scenario) %>%
    mutate(retired_capacity = lag(installed_capacity_new, n = 1, order_by = year)) %>%
    ungroup() %>%
    replace_na(list(retired_capacity = 0)) %>%
    select(names(resource_tab))

  resource_tab <- bind_rows(resource_tab, storage_tab) %>%
    arrange(model, scenario, year, zone_name, resource_id)

write_csv(resource_tab, "outputs_permutations/gcam_generation.csv")

# Gas consumption by reporting category and state
gas_consumption_nonelec <- getQuery(query_out_all.proj, "inputs by tech") %>%
  filter(!region %in% EXCLUDED_STATES,
         !grepl("grid", region),
         sector != "delivered gas",
         input %in% c("delivered gas", "wholesale gas"),
         year %in% ANALYSIS_YEARS) %>%
  group_by(scenario, region, sector, year) %>%
  summarise(consumption_Tbtu = round(sum(value) * CONV_NG_EJ_TBTU, digits = REPORTING_DIGITS_TBTU)) %>%
  ungroup()

gas_consumption_elec <- getQuery(query_out_all.proj, "inputs by nested tech") %>%
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

gas_prices_tab <- fuelprices %>%
  filter(energy_input=="wholesale gas") %>%
  mutate(model="GCAM",
         fuelprice_USDperMMbtu*gdp_deflator_2021_2025) %>%
  select(model,scenario,year,region,fuelprice_USDperMMbtu) %>%
  rename(zone_name=region,
         gas_price=fuelprice_USDperMMbtu)

gas_cons_tab <- gas_consumption_sector_state %>%
  gather_years() %>%
  group_by(scenario, region, year) %>%
  summarise(gas_consumption = sum(value) * 1e6) %>%
  ungroup() %>%
  mutate(model = "GCAM") %>%
  select(model, scenario, year, zone_name = region, gas_consumption)

gas_tab <- gas_prices_tab %>%
  left_join_error_no_match(gas_cons_tab, by = c("model", "scenario", "year", "zone_name"))

write_csv(gas_tab,"outputs_permutations/gcam_gas.csv")

# System cost calculation
# "System costs = net present value (2% discount rate) of all capital and operating expenditures from 2025 to 2050 for
# existing and new generation, storage, and transmission. Real 2024 dollars are used."
# Capital costs older vintages are estimated post-hoc and deducted
# First compile the levelized technology capital costs, in the model unit of 1975$/GJ
capital_cost_2015 <- getQuery(query_out_all.proj, "elec gen non-energy costs") %>%
  filter(input == "capital",
         year %in% 2015:2021) %>%
  select(scenario, region, subsector = subsector...6, technology, vintage = year, capcost_75USDGJ = value)

# For hydropower, use the 2019 ATB, levelized with the 38% capacity factor above
# The different values are from the different cost bins ("NPD")
# This approach, using the minimum, assumes that the historical vintages are in the least-cost bin
NREL_ATB_hydro_costs_USDperKW <- min(c(6231, 5672, 4173, 3935, 7306, 6522, 6388, 5751))
default_FCR <- 0.1
NREL_ATB_capcost_75USDGJ <- NREL_ATB_hydro_costs_USDperKW * default_FCR / gdp_deflator_1975_2021 / (HOURS_PER_YEAR * 0.38 * EJ_PER_TWH)

capital_cost_2015_hydro <- distinct(capital_cost_2015, scenario, region, vintage) %>%
  mutate(subsector = "hydro_base,depth=1",
         technology = "hydro_base",
         capcost_75USDGJ = NREL_ATB_capcost_75USDGJ)

capital_cost_2015 <- bind_rows(capital_cost_2015, capital_cost_2015_hydro)

# Deducting all levelized capital costs of power plants operating in 2021 would overestimate what should be deducted,
# as a portion of these power plants were built in recent years and still have capital payments.
# This next portion uses data from the 2024 EIA form 860 to estimate the portion of the stock of each power plant type
# that should be deducted
# The cutoff year is the first operational year after which we assume capital payments are still being made in 2025
CUTOFF_YEAR <- 1995
OldCapacityFractions <- readxl::read_xlsx(path = "data/EIA_form860_2024/3_1_Generator_Y2024.xlsx", sheet = "Operable", skip = 1) %>%
  select(EIA_technology = Technology, Capacity = `Nameplate Capacity (MW)`, StartYear = `Operating Year`) %>%
  left_join(EIAform860_techs, by = "EIA_technology") %>%
  drop_na(resource_type) %>%
  mutate(before_cutoff = if_else(StartYear < CUTOFF_YEAR, 1, 0),
         OldCapacity = Capacity * before_cutoff) %>%
  group_by(resource_type) %>%
  summarise(Capacity = sum(Capacity),
            OldCapacity = sum(OldCapacity)) %>%
  ungroup() %>%
  mutate(OldCapacityFraction = OldCapacity / Capacity) %>%
  select(resource_type, OldCapacityFraction)

# Compile the output from the 2015 vintage in the future years
# Rename the technologies to correct for the cooling system based technology naming conventions
# Join in the 2015 levelized capital costs, multiply through, and aggregate by scenario and year
capital_cost_deduction <- getQuery(query_out_all.proj, "elec gen by tech and vintage") %>%
  separate(technology, into = c("technology", "vintage"), sep = ",year=", convert = TRUE) %>%
  filter(vintage %in% 2015:2021,
         year %in% 2025:2050) %>%
  select(scenario, region, subsector = subsector...6, technology, vintage, year, output_EJ = value) %>%
  inner_join(capital_cost_2015, by = c("scenario", "region", "subsector", "technology", "vintage")) %>%
  left_join(elec_tech_mapping, by = "technology") %>%
  left_join(resource_mapping, by = c(reporting_technology = "technology")) %>%
  left_join(OldCapacityFractions, by = "resource_type") %>%
  replace_na(list(OldCapacityFraction = 0)) %>%
  mutate(deduction_bil21USD = output_EJ * capcost_75USDGJ * gdp_deflator_1975_2021 * (1 - OldCapacityFraction)) %>%
  group_by(scenario, year) %>%
  summarise(deduction_bil21USD = sum(deduction_bil21USD)) %>%
  ungroup()

# This method uses the price x quantity of electricity generation minus the "existing" capital costs
electricity_system_cost <- getQuery(query_out_all.proj, "outputs by nested tech") %>%
  filter(grepl("generation|elect_td_bld", sector),
         year %in% 2025:2050) %>%
  left_join(getQuery(query_out_all.proj, "prices by sector"),
            by = c("scenario", "region", "sector", "year"),
            suffix = c(".quantity", ".price")) %>%
  mutate(cost_bil21USD = value.quantity * value.price * gdp_deflator_1975_2021) %>%
  group_by(scenario, year) %>%
  summarise(cost_bil21USD = sum(cost_bil21USD)) %>%
  ungroup() %>%
  left_join(capital_cost_deduction, by = c("scenario", "year")) %>%
  mutate(cost_bil21USD = cost_bil21USD - deduction_bil21USD) %>%
  select(-deduction_bil21USD) %>%
  complete(nesting(scenario), year = 2025:2050) %>%
  group_by(scenario) %>%
  mutate(cost_bil21USD = approx_fun(year, cost_bil21USD)) %>%
  ungroup() %>%
  mutate(discount_factor = 1 / 1.02 ^ (year - 2025),
         discounted_cost_bil21USD = cost_bil21USD * discount_factor) %>%
  group_by(scenario) %>%
  summarise(discounted_cost_bil21USD = sum(discounted_cost_bil21USD)) %>%
  ungroup()

write_csv(electricity_system_cost, "outputs_permutations/electricity_system_cost.csv")
