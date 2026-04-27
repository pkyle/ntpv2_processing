###################################################################################################
###################################################################################################

# User settings to be specified by before running script

install_rgcam <- FALSE # TRUE if you need to install rgcam, FALSE if not

gcamdata_installed <- TRUE # TRUE if gcamdata R package is installed, FALSE if not

if (Sys.info()[["sysname"]] == "Darwin") {
  using_MAC <- TRUE
} else if (Sys.info()[["sysname"]] == "Windows") {
  using_MAC <- FALSE
}

gcam_database_directory <- "/Users/d3p747/Desktop/stash/decarb/output"

REQUERY_DATA <- FALSE # TRUE if you need to requery gcam results, FALSE if not

GCAM_COMMIT_HASH <- "cee704f" # Output filenames will be stamped with the commit hashes of both the model and the post-process reporting script..
                              # The model commit hash needs to be specified manually here

# Pick AEO version to compare
AEO_VERSION <- "2025"   # Options: "2025" or "2026"

if (!AEO_VERSION %in% c("2025", "2026")) {
  stop('AEO_VERSION must be "2025" or "2026"')
}

AEO_folder   <- file.path("data", paste0("AEO_", AEO_VERSION))
AEO_scenario <- paste0("AEO (", AEO_VERSION, ") Ref")


###################################################################################################
###################################################################################################

# If the rgcam package is not installed locally, use:
if( install_rgcam == TRUE ){
  devtools::install_github("JGCRI/rgcam")
}
library(rgcam)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(patchwork)
library(RColorBrewer)
library(devtools)
if( gcamdata_installed == FALSE ){
    devtools::load_all( ) # Needed for users who don't install gcamdata, so that "library(gcamdata)" doesn't error
}
library(gcamdata)
library(stringr)

# Set working directory to this file's location (fails on Mac)
if( using_MAC == FALSE ){
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Output filenames will be stamped with the commit hashes of both the model and the post-process reporting script.
# The model commit hash needs to be specified manually here, whereas the post-processing commit hash is available here
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
ANALYSIS_YEARS <- 2025:2050
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
gcam_output.proj <- loadProject("rgcam/gcam_output.proj")


if(REQUERY_DATA){
  scenarios <- c("GCAM_RefElecDemand", "GCAM_HighElecDemand", "GCAM_LowElecDemand", "GCAM_RefElecDemand_HighGasPrice", "GCAM_RefElecDemand_LowGasPrice")

  for(i in scenarios){
    conn <- localDBConn( gcam_database_directory, "database_basexdb")
    gcam_output.proj <- addScenario(conn, gcam_output.proj, i, "rgcam/BatchQueries_futuregrid.xml", clobber = TRUE)
  }
}

# List available queries:
listQueries(gcam_output.proj, 'GCAM_RefElecDemand')

###################################################################################################
###################################################################################################

# Key outputs from this script
# Electricity consumption (TWh/yr) by scenario and year: state, nation
# Electricity generation by scenario, technology, and year: by state, nation, for WECC region
# Gas prices by scenario, consumption sector, and year: state, nation
# Gas consumption by scenario, state, reporting category, and year
# Gas production by scenario, state, and year
# Gas trade:
#   pipeline imports from Mexico and Canada (by country and to state)
#   pipeline exports to Mexico and Canada (by country and from state)
#   LNG imports: nation
#   LNG exports: state, nation

# Electricity consumption
elec_consumption_state <- getQuery(gcam_output.proj, "inputs by tech") %>%
  filter(grepl("elect_td", input),
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

###################################################

# Electricity consumption comparisons with EER Reference scenario
ref_elec_EER_sector <- read_csv("data/EER_sectoral_projections_annual.csv") %>%
  filter(scenario == "reference") %>%
  mutate(elec_EER = load_MWh / 1e6) %>%
  select(region = state, EER_sector = sector, year, elec_EER)

ref_elec_EER <- ref_elec_EER_sector %>%
  group_by(region, year) %>%
  summarise(elec_EER = sum(elec_EER)) %>%
  ungroup()

ref_elec_GCAM <- getQuery(gcam_output.proj, "inputs by tech") %>%
  filter(grepl("elect_td", input),
         year %in% ANALYSIS_YEARS,
         !region %in% EXCLUDED_STATES,
         scenario == "GCAM_RefElecDemand") %>%
  group_by(region, year) %>%
  summarise(elec_GCAM = sum(value) * TWH_PER_EJ) %>%
  ungroup()

ref_elec_comparison <- left_join(ref_elec_EER, ref_elec_GCAM, by = c("region", "year")) %>%
  group_by(region) %>%
  mutate(target_increase = elec_EER - elec_GCAM)

ref_elec_comparison_nation <- ref_elec_comparison %>%
  group_by(year) %>%
  summarise(elec_EER = sum(elec_EER),
            elec_GCAM = sum(elec_GCAM),
            target_increase = sum(target_increase)) %>%
  ungroup()

write_stamp_csv(ref_elec_comparison_nation, "ref_elec_comparison_nation")

ref_elec_comparison_for_plot <- ref_elec_comparison %>%
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

###################################################

# Comparison at the sectoral level
elec_cons_sector_map <- read_csv("mappings/elec_cons_sector_map.csv")

ref_elec_cons_sector_gcam <- getQuery(gcam_output.proj, "inputs by tech") %>%
  filter(grepl("elect_td", input),
         scenario %in% c("GCAM_RefElecDemand"),
         !region %in% EXCLUDED_STATES,
         year %in% ANALYSIS_YEARS) %>%
  inner_join(elec_cons_sector_map, by = 'sector') %>%
  group_by(scenario, region, EER_sector, year) %>%
  summarise(elec_GCAM = sum(value) * TWH_PER_EJ) %>%
  ungroup()

ref_elec_cons_sector_comparison <- left_join(ref_elec_cons_sector_gcam,
                                             ref_elec_EER_sector,
                                             by = c("region", "EER_sector", "year")) %>%
  mutate(target_increase = elec_EER - elec_GCAM,
         highlight = if_else(abs(target_increase) < 15, FALSE, TRUE))

for(i in unique(ref_elec_cons_sector_comparison$EER_sector)){
  ggplot(filter(ref_elec_cons_sector_comparison, EER_sector == i),
         aes(x = elec_EER, y = elec_GCAM, color = highlight)) +
    geom_point() +
    facet_wrap(~year) +
    geom_abline(slope = 1, intercept = 0) +
    xlab("EER TWh") +
    ylab("GCAM TWh") +
    geom_text(data = filter(ref_elec_cons_sector_comparison, EER_sector == i & abs(target_increase) > 15),
              aes(label = region),
              vjust = -0.5) +
    scale_color_manual(values = c("black", "red")) +
    theme(legend.position = "none") +
    ggtitle(i)
  
  ggsave(paste0("figures/EER_GCAM_ref_", i, "_comparison.png"), width = 5, height = 5, units = "in")
}

ref_elec_cons_sector_comparison_nation <- ref_elec_cons_sector_comparison %>%
  group_by(EER_sector, year) %>%
  summarise(elec_GCAM = sum(elec_GCAM),
            elec_EER = sum(elec_EER),
            target_increase = sum(target_increase)) %>%
  ungroup()

ggplot(ref_elec_cons_sector_comparison_nation,
       aes(x = elec_EER, y = elec_GCAM, color = EER_sector)) +
  geom_point() +
  facet_wrap(~year) +
  geom_abline(slope = 1, intercept = 0) +
  xlab("EER TWh") +
  ylab("GCAM TWh")

ggsave("figures/EER_GCAM_ref_sector_nation_comparison.png", width = 5, height = 5, units = "in")

###################################################################################################

# Electricity generation
elec_gen_fuel_state <- getQuery(gcam_output.proj, "outputs by nested tech") %>%
  bind_rows(getQuery(gcam_output.proj, "outputs by tech")) %>%
  filter(grepl("generation", sector) | technology == "rooftop_pv" | (grepl("cogen", technology) & output == "electricity"),
         technology != "hydrogen cogen",
         year %in% ANALYSIS_YEARS,
         region %in% REPORTING_STATES) %>%
  mutate(subsector = if_else(is.na(subsector), subsector...5, subsector),
         technology = str_to_title(subsector)) %>%
  group_by(scenario, region, technology, year) %>%
  summarise(generation_TWh = round(sum(value) * TWH_PER_EJ, digits = REPORTING_DIGITS_TWH)) %>%
  ungroup()

write_stamp_csv(elec_gen_fuel_state, "elec_gen_fuel_state")

elec_gen_fuel_nation <- elec_gen_fuel_state %>%
  group_by(scenario, technology, year) %>%
  summarise(generation_TWh = sum(generation_TWh)) %>%
  ungroup()

write_stamp_csv(elec_gen_fuel_nation, "elec_gen_fuel_nation")

###################################################################################################

# WECC GridPath generation comparison
WECC_fractions <- read_csv( "data/GridPath_WECC_elec_gen_fraction-26_04_06.csv" ) %>%
  rename( region = state,
          WECC_fraction = "Fraction of State in WECC" )

elec_gen_fuel_WECC <- elec_gen_fuel_state %>%
  filter(scenario == "GCAM_RefElecDemand",
         region %in% WECC_states) %>%
  # Apply fractions from GridPath
  left_join_error_no_match( WECC_fractions, by = c( "region" ) ) %>%
  mutate( generation_TWh_WECC = generation_TWh * WECC_fraction,
          technology = if_else( technology == "Rooftop_pv", "Solar", technology ) ) %>%
  select( -generation_TWh, -WECC_fraction ) %>%
  # Summarize to WECC region
  mutate( region = "WECC" ) %>%
  group_by( scenario, region, technology, year ) %>%
  summarise( generation_TWh_WECC = sum( generation_TWh_WECC ), .groups = "keep" ) %>%
  ungroup()

GridPath_WECC_elec_gen <- read_csv( "data/GridPath_WECC_elec_gen-26_04_06.csv" ) %>%
  mutate( scenario = "GridPath",
          region = "WECC",
          technology = if_else( technology == "NG", "Gas", technology ) ) %>%
  rename( generation_TWh_WECC = "WECC Total Generation (TWh)" ) %>%
  select( colnames( elec_gen_fuel_WECC ) )

WECC_total_elec_comparison_data <- bind_rows( elec_gen_fuel_WECC, GridPath_WECC_elec_gen )

write_stamp_csv( WECC_total_elec_comparison_data, "elec_gen_tech_WECC")

WECC_total_elec_comparison_plot <- ggplot( WECC_total_elec_comparison_data,
                aes( fill = factor( technology,
                                    levels = c( "Geothermal",
                                                "Solar",
                                                "Wind",
                                                "Biomass",
                                                "Nuclear",
                                                "Hydro",
                                                "Gas",
                                                "Refined Liquids",
                                                "Coal" ) ),
                     x = scenario,
                     y = generation_TWh_WECC ) ) +
  geom_bar( position = "stack", stat = "identity", width = 0.75 ) +
  facet_wrap( ~year ) +
  theme_bw( ) +
  coord_cartesian( ylim = c( 0, 1500 ) ) +
  scale_y_continuous( breaks = seq( 0, 1500, by = 250 ) ) +
  #  http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  scale_fill_manual( values = c( '#666666', # "Geothermal"
                                 '#FFCC00', # "Solar"
                                 '#0099FF', # "Wind"
                                 '#00CC66', # "Biomass"
                                 '#990000', # "Nuclear"
                                 '#009999', # "Hydro"
                                 '#6600CC', # "Gas"
                                 '#FF3300', # "Refined Liquids"
                                 '#000000'  # "Coal"
                                 ) ) +
  theme( legend.title = element_blank( ) )

ggsave( "figures/WECC_elec_gen_by_tech_comparison.png",
        width = 10, height = 5, units = "in", dpi = 300 )

###################################################################################################

# AEO 2025 electricity generation comparisons

  # AEO map
  AEO_elec_map <- read_csv( "mappings/AEO_elec_map.csv" )

  # Process AEO data
  AEO_elec_generation <- bind_rows( read_csv( "data/AEO_2025/AEO_Table_54-WECC_B.csv", skip = 5 ) %>% mutate( region = "WECC" ),
                                    read_csv( "data/AEO_2025/AEO_Table_54-WECC_R.csv", skip = 5 ) %>% mutate( region = "WECC" ),
                                    read_csv( "data/AEO_2025/AEO_Table_54-WECC_NWPPA.csv", skip = 5 ) %>% mutate( region = "WECC" ),
                                    read_csv( "data/AEO_2025/AEO_Table_54-WECC_CA_S.csv", skip = 5 ) %>% mutate( region = "WECC" ),
                                    read_csv( "data/AEO_2025/AEO_Table_54-WECC_CA_N.csv", skip = 5 ) %>% mutate( region = "WECC" ),
                                    read_csv( "data/AEO_2025/AEO_Table_54-WECC_SW.csv", skip = 5 ) %>% mutate( region = "WECC" ),
                                    read_csv( "data/AEO_2025/AEO_Table_54-USA.csv", skip = 5 ) %>% mutate( region = "USA" ) ) %>%
    left_join( AEO_elec_map, by = c( "sector", "type", "full name" ),
               relationship = "many-to-many" ) %>%
    filter( technology != "not_a_fuel_type" ) %>%
    mutate( scenario = "AEO (2025) Ref" ) %>%
    select( scenario, region, technology, units, all_of( paste( seq( from = min( ANALYSIS_YEARS ), to = max( ANALYSIS_YEARS ), by = 5 ) ) ) ) %>%
    gather_years() %>%
    # Summarize
    # Label as "TWh", as this is equivalent to the reported unit of billion kWh (BkWh)
    group_by( scenario, region, technology, year ) %>%
    summarize( generation_TWh = sum( value ) ) %>%
    ungroup()

  # Calculate AEO non-WECC
  AEO_elec_generation_nonWECC <- AEO_elec_generation %>%
    filter( region == "USA" ) %>%
    rename( generation_TWh_USA = generation_TWh ) %>%
    select( -region ) %>%
    left_join( AEO_elec_generation %>%  filter( region == "WECC" ) %>%
                 rename( generation_TWh_WECC = generation_TWh ) %>%
                 select( -region ),
               by = c( "scenario", "technology", "year" ) ) %>%
    mutate( generation_TWh = generation_TWh_USA - generation_TWh_WECC,
            region = "Non_WECC" ) %>%
    select( colnames( AEO_elec_generation ) )

  AEO_elec_generation <- bind_rows( AEO_elec_generation, AEO_elec_generation_nonWECC )

# Downscale Table 54's "Renewables" category to specific renewable technologies based on Table 56
  AEO_renewable_generation <- bind_rows( read_csv( "data/AEO_2025/AEO_Table_56-WECC_B.csv", skip = 4 ) %>% mutate( region = "WECC" ),
                                    read_csv( "data/AEO_2025/AEO_Table_56-WECC_R.csv", skip = 4 ) %>% mutate( region = "WECC" ),
                                    read_csv( "data/AEO_2025/AEO_Table_56-WECC_NWPPA.csv", skip = 4 ) %>% mutate( region = "WECC" ),
                                    read_csv( "data/AEO_2025/AEO_Table_56-WECC_CA_S.csv", skip = 4 ) %>% mutate( region = "WECC" ),
                                    read_csv( "data/AEO_2025/AEO_Table_56-WECC_CA_N.csv", skip = 4 ) %>% mutate( region = "WECC" ),
                                    read_csv( "data/AEO_2025/AEO_Table_56-WECC_SW.csv", skip = 4 ) %>% mutate( region = "WECC" ),
                                    read_csv( "data/AEO_2025/AEO_Table_56-USA.csv", skip = 4 ) %>% mutate( region = "USA" ) ) %>%
    inner_join( AEO_elec_map, by = c( "type", "full name" )) %>%
    mutate( scenario = "AEO (2025) Ref" ) %>%
    select( scenario, region, technology, units, all_of( paste( seq( from = min( ANALYSIS_YEARS ), to = max( ANALYSIS_YEARS ), by = 5 ) ) ) ) %>%
    gather_years() %>%
    # Summarize
    # Label as "TWh", as this is equivalent to the reported unit of billion kWh (BkWh)
    group_by( scenario, region, technology, year ) %>%
    summarize( generation_TWh = sum( value ) ) %>%
    ungroup()
  
  AEO_renewable_generation_nonWECC <- AEO_renewable_generation %>%
    filter( region == "USA" ) %>%
    rename( generation_TWh_USA = generation_TWh ) %>%
    select( -region ) %>%
    left_join( AEO_renewable_generation %>%  filter( region == "WECC" ) %>%
                 rename( generation_TWh_WECC = generation_TWh ) %>%
                 select( -region ),
               by = c( "scenario", "technology", "year" ) ) %>%
    mutate( generation_TWh = generation_TWh_USA - generation_TWh_WECC,
            region = "Non_WECC" ) %>%
    select( colnames( AEO_renewable_generation ) )
  
  AEO_renewable_generation <- bind_rows( AEO_renewable_generation, AEO_renewable_generation_nonWECC )
  
  AEO_elec_generation <- filter(AEO_elec_generation, technology != "Renewable") %>%
    bind_rows(AEO_renewable_generation) %>%
    arrange(scenario, region, technology, year)
  
  # Process GCAM data - re-aggregate from model queries in order to differentiate solar and wind categories
  elec_gen_fuel_AEO_comp_WECC <- getQuery(gcam_output.proj, "outputs by nested tech") %>%
    bind_rows(getQuery(gcam_output.proj, "outputs by tech")) %>%
    filter(scenario == "GCAM_RefElecDemand",
           grepl("generation", sector) | technology == "rooftop_pv" | (grepl("cogen", technology) & output == "electricity"),
           technology != "hydrogen cogen",
           year %in% ANALYSIS_YEARS,
           region %in% REPORTING_STATES) %>%
    mutate(region = if_else(region %in% WECC_states, "WECC", "non-Non_WECC"),
           subsector = if_else(is.na(subsector), subsector...5, subsector),
           subsector = if_else(grepl("offshore", technology), "Wind Offshore", subsector),
           subsector = if_else(grepl("wind", technology) & !grepl("offshore", technology), "Wind Onshore", subsector),
           subsector = if_else(grepl("CSP", technology), "Solar CSP", subsector),
           subsector = if_else(grepl("PV", technology, ignore.case = TRUE), "Solar PV", subsector),
           technology = tools::toTitleCase(subsector)) %>%
    group_by(scenario, region, technology, year) %>%
    summarise(generation_TWh = round(sum(value) * TWH_PER_EJ, digits = REPORTING_DIGITS_TWH)) %>%
    ungroup()
  
  elec_gen_fuel_AEO_comp_USA <- elec_gen_fuel_AEO_comp_WECC %>%
    mutate(region = "USA") %>%
    group_by(scenario, region, technology, year) %>%
    summarise(generation_TWh = sum(generation_TWh)) %>%
    ungroup()
  
  elec_gen_fuel_AEO_comp <- bind_rows(elec_gen_fuel_AEO_comp_USA, elec_gen_fuel_AEO_comp_WECC)

  # Graph and save data
  elec_gen_AEO_fuel_region_comparison_data <- bind_rows( elec_gen_fuel_AEO_comp,
                                                         AEO_elec_generation )

  write_stamp_csv( elec_gen_AEO_fuel_region_comparison_data, "elec_gen_tech_AEO_comparison" )

  aeo_elec_comp_fun <- function( data, region_of_interest ){

      data_region <- data %>% filter( region == region_of_interest)

      plot <- ggplot( data_region,
                        aes( fill = factor( technology,
                                            levels = c( "Other",
                                                        "Wind Offshore",
                                                        "Wind Onshore",
                                                        "Solar PV",
                                                        "Solar CSP",
                                                        "Geothermal",
                                                        "Hydro",
                                                        "Biomass",
                                                        "Nuclear",
                                                        "Gas",
                                                        "Refined Liquids",
                                                        "Coal" ) ),
                                                      x = scenario,
                                                      y = generation_TWh ) ) +
        geom_bar( position = "stack", stat = "identity", width = 0.75 ) +
        facet_wrap( ~year ) +
        theme_bw( ) +
        coord_cartesian( ylim = c( -100, 7000 ) ) +
        scale_y_continuous( breaks = seq( 0, 7000, by = 1000 ) ) +
        #  http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
        scale_fill_manual( values = c( '#666666', # "Other"
                                       'skyblue', # "Wind Offshore"
                                       'magenta', # "Wind Onshore"
                                       'gray90',  # "Solar PV"
                                       'red',     # "Solar CSP"
                                       'yellow',  # "Geothermal"
                                       '#009999', # "Hydro"
                                       '#00CC66', # "Biomass"
                                       'orange',  # "Nuclear"
                                       'blue',    # "Gas"
                                       'brown',   # "Refined Liquids"
                                       '#000000'  # "Coal"
                                      ) ) +
        theme( legend.title = element_blank( ) )

      ggsave( paste0( "figures/AEO__elec_gen_by_tech_comparison-", region_of_interest, ".png" ),
              width = 10, height = 5, units = "in", dpi = 300 )

  }

  aeo_elec_comp_fun( data = elec_gen_AEO_fuel_region_comparison_data,
                     region_of_interest = "USA" )

  aeo_elec_comp_fun( data = elec_gen_AEO_fuel_region_comparison_data,
                     region_of_interest = "WECC" )

  aeo_elec_comp_fun( data = elec_gen_AEO_fuel_region_comparison_data,
                     region_of_interest = "Non_WECC" )

###################################################################################################

# WECC GridPath electricity consumption comparison
EER_WECC_consumption <- ref_elec_EER %>%
  inner_join(WECC_fractions, by = "region") %>%
  mutate(EER_TWh = elec_EER * WECC_fraction) %>%
  group_by(year) %>%
  summarise(EER_Consumption_TWh = sum(EER_TWh)) %>%
  ungroup()

GCAM_WECC_consumption <- ref_elec_GCAM %>%
  inner_join(WECC_fractions, by = "region") %>%
  mutate(GCAM_TWh = elec_GCAM * WECC_fraction) %>%
  group_by(year) %>%
  summarise(GCAM_Consumption_TWh = sum(GCAM_TWh)) %>%
  ungroup()

GCAM_WECC_generation <- elec_gen_fuel_WECC %>%
  group_by(year) %>%
  summarise(GCAM_Generation_TWh = sum(generation_TWh_WECC)) %>%
  ungroup()

Gridpath_WECC_generation <- GridPath_WECC_elec_gen %>%
  group_by(year) %>%
  summarise(Gridpath_Generation_TWh = sum(generation_TWh_WECC)) %>%
  ungroup()

WECC_total_elec_comparison <- EER_WECC_consumption %>%
  left_join(GCAM_WECC_consumption, by = "year") %>%
  left_join(GCAM_WECC_generation, by = "year") %>%
  left_join(Gridpath_WECC_generation, by = "year")

write_stamp_csv(WECC_total_elec_comparison, "WECC_total_elec_comparison")


###################################################################################################

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

# Write out the data center electricity consumption. Here 2021 is included with the results.
elec_consumption_datacenters_state <- getQuery(gcam_output.proj, "inputs by tech") %>%
  filter(region %in% REPORTING_STATES,
         sector == "data center",
         year %in% 2021:2050) %>%
  mutate(consumption_TWh = value * TWH_PER_EJ) %>%
  select(scenario, region, sector, year, consumption_TWh)

write_stamp_csv(elec_consumption_datacenters_state, "elec_consumption_datacenters_state")


# Compare GCAM and AEO 2025 or 2026 electricity capacities --------------
# Disclaimer: Some of the disparities in capacities are due to the imperfect technology mapping between GCAM and AEO, see the mapping file for details

# First read in WECC and USA capacities from AEO Table 56, and map to GCAM technologies
AEO_elec_capacity <- bind_rows(
  read_csv(file.path(AEO_folder, "AEO_Table_56-WECC_B.csv"), skip = 4) %>% mutate(region = "WECC"),
  read_csv(file.path(AEO_folder, "AEO_Table_56-WECC_R.csv"), skip = 4) %>% mutate(region = "WECC"),
  read_csv(file.path(AEO_folder, "AEO_Table_56-WECC_NWPPA.csv"), skip = 4) %>% mutate(region = "WECC"),
  read_csv(file.path(AEO_folder, "AEO_Table_56-WECC_CA_S.csv"), skip = 4) %>% mutate(region = "WECC"),
  read_csv(file.path(AEO_folder, "AEO_Table_56-WECC_CA_N.csv"), skip = 4) %>% mutate(region = "WECC"),
  read_csv(file.path(AEO_folder, "AEO_Table_56-WECC_SW.csv"), skip = 4) %>% mutate(region = "WECC"),
  read_csv(file.path(AEO_folder, "AEO_Table_56-USA.csv"), skip = 4) %>% mutate(region = "USA")
) %>%
  filter(grepl("gigawatts|GW", units, ignore.case = TRUE)) %>%
  inner_join(
    AEO_elec_map,
    by = c("type", "full name")
  ) %>%
  mutate(scenario = AEO_scenario) %>%
  select(
    scenario, region, technology, units,
    all_of(paste(seq(from = min(ANALYSIS_YEARS),
                     to = max(ANALYSIS_YEARS),
                     by = 5)))
  ) %>%
  gather_years() %>%
  group_by(scenario, region, technology, year) %>%
  summarise(
    capacity_GW = sum(value),
    .groups = "drop"
  )

# Calculate AEO non-WECC capacities 
AEO_elec_capacity_nonWECC <- AEO_elec_capacity %>%
  filter(region == "USA") %>%
  rename(capacity_GW_USA = capacity_GW) %>%
  select(-region) %>%
  left_join(
    AEO_elec_capacity %>%
      filter(region == "WECC") %>%
      rename(capacity_GW_WECC = capacity_GW) %>%
      select(-region),
    by = c("scenario", "technology", "year")
  ) %>%
  mutate(
    capacity_GW = capacity_GW_USA - capacity_GW_WECC,
    region = "Non_WECC"
  ) %>%
  select(colnames(AEO_elec_capacity))

AEO_elec_capacity <- bind_rows(AEO_elec_capacity, AEO_elec_capacity_nonWECC)


# Read in other (non-renewable) electricity capacities from AEO Table 9
AEO_elec_cap_non_RE <- read_csv(
  file.path(AEO_folder, "AEO_Table_9-USA.csv"),
  skip = 4
) %>%
  filter(`...1` != "Renewable Sources") %>%
  mutate(scenario = paste0("AEO (", AEO_VERSION, ") Ref")) %>%
  left_join(AEO_elec_map, by = "full name") %>%
  filter(!is.na(technology), technology != "not_a_fuel_type") %>%
  select(
    scenario,
    technology,
    units,
    all_of(paste(seq(from = min(ANALYSIS_YEARS),
                     to = max(ANALYSIS_YEARS),
                     by = 5)))
  ) %>%
  gather_years() %>%
  group_by(scenario, technology, year) %>%
  summarise(
    capacity_GW = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

# differentiate USA, WECC and non-WECC for non-renewable capacities (as above for renewable capacities)
AEO_elec_cap_non_RE <- AEO_elec_cap_non_RE %>%
  mutate(region = "USA") %>%
  select(scenario, region, technology, year, capacity_GW)

AEO_elec_cap_non_RE_WECC_share <- AEO_elec_generation %>%
  filter(region %in% c("USA", "WECC"),
         technology %in% unique(AEO_elec_cap_non_RE$technology)) %>%
  select(scenario, region, technology, year, generation_TWh) %>%
  pivot_wider(names_from = region, values_from = generation_TWh) %>%
  mutate(WECC_share = if_else(USA > 0, WECC / USA, 0)) %>%
  select(scenario, technology, year, WECC_share)

AEO_elec_cap_non_RE_WECC <- AEO_elec_cap_non_RE %>%
  filter(region == "USA") %>%
  left_join(AEO_elec_cap_non_RE_WECC_share,
            by = c("scenario", "technology", "year")) %>%
  mutate(
    WECC_share = replace_na(WECC_share, 0),
    capacity_GW = capacity_GW * WECC_share,
    region = "WECC"
  ) %>%
  select(colnames(AEO_elec_cap_non_RE))

AEO_elec_cap_non_RE_nonWECC <- AEO_elec_cap_non_RE %>%
  filter(region == "USA") %>%
  rename(capacity_GW_USA = capacity_GW) %>%
  select(-region) %>%
  left_join(
    AEO_elec_cap_non_RE_WECC %>%
      rename(capacity_GW_WECC = capacity_GW) %>%
      select(-region),
    by = c("scenario", "technology", "year")
  ) %>%
  mutate(
    capacity_GW = capacity_GW_USA - capacity_GW_WECC,
    region = "Non_WECC"
  ) %>%
  select(colnames(AEO_elec_cap_non_RE))

AEO_elec_cap_non_RE <- bind_rows(
  AEO_elec_cap_non_RE,
  AEO_elec_cap_non_RE_WECC,
  AEO_elec_cap_non_RE_nonWECC
)

# Combine AEO renewable and non-renewable capacities
AEO_elec_cap <- AEO_elec_cap_non_RE %>%
  bind_rows(AEO_elec_capacity)


# Process GCAM capacity data
elec_cap_GCAM_WECC_nonWECC <- elec_allvars_state_tech_vintage %>%
  filter(
    scenario == "GCAM_RefElecDemand",
    year %in% ANALYSIS_YEARS,
    region %in% REPORTING_STATES
  ) %>%
  mutate(
    region = if_else(region %in% WECC_states, "WECC", "Non_WECC"),
    technology = case_when(
      grepl("offshore.*wind|wind.*offshore", technology, ignore.case = TRUE) ~ "Wind Offshore",
      grepl("wind", technology, ignore.case = TRUE) ~ "Wind Onshore",
      grepl("csp", technology, ignore.case = TRUE) ~ "Solar CSP",
      grepl("solar|pv|rooftop", technology, ignore.case = TRUE) ~ "Solar PV",
      grepl("gas", technology, ignore.case = TRUE) ~ "Gas",
      grepl("coal", technology, ignore.case = TRUE) ~ "Coal",
      grepl("refined liquids|oil", technology, ignore.case = TRUE) ~ "Refined Liquids",
      grepl("hydro", technology, ignore.case = TRUE) ~ "Hydro",
      grepl("nuclear", technology, ignore.case = TRUE) ~ "Nuclear",
      grepl("biomass", technology, ignore.case = TRUE) ~ "Biomass",
      grepl("geothermal", technology, ignore.case = TRUE) ~ "Geothermal",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(scenario, region, technology, year) %>%
  summarise(
    capacity_GW = sum(capacity_GW, na.rm = TRUE),
    .groups = "drop"
  )

# aggregate WECC and non-WECC to USA level and attach 
elec_cap_GCAM_USA <- elec_cap_GCAM_WECC_nonWECC %>%
  mutate(region = "USA") %>%
  group_by(scenario, region, technology, year) %>%
  summarise(capacity_GW = sum(capacity_GW, na.rm = TRUE), .groups = "drop")

elec_cap_GCAM <- bind_rows(elec_cap_GCAM_USA, elec_cap_GCAM_WECC_nonWECC)


# Graph and save data
elec_cap_AEO_GCAM_comp <- bind_rows(
  elec_cap_GCAM,
  AEO_elec_cap
)

write_stamp_csv(
  elec_cap_AEO_GCAM_comp,
  paste0("elec_cap_tech_AEO_", AEO_VERSION, "_GCAM_comparison")
)
  

aeo_elec_cap_comp_fun <- function(data, region_of_interest) {
  
  data_region <- data %>%
    filter(region == region_of_interest)
  
  plot <- ggplot(
    data_region,
    aes(
      fill = factor(
        technology,
        levels = c(
          "Other",
          "Wind Offshore",
          "Wind Onshore",
          "Solar PV",
          "Solar CSP",
          "Geothermal",
          "Hydro",
          "Biomass",
          "Nuclear",
          "Gas",
          "Refined Liquids",
          "Coal"
        )
      ),
      x = scenario,
      y = capacity_GW
    )
  ) +
    geom_bar(position = "stack", stat = "identity", width = 0.75) +
    facet_wrap(~year) +
    theme_bw() +
    coord_cartesian(ylim = c(0, 3000)) +
    scale_y_continuous(breaks = seq(0, 3000, by = 500)) +
    scale_fill_manual(
      values = c(
        '#666666', # Other
        'skyblue', # Wind Offshore
        'magenta', # Wind Onshore
        'gray90',  # Solar PV
        'red',     # Solar CSP
        'yellow',  # Geothermal
        '#009999', # Hydro
        '#00CC66', # Biomass
        'orange',  # Nuclear
        'blue',    # Gas
        'brown',   # Refined Liquids
        '#000000'  # Coal
      )
    ) +
    theme(legend.title = element_blank())
  
  ggsave(
    paste0("figures/AEO_", AEO_VERSION, "_elec_cap_by_tech_comparison-", region_of_interest, ".png"),
    plot = plot,
    width = 10,
    height = 5,
    units = "in",
    dpi = 300
  )
}

aeo_elec_cap_comp_fun(
  data = elec_cap_AEO_GCAM_comp,
  region_of_interest = "USA"
)

aeo_elec_cap_comp_fun(
  data = elec_cap_AEO_GCAM_comp,
  region_of_interest = "WECC"
)

aeo_elec_cap_comp_fun(
  data = elec_cap_AEO_GCAM_comp,
  region_of_interest = "Non_WECC"
)


###################################################################################################

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

###################################################

# Compare Henry Hub prices
AEO_henryhub <- read_csv(file.path(AEO_folder, "AEO_Table13.csv")) %>%
  bind_rows(
    read_csv(file.path(AEO_folder, "AEO_Table13_HighOGS.csv")),
    read_csv(file.path(AEO_folder, "AEO_Table13_LowOGS.csv"))
  ) %>%
  filter(grepl("Henry Hub Spot Price", `full name`))  %>%
  separate(`full name`, into = c("fuel", "variable", "scenario"), sep = ": ") %>%
  mutate(
    scenario = if_else(
      scenario == "Reference case", "Reference",
      if_else(scenario == "High Oil and Gas Supply", "High Gas Supply", "Low Gas Supply")
    )
  ) %>%
  gather_years() %>%
  mutate(model = paste0("AEO", AEO_VERSION)) %>%
  select(model, scenario, year, price_USDperMMBTU = value) %>%
  distinct()

HH_gasprice_comparison <- gas_prices_nation %>%
  filter(scenario %in% c("GCAM_RefElecDemand", "GCAM_RefElecDemand_HighGasPrice", "GCAM_RefElecDemand_LowGasPrice")) %>%
  mutate(
    model = "GCAM-USA",
    scenario = if_else(
      scenario == "GCAM_RefElecDemand", "Reference",
      if_else(scenario == "GCAM_RefElecDemand_LowGasPrice", "High Gas Supply", "Low Gas Supply")
    )
  ) %>%
  filter(rate_class == "Henry Hub") %>%
  select(model, scenario, year, price_USDperMMBTU) %>%
  bind_rows(AEO_henryhub)

ggplot(HH_gasprice_comparison, aes(x = year, y = price_USDperMMBTU, color = scenario, linetype = model)) +
  geom_line() +
  xlab("") +
  ylab("2024$ per mmbtu") +
  theme_bw() +
  ylim(c(0, NA))


ggsave(
  paste0("figures/AEO_", AEO_VERSION, "_gas_prices_HenryHub.png"),
  width = 5,
  height = 3.5,
  units = "in",
  dpi = 300
)

###################################################################################################

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

###################################################################################################

# Gas production by state
gas_production_state <- getQuery(gcam_output.proj, "outputs by tech") %>%
  filter(sector == "natural gas production",
         year %in% ANALYSIS_YEARS,
         !region %in% c("USA", EXCLUDED_STATES)) %>%
  group_by(scenario, region, year) %>%
  summarise(production_Tbtu = round(sum(value) * CONV_NG_EJ_TBTU, digits = REPORTING_DIGITS_TBTU)) %>%
  ungroup()

write_stamp_csv(gas_production_state, "gas_production_state")

###################################################################################################

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

###################################################

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

###################################################

# Gas pipeline imports and exports by state

# Read in the data for historical pipeline trade
US_pipeline_import_export <- read.csv( "data/US_pipeline_trade_by_state.csv" )

# Process historical pipeline trade data
future_gas_trade_years <- c( paste0( "X", seq( from = 2030, to = 2050, by = 5 ) ) )
gas_trade_years <- c( "X2021", "X2025", future_gas_trade_years )
gas_trade_years_no_X <- as.numeric( gsub( "X", "", gas_trade_years ) )

US_pipeline_import_export_capacity <- US_pipeline_import_export %>%
  mutate( From_Country = if_else( From_Country == "Can",
                                  "Canada",
                         if_else( From_Country == "Mex",
                                  "Mexico",
                         if_else( From_Country == "US",
                                  "USA",
                                  From_Country ) ) ),
          To_Country = if_else( To_Country == "Can",
                                "Canada",
                       if_else( To_Country == "Mex",
                                "Mexico",
                       if_else( To_Country == "US",
                                "USA",
                                To_Country ) ) ),
          From_State = if_else( From_Country %in% c( "Canada", "Mexico" ),
                                From_Country,
                                From_State ),
          To_State = if_else( To_Country %in% c( "Canada", "Mexico" ),
                              To_Country,
                              To_State  ) ) %>%
  group_by( From_Country, From_State, To_Country, To_State ) %>%
  summarise_all( sum, na.rm = TRUE ) %>%
  ungroup( ) %>%
  # Extend forward to all future years
  mutate( !!!setNames( rep( NA_real_,
                       length( future_gas_trade_years ) ),
                       future_gas_trade_years ),
          across( all_of( future_gas_trade_years ), ~X2025 ) )  %>%
  select( From_Country, From_State, To_Country, To_State, all_of( gas_trade_years ) ) %>%
  gather( key = year, value = capacity, all_of( gas_trade_years ) )

# Calculate shares
US_pipeline_import_export_capacity_total <- US_pipeline_import_export_capacity %>%
  select( -From_State, -To_State ) %>%
  group_by( From_Country, To_Country, year ) %>%
  summarise_all( sum, na.rm = TRUE ) %>%
  ungroup( ) %>%
  rename( capacity_total = capacity )

US_pipeline_import_export_share <- US_pipeline_import_export_capacity %>%
  left_join( US_pipeline_import_export_capacity_total, by = c( "From_Country", "To_Country", "year" ),
             relationship = "many-to-many" ) %>%
  mutate( share = capacity / capacity_total,
          year = as.numeric( gsub( "X", "", year ) ) )

# Calculate future imports and exports by state
gas_trade_state <- gas_trade_nation %>%
  mutate( year = as.numeric( year ) ) %>%
  filter( Commodity == "Pipeline gas",
          year %in% gas_trade_years_no_X ) %>%
  rename( From_Country = From.Country,
          To_Country = To.Country ) %>%
  left_join( US_pipeline_import_export_share %>%
               select( -capacity, -capacity_total ),
             by = c( "From_Country", "To_Country", "year" ),
             relationship = "many-to-many" ) %>%
  mutate( Flow_Tbtu_by_state = Flow_Tbtu * share ) %>%
  select( scenario, Commodity, From_Country, From_State, To_Country, To_State, year, Flow_Tbtu_by_state )

write_stamp_csv( gas_trade_state, "gas_trade_pipeline_state" )

# Plot exports by state
plot_export_df <- gas_trade_state %>%
  filter( From_Country == "USA" ) %>%
  select( scenario, Commodity, From_Country, From_State, year, Flow_Tbtu_by_state ) %>%
  group_by( scenario, Commodity, From_Country, From_State, year ) %>%
  summarise_all( sum, na.rm = TRUE ) %>%
  ungroup( )

plot_export <- ggplot( plot_export_df,
                       aes( x = year, y = Flow_Tbtu_by_state, fill = From_State ) ) +
  facet_wrap( ~scenario ) +
  geom_col( ) +
  labs( title = "pipeline exports by state",
        x = NULL,
        y = "Tbtu" )

ggsave( "figures/gas_pipeline_exports_by_state.png",
        plot = plot_export,
        width = 5, height = 3.5, units = "in", dpi = 300 )

# Plot imports by state
plot_imports_df <- gas_trade_state %>%
  filter( To_Country == "USA" ) %>%
  select( scenario, Commodity, To_Country, To_State, year, Flow_Tbtu_by_state ) %>%
  group_by( scenario, Commodity, To_Country, To_State, year ) %>%
  summarise_all( sum, na.rm = TRUE ) %>%
  ungroup( )

plot_import <- ggplot( plot_imports_df,
                       aes( x = year, y = Flow_Tbtu_by_state, fill = To_State ) ) +
  facet_wrap( ~scenario ) +
  geom_col( ) +
  labs( title = "pipeline imports by state",
        x = NULL,
        y = "Tbtu" )

ggsave( "figures/gas_pipeline_imports_by_state.png",
        plot = plot_import,
        width = 5, height = 3.5, units = "in", dpi = 300 )
