# Clear workspace and set work directory
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
working_dir <- getwd()


# devtools::install_github("JGCRI/rgcam)
library(rgcam)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(patchwork)
library(RColorBrewer)
library(gcamdata)
library(tidyverse)


# read mapping files ----
#gas_demand_mapping <- read_csv("./mappings/gas_demand_mapping.csv")
#fuel_mapping <- read_csv("./mappings/fuel_mapping.csv")
#enduse_technology_mapping <- read_csv("./mappings/enduse_technology_mapping.csv")

# define constants ----
ANALYSIS_YEARS <- 2021:2050
CONV_NG_EJ_QBTU <- 1/(1.0551 * .905)    # conversion from EJ LHV of natural gas to Quadrillion btu HHV
CONV_elec_EJ_TWh <- 1/.0036             # conversion from EJ of electricity to TWh
gdp_deflator_1975_2021 <- 3.964         # conversion from 1975$ in GCAM output to 2021$ for reporting
EJ_Tcf <- .981
EJ_Mmcf <- 981000                       # conversion from million cubic feet of natural gas to EJ (LHV)


# read rgcam proj file----
gcam_output.proj <- loadProject("rgcam/gcam_output.proj")

REQUERY_DATA <- FALSE
if(REQUERY_DATA){
  scenarios <- c("GCAM-USA_Reference")

  for(i in scenarios){
    conn <- localDBConn("/Users/d3p747/Desktop/stash/decarb/output", "database_basexdb")
    gcam_output.proj <- addScenario(conn, gcam_output.proj, i, "rgcam/BatchQueries_futuregrid.xml", clobber = TRUE)
  }
}


# Available queries:
queries_futuregrid <- listQueries(gcam_output.proj, 'GCAM-USA_Reference')
queries_futuregrid



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
