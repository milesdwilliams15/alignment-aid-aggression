

rm(list = ls()) # clean the environment

# packages + source files -------------------------------------------------

# To make the dataset:
## I need {tidyverse} and {peacesciencer}
library(tidyverse)
library(peacesciencer)

## And I need my tools for getting aid data
source("http://tinyurl.com/aiddatadownloads")


# read in + prep the data -------------------------------------------------

# Make the base dataset:
## Use create_stateyears()
dt <- create_stateyears()

nr <- nrow(dt) # document the no. of rows

# Start populating the dataset:
dt <- dt |>
  
  ## MID onsets and initiation
  add_gml_mids() |>
  
  ## Peace spells
  add_spells() |>
  
  ## Economic data
  add_sdp_gdp() |>
  
  ## Foreign policy similarity data
  left_join(
    # Start at dyad-year level
    cow_ddy |>
      add_fpsim(
        keep = c("kappaba", # alliances
                 "kappavv") # UN voting
      ) |>
      # Aggregate to country-year averages
      rename(ccode = ccode1) |>
      group_by(ccode, year) |>
      summarize(
        across(kappaba:kappavv,
               ~ mean(.x, na.rm = T))
      )
  ) |>
  
  ## Power and systemic dissatisfaction
  left_join(
    read_rds(
      here::here(
        "03_data",
        "ken_final_measure.rds"
      ) 
    ) |>
      mutate(
        ccode = countrycode::countrycode(
          stateabb, "cowc", "cown"
        )
      )
  )

nr == nrow(dt) # check that these are the same
# [1] TRUE

# Bring in foreign aid data:

## Read in Donor-Recipient-Year data
adt <- get_aiddata(
  level = "total" 
)

## Only keep donors in the cow system
adt <- adt |>
  filter(!is.na(ccode_d))

## Aggregate to the donor-year
adt <- adt |>
  group_by(ccode_d, year) |>
  summarize(
    aid = sum(commitment_2011_constant)
  )

## Merge with main dataset
dt <- dt |>
  left_join(
    adt,
    by = c("ccode" = "ccode_d", "year")
  )

nr == nrow(dt) # still the same?
# [1] TRUE

# no new mids past 2010
dt$gmlmidonset_init[
  dt$year > 2010
] <- NA


# save the data -----------------------------------------------------------

write_rds(
  dt,
  file = here::here("03_data", "williams_final_data.rds")
)
