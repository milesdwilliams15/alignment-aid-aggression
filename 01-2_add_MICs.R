
# Update analysis data with MICs ------------------------------------------

source(
  "https://raw.githubusercontent.com/milesdwilliams15/death-destruction-data/refs/heads/main/helpers/peacesciencer_extras.R"
)

read_rds(
  here::here("03_data", "williams_final_data.rds")
) -> dt

dt |>
  add_icd_mics(level = 4) -> dt

write_rds(
  dt,
  file = here::here("03_data", "williams_final_data[ADDMICS].rds")
)
