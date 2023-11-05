##  UN DATATHON 2023
## Author : Team Equity Visualisers
## November, 2023
#*** =================================================================##

library(pacman)
p_load(tidyverse, lubridate, skimr, arrow, usethis,table1, here, data.table)

# Loading the mapping packages for geospatial visualizations
p_load(leaflet, mapview, sf, tmap, viridis)

## Working with the world food prices data wfp_food_prices_uga
## Source:
wfp_uga <- read_csv(here::here("data", "wfp_food_prices_uga.csv"),
  col_names = T,
  skip_empty_rows = T
)

# Let us drop the second row with extra hash tags and only select a 4- year period (2020 - 2023)
wfp_uga <- wfp_uga %>% slice(-1)

str(wfp_uga)

wfp_uga_select <- wfp_uga %>%
  dplyr::mutate(
    date_u = ymd(date),
    year = year(date_u),
    month = month(date_u)
  ) %>%
  dplyr::filter(date >= 2020) %>%
  dplyr::filter(date < 2024) %>%
  dplyr::filter(priceflag == "actual")
write_csv(wfp_uga_select, file = "data/food_px.csv", col_names = T)


##  Mapping the data
wfp_uga_sf <- st_as_sf(wfp_uga_select, coords = c("longitude", "latitude"))
str(wfp_uga_sf)
# Assign CRS

st_crs(wfp_uga_sf) <- 4326
st_crs(wfp_uga_sf)

st_write(wfp_uga_sf, "data/wfp_data_geom", driver = "CSV")

wfp_uga_sf1 <- wfp_uga_sf %>%
  filter(month == 1 & year == 2022) %>%
  filter(commodity == "Maize flour") %>%
  select(usdprice, admin1, geometry) %>%
  mutate(
    usdprice = as.numeric(usdprice),
    size = round(usdprice * 10, 1)
  )

ggplot(wfp_uga_sf1) +
  geom_sf(aes(col = usdprice, size = size)) +
  scale_color_viridis()


plot(st_geometry(wfp_uga_sf1))

d$size <- sqrt(d$vble)

mapview(wfp_uga_sf1, zcol = "usdprice", cex = "size")

## DATASET 2: High-Frequency Phone Survey 2020-2023 ----------- 

clm_shock <- read_csv(here::here("data/UGA_2020_HFPS_v13_M_CSV/round8","SEC13.csv"),col_names = T)
skim(clm_shock)

 # Focus on drought
clm_shock_dr <- clm_shock %>% 
  dplyr::filter(weather__id == 1)
n_distinct(clm_shock_dr$hhid)
table1(~ as.factor(s13q03), data = clm_shock_dr, rowlabelhead = "Experienced drought")


table(~clm_shock_dr$s13q03)





