
#### Get OSM Data ####

cycleways_Montreal = oe_get(
  "Montreal",
  quiet = FALSE,
  query = "SELECT * FROM 'lines' WHERE highway = 'cycleway'",
  force_download = TRUE
)

cycleways_Montreal <- cycleways_Montreal %>%
  mutate(
    cycleway_type = str_extract(other_tags, "cycleway=[^;]+"),  # extract cycleway=...
    cycleway_type = str_remove(cycleway_type, "cycleway=")      # remove the prefix
  )

saveRDS(cycleways_Montreal, "data/cycleways_Montreal.rds")

montreal <- getbb("Montreal, Quebec, Canada")

q <- opq("Montreal") %>%
  add_osm_feature(key = "cycleway")


bike_osm <- osmdata_sf(q)


bike_lanes <- bike_osm$osm_lines# |>
  # filter(
  #   highway == "cycleway" |
  #     !is.na(cycleway)
  # )

saveRDS(bike_lanes, "data/bike_lanes.rds")

# Load packages
library(osmdata)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
bbbike_streets <- readRDS("data/cycleways_Montreal.rds")
# -----------------------------
# 1. Query Montreal for all cycleways
# -----------------------------
q <- opq("Montreal, Canada") %>%
  add_osm_feature(key = "cycleway")  # all segments with a cycleway tag

osm_data <- osmdata_sf(q)
osm_lines <- osm_data$osm_lines

# -----------------------------
# 3. Combine left/right/both into one column
# -----------------------------
osm_lines <- osm_lines %>%
  mutate(
    cycleway_type = coalesce(cycleway, `cycleway:both`, `cycleway:left`, `cycleway:right`)
  ) %>%
  filter(!is.na(cycleway_type))  # keep only tagged segments

# -----------------------------
# 4. Spatial join: tag BBBike streets with OSM cycleway info
# -----------------------------
# Make sure both are in same CRS
bbbike_streets <- st_transform(bbbike_streets, 4326)
osm_lines <- st_transform(osm_lines, 4326)

# Spatial join: find BBBike street segments that overlap any OSM cycleway segment
streets_with_cycleway <- st_join(
  bbbike_streets,
  osm_lines %>% select(cycleway_type),
  join = st_intersects,
  left = TRUE
)

# -----------------------------
# 5. Fill in NA for streets without OSM cycleway tags
# -----------------------------
streets_with_cycleway <- streets_with_cycleway %>%
  mutate(
    cycleway_type = ifelse(is.na(cycleway_type.y), "none", cycleway_type.y)
  )

# Check distribution
table(streets_with_cycleway$cycleway_type)

# -----------------------------
# 6. Optional: plot
# -----------------------------

streets_with_cycleway %>% 
  filter(cycleway_type != "crossing") %>% 
ggplot() +
  geom_sf(aes(color = cycleway_type), size = 0.8) +
  theme_minimal() +
  labs(title = "Montreal bike network (BBBike + OSM tags)",
       color = "Cycleway type")

# -----------------------------
# 7. Optional: convert to network for routing
# -----------------------------
bike_net <- streets_with_cycleway %>%
  st_transform(26918) %>%        # project to meters
  as_sfnetwork(directed = FALSE) %>%
  convert(to_spatial_subdivision) # ensure intersections become nodes
