
#### Get OSM Data ####

cycleways_Montreal = oe_get(
  "Montreal",
  quiet = FALSE,
  query = "SELECT * FROM 'lines' WHERE highway = 'cycleway'"
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

