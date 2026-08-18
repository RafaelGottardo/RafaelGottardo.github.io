
toronto_permits <- read.csv("data/data_raw/Building_permit_data/Cleared Building Permits since 2017.csv")

toronto_permits <- toronto_permits %>% 
  mutate(change = ifelse(PROPOSED_USE == CURRENT_USE | str_detect(PROPOSED_USE, "Same"), 0, 1))

toronto_permits_change <- toronto_permits %>% 
  filter(change == 1)

toronto_permits_change <- toronto_permits_change %>% 
  mutate(Address = paste(STREET_NUM, STREET_NAME, STREET_TYPE),
         Address = str_to_title(Address))

Toronto_address_points <- read_sf("data/data_raw/Address Points - 4326.geojson")

Toronto_address_points <- Toronto_address_points %>%
  distinct(ADDRESS_FULL, .keep_all = TRUE)

toronto_permits_change <- toronto_permits_change %>% 
  left_join(Toronto_address_points %>%  select(ADDRESS_FULL, geometry),
            by = c("Address" = "ADDRESS_FULL")) %>% 
  st_as_sf()


toronto_permits_change <- st_transform(toronto_permits_change, crs = 4326)


toronto_permits_change <- toronto_permits_change %>%
  filter(!st_is_empty(geometry)) %>%   # drop the 82,484 unmatched rows
  st_cast("POINT")                      # MULTIPOINT -> POINT


toronto_permits_change <- toronto_permits_change %>% 
  mutate(pop_up = paste0("<b>Permit Type:</b> ", PERMIT_TYPE, "<br>",
                         "<b>Structure Type:</b> ", STRUCTURE_TYPE, "<br>",
                         "<b>Current Use:</b> ", CURRENT_USE,  "<br>",
                         "<b>Proposed Use:</b> ", PROPOSED_USE,  "<br>",
                         "<b>Address:</b> ", Address, "<br>",
                         "<b>Description:</b> ", DESCRIPTION,  "<br>"
  ))

toronto_permits_2021 <- toronto_permits_change %>% 
  mutate(APPLICATION_DATE = as.Date(APPLICATION_DATE)) %>% 
  filter(APPLICATION_DATE >= "2020-01-01" & APPLICATION_DATE <= "2022-12-31")


toronto_FSA <- read_sf("data/data_raw/ldb_000b21a_e/ldb_000b21a_e.shp")

toronto_FSA <- st_transform(toronto_FSA, crs = 4326) %>% 
  st_make_valid()

toronto_boundary <- read_sf("data/data_raw/toronto-boundary-wgs84/citygcs_regional_mun_wgs84.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid()

toronto_FSA <- toronto_FSA[st_within(toronto_FSA$geometry, toronto_boundary, sparse = FALSE), ]

source("analysis_files/ontario_db_demographics.R")

toronto_FSA <- toronto_FSA %>% 
  left_join(ontario_db, by = c("DBUID" = "GeoUID"))


toronto_FSA <- toronto_FSA %>%
  mutate(Denisty = Population/`Area (sq km)`)
