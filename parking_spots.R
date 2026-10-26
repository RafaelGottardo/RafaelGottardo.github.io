#### Estimate on-street parking spots in Montreal ####

# Free and permit spots are estimated from the parking sign inventory:
# signs are snapped to the street network, the curb between consecutive
# poles takes the rules of the signs pointing at it, and the usable curb
# length is divided by the length of a parking spot.
# Paid spots come straight from the Agence de mobilité durable inventory.
#
# Inputs (data/data_raw/map_layers):
#   signalisation_stationnement.geojson  https://donnees.montreal.ca/dataset/stationnement-sur-rue-signalisation-courant
#   geobase/geobase_mtl.shp              https://donnees.montreal.ca/dataset/geobase
#   places_tarifees.csv                  https://www.agencemobilitedurable.ca/images/data/Places.csv
# Output: data/parking_spots.rds

library(sf)
library(tidyverse)

crs_mtm <- 32188        # NAD83 / MTM zone 8, metres
spot_length <- 5.5      # metres of curb per parallel parking spot
corner_setback <- 5     # no parking within 5 m of an intersection (CSR art. 386)
curb_offset <- 6        # metres from the centreline to draw / locate each curb
max_snap_dist <- 25     # poles further than this from a street are dropped

#### Streets ####

# Géobase classes: 0 local, 5 collector, 6 secondary arterial, 7 primary arterial
# (dropped: highways, ramps, private/service roads, pedestrian streets, quays)
streets <- read_sf("data/data_raw/map_layers/geobase/geobase_mtl.shp") %>%
  filter(CLASSE %in% c(0, 5, 6, 7), !(NOM_VOIE %in% "Non-nommée")) %>%
  st_transform(crs_mtm) %>%
  st_zm() %>%
  mutate(street_id = row_number(),
         street_length = as.numeric(st_length(geometry)))

street_coords <- map(st_geometry(streets), ~ st_coordinates(.x)[, 1:2, drop = FALSE])

# Locate points on their nearest street: distance along it and side of it
snap_to_streets <- function(points) {
  idx <- st_nearest_feature(points, streets)
  street_geom <- st_geometry(streets)[idx]
  dist <- as.numeric(st_distance(points, street_geom, by_element = TRUE))
  m <- st_line_project(street_geom, st_geometry(points))

  # Side from the cross product of the street direction and the point offset
  ahead <- st_line_interpolate(street_geom, pmin(m + 0.5, streets$street_length[idx]))
  behind <- st_line_interpolate(street_geom, pmax(m - 0.5, 0))
  p <- st_coordinates(points)
  a <- st_coordinates(ahead)
  b <- st_coordinates(behind)
  cross <- (a[, 1] - b[, 1]) * (p[, 2] - b[, 2]) - (a[, 2] - b[, 2]) * (p[, 1] - b[, 1])

  tibble(street_id = idx, m = m, snap_dist = dist,
         side = ifelse(cross > 0, "left", "right"))
}

#### Signs ####

signs <- read_sf("data/data_raw/map_layers/signalisation_stationnement.geojson") %>%
  filter(DESCRIPTION_REP == "Réel", is.na(PAS_SUR_RUE)) %>%
  st_transform(crs_mtm)

reserved <- regex("HANDICAP|TAXI|LIVRAISON|AUTOBUS|RESERV|RÉSERV|GARDERIE|HOTEL|HÔTEL|DEBARCADERE|DÉBARCADÈRE|VELO|VÉLO|ELECTRIQUE|ÉLECTRIQUE|AUTOPARTAGE|COMMUNAUTO|POLICE|DIPLOMAT|CORBILLARD|MOTO|URGENCE",
                  ignore_case = TRUE)
clock_hours <- regex("\\d ?h|\\dH")

# Rule each sign imposes on the general public
signs <- signs %>%
  mutate(
    has_hours = str_detect(DESCRIPTION_RPA, clock_hours),
    prohibition = str_detect(DESCRIPTION_RPA, "^\\\\"),  # \P no parking, \A no stopping
    rule = case_when(
      DESCRIPTION_CAT == "STAT-$" | str_detect(DESCRIPTION_RPA, "TARIF|\\$") ~ "Paid",
      str_detect(DESCRIPTION_RPA, "S3R|PERMIS") ~ "Permit",
      str_detect(DESCRIPTION_RPA, reserved) & !has_hours ~ "No parking",
      prohibition & (str_detect(DESCRIPTION_RPA, "EN TOUT TEMPS") | !has_hours) ~ "No parking",
      TRUE ~ "Free"  # time limits, street cleaning, rush hour: parking allowed part of the time
    )
  )

# A "permit holders excepted" sub-panel turns the all-time bans on its pole into permit parking
permit_poles <- signs %>%
  filter(str_detect(DESCRIPTION_RPA, "PANONCEAU") & rule == "Permit") %>%
  pull(POTEAU_ID_POT)

signs <- signs %>%
  mutate(rule = ifelse(POTEAU_ID_POT %in% permit_poles & rule == "No parking" &
                         !str_detect(DESCRIPTION_RPA, reserved),
                       "Permit", rule))

poles <- signs %>%
  distinct(POTEAU_ID_POT, .keep_all = TRUE) %>%
  select(POTEAU_ID_POT)

poles <- bind_cols(poles, snap_to_streets(poles)) %>%
  filter(snap_dist <= max_snap_dist) %>%
  st_drop_geometry()

# Arrow direction along the street: +1 towards the end of the segment, -1 towards
# the start, 0 both ways (no arrow: a sign in the middle of a zone, or double arrow). Arrows read facing the sign
# from the street, so "right" points backwards along the street on its right side.
signs <- signs %>%
  st_drop_geometry() %>%
  inner_join(poles, by = "POTEAU_ID_POT") %>%
  mutate(dir = case_when(
    FLECHE_PAN == 3 ~ ifelse(side == "left", 1, -1),
    FLECHE_PAN == 2 ~ ifelse(side == "left", -1, 1),
    TRUE ~ 0
  ))

#### Curb intervals between poles ####

rule_rank <- c("Free" = 1, "Permit" = 2, "Paid" = 3, "No parking" = 4)

pole_breaks <- poles %>%
  group_by(street_id, side) %>%
  summarise(m = list(sort(unique(m))), .groups = "drop")

# Each side of each street is a curb, labelled with the borough on that side
curbs_all <- streets %>%
  st_drop_geometry() %>%
  select(street_id, street_length, ARR_GCH, ARR_DRT) %>%
  crossing(side = c("left", "right")) %>%
  mutate(borough = ifelse(side == "left", ARR_GCH, ARR_DRT)) %>%
  select(street_id, side, street_length, borough) %>%
  left_join(pole_breaks, by = c("street_id", "side")) %>%
  mutate(signed = !map_lgl(m, is.null))

# The sign inventory is only complete in some boroughs. Where it is, a curb with
# no signs has no restriction (free); elsewhere it is left unestimated.
sign_coverage <- curbs_all %>%
  group_by(borough) %>%
  summarise(share_signed = weighted.mean(signed, street_length), .groups = "drop")

covered_boroughs <- sign_coverage %>%
  filter(share_signed >= 0.5, borough != "N/A") %>%
  pull(borough)

# Every curb is split at each of its poles
intervals <- curbs_all %>%
  mutate(covered = borough %in% covered_boroughs,
         breaks = map2(m, street_length, ~ c(0, .x, .y))) %>%
  select(street_id, side, borough, covered, breaks) %>%
  mutate(from = map(breaks, ~ head(.x, -1)),
         to = map(breaks, ~ tail(.x, -1))) %>%
  select(-breaks) %>%
  unnest(c(from, to)) %>%
  mutate(interval_id = row_number())

# Signs governing each interval: those at its start pointing forward and those at its end pointing back
governing <- bind_rows(
  intervals %>%
    inner_join(signs %>% filter(dir >= 0), by = c("street_id", "side", "from" = "m"),
               relationship = "many-to-many"),
  intervals %>%
    inner_join(signs %>% filter(dir <= 0), by = c("street_id", "side", "to" = "m"),
               relationship = "many-to-many")
) %>%
  group_by(interval_id) %>%
  summarise(rule = names(rule_rank)[max(rule_rank[rule])], .groups = "drop")

# Paid spots, snapped to the same curbs so they are not counted twice
paid <- read.csv("data/data_raw/map_layers/places_tarifees.csv", fileEncoding = "latin1") %>%
  filter(sStatut == 1, sGenre == "NORMAL") %>%
  st_as_sf(coords = c("nPositionCentreLongitude", "nPositionCentreLatitude"), crs = 4326) %>%
  st_transform(crs_mtm)

paid <- bind_cols(paid, snap_to_streets(paid))

paid_per_interval <- paid %>%
  st_drop_geometry() %>%
  filter(snap_dist <= max_snap_dist) %>%
  inner_join(intervals, by = c("street_id", "side"), relationship = "many-to-many") %>%
  filter(m >= from, m < to) %>%
  count(interval_id, name = "n_paid")

intervals <- intervals %>%
  left_join(governing, by = "interval_id") %>%
  left_join(paid_per_interval, by = "interval_id") %>%
  mutate(
    # Curb not covered by any arrow: no restriction, if the borough's signs are inventoried
    rule = case_when(!is.na(rule) ~ rule,
                     covered ~ "Free",
                     TRUE ~ "No sign data"),
    n_paid = replace_na(n_paid, 0),
    street_length = streets$street_length[street_id],
    usable_from = pmax(from, corner_setback),
    usable_to = pmin(to, street_length - corner_setback),
    usable_length = pmax(usable_to - usable_from - n_paid * spot_length, 0),
    n_spots = floor(usable_length / spot_length)
  ) %>%
  filter(rule %in% c("Free", "Permit", "No sign data"), n_spots > 0) %>%
  mutate(n_spots = ifelse(rule == "No sign data", NA, n_spots))

#### Curb geometry ####

# Piece of a line between two distances along it, shifted sideways (left is positive)
offset_substring <- function(xy, from, to, offset) {
  seg <- sqrt(rowSums(diff(xy)^2))
  cum <- c(0, cumsum(seg))
  interp <- function(d) {
    i <- min(findInterval(d, cum, rightmost.closed = TRUE), nrow(xy) - 1)
    t <- if (seg[i] > 0) (d - cum[i]) / seg[i] else 0
    xy[i, ] + t * (xy[i + 1, ] - xy[i, ])
  }
  inside <- cum > from & cum < to
  pts <- rbind(interp(from), xy[inside, , drop = FALSE], interp(to))

  # Shift each vertex along the normal of the adjoining segments
  d <- diff(pts)
  d <- d / pmax(sqrt(rowSums(d^2)), 1e-9)
  tangent <- rbind(d[1, ], (head(d, -1) + tail(d, -1)) / 2, d[nrow(d), ])
  normal <- cbind(-tangent[, 2], tangent[, 1])
  st_linestring(pts + offset * normal)
}

curb_geoms <- pmap(
  list(intervals$street_id, intervals$usable_from, intervals$usable_to, intervals$side),
  function(id, from, to, side) {
    offset_substring(street_coords[[id]], from, to,
                     ifelse(side == "left", curb_offset, -curb_offset))
  }
)

curbs <- intervals %>%
  select(parking_type = rule, n_spots, usable_length, borough) %>%
  st_sf(geometry = st_sfc(curb_geoms, crs = crs_mtm))

paid_spots <- paid %>%
  transmute(parking_type = "Paid", n_spots = 1, usable_length = spot_length)

parking_spots <- bind_rows(curbs, paid_spots) %>%
  st_transform(4326)

saveRDS(parking_spots, "data/parking_spots.rds")
