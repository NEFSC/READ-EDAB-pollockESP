## import SST files ----
sst_dir <- here::here('//nefscdata/EDAB_Datasets/OISST/V2/SOURCE/SST/')
sst_dir <- here::here('01_inputs/SST/')

nc_files <- list.files(
  path = sst_dir,
  pattern = "\\.nc$",
  full.names = TRUE,
  ignore.case = TRUE
)

### load files ----
sst_stack <- terra::rast(nc_files[1:46])

### subset to sept-apr months ----
months <- which(stringr::str_detect(
  terra::time(sst_stack),
  "-(09|10|11|12|01|02|03|04)-"
))
sst_subset <- sst_stack |>
  terra::subset(months)

# check dates
# 1981 is not a full year
terra::time(sst_stack)

### create vector of years to assess ----
years_vector <- lubridate::year(as.Date(terra::time(sst_subset))) |>
  unique()
years_vector

## get shapefile info ----

## create stock shapefile from strata provided
shp <- terra::vect(here::here('01_inputs', 'BTS_STRATA.shp'))


## functions ----
create_shp <- function(strata, orig_shp = shp) {
  shp_out <- orig_shp[orig_shp$STRATUMA %in% strata, ] |>
    terra::aggregate()
  # add dummy attribute so it works with edab_utils
  shp_out$region <- "stock_area"

  return(shp_out)
}

species_shp <- create_shp(
  strata = c(
    "01130",
    "01140",
    "01150",
    "01160",
    "01170",
    "01180",
    "01190",
    "01200",
    "01210",
    "01220",
    "01230",
    "01240",
    "01250",
    "01260",
    "01270",
    "01280",
    "01290",
    "01300",
    "01360",
    "01370",
    "01380",
    "01390",
    "01400"
  ),
  orig_shp = shp
)

## loop through years to create output ----
## this is SLOW

nday_below_8 <- c()
for (i in unique(years_vector)) {
  this_dat <- sst_subset |>
    terra::subset(which(stringr::str_detect(
      terra::time(sst_subset),
      as.character(i)
    )))

  sst_converted <- EDABUtilities::convert_2d_longitude_gridded(data = this_dat)

  data <- EDABUtilities::make_2d_deg_day_ts(
    data.in = sst_converted,
    var.name = "sst",
    metric = "nd",
    ref.value = 8,
    type = "below",
    shp.file = species_shp,
    area.names = "stock_area",
    write.out = FALSE
  )

  nday_below_8 <- dplyr::bind_rows(
    nday_below_8,
    data[[1]] |>
      dplyr::mutate(year = i)
  )
}

## save output ----

write.csv(nday_below_8, here::here("03_outputs/nday_below_8.csv"))

## plot

nday_formatted <- nday_below_8_fullyears |>
  dplyr::rename(YEAR = year,
                DATA_VALUE = value,
                INDICATOR_NAME = statistic) |>
  dplyr::mutate(INDICATOR_NAME = 'nd_below_8c')

NEesp2::plt_indicator(data = nday_formatted, include_trends = TRUE)

## remove 1981, 2025, and 2026 

nday_below_8_fullyears <- read.csv(here::here("03_outputs/nday_below_8.csv")) |>
  dplyr::filter(!year %in% c(1981, 2025, 2026))


