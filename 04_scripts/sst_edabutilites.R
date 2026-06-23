## import SST files ----
sst_dir <- here::here('//nefscdata/EDAB_Datasets/OISST/V2/SOURCE/SST/')

nc_files <- list.files(
  path = sst_dir,
  pattern = "\\.nc$",
  full.names = TRUE,
  ignore.case = TRUE
)

### load files ----
## just doing 2 for testing
sst_stack <- terra::rast(nc_files[1:2])

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

## get shapefile info ----

## create stock shapefile from strata provided
shp <- terra::vect(here::here('01_inputs', 'BTS_STRATA.shp'))
epu.shp <- terra::vect(here::here('01_inputs', 'EPU_NOESTUARIES.shp'))

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
    statistic = "nd",
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
