
#############################################

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
  strata = 'c(
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
  )',
  orig_shp = shp
)

sst_converted <- EDABUtilities::convert_2d_longitude_gridded(data = here::here('01_inputs','oisst_monthly_1981_2026.nc'))

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


nday_below_8 <- dplyr::bind_rows(data)

##################################
#EPU

epu.shp <- here::here("01_inputs/EPU_NOESTUARIES.shp")

epu.data <- EDABUtilities::make_2d_deg_day_ts(
  data.in = sst_converted,
  var.name = "sst",
  statistic = "nd",
  ref.value = 8,
  type = "below",
  shp.file = epu.shp,
  area.names = c("MAB","GB","GOM","SS"),
  write.out = FALSE
)

epu_nday_below_8 <- dplyr::bind_rows(epu.data)
