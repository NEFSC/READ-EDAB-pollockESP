
# 1. Define the path to your 'SST' folder
sst_dir <- here::here('//nefscdata/EDAB_Datasets/OISST/V2/SOURCE/SST/')

# 2. List all NetCDF files (.nc or .nc4) in that folder
# 'full.names = TRUE' ensures the complete file path is retained
nc_files <- list.files(path = sst_dir, 
                       pattern = "\\.nc$", 
                       full.names = TRUE, 
                       ignore.case = TRUE)

# Optional: Sort the files to ensure they are in chronological order (1981 to 2026)
nc_files <- sort(nc_files)

# 3. Load all files at once into a multi-layer SpatRaster
# terra::rast() automatically recognizes a vector of file paths and stacks them
sst_stack <- terra::rast(nc_files)


# 2. Extract the dates and explicitly force them to standard R Dates
all_dates <- as.Date(time(sst_stack))

# 3. Extract the month number (1 to 12) for each date
months_vector <- lubridate::month(all_dates)

# 4. Define your logical condition (September is 9, April is 4)
# We want months >= 9 OR months <= 4
sept_april_indices <- which(months_vector >= 9 | months_vector <= 4)

# 5. Subset the SpatRaster using those indices
sst_subset <- sst_stack[[sept_april_indices]]

# 6. Check your work
print(sst_subset)

# 2. Extract the years for each remaining layer
# Note: Because your season crosses the New Year (Sep-Apr), 
# decide if you want to group by calendar year, or "winter season" year.
# For standard calendar year grouping:
years_vector <- year(as.Date(time(sst_subset)))

# 3. Use tapp() to group by year and apply your calculation
# 'fun' can be your custom function, or an anonymous function like below:
yearly_days_below_8 <- terra::tapp(sst_subset, 
                            index = years_vector)

sst_converted <- EDABUtilities::convert_2d_longitude_gridded(data = sst_subset)

#############################################

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


data <- EDABUtilities::make_2d_deg_day_ts(
  data.in = test,
  var.name = "sst",
  statistic = "nd",
  ref.value = 8,
  type = "below",
  shp.file = species_shp,
  area.names = "stock_area",
  write.out = FALSE
)


nday_below_8 <- dplyr::bind_rows(data)

