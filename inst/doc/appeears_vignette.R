## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# load the library
library(ncdf4)
library(terra)
library(sf)
library(appeears)
library(dplyr)
library(ggplot2)
library(patchwork)

# load demo data

r_polygon <- terra::rast(
  file.path(system.file(package = "appeears"),"extdata/polygon/MCD12Q2.006_Greenup_0_doy2010001_aid0001.tif")
  )

r_raster <- terra::rast(
  file.path(system.file(package = "appeears"),"extdata/raster/MCD12Q2.006_Greenup_0_doy2010001_aid0001.tif")
  )

time_series <- read.table(
  file.path(system.file(package = "appeears"),"extdata/time_series/time-series-MCD43A4-061-results.csv"),
  header = TRUE,
  sep = ","
  )

## ----eval = FALSE-------------------------------------------------------------
#  library(appeears)
#  
#  # set a key to the keychain
#  rs_set_key(
#    user = "earth_data_user",
#    password = "XXXXXXXXXXXXXXXXXXXXXX"
#    )
#  
#  # you can retrieve the password using
#  rs_get_key(user = "earth_data_user")
#  
#  # the output should be the key you provided
#  # "XXXXXXXXXXXXXXXXXXXXXX"

## ----eval = FALSE-------------------------------------------------------------
#  # request the current token
#  token <- rs_login(user = "earth_data_user")
#  
#  # invalidate the current session
#  rs_logout(token)

## -----------------------------------------------------------------------------
# list all product information
products <- rs_products()

# print the start of all products with their versions
head(products$ProductAndVersion)

# list all layers for a particular
# product
layers <- rs_layers(
  product = "MCD12Q2.006"
)

head(layers)

## ----eval = FALSE-------------------------------------------------------------
#  # Load the library
#  library(appeears)
#  
#  # list all products
#  rs_products()
#  
#  # list layers of the MOD11A2.061 product
#  rs_layers("MOD11A2.061")
#  
#  df <- data.frame(
#    task = "time_series",
#    subtask = "US-Ha1",
#    latitude = 42.5378,
#    longitude = -72.1715,
#    start = "2010-01-01",
#    end = "2010-12-31",
#    product = "MCD43A4.061",
#    layer = c("Nadir_Reflectance_Band3","Nadir_Reflectance_Band4")
#  )
#  
#  # build the area based request/task
#  # rename the task name so data will
#  # be saved in the "point" folder
#  # as defined by the task name
#  df$task <- "point"
#  task <- rs_build_task(df = df)
#  
#  # request the task to be executed
#  rs_request(
#    request = task,
#    user = "earth_data_user",
#    transfer = TRUE,
#    path = "~/some_path",
#    verbose = TRUE
#  )

## ----eval = FALSE-------------------------------------------------------------
#  # read in data
#  time_series <- read.table(
#    "~/some_path/time_series/time-series-MCD43A4-061-results.csv",
#    header = TRUE,
#    sep = ","
#    )

## ----warning=FALSE------------------------------------------------------------
# convert band 3 and 4 to NDVI
time_series <- time_series |>
  mutate(
    Date = as.Date(Date),
    NDVI = (MCD43A4_061_Nadir_Reflectance_Band4 - MCD43A4_061_Nadir_Reflectance_Band3)/
      (MCD43A4_061_Nadir_Reflectance_Band4 + MCD43A4_061_Nadir_Reflectance_Band3)
  )

# screen for quality control
time_series <- time_series |>
  mutate(
    NDVI = ifelse(MCD43A4_061_BRDF_Albedo_Band_Mandatory_Quality_Band4 == 255 |
                    MCD43A4_061_BRDF_Albedo_Band_Mandatory_Quality_Band3 == 255,
                  NA, NDVI)
  )

# plot the time series
ggplot(time_series) +
  geom_point(
    aes(
      Date,
      NDVI
    )
  ) +
  theme_bw()


## -----------------------------------------------------------------------------
# load the required libraries
library(appeears)
library(sf)
library(dplyr)
library(ggplot2)

df <- data.frame(
  task = "time_series",
  subtask = "subtask",
  latitude = 42.5378,
  longitude = -72.1715,
  start = "2010-01-01",
  end = "2010-12-31",
  product = "MCD12Q2.006",
  layer = c("Greenup")
)

# load the north carolina demo data
# included in the {sf} package
# and only retain Camden county
roi <- st_read(system.file("gpkg/nc.gpkg", package="sf"), quiet = TRUE) |>
  filter(
    NAME == "Camden"
  )

## ----eval = FALSE-------------------------------------------------------------
#  # build the area based request/task
#  # rename the task name so data will
#  # be saved in the "polygon" folder
#  # as defined by the task name
#  df$task <- "polygon"
#  task <- rs_build_task(
#    df = df,
#    roi = roi,
#    format = "geotiff"
#  )
#  
#  # request the task to be executed
#  rs_request(
#    request = task,
#    user = "earth_data_user",
#    transfer = TRUE,
#    path = "~/some_path",
#    verbose = TRUE
#  )

## ----eval=FALSE---------------------------------------------------------------
#  library(terra)
#  r_polygon <- terra::rast(
#    file.path("~/some_path","polygon/MCD12Q2.006_Greenup_0_doy2010001_aid0001.tif")
#    )

## -----------------------------------------------------------------------------
# convert to data frame for plotting
# with ggplot2, otherwise use the
# tidyterra package and geom_spatrast()
df_polygon <- r_polygon |>
  as.data.frame(xy=TRUE)

# convert incremental values (days since Jan 1, 1970
# to DOY)
df_polygon <- df_polygon |>
  mutate(
    DOY =  as.numeric(
      format(
        as.Date("1970-01-01") + MCD12Q2.006_Greenup_0_doy2010001_aid0001, "%j"
        )
      )
  ) |>
  filter(
    DOY < 180
  )

head(df_polygon)

ggplot() +
  geom_raster(
    data = df_polygon,
    aes(
      x,
      y,
      fill = DOY
    )
  ) +
  labs(
    x = "",
    y = ""
  ) +
  scale_fill_viridis_c() +
  geom_sf(
    data = roi,
    fill = NA,
    colour = "red",
    lwd = 2
    ) +
  theme_bw()
  

## -----------------------------------------------------------------------------
# load the required libraries
library(terra)
library(ggplot2)
library(patchwork)

# create a SpatRaster ROI from the terra demo file
f <- system.file("ex/elev.tif", package="terra")
roi <- terra::rast(f)

## ----eval = FALSE-------------------------------------------------------------
#  
#  # build the area based request/task
#  # rename the task name so data will
#  # be saved in the "raster" folder
#  # as defined by the task name
#  df$task <- "raster"
#  task <- rs_build_task(
#    df = df,
#    roi = roi,
#    format = "geotiff"
#  )
#  
#  # request the task to be executed
#  rs_request(
#    request = task,
#    user = "earth_data_user",
#    transfer = TRUE,
#    path = "~/some_path",
#    verbose = TRUE
#  )

## ----eval=FALSE---------------------------------------------------------------
#  r_raster <- terra::rast(
#    file.path("~/some_path","raster/MCD12Q2.006_Greenup_0_doy2010001_aid0001.tif")
#    )

## -----------------------------------------------------------------------------
# convert to data frame for plotting
# with ggplot2, otherwise use the
# tidyterra package and geom_spatrast()

df_raster <- r_raster |>
  as.data.frame(xy=TRUE)

# convert incremental values (days since Jan 1, 1970
# to DOY)
df_raster <- df_raster |>
  mutate(
    DOY =  as.numeric(
      format(
        as.Date("1970-01-01") + MCD12Q2.006_Greenup_0_doy2010001_aid0001, "%j"
        )
      )
  ) |>
  filter(
    DOY < 180
  )

df_source <- roi |>
  as.data.frame(xy=TRUE)

p <- ggplot() +
  geom_raster(
    data = df_source,
    aes(
      x,
      y,
      fill = elevation
    )
  ) +
  labs(
    x = "",
    y = ""
  ) +
  scale_fill_viridis_c() +
  theme_bw()

p2 <- ggplot() +
  geom_raster(
    data = df_raster,
    aes(
      x,
      y,
      fill = DOY
    )
  ) +
  labs(
    x = "",
    y = ""
  ) +
  scale_fill_viridis_c() +
  theme_bw()

# patchwork side by side plot
p | p2


