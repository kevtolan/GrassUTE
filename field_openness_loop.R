# open grassGIS CLI
# enter 'open -na Rstudio' in console

library(tidyverse) # data manipulation, workflow
library(sf) # vector data
library(terra) # raster data
library(arcpullr) # download ESRI-hosted data
library(cli) # color outputs
library(beepr) # beep when error
library(ggmap)
library(terra)
library(mapview)
library(leaflet)
library(tmap)
library(leafem)
library(sp)
library(ggspatial)
library(raster)
library(ks)
library(grid)
library(gridExtra)
library(classInt)
library(rnaturalearth)
library(rgrass)
library(cowplot)

td <- tempdir()
out_dir <- '~/R/Grasslab hab/LiDAR_Loop'
  if(!dir.exists(out_dir)) dir.create(out_dir)

temp_zip22 <- tempfile(fileext = ".zip")

download.file("https://s3.us-east-2.amazonaws.com/vtopendata-prd/Landcover/_Packaged_Zips/LandLandcov_Agriculture2022.zip",
              destfile = temp_zip22, mode = "wb")
unzip(temp_zip22, exdir = td)

VT_agr_22 <- st_read(dsn = paste0(td, "/LandLandcov_Agriculture2022/LandLandcov_Agriculture2022.gdb"),
                             layer = "LandLandcov_Agriculture2022_poly") %>%
  st_transform(crs = 32145) %>% st_make_valid() %>%
  # dplyr::filter(Class != 'Crops') %>%
  # mutate(area_ha = Shape_Area/10000,
         # area_ac = Shape_Area*0.000247105,
         # perim_area = Shape_Length/Shape_Area,
         # sourceyear = '2022') %>%
  # filter(perim_area <= 0.03,
         # area_ac >= 10) %>%
  st_simplify(dTolerance = 1, preserveTopology = T) %>%
  dplyr::select(!c('gridcode'))

vt_blocks <- get_spatial_layer("https://services1.arcgis.com/d3OaJoSAh2eh6OA9/ArcGIS/rest/services/Vermont_Wildlife_Atlasing_Blocks/FeatureServer/0",
                               out_fields = c("BLOCKNAME","QUADNAME","GEOUNITDES")) %>% st_transform(crs = 32145) # download grid

aoi <- vt_blocks
files <- list.files(out_dir, pattern = "\\.tif$")
existing_blocks <- gsub("combined_sf_|\\.geojson", "", files)
aoi <- aoi[order(!(aoi$BLOCKNAME %in% existing_blocks)), ] # sort existing files to top

# dsm_cog_url_north <- rast("/vsicurl/https://s3.us-east-2.amazonaws.com/vtopendata-prd/_Other/Projects/2023_Lidar/PreliminaryData/Northern/Northern_2023_35cm_DSMFR.tif")
dsm_cog_url_central <- rast("/vsicurl/https://s3.us-east-2.amazonaws.com/vtopendata-prd/_Other/Projects/2023_Lidar/PreliminaryData/Central/Central_2023_35cm_DSMFR.tif")
# dsm_cog_url_south <- rast("/vsicurl/https://s3.us-east-2.amazonaws.com/vtopendata-prd/_Other/Projects/2023_Lidar/PreliminaryData/Southern/Southern_2023_35cm_DSMFR.tif")

dsm_cog_url <- dsm_cog_url_central
raster_extent <- ext(dsm_cog_url)
bbox_poly <- as.polygons(raster_extent, crs = crs(dsm_cog_url)) %>% project("EPSG:32145")
cropped_blocks <- st_crop(vt_blocks, bbox_poly)
bb <- st_bbox(cropped_blocks)

width <- bb[["xmax"]] - bb[["xmin"]]
western_cutoff <- bb[["xmin"]] + (width * 0.25)

west_coords <- matrix(c(
  bb[["xmin"]], bb[["ymin"]],
  western_cutoff, bb[["ymin"]],
  western_cutoff, bb[["ymax"]],
  bb[["xmin"]], bb[["ymax"]],
  bb[["xmin"]], bb[["ymin"]]),
  ncol = 2, byrow = TRUE)

subset_poly <- st_sfc(st_polygon(list(west_coords)), crs = st_crs(cropped_blocks))

blocks_subset <- cropped_blocks[st_intersects(cropped_blocks, subset_poly, sparse = FALSE), ]

blocks <- unique(blocks_subset$BLOCKNAME)
total_blocks <- length(blocks)
processed_count <- 0
skipped_count <- 0

script_start_time <- Sys.time()

for (counter in seq_along(blocks)) {
  i <- blocks[counter]
  iteration_start_time <- Sys.time()
  out_file <- file.path(out_dir, paste0(i, "_open.tif"))

  if (file.exists(out_file)) {
    cli_alert_info("Skipping block: {.val {i}} (file already exists)")
    skipped_count <- skipped_count + 1
    next }

  blockbound <- aoi[aoi$BLOCKNAME == i, ]

  agri_block <- tryCatch(
    st_crop(VT_agr_22, blockbound),
    error = function(e) { sf::st_sf(geometry = sf::st_sfc(crs = st_crs(VT_hay_pasture_22))) })

  if (nrow(agri_block) == 0) {
    cli_alert_warning("No hay/pasture in block {.val {i}} — skipping")
    skipped_count <- skipped_count + 1
    next
  }

  tryCatch({
    blockbound <- aoi[aoi$BLOCKNAME == i, ]

dsm_cog <- tryCatch(
  crop(dsm_cog_url, blockbound) %>% project("EPSG:32145", method = "bilinear"),
  error = function(e) {
    cli_alert_danger("DSM crop failed for block {.val {i}}: {e$message}")
    stop(e)
  })

loc <- initGRASS(gisBase = '/Applications/GRASS-8.2.app/Contents/Resources',
                 home = td,
                 SG = dsm_cog,
                 override = TRUE)

write_RAST(dsm_cog, vname = "rmask", flags = "overwrite")

execGRASS("r.skyview", flags=c("o","overwrite"),
          input= 'rmask', output = 'rmask.Open')

u1 <- read_RAST('rmask.Open')

hay_vect <- vect(agri_block)
u1 <- mask(u1, hay_vect)

names(u1) <- c("Open_decimal")

writeRaster(u1, paste0(out_dir,"/",i,"_open.tif"), overwrite = T)

processed_count <- processed_count + 1
now <- Sys.time()

elapsed_iteration <- difftime(now, iteration_start_time, units = "mins")
elapsed_session <- difftime(now, script_start_time, units = "mins")
avg_time_per_block <- elapsed_session / processed_count
remaining_blocks <- total_blocks - counter
eta_mins <- as.numeric(avg_time_per_block) * remaining_blocks

eta_label <- if (eta_mins > 60) {
  paste(round(eta_mins / 60, 1), "hours")
} else {
  paste(round(eta_mins, 1), "mins")
}

total_label <- if(elapsed_session > 60) {
  paste(round(as.numeric(elapsed_session)/60, 2), "hours")
} else {
  paste(round(elapsed_session, 2), "mins") }

cli_alert_success(
  "Finished block {.val {i}} {.val {counter}}/{.val {total_blocks}} @ {.val {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}} | \\
       This block took {.val {round(elapsed_iteration, 2)}} mins")

cli_alert_success(
  "Total time: {.val {total_label}} | \\
       Time remaining: {.val {eta_label}}")

rm(u1,dsm_cog)

gc(full = TRUE)
tmpFiles(remove = TRUE)

  }, error = function(e) {
    cli_alert_danger("***** FAILED ***** block: {.val {i}}  @ {.val {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}} {e$message}")
    beep(sound = 1, expr = NULL) }) # beep after error; optional
}


