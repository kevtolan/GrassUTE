# open grassGIS CLI
# enter 'open -na Rstudio' in console

library(tidyverse) # data manipulation, workflow
library(sf) # vector data
library(terra) # raster data
library(arcpullr) # download ESRI-hosted data
library(cli) # color outputs
library(beepr) # beep when error
library(mapview)
library(ks)
library(grid)
library(gridExtra)
library(classInt)
library(rgrass)
library(future)
library(furrr)

td <- tempdir()
out_dir <- '~/R/Grasslab hab/LiDAR_Loop'
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

# vt_blocks <- get_spatial_layer("https://services1.arcgis.com/d3OaJoSAh2eh6OA9/ArcGIS/rest/services/Vermont_Wildlife_Atlasing_Blocks/FeatureServer/0",
                               # out_fields = c("BLOCKNAME","QUADNAME","GEOUNITDES")) %>% st_transform(crs = 32145) # download grid
vt_blocks <- st_read('/Users/kevintolan/R/R_Spatial/BBA_Blocks/VT03_grid.shp') %>% st_transform(crs = 32145)

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
bb <- st_bbox(cropped_blocks) %>% st_as_sfc() %>% st_as_sf()

# subset_poly <- st_sfc(st_polygon(list(west_coords)), crs = st_crs(cropped_blocks))
# blocks_subset <- cropped_blocks[st_intersects(cropped_blocks, subset_poly, sparse = FALSE), ]
blocks_subset <- cropped_blocks[st_intersects(cropped_blocks, bb, sparse = FALSE), ]


all_out_files <- file.path(out_dir, paste0(blocks, "_open.tif"))

# 2. Check which ones DON'T exist (!)
missing_indices <- !file.exists(all_out_files)

blocks_to_process <- blocks[missing_indices]

blocks <- unique(blocks_subset$BLOCKNAME)

# blocks <- unique(cropped_blocks$BLOCKNAME)

plan(multisession, workers = 8)
future_walk(seq_along(blocks), function(counter) {
  i <- blocks[counter]
  out_file <- file.path(out_dir, paste0(i, "_open.tif"))

  if (file.exists(out_file)) {
    cli_alert_info("Skipping block: {.val {i}} (file already exists)")
    return(invisible(NULL))
  }

  blockbound <- aoi[aoi$BLOCKNAME == i, ]

  agri_block <- tryCatch(
    st_crop(VT_agr_22, blockbound),
    error = function(e) { sf::st_sf(geometry = sf::st_sfc(crs = st_crs(VT_agr_22))) })

  if (nrow(agri_block) == 0) {
    cli_alert_warning("No hay/pasture in block {.val {i}} — skipping")
    return(invisible(NULL))
  }

  tryCatch({
    dsm_cog_url_worker <- rast("/vsicurl/https://s3.us-east-2.amazonaws.com/vtopendata-prd/_Other/Projects/2023_Lidar/PreliminaryData/Central/Central_2023_35cm_DSMFR.tif")

    dsm_cog <- crop(dsm_cog_url_worker, blockbound) %>%
      project("EPSG:32145", method = "bilinear")

    unique_home <- file.path(td, paste0("grass_worker_", Sys.getpid()))
    dir.create(unique_home, showWarnings = FALSE, recursive = TRUE)

    loc <- initGRASS(gisBase = '/Applications/GRASS-8.2.app/Contents/Resources',
                     home = unique_home,
                     SG = dsm_cog,
                     override = TRUE)

    write_RAST(dsm_cog, vname = "rmask", flags = "overwrite")

    execGRASS("r.skyview", flags = c("o", "overwrite"),
              input = 'rmask', output = 'rmask.Open')

    u1 <- read_RAST('rmask.Open')

    hay_vect <- vect(agri_block)
    u1 <- mask(u1, hay_vect)
    names(u1) <- c("Open_decimal")

    writeRaster(u1, out_file, overwrite = TRUE)

    # elapsed_iteration <- difftime(Sys.time(), iteration_start_time, units = "mins")
    # cli_alert_success(
      # "Finished block {.val {i}} ({.val {counter}}/{.val {length(blocks)}}) | took {.val {round(elapsed_iteration, 2)}} mins")

    rm(u1, dsm_cog, dsm_cog_url_worker)
    gc(full = TRUE)

  }, error = function(e) {
    cli_alert_danger("***** FAILED ***** block: {.val {i}} @ {.val {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}} — {e$message}")
    # beep(sound = 1, expr = NULL)
  })
})



temp_zip22 <- tempfile(fileext = ".zip")
temp_dir <- tempdir()

openness_raw_dir <- '~/R/Grasslab hab/LiDAR_Loop'
openness_files <- list.files(openness_raw_dir, pattern = "\\.tif$", full.names = TRUE)

# download.file("https://s3.us-east-2.amazonaws.com/vtopendata-prd/Landcover/_Packaged_Zips/LandLandcov_Agriculture2022.zip",
#               destfile = temp_zip22, mode = "wb")
# unzip(temp_zip22, exdir = temp_dir)
#
# VT_hay_pasture_22 <- st_read(dsn = paste0(temp_dir, "/LandLandcov_Agriculture2022/LandLandcov_Agriculture2022.gdb"),
#                              layer = "LandLandcov_Agriculture2022_poly") %>%
#   st_transform(crs = 32145) %>% st_make_valid() %>%
#   dplyr::filter(Class != 'Crops') %>%
#   mutate(area_ha = Shape_Area/10000,
#          area_ac = Shape_Area*0.000247105,
#          perim_area = Shape_Length/Shape_Area,
#          sourceyear = '2022') %>%
#   filter(#perim_area <= 0.03,
#     area_ac >= 5) %>%
#   st_simplify(dTolerance = 1, preserveTopology = T) %>%
#   dplyr::select(!c('gridcode'))
#
#
# landtrans <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Cadastral_PTTR_point_WM_v1_view/FeatureServer/0",
#                                out_fields = c("addBuyrLoc"#, "addBuyrNam", "addSellLoc", "addSellNam","TownGlValu",
#                                               # "buyrAdjPrp", "buyEntNam", "buyFstNam", "buyLstNam","sUsePrExpl",
#                                               # "buyerState", "buyerStrt", "bUsePr", "bUsePrDesc","sUsePrDesc",
#                                               # "closeDate", "enrCrntUse", "LGTExDesc", "prTxExDesc","sUsePr",
#                                               # "sellAqDesc", "sellFstNam", "sellLstNam", "sellerSt",
#                                               )) %>% st_transform(crs = 32145) %>% st_make_valid()
#
# parcels <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Cadastral_VTPARCELS_poly_standardized_parcels_SP_v1/FeatureServer/0",
#                              out_fields = c("SPAN","YEAR","OWNER1","OWNER2","ADDRGL1","ADDRGL2",
#                                             "CITYGL","STGL","ZIPGL","E911ADDR","DESCPROP","LOCAPROP",
#                                             "REAL_FLV","HSTED_FLV","NRES_FLV","LAND_LV")) %>%
#   st_transform(crs = 32145) %>% st_make_valid()
#
# parcels_joined <- parcels %>%
#   mutate(has_landtrans = as.factor(as.integer(lengths(st_intersects(., landtrans)) > 0)))
# rm(parcels)
# rm(landtrans)
#
# VT_sig_soils <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Geologic_SOAG_poly_SP_v1/FeatureServer/0",
#                                   out_fields = c("OBJECTID","PRIME")) %>% st_transform(crs = 32145) %>% st_make_valid() %>%
#   filter(!PRIME %in% c("NPSL","Not rated", "", " ")) %>%
#   st_simplify(dTolerance = 1, preserveTopology = T) %>%
#   rename(SoilID = OBJECTID,
#          SoilClass = PRIME)
#
# VT_soils <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Geologic_SO_poly_SP_v1/FeatureServer/0") %>% st_transform(crs = 32145) %>% st_make_valid() %>%
#   st_simplify(dTolerance = 1, preserveTopology = T)
#
# VT_hay_pasture_22_soils1 <- VT_hay_pasture_22 %>% st_intersection(VT_soils) %>% st_make_valid()
#
# VT_hay_pasture_22_soils <- VT_hay_pasture_22_soils1 %>% st_intersection(VT_sig_soils) %>% st_make_valid()
#
# soils_clean   <- st_set_precision(VT_hay_pasture_22_soils, 1000) %>% st_make_valid()
# parcels_clean <- st_set_precision(parcels_joined, 1000) %>% st_make_valid()
#
# VT_hay_pasture_22_parcels <- soils_clean %>% st_intersection(parcels_joined) %>% st_buffer(0) %>%
#   st_intersection(parcels_joined %>% st_buffer(0)) %>%
#   st_make_valid() %>%
#   filter(st_geometry_type(Shape) %in% c("POLYGON", "MULTIPOLYGON")) %>%
#   mutate(rowID = row_number())

# saveRDS(VT_hay_pasture_22_parcels, "VT_hay_pasture_22_parcels.rds")
VT_hay_pasture_22_parcels <- readRDS('VT_hay_pasture_22_parcels.rds')

output_dir <- file.path(temp_dir, "masked_openness")
if(!dir.exists(output_dir)) dir.create(output_dir)

results_list <- list()
rm(r, r_masked, hay_sf, hay_vect, hay_vect_cropped, means)
gc()

for (i in seq_along(openness_files)) {

  # Wrap the heavy processing in tryCatch to handle the timeout
  result <- tryCatch({
    withTimeout({

      r <- rast(openness_files[i])
      hay_vect_cropped <- suppressWarnings(st_crop(VT_hay_pasture_22_parcels, ext(r)))

      if (nrow(hay_vect_cropped) == 0) {
        message(paste0("Skipping tile ", i, ": No overlapping parcels found."))
        return("skip") # Signal to skip
      }

      hay_vect_cropped <- st_make_valid(hay_vect_cropped) %>% .[!st_is_empty(.), ]

      if (inherits(st_geometry(hay_vect_cropped), "sfc_GEOMETRY")) {
        hay_vect_cropped <- st_collection_extract(hay_vect_cropped, "POLYGON")
      }

      means <- exact_extract(r, hay_vect_cropped, 'mean', progress = FALSE)

      results_list[[i]] <- hay_vect_cropped %>%
        st_drop_geometry() %>%
        mutate(Openness = means, tile_id = i)

      "success" # Signal completion

    }, timeout = 60) # 60 second limit

  }, TimeoutException = function(ex) {
    message(paste0("Skipping tile ", i, ": Timed out after 1 minute."))
    return("timeout")
  }, error = function(e) {
    message(paste0("Skipping tile ", i, " due to error: ", e$message))
    return("error")
  })

  # Handle cleanup and loop control based on the tryCatch result
  if (result %in% c("timeout", "skip", "error")) {
    rm(r, hay_vect_cropped) # Clean up if objects were created before fail
    next
  }

  rm(r, hay_vect_cropped, means)
  if (i %% 10 == 0) gc()

  message(paste0("Finished tile ", i, " of ", length(openness_files)))
}

# for (i in seq_along(openness_files)) {
#   # i <- 263
# # for (i in 434:length(openness_files)) {
# # for (i in 1:262) {
#   r <- rast(openness_files[i])
#
#   hay_vect_cropped <- suppressWarnings(st_crop(VT_hay_pasture_22_parcels, ext(r)))
#
#   if (nrow(hay_vect_cropped) == 0) {
#     message(paste0("Skipping tile ", i, ": No overlapping parcels found."))
#     next }
#
#   hay_vect_cropped <- st_make_valid(hay_vect_cropped) %>% .[!st_is_empty(.), ]
#
#     if (inherits(st_geometry(hay_vect_cropped), "sfc_GEOMETRY")) {
#     hay_vect_cropped <- st_collection_extract(hay_vect_cropped, "POLYGON") }
#
#   means <- exact_extract(r, hay_vect_cropped, 'mean', progress = FALSE)
#
#   results_list[[i]] <- hay_vect_cropped %>% st_drop_geometry() %>%
#     mutate(Open_decimal = means, tile_id = i)
#
#   rm(r, hay_vect_cropped, means)
#   if (i %% 10 == 0) gc()
#
#   message(paste0("Finished tile ", i, " of ", length(openness_files))) }


all_stats <- bind_rows(results_list) %>%
  group_by(rowID) %>%
  summarize(Openness = mean(Openness, na.rm = TRUE) * 0.9)

hay_pasture_stats <- VT_hay_pasture_22_parcels %>%
  left_join(all_stats, by = "rowID")

towns <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_towns_SP_v1/FeatureServer/0",
                           out_fields = c("TOWNNAMEMC")) %>% st_transform(crs = 32145) %>% st_make_valid()

counties <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_counties_SP_v1/FeatureServer/0",
                              out_fields = c("CNTYNAME")) %>% st_transform(crs = 32145) %>% st_make_valid()

hay_pasture_stats_towns <- hay_pasture_stats %>%
  st_intersection(towns) %>%
  st_intersection(counties)

# saveRDS(hay_pasture_stats_towns, "hay_pasture_stats_towns.RDS")


hay_polys <- hay_pasture_stats_towns[st_is(hay_pasture_stats_towns, c("POLYGON", "MULTIPOLYGON")), ] %>%
  st_cast("MULTIPOLYGON") %>%
  as.data.frame() %>%
  st_as_sf()


hay_list <- split(hay_polys, hay_polys$CNTYNAME)

# prefix <- "~/R/Grasslab hab/Combined_open/hay_polys"

# timestamp <- format(Sys.time(), "%Y%m%d_%H%M")

lapply(names(hay_list), function(county) {

  file_name <- paste0("~/R/Grasslab hab/Combined_open/hay_polys_", county,".shp")

  hay_list[[county]]$Shp__Ar <- round(as.numeric(hay_list[[county]]$Shape_Area), 2)

  st_write(hay_list[[county]], file_name, delete_dsn = TRUE, append = FALSE)
})




