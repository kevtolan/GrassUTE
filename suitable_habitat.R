library(tidyverse)
library(sf)
library(terra)
library(mapview)
library(tmap)
library(leafem)
library(sp)
library(ks)
library(exactextractr)
library(raster)
library(ggspatial)
library(ggmap)
library(gganimate)
library(timetk)
library(magick)
library(sf)
library(cowplot)
library(arcpullr)
library(ggspatial)
library(cowplot)
library(patchwork)
# bobo_conservation_plannding <- st_read('~/QGIS/EAME/BOBO_Opp_Map.shp')

temp_zip22 <- tempfile(fileext = ".zip")
temp_dir <- tempdir()

openness_raw_dir <- '~/R/Grasslab hab/LiDAR_Loop'
openness_files <- list.files(openness_raw_dir, pattern = "\\.tif$", full.names = TRUE)

download.file("https://s3.us-east-2.amazonaws.com/vtopendata-prd/Landcover/_Packaged_Zips/LandLandcov_Agriculture2022.zip",
              destfile = temp_zip22, mode = "wb")
unzip(temp_zip22, exdir = temp_dir)

VT_hay_pasture_22 <- st_read(dsn = paste0(temp_dir, "/LandLandcov_Agriculture2022/LandLandcov_Agriculture2022.gdb"),
                             layer = "LandLandcov_Agriculture2022_poly") %>%
  st_transform(crs = 32145) %>% st_make_valid() %>%
  dplyr::filter(Class != 'Crops') %>%
  mutate(area_ha = Shape_Area/10000,
         area_ac = Shape_Area*0.000247105,
         perim_area = Shape_Length/Shape_Area,
         sourceyear = '2022') %>%
  filter(#perim_area <= 0.03,
    area_ac >= 5) %>%
  st_simplify(dTolerance = 1, preserveTopology = T) %>%
  dplyr::select(!c('gridcode'))


landtrans <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Cadastral_PTTR_point_WM_v1_view/FeatureServer/0",
                               out_fields = c("addBuyrLoc", "addBuyrNam", "addSellLoc", "addSellNam","TownGlValu",
                                              # "buyrAdjPrp", "buyEntNam", "buyFstNam", "buyLstNam","sUsePrExpl",
                                              # "buyerState", "buyerStrt", "bUsePr", "bUsePrDesc","sUsePrDesc",
                                              # "closeDate", "enrCrntUse", "LGTExDesc", "prTxExDesc","sUsePr",
                                              # "sellAqDesc", "sellFstNam", "sellLstNam", "sellerSt",
                                              )) %>% st_transform(crs = 32145) %>% st_make_valid()

parcels <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Cadastral_VTPARCELS_poly_standardized_parcels_SP_v1/FeatureServer/0",
                             out_fields = c("SPAN","YEAR","OWNER1","OWNER2","ADDRGL1","ADDRGL2",
                                            "CITYGL","STGL","ZIPGL","E911ADDR","DESCPROP","LOCAPROP",
                                            "REAL_FLV","HSTED_FLV","NRES_FLV","LAND_LV")) %>%
  st_transform(crs = 32145) %>% st_make_valid()

parcels_joined <- parcels %>%
  mutate(has_landtrans = as.factor(as.integer(lengths(st_intersects(., landtrans)) > 0)))
rm(parcels)
rm(landtrans)

VT_sig_soils <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Geologic_SOAG_poly_SP_v1/FeatureServer/0",
                                  out_fields = c("OBJECTID","PRIME")) %>% st_transform(crs = 32145) %>% st_make_valid() %>%
  filter(!PRIME %in% c("NPSL","Not rated", "", " ")) %>%
  st_simplify(dTolerance = 1, preserveTopology = T) %>%
  rename(SoilID = OBJECTID,
         SoilClass = PRIME)

VT_soils <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Geologic_SO_poly_SP_v1/FeatureServer/0") %>% st_transform(crs = 32145) %>% st_make_valid() %>%
  st_simplify(dTolerance = 1, preserveTopology = T)

VT_hay_pasture_22_soils1 <- VT_hay_pasture_22 %>% st_intersection(VT_soils) %>% st_make_valid()

VT_hay_pasture_22_soils <- VT_hay_pasture_22_soils1 %>% st_intersection(VT_sig_soils) %>% st_make_valid()

soils_clean   <- st_set_precision(VT_hay_pasture_22_soils, 1000) %>% st_make_valid()
parcels_clean <- st_set_precision(parcels_joined, 1000) %>% st_make_valid()

VT_hay_pasture_22_parcels <- soils_clean %>% st_intersection(parcels_joined) %>% st_buffer(0) %>%
  st_intersection(parcels_joined %>% st_buffer(0)) %>%
  st_make_valid() %>%
  filter(st_geometry_type(Shape) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  mutate(rowID = row_number())

saveRDS(VT_hay_pasture_22_parcels, "VT_hay_pasture_22_parcels.rds")
VT_hay_pasture_22_parcels <- readRDS('VT_hay_pasture_22_parcels.rds')

output_dir <- file.path(temp_dir, "masked_openness")
if(!dir.exists(output_dir)) dir.create(output_dir)

results_list <- list()
rm(r, r_masked, hay_sf, hay_vect, hay_vect_cropped, means)
gc()


for (i in seq_along(openness_files)) {
  # i <- 263
# for (i in 264:length(openness_files)) {
# for (i in 1:262) {
  r <- rast(openness_files[i])

  hay_vect_cropped <- suppressWarnings(st_crop(VT_hay_pasture_22_parcels, ext(r)))

  if (nrow(hay_vect_cropped) == 0) {
    message(paste0("Skipping tile ", i, ": No overlapping parcels found."))
    next }

  hay_vect_cropped <- st_make_valid(hay_vect_cropped) %>% .[!st_is_empty(.), ]

    if (inherits(st_geometry(hay_vect_cropped), "sfc_GEOMETRY")) {
    hay_vect_cropped <- st_collection_extract(hay_vect_cropped, "POLYGON") }

  means <- exact_extract(r, hay_vect_cropped, 'mean', progress = FALSE)

  results_list[[i]] <- hay_vect_cropped %>% st_drop_geometry() %>%
    mutate(Open_decimal = means, tile_id = i)

  rm(r, hay_vect_cropped, means)
  if (i %% 10 == 0) gc()

  message(paste0("Finished tile ", i, " of ", length(openness_files))) }


all_stats <- bind_rows(results_list) %>%
  group_by(rowID) %>%
  summarize(mean_openness = mean(Open_decimal, na.rm = TRUE) * 0.9)

hay_pasture_stats <- VT_hay_pasture_22_parcels %>%
  left_join(all_stats, by = "rowID")

towns <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_towns_SP_v1/FeatureServer/0",
                           out_fields = c("TOWNNAMEMC")) %>% st_transform(crs = 32145) %>% st_make_valid()

counties <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_counties_SP_v1/FeatureServer/0",
                              out_fields = c("CNTYNAME")) %>% st_transform(crs = 32145) %>% st_make_valid()

hay_pasture_stats_towns <- hay_pasture_stats %>%
  st_intersection(towns) %>%
  st_intersection(counties)

saveRDS(hay_pasture_stats_towns, "hay_pasture_stats_towns.RDS")


hay_polys <- hay_pasture_stats_towns[st_is(hay_pasture_stats_towns, c("POLYGON", "MULTIPOLYGON")), ] %>%
  st_cast("MULTIPOLYGON") %>%
  as.data.frame() %>%
  st_as_sf()


hay_list <- split(hay_polys, hay_polys$CNTYNAME)

# prefix <- "~/R/Grasslab hab/Combined_open/hay_polys"

timestamp <- format(Sys.time(), "%Y%m%d_%H%M")

lapply(names(hay_list), function(county) {

  file_name <- sprintf("%s_%s_%s.shp", "~/R/Grasslab hab/Combined_open/hay_polys", county, timestamp)

  st_write(hay_list[[county]], file_name, delete_dsn = TRUE)
})






