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
         area_ac >= 1) %>%
  st_simplify(dTolerance = 1, preserveTopology = T) %>%
  dplyr::select(!c('gridcode'))


landtrans <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Cadastral_PTTR_point_WM_v1_view/FeatureServer/0",
                               out_fields = c("addBuyrLoc", "addBuyrNam", "addSellLoc", "addSellNam",
                                              "buyrAdjPrp", "buyEntNam", "buyFstNam", "buyLstNam",
                                              "buyerState", "buyerStrt", "bUsePr", "bUsePrDesc",
                                              "closeDate", "enrCrntUse", "LGTExDesc", "prTxExDesc",
                                              "sellAqDesc", "sellFstNam", "sellLstNam", "sellerSt",
                                              "sUsePr", "sUsePrDesc", "sUsePrExpl", "TownGlValu")) %>% st_transform(crs = 32145) %>% st_make_valid()

parcels <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Cadastral_VTPARCELS_poly_standardized_parcels_SP_v1/FeatureServer/0",
                             out_fields = c("SPAN","YEAR","OWNER1","OWNER2","ADDRGL1","ADDRGL2",
                                            "CITYGL","STGL","ZIPGL","E911ADDR","DESCPROP","LOCAPROP",
                                            "REAL_FLV","HSTED_FLV","NRES_FLV","LAND_LV")) %>%
  st_transform(crs = 32145) %>% st_make_valid()

parcels_joined <- parcels %>%
  mutate(has_landtrans = as.factor(as.integer(lengths(st_intersects(., landtrans)) > 0)))

VT_soils <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Geologic_SOAG_poly_SP_v1/FeatureServer/0",
                              out_fields = c("OBJECTID","PRIME")) %>% st_transform(crs = 32145) %>% st_make_valid() %>%
  filter(!PRIME %in% c("NPSL","Not rated", "", " ")) %>%
  st_simplify(dTolerance = 1, preserveTopology = T) %>%
  rename(SoilID = OBJECTID,
         SoilClass = PRIME)


VT_hay_pasture_22_soils <- VT_hay_pasture_22 %>% st_intersection(VT_soils) %>% st_make_valid()

VT_hay_pasture_22_parcels <- VT_hay_pasture_22_soils %>% st_intersection(parcels_joined) %>%
  filter(st_geometry_type(Shape) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  mutate(rowID = row_number())


output_dir <- file.path(temp_dir, "masked_openness")
  if(!dir.exists(output_dir)) dir.create(output_dir)

results_list <- list()

for (i in seq_along(openness_files)) {

  r_path <- openness_files[i]
  r <- rast(r_path)

  hay_vect_cropped <- st_crop(VT_hay_pasture_22_parcels,r)
  hay_vect <- vect(hay_vect_cropped)



  if(crs(r) != crs(hay_vect)) {
    r <- project(r, crs(hay_vect))
  }

  r_masked <- crop(r, hay_vect) %>%
    mask(hay_vect)

  stats <- terra::extract(r_masked, hay_vect, fun = mean, na.rm = TRUE)

  hay_stats <- cbind(as.data.frame(hay_vect), Open_decimal = stats$Open_decimal)

  results_list[[i]] <- hay_stats

  out_name <- paste0("masked_", basename(r_path))
  writeRaster(r_masked, file.path(output_dir, out_name), overwrite = TRUE)

  rm(r, r_masked, stats,hay_stats)
  gc()

  message(paste0("Finished tile ", i, " of ", length(openness_files)))
}


all_stats <- bind_rows(results_list) %>%
  group_by(rowID) %>%
  summarize(mean_openness = mean(Open_decimal, na.rm = TRUE) * 0.9)

hay_pasture_stats <- VT_hay_pasture_22_parcels %>%
  left_join(all_stats, by = "rowID")


mapview(hay_pasture_stats[hay_pasture_stats$mean_openness > 0.8,] %>% filter(!is.na(mean_openness)), zcol = "mean_openness")








towns <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_towns_SP_v1/FeatureServer/0",
                                   out_fields = c("TOWNNAMEMC")) %>% st_transform(crs = 32145) %>% st_make_valid()

counties <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_counties_SP_v1/FeatureServer/0",
                              out_fields = c("CNTYNAME")) %>% st_transform(crs = 32145) %>% st_make_valid()



hay_pasture_stats_towns <- hay_pasture_stats %>%
  # st_transform(crs = 32145) %>% st_make_valid() %>%
  # filter(perim_area <= 0.03,
  #        area_ac >= 10) %>%
  st_intersection(towns) %>%
  st_intersection(counties)


