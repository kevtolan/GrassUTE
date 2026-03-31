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

# download political boundaries

landtrans <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Cadastral_PTTR_point_WM_v1_view/FeatureServer/0",
                           out_fields = c("span")) %>% st_transform(crs = 32145) %>% st_make_valid()

towns <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_towns_SP_v1/FeatureServer/0",
                                   out_fields = c("TOWNNAMEMC")) %>% st_transform(crs = 32145) %>% st_make_valid()

counties <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Boundary_BNDHASH_poly_counties_SP_v1/FeatureServer/0",
                              out_fields = c("CNTYNAME")) %>% st_transform(crs = 32145) %>% st_make_valid()


parcels <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Cadastral_VTPARCELS_poly_standardized_parcels_SP_v1/FeatureServer/0",
                             out_fields = c("SPAN","YEAR","OWNER1","OWNER2","ADDRGL1","ADDRGL2",
                                            "CITYGL","STGL","ZIPGL","E911ADDR","DESCPROP","LOCAPROP",
                                            "REAL_FLV","HSTED_FLV","NRES_FLV","LAND_LV")) %>%
                             st_transform(crs = 32145) %>% st_make_valid()


VT_soils <- get_spatial_layer("https://services1.arcgis.com/BkFxaEFNwHqX3tAw/ArcGIS/rest/services/FS_VCGI_OPENDATA_Geologic_SOAG_poly_SP_v1/FeatureServer/0",
                              out_fields = c("OBJECTID","PRIME")) %>% st_transform(crs = 32145) %>% st_make_valid() %>%
                              filter(!PRIME %in% c("NPSL","Not rated", "", " ")) %>%
                              st_simplify(dTolerance = 1, preserveTopology = T) %>%
                              rename(SoilID = OBJECTID,
                                     SoilClass = PRIME)

temp_zip22 <- tempfile(fileext = ".zip")
temp_zip16 <- tempfile(fileext = ".zip")
temp_dir <- tempdir()

download.file("https://s3.us-east-2.amazonaws.com/vtopendata-prd/Landcover/_Packaged_Zips/LandLandcov_Agriculture2022.zip",
                  destfile = temp_zip22, mode = "wb")
unzip(temp_zip22, exdir = temp_dir)

download.file("https://s3.us-east-2.amazonaws.com/vtopendata-prd/Landcover/_Packaged_Zips/LandLandcov_Agriculture2016.zip",
                  destfile = temp_zip16, mode = "wb")
unzip(temp_zip16, exdir = temp_dir)

VT_hay_pasture_22 <- st_read(dsn = paste0(temp_dir, "/LandLandcov_Agriculture2022/LandLandcov_Agriculture2022.gdb"),
                        layer = "LandLandcov_Agriculture2022_poly") %>%
  st_transform(crs = 32145) %>% st_make_valid() %>%
  dplyr::filter(Class != 'Crops') %>%
  mutate(area_ha = Shape_Area/10000,
         area_ac = Shape_Area*0.000247105,
         perim_area = Shape_Length/Shape_Area,
         sourceyear = '2022') %>%
  filter(perim_area <= 0.03,
         area_ac >= 10) %>%
  st_simplify(dTolerance = 1, preserveTopology = T) %>%
  st_intersection(towns) %>%
  st_intersection(counties) %>%
  dplyr::select(!c('gridcode'))

VT_hay_pasture_22_soils <- VT_hay_pasture_22 %>%
  st_intersection(VT_soils)

VT_hay_pasture_22_parcels <- VT_hay_pasture_22_soils %>%
  st_intersection(parcels) %>%
  filter(st_geometry_type(Shape) %in% c("POLYGON", "MULTIPOLYGON"))

# st_write(VT_hay_pasture_22_parcels,"~/R/Grasslab hab/VT_grasslands_2022.shp")

VT_hay_pasture_16 <- st_read(dsn = paste0(temp_dir, "/LandLandcov_Agriculture2016/LandLandcov_Agriculture2016.gdb"),
                             layer = "LandLandcov_Agriculture2016_poly") %>%
  st_transform(crs = 32145) %>% st_make_valid() %>%
  dplyr::filter(Class != 'Crops') %>%
  mutate(area_ha = Shape_Area/10000,
         area_ac = Shape_Area*0.000247105,
         perim_area = Shape_Length/Shape_Area,
         sourceyear = '2016') %>%
  filter(perim_area <= 0.03,
         area_ac >= 10) %>%
  st_simplify(dTolerance = 1, preserveTopology = T) %>%
  st_intersection(parcels) %>%
  st_intersection(towns) %>%
  st_intersection(counties) %>%
  dplyr::select(!c('gridcode'))

VT_hay_pasture_16_soils <- VT_hay_pasture_16 %>%
  st_intersection(VT_soils) %>% st_make_valid()

VT_hay_pasture_16_parcels <- VT_hay_pasture_16_soils %>%
  st_intersection(parcels) %>%
  filter(st_geometry_type(Shape) %in% c("POLYGON", "MULTIPOLYGON"))


# st_write(VT_hay_pasture_16_parcels,"~/R/Grasslab hab/VT_grasslands_2016.shp")


hay_16_union <- st_union(st_geometry(VT_hay_pasture_16))
hay_22_union <- st_union(st_geometry(VT_hay_pasture_22))

VT_lost_gland <- st_difference(VT_hay_pasture_16_soils, hay_22_union) %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON") %>%
  st_cast("POLYGON") %>%
  mutate(Status = "lost",
         Shape_Area = as.numeric(st_area(Shape)),
         area_ha = Shape_Area/10000,
         area_ac = Shape_Area*0.000247105) %>%
  filter(Shape_Area > 0)



VT_gained_gland <- st_difference(VT_hay_pasture_22_soils, hay_16_union) %>%
  mutate(Status = "gained") %>%
  mutate(Shape_Area = st_area(Shape),
         area_ha = Shape_Area/10000,
         area_ac = Shape_Area*0.000247105) %>%
  filter(Shape_Area > 0)
