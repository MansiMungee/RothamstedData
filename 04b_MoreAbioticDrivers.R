# constructing a time series of landcover - using na_interpolate for gaps
# LAT-LONG LOCATIONS OF THE FOUR SUCTION TRAP SITES
site_coords <- read.csv("./SuctionTrap_Coords.csv", header = TRUE)
coords <- as.data.frame.matrix(site_coords[,c(3,4)])
coord.dec = SpatialPoints(cbind(coords$Lng, coords$Lat), proj4string = CRS("+proj=longlat"))
coord.UTM <- spTransform(coord.dec, CRS("+init=epsg:32748"))



# calculate percentage cover of arable, grassland (improved + semi-natural), and built-upareas and garden
# calling these three classes as LC_Ar, LC_Gr, LC_Ur
# the unit in which you provide the buffer value should be in meteres when the raster itself is projected (that is it has lat/long associated)
func_proc_lcm <- function(raster_path, buffer_radius){
  jnk1 <- stack(raster_path)
  jnk2 <- raster::extract(jnk1, coord.UTM, buffer = buffer_radius)# based on documentation: LC_Ar = 3; LC_Gr = 4,5,6,7,8; LC_Ur = 20,21
  agg.func <- function(x){
    n = length(x)
    t <- table(x)
    p <- t/n * 100
  }
  jnk3 <- lapply(jnk2, agg.func)
  cols <- unique(unlist(sapply(jnk3, names)))
  for(ii in 1:length(jnk3)){
    missingnames <- setdiff(cols, names(jnk3[[ii]]))
    missingstuff <- rep(0, length(missingnames))
    names(missingstuff) <- missingnames
    jnk3[[ii]] <- c(jnk3[[ii]], missingstuff)
    jnk3[[ii]] <- data.frame(Names = names(jnk3[[ii]]), value = jnk3[[ii]])
    jnk3[[ii]] <- jnk3[[ii]][order(jnk3[[ii]]$Names),]
  }
  jnk4 <- list.cbind(jnk3)
  jnk4 <- jnk4[,c(2,4,6,8)]
  jnk4$Class <- rownames(jnk4)
  colnames(jnk4)[1:4] <- site_coords$Name
  jnk4$Class <- as.numeric(jnk4$Class)
  jnk4 <- jnk4[order(jnk4$Class),]
  return(jnk4)
}

lcm_1990_5km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/01_1km_DominantAggregateClass_1990/84c07c67-88a4-439a-a339-b0577afd3886/data/gb1990lcm1km_agg_dominant.tif", buffer_radius = 5000)
lcm_1990_10km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/01_1km_DominantAggregateClass_1990/84c07c67-88a4-439a-a339-b0577afd3886/data/gb1990lcm1km_agg_dominant.tif", buffer_radius = 10000)
lcm_1990_15km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/01_1km_DominantAggregateClass_1990/84c07c67-88a4-439a-a339-b0577afd3886/data/gb1990lcm1km_agg_dominant.tif", buffer_radius = 15000)
lcm_1990_25km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/01_1km_DominantAggregateClass_1990/84c07c67-88a4-439a-a339-b0577afd3886/data/gb1990lcm1km_agg_dominant.tif", buffer_radius = 25000)
lcm_1990_50km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/01_1km_DominantAggregateClass_1990/84c07c67-88a4-439a-a339-b0577afd3886/data/gb1990lcm1km_agg_dominant.tif", buffer_radius = 50000)
lcm_1990_75km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/01_1km_DominantAggregateClass_1990/84c07c67-88a4-439a-a339-b0577afd3886/data/gb1990lcm1km_agg_dominant.tif", buffer_radius = 75000)
lcm_1990_100km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/01_1km_DominantAggregateClass_1990/84c07c67-88a4-439a-a339-b0577afd3886/data/gb1990lcm1km_agg_dominant.tif", buffer_radius = 100000)

lcm_2000_5km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/02_1km_DominantAggregateClass_2000/b8b8a266-9162-40d8-98a6-f44178d31543/data/b8b8a266-9162-40d8-98a6-f44178d31543/LCM2000_GB_1K_DOM_AGG.tif", buffer_radius = 5000)
lcm_2000_10km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/02_1km_DominantAggregateClass_2000/b8b8a266-9162-40d8-98a6-f44178d31543/data/b8b8a266-9162-40d8-98a6-f44178d31543/LCM2000_GB_1K_DOM_AGG.tif", buffer_radius = 10000)
lcm_2000_15km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/02_1km_DominantAggregateClass_2000/b8b8a266-9162-40d8-98a6-f44178d31543/data/b8b8a266-9162-40d8-98a6-f44178d31543/LCM2000_GB_1K_DOM_AGG.tif", buffer_radius = 15000)
lcm_2000_25km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/02_1km_DominantAggregateClass_2000/b8b8a266-9162-40d8-98a6-f44178d31543/data/b8b8a266-9162-40d8-98a6-f44178d31543/LCM2000_GB_1K_DOM_AGG.tif", buffer_radius = 25000)
lcm_2000_50km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/02_1km_DominantAggregateClass_2000/b8b8a266-9162-40d8-98a6-f44178d31543/data/b8b8a266-9162-40d8-98a6-f44178d31543/LCM2000_GB_1K_DOM_AGG.tif", buffer_radius = 50000)
lcm_2000_75km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/02_1km_DominantAggregateClass_2000/b8b8a266-9162-40d8-98a6-f44178d31543/data/b8b8a266-9162-40d8-98a6-f44178d31543/LCM2000_GB_1K_DOM_AGG.tif", buffer_radius = 75000)
lcm_2000_100km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/02_1km_DominantAggregateClass_2000/b8b8a266-9162-40d8-98a6-f44178d31543/data/b8b8a266-9162-40d8-98a6-f44178d31543/LCM2000_GB_1K_DOM_AGG.tif", buffer_radius = 100000)


lcm_2007_5km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/03_1km_DominantAggregateClass_2007/c3723adb-1a8c-4b57-958b-1d610d2c37fe/data/c3723adb-1a8c-4b57-958b-1d610d2c37fe/lcm2007_dominant_aggregate_class_1km_gb.img", buffer_radius = 5000)
lcm_2007_10km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/03_1km_DominantAggregateClass_2007/c3723adb-1a8c-4b57-958b-1d610d2c37fe/data/c3723adb-1a8c-4b57-958b-1d610d2c37fe/lcm2007_dominant_aggregate_class_1km_gb.img", buffer_radius = 10000)
lcm_2007_15km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/03_1km_DominantAggregateClass_2007/c3723adb-1a8c-4b57-958b-1d610d2c37fe/data/c3723adb-1a8c-4b57-958b-1d610d2c37fe/lcm2007_dominant_aggregate_class_1km_gb.img", buffer_radius = 15000)
lcm_2007_25km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/03_1km_DominantAggregateClass_2007/c3723adb-1a8c-4b57-958b-1d610d2c37fe/data/c3723adb-1a8c-4b57-958b-1d610d2c37fe/lcm2007_dominant_aggregate_class_1km_gb.img", buffer_radius = 25000)
lcm_2007_50km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/03_1km_DominantAggregateClass_2007/c3723adb-1a8c-4b57-958b-1d610d2c37fe/data/c3723adb-1a8c-4b57-958b-1d610d2c37fe/lcm2007_dominant_aggregate_class_1km_gb.img", buffer_radius = 50000)
lcm_2007_75km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/03_1km_DominantAggregateClass_2007/c3723adb-1a8c-4b57-958b-1d610d2c37fe/data/c3723adb-1a8c-4b57-958b-1d610d2c37fe/lcm2007_dominant_aggregate_class_1km_gb.img", buffer_radius = 75000)
lcm_2007_100km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/03_1km_DominantAggregateClass_2007/c3723adb-1a8c-4b57-958b-1d610d2c37fe/data/c3723adb-1a8c-4b57-958b-1d610d2c37fe/lcm2007_dominant_aggregate_class_1km_gb.img", buffer_radius = 100000)


lcm_2015_5km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/04_1km_DominantAggregateClass_2015/711c8dc1-0f4e-42ad-a703-8b5d19c92247/data/lcm2015_gb_1km_dominant_aggregate_class.img", buffer_radius = 5000)
lcm_2015_10km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/04_1km_DominantAggregateClass_2015/711c8dc1-0f4e-42ad-a703-8b5d19c92247/data/lcm2015_gb_1km_dominant_aggregate_class.img", buffer_radius = 10000)
lcm_2015_15km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/04_1km_DominantAggregateClass_2015/711c8dc1-0f4e-42ad-a703-8b5d19c92247/data/lcm2015_gb_1km_dominant_aggregate_class.img", buffer_radius = 15000)
lcm_2015_25km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/04_1km_DominantAggregateClass_2015/711c8dc1-0f4e-42ad-a703-8b5d19c92247/data/lcm2015_gb_1km_dominant_aggregate_class.img", buffer_radius = 25000)
lcm_2015_50km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/04_1km_DominantAggregateClass_2015/711c8dc1-0f4e-42ad-a703-8b5d19c92247/data/lcm2015_gb_1km_dominant_aggregate_class.img", buffer_radius = 50000)
lcm_2015_75km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/04_1km_DominantAggregateClass_2015/711c8dc1-0f4e-42ad-a703-8b5d19c92247/data/lcm2015_gb_1km_dominant_aggregate_class.img", buffer_radius = 75000)
lcm_2015_100km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/04_1km_DominantAggregateClass_2015/711c8dc1-0f4e-42ad-a703-8b5d19c92247/data/lcm2015_gb_1km_dominant_aggregate_class.img", buffer_radius = 100000)

lcm_2017_5km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/05_1km_DominantAggregateClass_2017/gb2017lcm1km_dominant_aggregate.tif", buffer_radius = 5000)
lcm_2017_10km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/05_1km_DominantAggregateClass_2017/gb2017lcm1km_dominant_aggregate.tif", buffer_radius = 10000)
lcm_2017_15km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/05_1km_DominantAggregateClass_2017/gb2017lcm1km_dominant_aggregate.tif", buffer_radius = 15000)
lcm_2017_25km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/05_1km_DominantAggregateClass_2017/gb2017lcm1km_dominant_aggregate.tif", buffer_radius = 25000)
lcm_2017_50km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/05_1km_DominantAggregateClass_2017/gb2017lcm1km_dominant_aggregate.tif", buffer_radius = 50000)
lcm_2017_75km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/05_1km_DominantAggregateClass_2017/gb2017lcm1km_dominant_aggregate.tif", buffer_radius = 75000)
lcm_2017_100km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/05_1km_DominantAggregateClass_2017/gb2017lcm1km_dominant_aggregate.tif", buffer_radius = 100000)

lcm_2018_5km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/06_1km_DominantAggregateClasses_2018/gb2018lcm1km_dominant_aggregate.tif", buffer_radius = 5000)
lcm_2018_10km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/06_1km_DominantAggregateClasses_2018/gb2018lcm1km_dominant_aggregate.tif", buffer_radius = 10000)
lcm_2018_15km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/06_1km_DominantAggregateClasses_2018/gb2018lcm1km_dominant_aggregate.tif", buffer_radius = 15000)
lcm_2018_25km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/06_1km_DominantAggregateClasses_2018/gb2018lcm1km_dominant_aggregate.tif", buffer_radius = 25000)
lcm_2018_50km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/06_1km_DominantAggregateClasses_2018/gb2018lcm1km_dominant_aggregate.tif", buffer_radius = 50000)
lcm_2018_75km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/06_1km_DominantAggregateClasses_2018/gb2018lcm1km_dominant_aggregate.tif", buffer_radius = 75000)
lcm_2018_100km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/06_1km_DominantAggregateClasses_2018/gb2018lcm1km_dominant_aggregate.tif", buffer_radius = 100000)

lcm_2019_5km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/07_1km_DominantAggregateClasses_2019/gb2019lcm1km_dominant_aggregate.tif", buffer_radius = 5000)
lcm_2019_10km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/07_1km_DominantAggregateClasses_2019/gb2019lcm1km_dominant_aggregate.tif", buffer_radius = 10000)
lcm_2019_15km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/07_1km_DominantAggregateClasses_2019/gb2019lcm1km_dominant_aggregate.tif", buffer_radius = 15000)
lcm_2019_25km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/07_1km_DominantAggregateClasses_2019/gb2019lcm1km_dominant_aggregate.tif", buffer_radius = 25000)
lcm_2019_50km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/07_1km_DominantAggregateClasses_2019/gb2019lcm1km_dominant_aggregate.tif", buffer_radius = 50000)
lcm_2019_75km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/07_1km_DominantAggregateClasses_2019/gb2019lcm1km_dominant_aggregate.tif", buffer_radius = 75000)
lcm_2019_100km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/07_1km_DominantAggregateClasses_2019/gb2019lcm1km_dominant_aggregate.tif", buffer_radius = 100000)

lcm_2020_5km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/08_1km_DominantAggregatedClasses_2020/gb2020lcm1km_dominant_aggregate.tif", buffer_radius = 5000)
lcm_2020_10km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/08_1km_DominantAggregatedClasses_2020/gb2020lcm1km_dominant_aggregate.tif", buffer_radius = 10000)
lcm_2020_15km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/08_1km_DominantAggregatedClasses_2020/gb2020lcm1km_dominant_aggregate.tif", buffer_radius = 15000)
lcm_2020_25km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/08_1km_DominantAggregatedClasses_2020/gb2020lcm1km_dominant_aggregate.tif", buffer_radius = 25000)
lcm_2020_50km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/08_1km_DominantAggregatedClasses_2020/gb2020lcm1km_dominant_aggregate.tif", buffer_radius = 50000)
lcm_2020_75km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/08_1km_DominantAggregatedClasses_2020/gb2020lcm1km_dominant_aggregate.tif", buffer_radius = 75000)
lcm_2020_100km <- func_proc_lcm(raster_path = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/zz_All_LandCoverMaps_DominantAggregateClassesAt_1km/08_1km_DominantAggregatedClasses_2020/gb2020lcm1km_dominant_aggregate.tif", buffer_radius = 100000)




lcm_1990_5km$Year <- 1990
lcm_1990_10km$Year <- 1990
lcm_1990_15km$Year <- 1990
lcm_1990_25km$Year <- 1990
lcm_1990_50km$Year <- 1990
lcm_1990_75km$Year <- 1990
lcm_1990_100km$Year <- 1990

lcm_2000_5km$Year <- 2000
lcm_2000_10km$Year <- 2000
lcm_2000_15km$Year <- 2000
lcm_2000_25km$Year <- 2000
lcm_2000_50km$Year <- 2000
lcm_2000_75km$Year <- 2000
lcm_2000_100km$Year <- 2000

lcm_2007_5km$Year <- 2007
lcm_2007_10km$Year <- 2007
lcm_2007_15km$Year <- 2007
lcm_2007_25km$Year <- 2007
lcm_2007_50km$Year <- 2007
lcm_2007_75km$Year <- 2007
lcm_2007_100km$Year <- 2007

lcm_2015_5km$Year <- 2015
lcm_2015_10km$Year <- 2015
lcm_2015_15km$Year <- 2015
lcm_2015_25km$Year <- 2015
lcm_2015_50km$Year <- 2015
lcm_2015_75km$Year <- 2015
lcm_2015_100km$Year <- 2015

lcm_2017_5km$Year <- 2017
lcm_2017_10km$Year <- 2017
lcm_2017_15km$Year <- 2017
lcm_2017_25km$Year <- 2017
lcm_2017_50km$Year <- 2017
lcm_2017_75km$Year <- 2017
lcm_2017_100km$Year <- 2017

lcm_2018_5km$Year <- 2018
lcm_2018_10km$Year <- 2018
lcm_2018_15km$Year <- 2018
lcm_2018_25km$Year <- 2018
lcm_2018_50km$Year <- 2018
lcm_2018_75km$Year <- 2018
lcm_2018_100km$Year <- 2018

lcm_2019_5km$Year <- 2019
lcm_2019_10km$Year <- 2019
lcm_2019_15km$Year <- 2019
lcm_2019_25km$Year <- 2019
lcm_2019_50km$Year <- 2019
lcm_2019_75km$Year <- 2019
lcm_2019_100km$Year <- 2019

lcm_2020_5km$Year <- 2020
lcm_2020_10km$Year <- 2020
lcm_2020_15km$Year <- 2020
lcm_2020_25km$Year <- 2020
lcm_2020_50km$Year <- 2020
lcm_2020_75km$Year <- 2020
lcm_2020_100km$Year <- 2020

lcm_1990 <- rbind(lcm_1990_5km, lcm_1990_10km, lcm_1990_15km, lcm_1990_25km, lcm_1990_50km, lcm_1990_75km, lcm_1990_100km)
lcm_2000 <- rbind(lcm_2000_5km, lcm_2000_10km, lcm_2000_15km, lcm_2000_25km, lcm_2000_50km, lcm_2000_75km, lcm_2000_100km)
lcm_2007 <- rbind(lcm_2007_5km, lcm_2007_10km, lcm_2007_15km, lcm_2007_25km, lcm_2007_50km, lcm_2007_75km, lcm_2007_100km)

lcm_2015 <- rbind(lcm_2015_5km, lcm_2015_10km, lcm_2015_15km, lcm_2015_25km, lcm_2015_50km, lcm_2015_75km, lcm_2015_100km)
lcm_2017 <- rbind(lcm_2017_5km, lcm_2017_10km, lcm_2017_15km, lcm_2017_25km, lcm_2017_50km, lcm_2017_75km, lcm_2017_100km)
lcm_2018 <- rbind(lcm_2018_5km, lcm_2018_10km, lcm_2018_15km, lcm_2018_25km, lcm_2018_50km, lcm_2018_75km, lcm_2018_100km)

lcm_2019 <- rbind(lcm_2019_5km, lcm_2019_10km, lcm_2019_15km, lcm_2019_25km, lcm_2019_50km, lcm_2019_75km, lcm_2019_100km)
lcm_2020 <- rbind(lcm_2020_5km, lcm_2020_10km, lcm_2020_15km, lcm_2020_25km, lcm_2020_50km, lcm_2020_75km, lcm_2020_100km)

lcm_final_df <- rbind(lcm_1990, lcm_2000, lcm_2007, lcm_2015, lcm_2017, lcm_2018, lcm_2018, lcm_2019, lcm_2020)
# write.csv(df, "jnk1.csv") # edit manuall to add agg
lcm_gam_df <- read.csv("./IntermediateFiles_NeededForPlotting/LCM_Final_Plotting.csv", header = TRUE)
lcm_gam_df <- lcm_gam_df[lcm_gam_df$Class %in% c("03_Arable", "04_Improved Grassland", "05_Semi-natural Grassland", "10_BuiltUp_UrbanGardens"),]
lcm_gam_df$Class <- gsub("04_Improved Grassland", "Grassland", lcm_gam_df$Class)
lcm_gam_df$Class <- gsub("05_Semi-natural Grassland", "Grassland", lcm_gam_df$Class)
lcm_gam_df$Class <- gsub("03_Arable", "Arable", lcm_gam_df$Class)
lcm_gam_df$Class <- gsub("10_BuiltUp_UrbanGardens", "Urban", lcm_gam_df$Class)
head(lcm_gam_df)
lcm_gam_df <- aggregate(lcm_gam_df$Percentage, by = list(lcm_gam_df$Class, lcm_gam_df$Year, lcm_gam_df$Agg, lcm_gam_df$Site), mean, na.rm = TRUE)
colnames(lcm_gam_df) <- c("Class", "Year", "Agg", "Site", "Percentage")
unique(lcm_gam_df$Agg)
lcm_gam_df <- lcm_gam_df[lcm_gam_df$Agg %in% c("005km", "025km", "100km"),]
head(lcm_gam_df)
# fill gaps and generate time series per site per agg
library(imputeTS)
library(tseries)
func_to_fill_ts <- function(df, site, agg, class){
  jnk1 <- df
  ts1 <- jnk1[jnk1$Site == site,]
  ts1 <- ts1[ts1$Agg == agg,]
  ts1 <- ts1[ts1$Class == class,]
  ts2 <- data.frame(Year = seq(1990, 2020), Percentage = NA)
  ts3 <- merge(ts1, ts2, by = "Year", all.x = TRUE, all.y = TRUE)
  ts3 <- ts3[,c(1,5)]
 
  ts3 <- as.ts(ts3)
  ts3 <- na_interpolation(x = ts3)
  ts3 <- as.data.frame.matrix(ts3)
  ts3$Site <- site
  colnames(ts3) <- c("Year", paste(agg, class, sep = "_"), "Site")
  return(ts3)
}

hereford_5km_arable <- func_to_fill_ts(df = lcm_gam_df, site = "Hereford", agg = "005km", class = "Arable")
hereford_25km_arable <- func_to_fill_ts(df = lcm_gam_df, site = "Hereford", agg = "025km", class = "Arable")
hereford_100km_arable <- func_to_fill_ts(df = lcm_gam_df, site = "Hereford", agg = "100km", class = "Arable")
hereford_5km_grassland <- func_to_fill_ts(df = lcm_gam_df, site = "Hereford", agg = "005km", class = "Grassland")
hereford_25km_grassland <- func_to_fill_ts(df = lcm_gam_df, site = "Hereford", agg = "025km", class = "Grassland")
hereford_100km_grassland <- func_to_fill_ts(df = lcm_gam_df, site = "Hereford", agg = "100km", class = "Grassland")
hereford_5km_urban <- func_to_fill_ts(df = lcm_gam_df, site = "Hereford", agg = "005km", class = "Urban")
hereford_25km_urban <- func_to_fill_ts(df = lcm_gam_df, site = "Hereford", agg = "025km", class = "Urban")
hereford_100km_urban <- func_to_fill_ts(df = lcm_gam_df, site = "Hereford", agg = "100km", class = "Urban")

newcastle_5km_arable <- func_to_fill_ts(df = lcm_gam_df, site = "Newcastle", agg = "005km", class = "Arable")
newcastle_25km_arable <- func_to_fill_ts(df = lcm_gam_df, site = "Newcastle", agg = "025km", class = "Arable")
newcastle_100km_arable <- func_to_fill_ts(df = lcm_gam_df, site = "Newcastle", agg = "100km", class = "Arable")
newcastle_5km_grassland <- func_to_fill_ts(df = lcm_gam_df, site = "Newcastle", agg = "005km", class = "Grassland")
newcastle_25km_grassland <- func_to_fill_ts(df = lcm_gam_df, site = "Newcastle", agg = "025km", class = "Grassland")
newcastle_100km_grassland <- func_to_fill_ts(df = lcm_gam_df, site = "Newcastle", agg = "100km", class = "Grassland")
newcastle_5km_urban <- func_to_fill_ts(df = lcm_gam_df, site = "Newcastle", agg = "005km", class = "Urban")
newcastle_25km_urban <- func_to_fill_ts(df = lcm_gam_df, site = "Newcastle", agg = "025km", class = "Urban")
newcastle_100km_urban <- func_to_fill_ts(df = lcm_gam_df, site = "Newcastle", agg = "100km", class = "Urban")


rothamsted_5km_arable <- func_to_fill_ts(df = lcm_gam_df, site = "Rothamsted", agg = "005km", class = "Arable")
rothamsted_25km_arable <- func_to_fill_ts(df = lcm_gam_df, site = "Rothamsted", agg = "025km", class = "Arable")
rothamsted_100km_arable <- func_to_fill_ts(df = lcm_gam_df, site = "Rothamsted", agg = "100km", class = "Arable")
rothamsted_5km_grassland <- func_to_fill_ts(df = lcm_gam_df, site = "Rothamsted", agg = "005km", class = "Grassland")
rothamsted_25km_grassland <- func_to_fill_ts(df = lcm_gam_df, site = "Rothamsted", agg = "025km", class = "Grassland")
rothamsted_100km_grassland <- func_to_fill_ts(df = lcm_gam_df, site = "Rothamsted", agg = "100km", class = "Grassland")
rothamsted_5km_urban <- func_to_fill_ts(df = lcm_gam_df, site = "Rothamsted", agg = "005km", class = "Urban")
rothamsted_25km_urban <- func_to_fill_ts(df = lcm_gam_df, site = "Rothamsted", agg = "025km", class = "Urban")
rothamsted_100km_urban <- func_to_fill_ts(df = lcm_gam_df, site = "Rothamsted", agg = "100km", class = "Urban")



starcross_5km_arable <- func_to_fill_ts(df = lcm_gam_df, site = "Starcross", agg = "005km", class = "Arable")
starcross_25km_arable <- func_to_fill_ts(df = lcm_gam_df, site = "Starcross", agg = "025km", class = "Arable")
starcross_100km_arable <- func_to_fill_ts(df = lcm_gam_df, site = "Starcross", agg = "100km", class = "Arable")
starcross_5km_grassland <- func_to_fill_ts(df = lcm_gam_df, site = "Starcross", agg = "005km", class = "Grassland")
starcross_25km_grassland <- func_to_fill_ts(df = lcm_gam_df, site = "Starcross", agg = "025km", class = "Grassland")
starcross_100km_grassland <- func_to_fill_ts(df = lcm_gam_df, site = "Starcross", agg = "100km", class = "Grassland")
starcross_5km_urban <- func_to_fill_ts(df = lcm_gam_df, site = "Starcross", agg = "005km", class = "Urban")
starcross_25km_urban <- func_to_fill_ts(df = lcm_gam_df, site = "Starcross", agg = "025km", class = "Urban")
starcross_100km_urban <- func_to_fill_ts(df = lcm_gam_df, site = "Starcross", agg = "100km", class = "Urban")


df1 <- rbind(hereford_5km_arable, newcastle_5km_arable, rothamsted_5km_arable, starcross_5km_arable)
df2 <- rbind(hereford_5km_grassland, newcastle_5km_grassland, rothamsted_5km_grassland, starcross_5km_grassland)
df3 <- rbind(hereford_5km_urban, newcastle_5km_urban, rothamsted_5km_urban, starcross_5km_urban)
dfA <- merge(merge(df1, df2, by = c("Year", "Site")), df3, by = c("Year", "Site"))


df1 <- rbind(hereford_25km_arable, newcastle_25km_arable, rothamsted_25km_arable, starcross_25km_arable)
df2 <- rbind(hereford_25km_grassland, newcastle_25km_grassland, rothamsted_25km_grassland, starcross_25km_grassland)
df3 <- rbind(hereford_25km_urban, newcastle_25km_urban, rothamsted_25km_urban, starcross_25km_urban)
dfB <- merge(merge(df1, df2, by = c("Year", "Site")), df3, by = c("Year", "Site"))


df1 <- rbind(hereford_100km_arable, newcastle_100km_arable, rothamsted_100km_arable, starcross_100km_arable)
df2 <- rbind(hereford_100km_grassland, newcastle_100km_grassland, rothamsted_100km_grassland, starcross_100km_grassland)
df3 <- rbind(hereford_100km_urban, newcastle_100km_urban, rothamsted_100km_urban, starcross_100km_urban)
dfC <- merge(merge(df1, df2, by = c("Year", "Site")), df3, by = c("Year", "Site"))

head(dfA)
head(dfB)
head(dfC)
lcm_gam_df <- merge(merge(dfA, dfB, by = c("Year", "Site")), dfC, by = c("Year", "Site"))
colnames(lcm_gam_df) <- c("Year", "Site", "Arable_5km", "Grassland_5km", "Urban_5km", "Arable_25km", "Grassland_25km", "Urban_25km", "Arable_100km", "Grassland_100km", "Urban_100km")


# in previous script you decided to keep only 25km
lcm_gam_df <- lcm_gam_df[,c(1,2,6,7,8)]
head(lcm_gam_df)
