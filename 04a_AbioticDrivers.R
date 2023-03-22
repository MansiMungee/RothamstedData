############################################### CHUNK - 01 - LAT-LONG LOCATIONS OF THE FOUR SUCTION TRAP SITES##########################
site_coords <- read.csv("./SuctionTrap_Coords.csv", header = TRUE)
## convert lat long to spatial points
coords <- as.data.frame.matrix(site_coords[,c(3,4)])
coord.dec = SpatialPoints(cbind(coords$Lng, coords$Lat), proj4string = CRS("+proj=longlat"))
# Transforming coordinate to UTM using EPSG=32748 for WGS=84, UTM Zone=48M,
# Southern Hemisphere)
coord.UTM <- spTransform(coord.dec, CRS("+init=epsg:32748"))



################################################  CHUNK 02 VIIRS DATASETS############################################### 
jnk1 <- list.files("./Drivers/LightsAtNight/", pattern = ".tif", recursive = FALSE, full.names = TRUE)
lights_df <- list()
for(ii in 1:length(jnk1)){
  jnk2 <- jnk1[ii]
  jnk2 <- raster(jnk2)
  lights_df[[ii]] <- raster::extract(x = jnk2, y = coord.UTM)
  names(lights_df[[ii]]) <- site_coords$Name
}
names_lights <- jnk1
names_lights <- data.frame(do.call('rbind',strsplit(as.character(names_lights),'_',fixed=TRUE)))
names_lights <- paste("lights_", names_lights$X4, sep = "")
jnk1 <- list()
for(ii in 1:length(lights_df)){
  jnk1[[ii]] <- as.data.frame(lights_df[[ii]])
  jnk1[[ii]] <- data.frame(Site = rownames(jnk1[[ii]]), Lumen = jnk1[[ii]]$`lights_df[[ii]]`)
}

names(jnk1) <- names_lights
jnk1 <- ldply(jnk1, data.frame)
jnk1$.id <- gsub("lights_", "", jnk1$.id)
colnames(jnk1) <- c("Year", "Site", "Lumen")
jnk1$Year <- as.numeric(jnk1$Year)
lights_df <- jnk1

lights_df$Site <- gsub("Rothamsted Tower", "Rothamsted", lights_df$Site)
lights_df$Site <- gsub("Rothamsted ", "Rothamsted", lights_df$Site)






################################################  CHUNK 03 CEDA DATASETS - FUNCTIONS ############################################### 
# FUNCTIONS FOR EXTRACTING AND PROCESSING MET OFFICE - CEDA VARIABLES
func_extract_ceda <- function(dir, coord_df){
  files <- list.files(path = dir, pattern = ".nc", full.names = TRUE, recursive = TRUE)
  metric_df <- list()
  for (ii in 1:length(files)){
    file_name <- files[ii]
    jnk1_nc <- raster::stack(file_name)
    metric_df[[ii]] <- data.frame(raster::extract(x = jnk1_nc, y = coord_df))
  }
  names(metric_df) <- files
  return(metric_df)
}
func_process_ceda <- function(list_object, site_object){
  jnk1 <- names(list_object)
  jnk1 <- data.frame(do.call('rbind',strsplit(as.character(jnk1),'_',fixed=TRUE)))
  jnk1 <- data.frame(do.call('rbind',strsplit(as.character(jnk1$X11),'-',fixed=TRUE)))
  jnk1 <- data.frame(do.call('rbind',strsplit(as.character(jnk1$X1),'',fixed=TRUE)))
  jnk1 <- paste(jnk1$X1, jnk1$X2, jnk1$X3, jnk1$X4, sep = "")
  jnk1 <- paste(rep(jnk1, each = 12), month.abb, sep = "_")
  y <- list.cbind(list_object)
  colnames(y) <- jnk1
  y <- data.frame(site_object, y)
  return(y)
}
# FUNCTIONS FOR EXTRACTING AND PROCESSING MET OFFICE - CEDA VARIABLES - WITH DAILY COVERGAE i.e. PET DATASET
func_extract_ceda_pet <- function(dir, coord_df){
  files <- list.files(path = dir, pattern = ".nc", full.names = TRUE, recursive = TRUE)
  years_to_extract <- seq(1990, 2021, by = 1)
  years_to_extract <- paste("_", years_to_extract, "_", sep = "")
  output_mat <- list()
  for(ii in 1:length(years_to_extract)){
    print(ii)
    files_subset <- files[grep(as.character(years_to_extract[ii]), files)]
    if(length(files_subset) > 0){
      output_mat[[ii]] <- matrix(data = NA, nrow = 12, ncol = 7)
      colnames(output_mat[[ii]]) <- c("Hereford", "Newcastle", "Rothamsted", "Starcross", "Year", "Month", "Variable")
      rownames(output_mat[[ii]]) <- seq(1,12)
      output_mat[[ii]] <- as.data.frame.matrix(output_mat[[ii]])
      output_mat[[ii]]$Variable <- "PET"
      output_mat[[ii]]$Year <- seq(1990, 2021, by = 1)[ii]
      output_mat[[ii]]$Month <- month.abb
      for (jj in 1:length(files_subset)){
        file_name <- files_subset[jj]
        jnk1_nc <- stack(file_name)
        jnk1 <- data.frame(raster::extract(x = jnk1_nc, y = coord_df))
        jnk1 <- rowMeans(jnk1, na.rm = TRUE)
        output_mat[[ii]][jj,][1:4] <- jnk1
      }
    }
  }
  return(output_mat)
}











###############################################  CHUNK 04 - EXTRACTING CEDA VARIABLES ############################################### 
# 8. rainfall 
rainfall_08 <- func_extract_ceda(dir = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/08_RAINFALL/Monthly/", coord_df = coord.UTM)
rainfall_08 <- func_process_ceda(rainfall_08, site_object = coord.UTM)
rainfall_08$Site <- c(site_coords$Name)
rainfall_08 <- rainfall_08[,-c(1:2)]
rainfall_08 <- t(rainfall_08)
rainfall_08 <- as.data.frame.matrix(rainfall_08)
rainfall_08$Year <- rownames(rainfall_08)
jnk1 <- data.frame(do.call('rbind',strsplit(as.character(rainfall_08$Year),'_',fixed=TRUE)))
rainfall_08$Year <- gsub("X", "", jnk1$X1)
rainfall_08$Month <- jnk1$X2
rainfall_08$Variable <- "rainfall"
rownames(rainfall_08) <- seq(1, nrow(rainfall_08))
colnames(rainfall_08) <- c("Hereford", "Newcastle", "Rothamsted", "Starcross", "Year", "Month", "Variable")

# 9. tasmax - maximum temperature
tasmax_09 <- func_extract_ceda(dir = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/09_TASMAX/Monthly/", coord_df = coord.UTM)
tasmax_09 <- func_process_ceda(tasmax_09, site_object = coord.UTM)
tasmax_09$Site <- c(site_coords$Name)
tasmax_09 <- tasmax_09[,-c(1:2)]
tasmax_09 <- t(tasmax_09)
tasmax_09 <- as.data.frame.matrix(tasmax_09)
tasmax_09$Year <- rownames(tasmax_09)
jnk1 <- data.frame(do.call('rbind',strsplit(as.character(tasmax_09$Year),'_',fixed=TRUE)))
tasmax_09$Year <- gsub("X", "", jnk1$X1)
tasmax_09$Month <- jnk1$X2
tasmax_09$Variable <- "tmax"
rownames(tasmax_09) <- seq(1, nrow(tasmax_09))
colnames(tasmax_09) <- c("Hereford", "Newcastle", "Rothamsted", "Starcross", "Year", "Month", "Variable")

# 10. tas - mean temperature
tas_10 <- func_extract_ceda(dir = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/10_TAS/Monthly/", coord_df = coord.UTM)
tas_10 <- func_process_ceda(tas_10, site_object = coord.UTM)
tas_10$Site <- c(site_coords$Name)
tas_10 <- tas_10[,-c(1:2)]
tas_10 <- t(tas_10)
tas_10 <- as.data.frame.matrix(tas_10)
tas_10$Year <- rownames(tas_10)
jnk1 <- data.frame(do.call('rbind',strsplit(as.character(tas_10$Year),'_',fixed=TRUE)))
tas_10$Year <- gsub("X", "", jnk1$X1)
tas_10$Month <- jnk1$X2
tas_10$Variable <- "tmean"
rownames(tas_10) <- seq(1, nrow(tas_10))
colnames(tas_10) <- c("Hereford", "Newcastle", "Rothamsted", "Starcross", "Year", "Month", "Variable")


# 11. tasmin - minimum temperature
tasmin_11 <- func_extract_ceda(dir = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/11_TASMIN/Monthly/", coord_df = coord.UTM)
tasmin_11 <- func_process_ceda(tasmin_11, site_object = coord.UTM)
tasmin_11$Site <- c(site_coords$Name)
tasmin_11 <- tasmin_11[,-c(1:2)]
tasmin_11 <- t(tasmin_11)
tasmin_11 <- as.data.frame.matrix(tasmin_11)
tasmin_11$Year <- rownames(tasmin_11)
jnk1 <- data.frame(do.call('rbind',strsplit(as.character(tasmin_11$Year),'_',fixed=TRUE)))
tasmin_11$Year <- gsub("X", "", jnk1$X1)
tasmin_11$Month <- jnk1$X2
tasmin_11$Variable <- "tmin"
rownames(tasmin_11) <- seq(1, nrow(tasmin_11))
colnames(tasmin_11) <- c("Hereford", "Newcastle", "Rothamsted", "Starcross", "Year", "Month", "Variable")


# 12. SURFACE WINDS
sfcwind_12 <- func_extract_ceda(dir = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/12_SFCWIND/Monthly/", coord_df = coord.UTM)
sfcwind_12 <- func_process_ceda(sfcwind_12, site_object = coord.UTM)
sfcwind_12$Site <- c(site_coords$Name)
sfcwind_12 <- sfcwind_12[,-c(1:2)]
sfcwind_12 <- t(sfcwind_12)
sfcwind_12 <- as.data.frame.matrix(sfcwind_12)
sfcwind_12$Year <- rownames(sfcwind_12)
jnk1 <- data.frame(do.call('rbind',strsplit(as.character(sfcwind_12$Year),'_',fixed=TRUE)))
sfcwind_12$Year <- gsub("X", "", jnk1$X1)
sfcwind_12$Month <- jnk1$X2
sfcwind_12$Variable <- "sfcwind"
rownames(sfcwind_12) <- seq(1, nrow(sfcwind_12))
colnames(sfcwind_12) <- c("Hereford", "Newcastle", "Rothamsted", "Starcross", "Year", "Month", "Variable")

# 13. SUN - sunshine
sun_13 <- func_extract_ceda(dir = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/13_SUN/Monthly/", coord_df = coord.UTM)
sun_13 <- func_process_ceda(sun_13, site_object = coord.UTM)
sun_13$Site <- c(site_coords$Name)
sun_13 <- sun_13[,-c(1:2)]
sun_13 <- t(sun_13)
sun_13 <- as.data.frame.matrix(sun_13)
sun_13$Year <- rownames(sun_13)
jnk1 <- data.frame(do.call('rbind',strsplit(as.character(sun_13$Year),'_',fixed=TRUE)))
sun_13$Year <- gsub("X", "", jnk1$X1)
sun_13$Month <- jnk1$X2
sun_13$Variable <- "sun"
rownames(sun_13) <- seq(1, nrow(sun_13))
colnames(sun_13) <- c("Hereford", "Newcastle", "Rothamsted", "Starcross", "Year", "Month", "Variable")


# 14. PSL - pressure
psl_14 <- func_extract_ceda(dir = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/14_PSL/Monthly/", coord_df = coord.UTM)
psl_14 <- func_process_ceda(psl_14, site_object = coord.UTM)
psl_14$Site <- c(site_coords$Name)
psl_14 <- psl_14[,-c(1:2)]
psl_14 <- t(psl_14)
psl_14 <- as.data.frame.matrix(psl_14)
psl_14$Year <- rownames(psl_14)
jnk1 <- data.frame(do.call('rbind',strsplit(as.character(psl_14$Year),'_',fixed=TRUE)))
psl_14$Year <- gsub("X", "", jnk1$X1)
psl_14$Month <- jnk1$X2
psl_14$Variable <- "psl"
rownames(psl_14) <- seq(1, nrow(psl_14))
colnames(psl_14) <- c("Hereford", "Newcastle", "Rothamsted", "Starcross", "Year", "Month", "Variable")

# 15. HURS - relative humidity
hurs_15 <- func_extract_ceda(dir = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/15_HURS/Monthly/", coord_df = coord.UTM)
hurs_15 <- func_process_ceda(hurs_15, site_object = coord.UTM)
hurs_15$Site <- c(site_coords$Name)
hurs_15 <- hurs_15[,-c(1:2)]
hurs_15 <- t(hurs_15)
hurs_15 <- as.data.frame.matrix(hurs_15)
hurs_15$Year <- rownames(hurs_15)
jnk1 <- data.frame(do.call('rbind',strsplit(as.character(hurs_15$Year),'_',fixed=TRUE)))
hurs_15$Year <- gsub("X", "", jnk1$X1)
hurs_15$Month <- jnk1$X2
hurs_15$Variable <- "hurs"
rownames(hurs_15) <- seq(1, nrow(hurs_15))
colnames(hurs_15) <- c("Hereford", "Newcastle", "Rothamsted", "Starcross", "Year", "Month", "Variable")


# 16. PV = vapour pressure
pv_16 <- func_extract_ceda(dir = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/16_PV/Monthly/", coord_df = coord.UTM)
pv_16 <- func_process_ceda(pv_16, site_object = coord.UTM)
pv_16$Site <- c(site_coords$Name)
pv_16 <- pv_16[,-c(1:2)]
pv_16 <- t(pv_16)
pv_16 <- as.data.frame.matrix(pv_16)
pv_16$Year <- rownames(pv_16)
jnk1 <- data.frame(do.call('rbind',strsplit(as.character(pv_16$Year),'_',fixed=TRUE)))
pv_16$Year <- gsub("X", "", jnk1$X1)
pv_16$Month <- jnk1$X2
pv_16$Variable <- "pv"
rownames(pv_16) <- seq(1, nrow(pv_16))
colnames(pv_16) <- c("Hereford", "Newcastle", "Rothamsted", "Starcross", "Year", "Month", "Variable")

# 17. snowlying
snowlying_17 <- func_extract_ceda(dir = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/17_SNOWLYING/Monthly/", coord_df = coord.UTM)
snowlying_17 <- func_process_ceda(snowlying_17, site_object = coord.UTM)
snowlying_17$Site <- c(site_coords$Name)
snowlying_17 <- snowlying_17[,-c(1:2)]
snowlying_17 <- t(snowlying_17)
snowlying_17 <- as.data.frame.matrix(snowlying_17)
snowlying_17$Year <- rownames(snowlying_17)
jnk1 <- data.frame(do.call('rbind',strsplit(as.character(snowlying_17$Year),'_',fixed=TRUE)))
snowlying_17$Year <- gsub("X", "", jnk1$X1)
snowlying_17$Month <- jnk1$X2
snowlying_17$Variable <- "snowlying"
rownames(snowlying_17) <- seq(1, nrow(snowlying_17))
colnames(snowlying_17) <- c("Hereford", "Newcastle", "Rothamsted", "Starcross", "Year", "Month", "Variable")


# 18. groundfrost
groundfrost_18 <- func_extract_ceda(dir = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/18_GROUNDFROST/Monthly/", coord_df = coord.UTM)
groundfrost_18 <- func_process_ceda(groundfrost_18, site_object = coord.UTM)
groundfrost_18$Site <- c(site_coords$Name)
groundfrost_18 <- groundfrost_18[,-c(1:2)]
groundfrost_18 <- t(groundfrost_18)
groundfrost_18 <- as.data.frame.matrix(groundfrost_18)
groundfrost_18$Year <- rownames(groundfrost_18)
jnk1 <- data.frame(do.call('rbind',strsplit(as.character(groundfrost_18$Year),'_',fixed=TRUE)))
groundfrost_18$Year <- gsub("X", "", jnk1$X1)
groundfrost_18$Month <- jnk1$X2
groundfrost_18$Variable <- "groundfrost"
rownames(groundfrost_18) <- seq(1, nrow(groundfrost_18))
colnames(groundfrost_18) <- c("Hereford", "Newcastle", "Rothamsted", "Starcross", "Year", "Month", "Variable")

# 19. dtm - elevation
dtm_19 <- raster("./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/19_DTM/w001001.adf")
dtm_19 <- raster::extract(x = dtm_19, y = coord.UTM)
dtm_19 <- data.frame(elevation = dtm_19, Site = site_coords$Name)
dtm_19$Variable <- "elevation"

# Pooling environmental predictors
ceda_df <- rbind(tas_10, tasmax_09, tasmin_11, rainfall_08, sfcwind_12, sun_13, psl_14, hurs_15, pv_16, snowlying_17, groundfrost_18)

# 21 PET - potential evapotranspiration
pet1 <- func_extract_ceda_pet(dir = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/21_PET/PET_1986-1990//",  coord_df = coord.UTM)
pet2 <- func_extract_ceda_pet(dir = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/21_PET/PET_1991-1995//",  coord_df = coord.UTM)
pet3 <- func_extract_ceda_pet(dir = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/21_PET/PET_1996-2000//",  coord_df = coord.UTM)
pet4 <- func_extract_ceda_pet(dir = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/21_PET/PET_2001-2005//",  coord_df = coord.UTM)
pet5 <- func_extract_ceda_pet(dir = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/21_PET/PET_2006-2010//",  coord_df = coord.UTM)
pet6 <- func_extract_ceda_pet(dir = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/21_PET/PET_2011-2015//",  coord_df = coord.UTM)
pet7 <- func_extract_ceda_pet(dir = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/21_PET/PET_2016-2020//",  coord_df = coord.UTM)
pet8 <- func_extract_ceda_pet(dir = "./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/21_PET/PET_2021//",  coord_df = coord.UTM)
pet1 <- list.rbind(pet1)
pet2 <- list.rbind(pet2)
pet3 <- list.rbind(pet3)
pet4 <- list.rbind(pet4)
pet5 <- list.rbind(pet5)
pet6 <- list.rbind(pet6)
pet7 <- list.rbind(pet7)
pet8 <- list.rbind(pet8)
pet_df <- rbind(pet1, pet2, pet3, pet4, pet5, pet6, pet7, pet8)










##################################################### CHUNK 05 - Combining all drivers (Lights, CEDA, PET, LCM from 04bMoreAbioticDrivers) & checking for correlations############################
lights_df
head(lights_df)
lights_df <- data.frame("Site" = lights_df$Site, Variable = "Lumen", Year = lights_df$Year, value = lights_df$Lumen)
lights_df <- reshape(lights_df, idvar = "Year", timevar = "Site", direction = "wide")
lights_df <- data.frame(Hereford = lights_df$value.Hereford, Newcastle = lights_df$value.Newcastle, Rothamsted = lights_df$value.Rothamsted, Starcross = lights_df$value.Starcross, Year = lights_df$Year, Month = NA, Variable = "Lumen")
# na_interpolate lights_df
library(imputeTS)
jnk1 <- lights_df$Hereford
jnk1 <- c(NA, NA, jnk1[1:25], NA, jnk1[26], NA, NA, NA)
jnk1 <- ts(jnk1)
jnk1 <- imputeTS::na_interpolation(jnk1, "spline")
jnk2 <- lights_df$Newcastle
jnk2 <- c(NA, NA, jnk2[1:25], NA, jnk2[26], NA, NA, NA)
jnk2 <- ts(jnk2)
jnk2 <- imputeTS::na_interpolation(jnk2, "spline")
jnk3 <- lights_df$Rothamsted
jnk3 <- c(NA, NA, jnk3[1:25], NA, jnk3[26], NA, NA, NA)
jnk3 <- ts(jnk3)
jnk3 <- imputeTS::na_interpolation(jnk3, "spline")
jnk4 <- lights_df$Starcross
jnk4 <- c(NA, NA, jnk4[1:25], NA, jnk4[26], NA, NA, NA)
jnk4 <- ts(jnk4)
jnk4 <- imputeTS::na_interpolation(jnk4, "spline")

Year <- seq(1990, 2021, by = 1)
Year <- rep(Year, each = 12)
df <- data.frame(Year = Year, Month = rep(month.abb, length(seq(1990,2021, by = 1))))
df$Hereford <- rep(jnk1, each = 12)
df$Newcastle <- rep(jnk2, each = 12)
df$Rothamsted <- rep(jnk3, each = 12)
df$Starcross <- rep(jnk4, each = 12)
head(df)
lights_df <- df
lights_df <- data.frame(Hereford = lights_df$Hereford, Newcastle = lights_df$Newcastle, Rothamsted = lights_df$Rothamsted, Starcross = lights_df$Starcross, Year = lights_df$Year, Month = lights_df$Month, Variable = "LUMEN")
# ceda_df
head(ceda_df)
# # pet_df
head(pet_df)

# Final compiled df (+ lcm_df)
env_df <- rbind(ceda_df, lights_df, pet_df)
head(env_df)
unique(env_df$Rothamsted)
table(env_df$Year) 
env_df <- env_df[!env_df$Month == "Site",]
# function to calculte vifs and correlations for each site:
# VIF HELP
# The Variance Inflation Factor (VIF) looks at how well a single xi is determined by all the other xi (jointly) in your model.
# For each xi in your model, you run a (auxiliary) linear regression:
#  x1,i = β1 + β2x2,i + ... + βnxn,i + u.
# You retrieve the R2 for each of these models and calculate the VIF:
#  VIF1 = 1/(1−R^2_1).
# What about the VIF=10 rule of thumb?
# VIF = 10 is equal to having an R2 = 0.9 in the auxiliary regression in step 1 
# above (because 1/(1−0.9)=10). 
# This means that your other xi (in the model) explain the xi under consideration to a 
# large extent (90% if you want to say so). This of course is just a rule of thumb.
# In essence, the VIF boils down to the question: "How well is one of my xi explained by all other x jointly".

# read more about VIF at https://www.displayr.com/variance-inflation-factors-vifs/



# so i generated the correlation plots - and i found strong correlations between land caetrories
# when i do vif it fails because of these strong correlations, so i need to check which variables to drop before vif (chutiya phir yeh vif kya karega)
# anyway - slecting which variables to drop is very confusing so what i am doing in running the vif model and everything which gives NA - you remove
# so, 


# # manually selecting variables
# # 1. hereford
# jnk1 <- var_cor_hereford
# vif_model <- lm(groundfrost ~ ., data = as.data.frame.matrix(var_cor_hereford))
# # for hereford drop  Arable_100km  Grassland_100km      Urban_100km
# jnk1 <- hereford_df[,!names(hereford_df) %in% c("value.Arable_100km", "value.Grassland_100km", "value.Urban_100km")]
# jnk1 <- jnk1[complete.cases(jnk1),]
# jnk1 <- jnk1[,-1]
# colnames(jnk1) <- gsub("value.", "", colnames(jnk1))
# jnk1 <- as.data.frame.matrix(apply(jnk1, 2, as.numeric))
# jnk1 <- jnk1[,-c(11,13,16)] # remove tmin, tmax, and urban5km
# vif_model <- lm(groundfrost ~ ., data = as.data.frame.matrix(jnk1))
# vif_result <- car::vif(vif_model)
# # remove arable5km, arable25km, grassland25km and check again
# head(jnk1)
# jnk1 <- jnk1[,-c(12,14,15)]
# vif_model <- lm(groundfrost ~ ., data = as.data.frame.matrix(jnk1))
# vif_result <- car::vif(vif_model)
# vif_result_trunc <- vif_result[vif_result < 8]
# hereford_final_variables <- vif_result_trunc
# 
# 

# 1. newcastle
# jnk1 <- var_cor_newcastle
# vif_model <- lm(groundfrost ~ ., data = as.data.frame.matrix(var_cor_newcastle))
# # drop Urban_25km     Arable_100km  Grassland_100km      Urban_100km 
# jnk1 <- newcastle_df[,!names(newcastle_df) %in% c("value.Arable_100km", "value.Grassland_100km", "value.Urban_100km", "value.Urban_25km" )]
# jnk1 <- jnk1[complete.cases(jnk1),]
# jnk1 <- jnk1[,-1]
# colnames(jnk1) <- gsub("value.", "", colnames(jnk1))
# jnk1 <- as.data.frame.matrix(apply(jnk1, 2, as.numeric))
# jnk1 <- jnk1[,-c(11,13)] # remove tmin, tmax, 
# vif_model <- lm(groundfrost ~ ., data = as.data.frame.matrix(jnk1))
# vif_result <- car::vif(vif_model)
# vif_result_trunc <- vif_result[vif_result < 8]
# # remove tmean, Grassland_5km      Urban_5km    Arable_25km Grassland_25km 
# head(jnk1)
# jnk1 <- jnk1[,-c(11,13,14,15,16)]
# vif_model <- lm(groundfrost ~ ., data = as.data.frame.matrix(jnk1))
# vif_result <- car::vif(vif_model)
# vif_result_trunc <- vif_result[vif_result < 8]
# newcastle_final_variables <- vif_result_trunc
# 
# 
# # too complicated - different variables at different site - 
# final just keep 25km grassland, 25km, urban and 25km arable final
# do variable selection for only abiotic variables
# jnk1 <- hereford_df
# jnk1 <- jnk1[,-c(1, 15:ncol(jnk1))]
# #jnk1 <- jnk1[complete.cases(jnk1),]
# colnames(jnk1) <- gsub("value.", "", colnames(jnk1))
# jnk1 <- as.data.frame.matrix(apply(jnk1, 2, as.numeric))
# jnk1 <- jnk1[,-c(11,13)] # remove tmin, tmax, 
# jnk1 <- jnk1[,-c(2,5,6)]
# vif_model <- lm(groundfrost ~ ., data = as.data.frame.matrix(jnk1))
# vif_result <- car::vif(vif_model)
# vif_result_trunc <- vif_result[vif_result < 10]

# final variables = lumen, PET, rainfall, sfcwind, tmean
# urban, grassland and arable 25km













###################################################### CHUNK 07 - Combine final env + lcm for gam and output #################################
head(env_df)
hereford_df <- env_df[,c(1,5,6,7)]
hereford_df$Year <- paste(hereford_df$Year, hereford_df$Month, sep = "_")
hereford_df <- hereford_df[,c(1,2,4)]
hereford_df <- reshape(hereford_df, idvar = "Year", timevar = "Variable", direction = "wide")
colnames(hereford_df) <- gsub("Hereford.", "", colnames(hereford_df))
colnames(hereford_df) <- c("Year", "Tmean", "Tmax", "Tmin", "PPT", "WIND", "SUN", "PSL", "RH", "PV", "SNOW", "FROST", "LUMEN", "PET")
hereford_df$Site <- "Hereford"
jnk1 <- data.frame(do.call('rbind',strsplit(as.character(hereford_df$Year),'_',fixed=TRUE)))
hereford_df$Year <- jnk1$X1
hereford_df$Month <- jnk1$X2
head(hereford_df)


newcastle_df <- env_df[,c(2,5,6,7)]
newcastle_df$Year <- paste(newcastle_df$Year, newcastle_df$Month, sep = "_")
newcastle_df <- newcastle_df[,c(1,2,4)]
newcastle_df <- reshape(newcastle_df, idvar = "Year", timevar = "Variable", direction = "wide")
colnames(newcastle_df) <- gsub("Newcastle.", "", colnames(newcastle_df))
colnames(newcastle_df) <- c("Year", "Tmean", "Tmax", "Tmin", "PPT", "WIND", "SUN", "PSL", "RH", "PV", "SNOW", "FROST", "LUMEN", "PET")
newcastle_df$Site <- "Newcastle"
jnk1 <- data.frame(do.call('rbind',strsplit(as.character(newcastle_df$Year),'_',fixed=TRUE)))
newcastle_df$Year <- jnk1$X1
newcastle_df$Month <- jnk1$X2
head(newcastle_df)



rothamsted_df <- env_df[,c(3,5,6,7)]
rothamsted_df$Year <- paste(rothamsted_df$Year, rothamsted_df$Month, sep = "_")
rothamsted_df <- rothamsted_df[,c(1,2,4)]
rothamsted_df <- reshape(rothamsted_df, idvar = "Year", timevar = "Variable", direction = "wide")
colnames(rothamsted_df) <- gsub("Rothamsted.", "", colnames(rothamsted_df))
colnames(rothamsted_df) <- c("Year", "Tmean", "Tmax", "Tmin", "PPT", "WIND", "SUN", "PSL", "RH", "PV", "SNOW", "FROST", "LUMEN", "PET")
rothamsted_df$Site <- "Rothamsted"
jnk1 <- data.frame(do.call('rbind',strsplit(as.character(rothamsted_df$Year),'_',fixed=TRUE)))
rothamsted_df$Year <- jnk1$X1
rothamsted_df$Month <- jnk1$X2
head(rothamsted_df)




starcross_df <- env_df[,c(4,5,6,7)]
starcross_df$Year <- paste(starcross_df$Year, starcross_df$Month, sep = "_")
starcross_df <- starcross_df[,c(1,2,4)]
starcross_df <- reshape(starcross_df, idvar = "Year", timevar = "Variable", direction = "wide")
colnames(starcross_df) <- gsub("Starcross.", "", colnames(starcross_df))
colnames(starcross_df) <- c("Year", "Tmean", "Tmax", "Tmin", "PPT", "WIND", "SUN", "PSL", "RH", "PV", "SNOW", "FROST", "LUMEN", "PET")
starcross_df$Site <- "Starcross"
jnk1 <- data.frame(do.call('rbind',strsplit(as.character(starcross_df$Year),'_',fixed=TRUE)))
starcross_df$Year <- jnk1$X1
starcross_df$Month <- jnk1$X2
head(starcross_df)


env_df_final <- rbind(hereford_df, newcastle_df, rothamsted_df, starcross_df)
# add lcm 
final_predictor_df <- merge(env_df_final, lcm_gam_df, by = c("Year", "Site"), all.x = TRUE)
write.csv(final_predictor_df, "./IntermediateFiles_NeededForPlotting/Final_Predictor_DF_For_GAM.csv")


