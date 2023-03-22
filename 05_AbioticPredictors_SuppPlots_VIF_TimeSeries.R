############################################ Correlation plot all environmental variables #######################################
jnk1 <- read.csv("./IntermediateFiles_NeededForPlotting/Final_Predictor_DF_For_GAM.csv", header = TRUE)
corr_df <- jnk1[,c(4,6:15,17)]
corr_df <- apply(corr_df, 2, as.numeric)
corr_df <- as.data.frame.matrix(corr_df)
corr_mat <- cor(corr_df, use = "complete.obs")
coloors <- viridis(n = 100)
jpeg("Env_Correlation_Matrix.jpg", height = 8, width = 8, unit = "cm", res = 300)
par(oma= c(0.1,0.1,0.1,0.1),mai= c(0.3,0.3,0.1,0.1), cex.axis = 0.3, cex.lab = 0.1, mgp =c(0.7,0.02,0), tck = -0.02)
corrplot(corr_mat, 
         method = "square", col = coloors, order = 'AOE', addCoef.col = 'black', 
         type = "upper", diag = FALSE, number.digits = 2, number.cex = 0.5,
         addrect = 4, rect.col = "black", rect.lwd = 2, tl.cex = 0.6, tl.col = "black", cl.cex = 0.6)
dev.off()

##################################################### Time series plot for the final subset of environmental variables ################################
mycolor_pallete <- RColorBrewer::brewer.pal(n = 12, name = "Paired")
mycolor_pallete <- c(mycolor_pallete[c(2, 4, 6, 8)])
jnk2 <- read.csv("IntermediateFiles_NeededForPlotting/EnvThroughTime_ForPlotting.csv", header = TRUE)
jnk2 <- jnk2[jnk2$Variable %in% c("PET", "Lumen", "rainfall", "sfcwind", "tmean"), ]
jnk2$Variable <- gsub("sfcwind", "WIND", jnk2$Variable)
jnk2$Variable <- gsub("rainfall", "PPT", jnk2$Variable)
jnk2$Variable <- gsub("tmean", "TMean", jnk2$Variable)

jpeg("Env_through_time.jpg", height = 12, width = 17, unit = "cm", res = 300)
ggplot(jnk2, aes(x = Year, y = Value, col = Site)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "loess") +
  facet_wrap(~ Variable, scales = "free_y") +
  scale_color_manual(values = mycolor_pallette) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()




########################################## correlation plot for LCM variables across radius ###############################################
jnk1 <- read.csv("./IntermediateFiles_NeededForPlotting/LCM_Final_Plotting.csv", header = TRUE)
jnk1 <- jnk1[jnk1$Class %in% c("03_Arable", "04_Improved Grassland","05_Semi-natural Grassland", "10_BuiltUp_UrbanGardens"), ]
jnk1 <- jnk1[,c(1,2,4)]
jnk2 <- reshape(jnk1, idvar = "Class", timevar = "Agg", direction = "wide")
jnk3 <- jnk2[,-1]
jnk3 <- apply(jnk3, 2, as.numeric)
jnk3 <- as.data.frame.matrix(jnk3)
rownames(jnk3) <- jnk2$Class
corr_mat <- cor(jnk3)
coloors <- viridis(10)
jpeg("LCM_Correlation_Matrix.jpg", height = 8, width = 8, unit = "cm", res = 300)
par(oma= c(0.1,0.1,0.1,0.1),mai= c(0.3,0.3,0.1,0.1), cex.axis = 0.3, cex.lab = 0.1, mgp =c(0.7,0.02,0), tck = -0.02)
corrplot(corr_mat, 
         method = "square", col = coloors, order = 'AOE', addCoef.col = 'black', 
         type = "upper", diag = FALSE, number.digits = 2, number.cex = 0.5,
         addrect = 4, rect.col = "black", rect.lwd = 2, tl.cex = 0.6, tl.col = "black", cl.cex = 0.6)
dev.off()

########################################## Tie series plot for 25km variables through time###############################################
jnk1 <- read.csv("./IntermediateFiles_NeededForPlotting/LCM_Final_Plotting.csv", header = TRUE)
jnk1 <- jnk1[jnk1$Class %in% c("03_Arable", "04_Improved Grassland","05_Semi-natural Grassland", "10_BuiltUp_UrbanGardens"), ]
jnk1 <- jnk1[jnk1$Agg == "025km",]
jnk2 <- jnk1[!jnk1$Class %in% c("04_Improved Grassland","05_Semi-natural Grassland"),]
jnk3 <- jnk1[jnk1$Class %in% c("04_Improved Grassland","05_Semi-natural Grassland"),]
jnk3 <- aggregate(jnk3$Percentage, by = list(jnk3$Year, jnk3$Site, jnk3$Agg), sum, na.rm = TRUE)
jnk3 <- data.frame(Percentage = jnk3$x, Class = "Grassland", Year = jnk3$Group.1, Agg = jnk3$Group.3, Site = jnk3$Group.2)
jnk1 <- rbind(jnk2,jnk3)
unique(jnk1$Class)
jnk1$Class <- gsub("03_Arable", "Arable", jnk1$Class)
jnk1$Class <- gsub("10_BuiltUp_UrbanGardens", "Urban", jnk1$Class)
head(jnk1)

jpeg("LCM_through_time.jpg", height = 8, width = 16, unit = "cm", res = 300)
ggplot(jnk1, aes(x = Year, y = Percentage, col = Site)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "loess", size = 0.7) +
  facet_wrap(~ Class, scales = "free") +
  scale_color_manual(values = mycolor_pallette) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()
