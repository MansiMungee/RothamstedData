library(eurostat)
library(sf)


wgs84 = "+init=epsg:4326"
bng = '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 
+ellps=airy +datum=OSGB36 +units=m +no_defs'

ConvertCoordinates <- function(easting,northing) {
  out = cbind(easting,northing)
  mask = !is.na(easting)
  sp <-  sp::spTransform(sp::SpatialPoints(list(easting[mask],northing[mask]),proj4string=sp::CRS(bng)),sp::CRS(wgs84))
  out[mask,]=sp@coords
  out
}


################### UK MAP ##################
europe <- get_eurostat_geospatial(resolution = 10, nuts_level = 1,  year = 2021)
UK_spdf <- as_Spatial(europe[grepl("UK", europe$id),])
# UK_spdf <- spTransform(UK_spdf, crs("+init=epsg:27700 +units=km +datum=WGS84"))


################### SUCTION TRAP COORDS ##################
coords <- read_sf("./../../Paper11_BioDAR_SpatialAnalysis/01_Data/03_Drivers/25_Radars_UKBoundary/DRUID_Data_Map.kml", "Suction Traps")
coords.2 <- as(coords$geometry, "Spatial")
coords.2 <- coords.2@coords
coloors <- c(rep("black", 7), mycolor_pallette[1], rep("black", 2), mycolor_pallette[2], rep("black", 1), mycolor_pallette[3], "black", mycolor_pallette[4], rep("black", 3))
pches <- c(c(rep(16, 7), 17, rep(16, 2), 17, rep(16, 1), 17, 16, 17, rep(16, 3)))
cexes <- c(c(rep(1.3, 7), 2, rep(1.3, 2), 2, rep(1.3, 1), 2, 1.3, 2, rep(1.3, 3)))
textes <- c(coords$Name)
textes <- data.frame(do.call('rbind',strsplit(as.character(textes),' ',fixed = FALSE)))
textes <- c(textes$X1[1], 
            textes$X1[2], 
            paste(textes$X1[3], textes$X2[3], sep = " "), 
            textes$X1[4], 
            paste(textes$X1[5], textes$X2[5], sep = " "),
            textes$X1[6], 
            textes$X1[7],
            textes$X1[8], 
            textes$X1[9],
            textes$X1[10],
            textes$X1[11],
            textes$X1[12],#
            paste(textes$X1[13], textes$X2[13], sep = " "),
            paste(textes$X1[14], textes$X2[14], sep = " "),
            textes$X1[15],
            textes$X1[16],
            textes$X1[17],
            textes$X1[18])



tiff("Figure1A.tiff", width = 16, height = 16, unit = "cm", res = 300)
par(mar = c(0,0,0,0), oma = c(0,0,0,0), mai = c(0,0,0,0))
plot(UK_spdf)
points(coords.2[,c(1,2)], cex = cexes, pch = pches, col = coloors)
#text(x = coords.2[,1] + 0.1, y = coords.2[,2] + 0.3, labels = textes, cex = 0.8)
dev.off()

# 1. Sites and sampling covergae (temporal)
# insect_df & aphid_df
jnk1 <- read.csv("GanttChart_Data.csv", header = TRUE)
tickos <- seq(1990, 2018, by = 2)


tiff("Figure1B.tiff", height = 12, width = 16, unit = "cm", res = 300)
ggplot(jnk1, aes(xmin = start1, xmax = end1, y = name, group = Data)) +
  theme_bw() + 
  geom_linerange(aes(color = name, group = Data), position = position_dodge(width = 0.2), size = 4) +
  scale_x_continuous(breaks = tickos, labels = paste(tickos)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(legend.position = "none") +
  scale_color_manual(values = c(mycolor_pallette[1], mycolor_pallette[2],mycolor_pallette[3],mycolor_pallette[4])) +
  theme(panel.grid.major.x = element_line(color = "darkgrey",
                                          size = 0.25,
                                          linetype = 1)) +
  theme(panel.grid.minor = element_line(color = "lightgrey",
                                        size = 0.01,
                                        linetype = 1))
dev.off()






# taxa
coloors.1 <- ggsci::pal_uchicago("light", alpha = 0.8)(9)
coloors.2 <- ggsci::pal_uchicago("dark", alpha = 1)(9)
coloors <- c(coloors.1, coloors.2)

jnk1 <- table(insect_df$Order) 
jnk2 <- nrow(list_raw_dfs$aphids)
jnk3 <- c(jnk1,jnk2)
names(jnk3) <- c("spiders", "beetles", "earwigs", "trueflies", "mayflies",
                 "truebugs", "ants", "moths_butterflies", 
                 "lacewings", "booklice", "caddisflies", "aphids")
jnk4 <- as.data.frame(jnk3)
jnk4$group <- rownames(jnk4)
jnk4 <- jnk4[!jnk4$group %in% c("aphids"),]
colnames(jnk4) <- c("Abundance", "Taxa")
jnk4$Abundance <- jnk4$Abundance/sum(jnk4$Abundance) * 100
jnk4$Abundance <- round(jnk4$Abundance, digits = 0)
jnk5 <- jnk4 %>% 
  mutate(csum = rev(cumsum(rev(Abundance))), 
         pos = Abundance/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Abundance/2, pos))

jnk5$csum <- round(jnk5$csum, digits = 1)
jnk5$pos <- round(jnk5$pos, digits = 1)
jnk5$Abundance <- round(jnk5$Abundance, digits = 2)

tiff("Figure1C.tiff", width = 12, height = 12, unit = "cm", res = 300)
ggplot(jnk5, aes(x = "" , y = Abundance, fill = fct_inorder(Taxa))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = coloors) +
  geom_label_repel(data = jnk5,
                   aes(y = pos, label = paste0(Abundance, "%")),
                   size = 3, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Group")) +
  theme_void()
dev.off()






























