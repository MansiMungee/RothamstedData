######################################### all aphid summary stats and rare curves ###################################
jnk1 <- list.gam.dfs
sum(jnk1$abundance_aphids$Metric)
aggregate(jnk1$abundance_aphids$Metric, by = list(jnk1$abundance_aphids$Site), sum)
length(unique(aphid_df$taxa_down))
head(aphid_df)
vegan_aphid_dfs <- split(aphid_df, f = aphid_df$site)
for(ii in 1:length(vegan_aphid_dfs)){
  vegan_aphid_dfs[[ii]] <- as.data.frame.matrix(t(table(vegan_aphid_dfs[[ii]]$taxa_down, vegan_aphid_dfs[[ii]]$Year)))
}
sites <- c("Hereford", "Newcastle", "Rothamsted", "Starcross")
plotnames <- paste("Aphid_Rarecurves_", sites, ".jpg",sep = "")
for(ii in 1:length(vegan_aphid_dfs)){
  jpeg(plotnames[ii], width = 8, height = 8, unit = "cm", res = 300)
  par(mfrow = c(1,1), oma= c(0.1,0.1,0.1,0.1),mai= c(0.3,0.3,0.1,0.1),
      cex.axis = 0.4, cex.lab = 0.6, font.lab = 1, mgp = c(0.4,0.02, 0), tck = -0.01)
    jnk1 <- vegan_aphid_dfs[[ii]]
    jnk2 <- rarecurve(jnk1)
    max_ind <- max(apply(jnk1, 1, sum))
    coloors = viridis(nrow(jnk1))
 plot(jnk2[[1]], xlab = "Individuals", ylab = "Species Number", xlim = c(0, max_ind), ylim = c(0, 175), type = "l", lwd = 1,col = coloors[1])
    for(jj in 2:length(jnk2)){
      lines(jnk2[[jj]], lwd = 1, col = coloors[jj])
    }
  legend("topleft", paste(sites[ii]), bty = "n", cex = 0.5)
  legend("bottomright", lty = c(rep(1, nrow(jnk1))), lwd = c(rep(2, nrow(jnk1))),
         col = coloors, legend = c(rownames(jnk1)), cex = 0.3, bty = "n")
  dev.off()
}





################################# all others summary stats and rare curves###########################
jnk1 <- list.gam.dfs
sum(jnk1$abundance_others$Metric)
aggregate(jnk1$abundance_others$Metric, by = list(jnk1$abundance_others$Site), sum)
length(unique(insect_df$taxa_down))
head(insect_df)
sort(table(insect_df$taxa_down))
jnk3 <- rep(insect_df$Order, insect_df$value)
sort(table(jnk3))
sort(table(jnk3))/1112680 * 100

vegan_insect_dfs <- split(insect_df, f = insect_df$site)
for(ii in 1:length(vegan_insect_dfs)){
  vegan_insect_dfs[[ii]] <- as.data.frame.matrix(t(table(vegan_insect_dfs[[ii]]$taxa_down, vegan_insect_dfs[[ii]]$Year)))
}

sites <- c("Hereford", "Newcastle", "Rothamsted", "Starcross")
plotnames <- paste("Insect_Rarecurves_", sites, ".jpg",sep = "")
for(ii in 1:length(vegan_insect_dfs)){
  jpeg(plotnames[ii], width = 8, height = 8, unit = "cm", res = 300)
  par(mfrow = c(1,1), oma= c(0.1,0.1,0.1,0.1),mai= c(0.3,0.3,0.1,0.1),
      cex.axis = 0.4, cex.lab = 0.6, font.lab = 1, mgp = c(0.4,0.02, 0), tck = -0.01)
  jnk1 <- vegan_insect_dfs[[ii]]
  jnk2 <- rarecurve(jnk1)
  max_ind <- max(apply(jnk1, 1, sum))
  coloors = viridis(nrow(jnk1))
  plot(jnk2[[1]], xlab = "Individuals", ylab = "Taxa Number", xlim = c(0, max_ind), ylim = c(0, 75), type = "l", lwd = 1,col = coloors[1])
  for(jj in 2:length(jnk2)){
    lines(jnk2[[jj]], lwd = 1, col = coloors[jj])
  }
  legend("topleft", paste(sites[ii]), bty = "n", cex = 0.5)
  legend("bottomright", lty = c(rep(1, nrow(jnk1))), lwd = c(rep(2, nrow(jnk1))),
         col = coloors, legend = c(rownames(jnk1)), cex = 0.3, bty = "n")
  dev.off()
}
