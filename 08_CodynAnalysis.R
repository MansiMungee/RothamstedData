library(betapart)


#-------------------------------HIGHER CHUNK 01 - PRODUCING VEGAN CDFS ---------------
aphids <- aphid_df
aphids <- aphids[aphids$value > 0, ]

beetles <- insect_df[insect_df$Order == "Coleoptera", ]
beetles <- beetles[beetles$value > 0, ]

trueflies <- insect_df[insect_df$Order == "Diptera", ]
trueflies <- trueflies[trueflies$value > 0, ]

ants <- insect_df[insect_df$Order == "Hymenoptera", ]
ants <- ants[ants$value > 0, ]

pooled <- insect_df[!insect_df$Order == "Araneae", ]
pooled <- pooled[pooled$value > 0, ]

# Bar graph of site wise relative abundance
list_raw_dfs <- list(aphids, beetles, trueflies, ants, pooled)
names(list_raw_dfs) <- c("Aphiodea","Coleoptera", "Diptera", "Hymenoptera", "Pooled")

# list_vegan_cdfs_taxo_down
list_vegan_cdfs_taxo_down <- list()
for(ii in 1:length(list_raw_dfs)){
  print(ii)
  list_vegan_cdfs_taxo_down[[ii]] <- list()
  jnk1 <- list_raw_dfs[[ii]]
  jnk1 <- split(jnk1, f = jnk1$site)
  for(jj in 1:length(jnk1)){
    print(jj)
    list_vegan_cdfs_taxo_down[[ii]][[jj]] <- aggregate(jnk1[[jj]]$value, by = list(jnk1[[jj]]$taxa_down, jnk1[[jj]]$Year, jnk1[[jj]]$Month),FUN = sum, na.rm =TRUE)
    list_vegan_cdfs_taxo_down[[ii]][[jj]]$Site <- paste(list_vegan_cdfs_taxo_down[[ii]][[jj]]$Group.2, list_vegan_cdfs_taxo_down[[ii]][[jj]]$Group.3, sep = "_")
    list_vegan_cdfs_taxo_down[[ii]][[jj]] <- list_vegan_cdfs_taxo_down[[ii]][[jj]][,-c(2:3)]
    list_vegan_cdfs_taxo_down[[ii]][[jj]] <- reshape(list_vegan_cdfs_taxo_down[[ii]][[jj]] , idvar = "Site", timevar = "Group.1", direction = "wide")
    rownames(list_vegan_cdfs_taxo_down[[ii]][[jj]]) <- list_vegan_cdfs_taxo_down[[ii]][[jj]]$Site
    if(ncol(list_vegan_cdfs_taxo_down[[ii]][[jj]]) > 2){
      list_vegan_cdfs_taxo_down[[ii]][[jj]] <- list_vegan_cdfs_taxo_down[[ii]][[jj]][,-1]
      list_vegan_cdfs_taxo_down[[ii]][[jj]][is.na(list_vegan_cdfs_taxo_down[[ii]][[jj]])] <- 0
      colnames(list_vegan_cdfs_taxo_down[[ii]][[jj]]) <- gsub("x.", "", colnames(list_vegan_cdfs_taxo_down[[ii]][[jj]]))
      colnames(list_vegan_cdfs_taxo_down[[ii]][[jj]]) <- gsub(" ", "_", colnames(list_vegan_cdfs_taxo_down[[ii]][[jj]]))
    } else{
      column <- colnames(list_vegan_cdfs_taxo_down[[ii]][[jj]])[2]
      rows <- list_vegan_cdfs_taxo_down[[ii]][[jj]][,1]
      list_vegan_cdfs_taxo_down[[ii]][[jj]] <- list_vegan_cdfs_taxo_down[[ii]][[jj]][,-1]
      list_vegan_cdfs_taxo_down[[ii]][[jj]] <- data.frame(list_vegan_cdfs_taxo_down[[ii]][[jj]])
      colnames(list_vegan_cdfs_taxo_down[[ii]][[jj]]) <- column
      rownames(list_vegan_cdfs_taxo_down[[ii]][[jj]]) <- rows
      colnames(list_vegan_cdfs_taxo_down[[ii]][[jj]]) <- gsub("x.", "", colnames(list_vegan_cdfs_taxo_down[[ii]][[jj]]))
    }
  }
}
names(list_vegan_cdfs_taxo_down) <- names(list_raw_dfs)
for(ii in 1:length(list_vegan_cdfs_taxo_down)){
  names(list_vegan_cdfs_taxo_down[[ii]]) <- names(jnk1)
}

#--CHUNK 08: TOMPORAL CDFS (VEGAN): TAXA TOP - THAT IS FAMILY LEVELS FOR ALL
# list_vegan_cdfs_taxo_top
list_vegan_cdfs_taxo_top <- list()
for(ii in 1:length(list_raw_dfs)){
  print(ii)
  list_vegan_cdfs_taxo_top[[ii]] <- list()
  jnk1 <- list_raw_dfs[[ii]]
  jnk1 <- split(jnk1, f = jnk1$site)
  for(jj in 1:length(jnk1)){
    print(jj)
    list_vegan_cdfs_taxo_top[[ii]][[jj]] <- aggregate(jnk1[[jj]]$value, by = list(jnk1[[jj]]$taxa_top, jnk1[[jj]]$Year, jnk1[[jj]]$Month),FUN = sum, na.rm =TRUE)
    list_vegan_cdfs_taxo_top[[ii]][[jj]]$Site <- paste(list_vegan_cdfs_taxo_top[[ii]][[jj]]$Group.2, list_vegan_cdfs_taxo_top[[ii]][[jj]]$Group.3, sep = "_")
    list_vegan_cdfs_taxo_top[[ii]][[jj]] <- list_vegan_cdfs_taxo_top[[ii]][[jj]][,-c(2:3)]
    list_vegan_cdfs_taxo_top[[ii]][[jj]] <- reshape(list_vegan_cdfs_taxo_top[[ii]][[jj]] , idvar = "Site", timevar = "Group.1", direction = "wide")
    rownames(list_vegan_cdfs_taxo_top[[ii]][[jj]]) <- list_vegan_cdfs_taxo_top[[ii]][[jj]]$Site
    if(ncol(list_vegan_cdfs_taxo_top[[ii]][[jj]]) > 2){
      list_vegan_cdfs_taxo_top[[ii]][[jj]] <- list_vegan_cdfs_taxo_top[[ii]][[jj]][,-1]
      list_vegan_cdfs_taxo_top[[ii]][[jj]][is.na(list_vegan_cdfs_taxo_top[[ii]][[jj]])] <- 0
      colnames(list_vegan_cdfs_taxo_top[[ii]][[jj]]) <- gsub("x.", "", colnames(list_vegan_cdfs_taxo_top[[ii]][[jj]]))
      colnames(list_vegan_cdfs_taxo_top[[ii]][[jj]]) <- gsub(" ", "_", colnames(list_vegan_cdfs_taxo_top[[ii]][[jj]]))
    } else{
      column <- colnames(list_vegan_cdfs_taxo_top[[ii]][[jj]])[2]
      rows <- list_vegan_cdfs_taxo_top[[ii]][[jj]][,1]
      list_vegan_cdfs_taxo_top[[ii]][[jj]] <- list_vegan_cdfs_taxo_top[[ii]][[jj]][,-1]
      list_vegan_cdfs_taxo_top[[ii]][[jj]] <- data.frame(list_vegan_cdfs_taxo_top[[ii]][[jj]])
      colnames(list_vegan_cdfs_taxo_top[[ii]][[jj]]) <- column
      rownames(list_vegan_cdfs_taxo_top[[ii]][[jj]]) <- rows
      colnames(list_vegan_cdfs_taxo_top[[ii]][[jj]]) <- gsub("x.", "", colnames(list_vegan_cdfs_taxo_top[[ii]][[jj]]))
    }
  }
}
names(list_vegan_cdfs_taxo_top) <- names(list_raw_dfs)
for(ii in 1:length(list_vegan_cdfs_taxo_top)){
  names(list_vegan_cdfs_taxo_top[[ii]]) <- names(jnk1)
}



#-------------------------------HIGHER CHUNK 02 - DISTANCE MATRICES - TAXONOMIC DISSIMILARITIES---------------
total.dissimilarity.taxa.top <- list()
for(ii in 1:length(list_vegan_cdfs_taxo_top)){
  print(ii)
  total.dissimilarity.taxa.top[[ii]] <- list()
  jnk1 <- list_vegan_cdfs_taxo_top[[ii]]
  for(jj in 1:length(jnk1)){
    total.dissimilarity.taxa.top[[ii]][[jj]] <- betapart::beta.pair.abund(x = jnk1[[jj]], index.family = "bray")
  }
}
names(total.dissimilarity.taxa.top) <- names(list_raw_dfs)
for(ii in 1:length(total.dissimilarity.taxa.top)){
  names(total.dissimilarity.taxa.top[[ii]]) <- names(jnk1)
}

total.dissimilarity.taxa.down <- list()
for(ii in 1:length(list_vegan_cdfs_taxo_down)){
  print(ii)
  total.dissimilarity.taxa.down[[ii]] <- list()
  jnk1 <- list_vegan_cdfs_taxo_down[[ii]]
  for(jj in 1:length(jnk1)){
    total.dissimilarity.taxa.down[[ii]][[jj]] <- betapart::beta.pair.abund(x = jnk1[[jj]], index.family = "bray")
  }
}
names(total.dissimilarity.taxa.down) <- names(list_raw_dfs)
for(ii in 1:length(total.dissimilarity.taxa.top)){
  names(total.dissimilarity.taxa.down[[ii]]) <- names(jnk1)
}




#-------------------------------HIGHER CHUNK 03 - DISTANCE MATRICES - TEMPORAL DISTANCES---------------
temporal_distance_matrices_taxa_top <- list()
for(ii in 1:length(list_vegan_cdfs_taxo_top)){
  temporal_distance_matrices_taxa_top[[ii]] <- list()
  jnk1 <- list_vegan_cdfs_taxo_top[[ii]]
  for(jj in 1:length(jnk1)){
    jnk2 <- rownames(jnk1[[jj]])
    jnk2 <- data.frame(do.call('rbind',strsplit(as.character(jnk2),'_',fixed=TRUE)))
    jnk2 <- paste(01, match(jnk2$X2, month.abb), jnk2$X1, sep = "/")
    jnk2 <- as.Date(jnk2, "%d/%m/%Y")
    min.Date <- min(jnk2)
    
    elapsed_months <- function(end_date, start_date) {
      ed <- as.POSIXlt(end_date)
      sd <- as.POSIXlt(start_date)
      12 * (ed$year - sd$year) + (ed$mon - sd$mon)
    }
    temporal_distance_matrices_taxa_top[[ii]][[jj]] <- as.matrix(dist(elapsed_months(jnk2, min.Date), diag = TRUE, upper = TRUE))
    rownames( temporal_distance_matrices_taxa_top[[ii]][[jj]]) <- as.character(jnk2)
    colnames( temporal_distance_matrices_taxa_top[[ii]][[jj]]) <- as.character(jnk2)
  }
}
temporal_distance_matrices_taxa_down <- list()
for(ii in 1:length(list_vegan_cdfs_taxo_down)){
  temporal_distance_matrices_taxa_down[[ii]] <- list()
  jnk1 <- list_vegan_cdfs_taxo_down[[ii]]
  for(jj in 1:length(jnk1)){
    jnk2 <- rownames(jnk1[[jj]])
    jnk2 <- data.frame(do.call('rbind',strsplit(as.character(jnk2),'_',fixed=TRUE)))
    jnk2 <- paste(01, match(jnk2$X2, month.abb), jnk2$X1, sep = "/")
    jnk2 <- as.Date(jnk2, "%d/%m/%Y")
    min.Date <- min(jnk2)
    
    elapsed_months <- function(end_date, start_date) {
      ed <- as.POSIXlt(end_date)
      sd <- as.POSIXlt(start_date)
      12 * (ed$year - sd$year) + (ed$mon - sd$mon)
    }
    temporal_distance_matrices_taxa_down[[ii]][[jj]] <- as.matrix(dist(elapsed_months(jnk2, min.Date), diag = TRUE, upper = TRUE))
    rownames( temporal_distance_matrices_taxa_down[[ii]][[jj]]) <- as.character(jnk2)
    colnames( temporal_distance_matrices_taxa_down[[ii]][[jj]]) <- as.character(jnk2)
  }
}

#-------------------------------HIGHER CHUNK 04 - DISTANCE DECAY MODELS---------------


exp.models.bray.top <- list()
exp.models.nest.top <- list()
exp.models.repl.top <- list()

for(ii in 1:length(total.dissimilarity.taxa.top)){
  print(ii)
  exp.models.bray.top[[ii]] <- list()
  exp.models.nest.top[[ii]] <- list()
  exp.models.repl.top[[ii]] <- list()
  jnk1 <- total.dissimilarity.taxa.top[[ii]]
  jnk2 <- temporal_distance_matrices_taxa_top[[ii]]
  for(jj in 1:length(jnk1)){
    print(jj)
    x <- as.dist(jnk2[[jj]])
    y <- jnk1[[jj]]$beta.bray
    exp.models.bray.top[[ii]][[jj]] <- decay.model(1-y, x, model.type = "exp", y.type = "sim")
    
    y <- jnk1[[jj]]$beta.bray.gra
    exp.models.nest.top[[ii]][[jj]] <- decay.model(1-y, x, model.type = "exp", y.type = "sim")
    
    y <- jnk1[[jj]]$beta.bray.bal
    exp.models.repl.top[[ii]][[jj]] <- decay.model(1-y, x, model.type = "exp", y.type = "sim")
    
  }
}


