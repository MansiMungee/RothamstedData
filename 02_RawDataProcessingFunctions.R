#--------------------------------HIGHER CHUNK 01 - AGGREGATED INSECTS WITH METADATA---------------
#--------------------------------CHUNK 01: BASIC FORMATTING 
# STEP 01.
# ADDING A COLUMN FOR ORDER NAMES
file01 <- read.csv("./AllFinal/aggregated_insects_with_metadata.csv", header = TRUE)
################################################### RETRIEVING ORDER NAMES FROM TAXSIZE 
# getting column names from ncbi using package taxize
# depending on the connection, this may interrupt several times before all orders can be retrieved - so I will output the 
# data as a csv at this stage to avoiding repeating this again in future
# extracting unique taxa names
# unique_taxa_top <- unique(file01$taxa_top)
# df <- data.frame(Orig_Taxa_top = unique_taxa_top, order_from_taxa_top = NA)
# for(ii in 1:nrow(df)){
#   df$order_from_taxa_top[ii] <- tax_name(sci = df$Orig_Taxa_top[ii], get = "order", db = "ncbi")$order
# }
# colnames(df) <- c("taxa_top", "Order")
# df1 <- df
# df2 <- df1[!complete.cases(df1$Order),]
# for(ii in 1:nrow(df2)){
#   df2$Order[ii] <- tax_name(sci = df2$taxa_top[ii], get = "order", db = "ncbi")$order
# }
# df1 <-  df1[complete.cases(df1$Order),]
# df <- rbind(df1, df2)
# write.csv(df, "Orders_Retrieved_from_Taxize.csv")
###
# for multi days sampling - I will divide the 'value' by the number of days, and round it off
head(file01)
file01$days[is.na(file01$days)] <- 1
jnk1 <- file01[file01$days > 1,]
jnk2 <- jnk1$value/jnk1$days
jnk2 <- round(jnk2, digits = 0)
jnk1$value <- jnk2
jnk2 <- file01[!file01$days > 1,]
nrow(file01)
file01_v2 <- rbind(jnk1,jnk2)
nrow(file01_v2)
file01 <- file01_v2
# merging order names to orig file
file01_trunc3 <- file01[,c(1,2,3,6,7,8)]
# now merge
df <- read.csv("Orders_Retrieved_from_Taxize.csv", header = TRUE)
df <- df[,-1]
file02 <- merge(file01_trunc3, df, by = "taxa_top")
# now fixing the missing orders
jnk1 <- file02[is.na(file02$Order),]
unique(jnk1$taxa_top) # "Apoidea (Apidae s.l.)" "Psylliodea"
for(ii in 1:nrow(jnk1)){
  if(jnk1$taxa_top[ii] == 'Apoidea (Apidae s.l.)'){
    jnk1$Order[ii] <- 'Diptera'
  } else{
    if(jnk1$taxa_top[ii] == 'Psylliodea'){
      jnk1$Order[ii] <- 'Hemiptera'
    } else{
      jnk1$Order[ii] <- NA
    }
  }
}
jnk2 <- file02[!is.na(file02$Order),]
file02 <- rbind(jnk1,jnk2)
nrow(file02)
insect_df <- file02
# add month and year to insect df for consistency
jnk1 <- insect_df
jnk2 <- jnk1$date
jnk2 <- data.frame(do.call('rbind',strsplit(as.character(jnk2),'/',fixed=TRUE)))
insect_df$Year <- jnk2$X3
insect_df$Month <- month.abb[as.numeric(jnk2$X2)]
# standardize insect_df to equal number of samples per year per site
head(insect_df)
insect_df_proc01 <- data.frame(Date = rep(insect_df$date, insect_df$value), 
                   Taxa_Top = rep(insect_df$taxa_top, insect_df$value),
                   Taxa_Down = rep(insect_df$taxa_down, insect_df$value),
                   Site = rep(insect_df$site, insect_df$value),
                   Order = rep(insect_df$Order, insect_df$value))

sort(table(insect_df_proc01$Order))
# total_insects = Hymenoptera + Coleoptera + Diptera + Hemiptera (not taking psocoptera cause only order level identification)  
# Individual orders = Hymenoptera, Coleoptera, Diptera, Hemiptera, Aphids
jnk1 <- table(insect_df_proc01$Order, insect_df_proc01$Taxa_Top)
unique.taxa <- function(x){
  x <- x[x>0]
  length(x)
}
n.taxa.down <- sort(apply(jnk1, 1, FUN = unique.taxa))
n.taxa.top <- sort(apply(jnk1, 1, FUN = unique.taxa))
# after this point - shift to chunk 1 of 03_RarefactionStandardization.R - NO standardization needed, but just some summary stats are produced for supplemenatry










#--------------------------------HIGHER CHUNK 02 - APHIDS---------------
aphid_df <- read.csv("./AllFinal/aphid_data_2000-2018.csv", 
                     header = TRUE)
# you need to get family for aphids (i.e. taxa down)
# RETRIEVING FAMILY NAMES FROM TAXSIZE #
# getting column names from ncbi using package taxize
# depending on the connection, this may interrupt several times before all orders can be retrieved - so I will output the 
# data as a csv at this stage to avoiding repeating this again in future
# df <- aphid_df
# binomials <- unique(df$binomial)
# jnk.df <- data.frame(binomials = binomials, families = NA)
# for(ii in 1:nrow(jnk.df)){
#   jnk.df$families[ii] <- tax_name(sci = jnk.df$binomials[ii], get = "family", db = "ncbi")$family
# }
# jnk1.df <- jnk.df[complete.cases(jnk.df$families),] # keep for rbinding later
# jnk2.df <- jnk.df[!complete.cases(jnk.df$families),] # re-run
# for(ii in 1:nrow(jnk2.df)){
#   jnk2.df$families[ii] <- tax_name(sci = jnk2.df$binomials[ii], get = "family", db = "ncbi")$family
# }
# jnk3.df <- jnk2.df[complete.cases(jnk2.df$families),] # keep for rbinding later
# jnk4.df <- jnk2.df[!complete.cases(jnk2.df$families),] # re-run
# for(ii in 1:nrow(jnk4.df)){
#   jnk4.df$families[ii] <- tax_name(sci = jnk4.df$binomials[ii], get = "family", db = "ncbi")$family
# }
# jnk5.df <- jnk4.df[complete.cases(jnk4.df$families),] # keep for rbinding later
# jnk6.df <- jnk4.df[!complete.cases(jnk4.df$families),] # re-run
# for(ii in 1:nrow(jnk6.df)){
#   jnk6.df$families[ii] <- tax_name(sci = jnk6.df$binomials[ii], get = "family", db = "ncbi")$family
# }
# jnk7.df <- jnk6.df[complete.cases(jnk6.df$families),] # keep for rbinding later
# jnk8.df <- jnk6.df[!complete.cases(jnk6.df$families),] # re-run
# for(ii in 1:nrow(jnk8.df)){
#   jnk8.df$families[ii] <- tax_name(sci = jnk8.df$binomials[ii], get = "family", db = "ncbi")$family
# }
# # finally done
# new.df <- rbind(jnk1.df, jnk3.df,jnk5.df, jnk7.df, jnk8.df)
# write.csv(new.df, "AphidFamilies_Retrieved_from_Taxize.csv") # fill the remaining ones manually by web sear in this csv
aphid_families <- read.csv("./IntermediateFiles_NeededForPlotting/AphidFamilies_Retrieved_from_Taxize.csv", header = TRUE)
aphid_families <- aphid_families[,-1]
aphid_df <- data.frame(taxa_top = "Aphids", date = aphid_df$CalDate,
                       site = aphid_df$TrapName, value = aphid_df$DailyCount,
                       taxa_down = aphid_df$binomial, Order = "Hemiptera")
# changing site name in aphid_df to match insect_df
aphids.hereford <- aphid_df[aphid_df$site %in% c("Hereford (Rosemaund)"),]
aphids.hereford$site <- c("Hereford")
aphids.rest <-  aphid_df[!aphid_df$site %in% c("Hereford (Rosemaund)"),]
aphid_df <- rbind(aphids.rest, aphids.hereford)
# replacing taxa_top by family names retrieved from taxsize
# for(ii in 1:nrow(aphid_df)){
#   for(jj in 1:nrow(aphid_families)){
#     print(nrow(aphid_df))
#     print(ii)
#     if(aphid_df$taxa_down[ii] == aphid_families$binomials[jj]){
#       aphid_df$taxa_top[ii] <- aphid_families$families[jj]
#     } else{
#       aphid_df$taxa_top[ii] <- aphid_df$taxa_top[ii]
#     }
#   }
# }
# head(aphid_df)
# # ridiculously slow so output the csv
# write.csv(aphid_df, "aphids_With_familiesfinal.csv")
aphid_df <- read.csv("./IntermediateFiles_NeededForPlotting/aphids_With_familiesfinal.csv", header = TRUE)
# let us look at the years for which aphids are collected
jnk1 <- aphid_df
jnk2 <- jnk1$date
jnk2 <- data.frame(do.call('rbind',strsplit(as.character(jnk2),'/',fixed=TRUE)))
aphid_df$Year <- jnk2$X3
aphid_df$Month <- month.abb[as.numeric(jnk2$X2)]
table(aphid_df$site, aphid_df$Year)
# at this point move to 03_RarefactionSTandardization.R - THEN COME BACK HERE AND CONTINUE





































































#--------------------------------HIGHER CHUNK 03 - PRODUCING GAM READY DATA FRAMES FOR #####
# All INsect
jnk1 <- insect_df
jnk1 <- jnk1[!jnk1$Order == "Araneae",]
jnk1 <- jnk1[jnk1$value > 0,]
unique(jnk1$Order)
jnk1$Group <- jnk1$Order
jnk1$Group <- gsub("Neuroptera", "Others",jnk1$Group)
jnk1$Group <- gsub("Dermaptera", "Others",jnk1$Group)
jnk1$Group <- gsub("Ephemeroptera", "Others",jnk1$Group)
jnk1$Group <- gsub("Trichoptera", "Others",jnk1$Group)
jnk1$Group <- gsub("Lepidoptera", "Others",jnk1$Group)
jnk1$Group <- gsub("Psocoptera", "Others",jnk1$Group)
jnk1$Group <- gsub("Hemiptera", "Others",jnk1$Group)
unique(jnk1$Group)
insect_df <- jnk1
insect_df_list <- split(insect_df, f = insect_df$Group)
names(insect_df_list)
func_to_produce_gam_dfs <- function(list_object){
  jnk1 <- list_object
  jnk1 <- list.rbind(jnk1)
  
  ####################### ABUNDANCES
  ####################### Total month wise abundance - across total by catch
  tot_abund <- aggregate(jnk1$value, by = list(jnk1$Year, jnk1$Month, jnk1$site),FUN = sum, na.rm =TRUE)
  colnames(tot_abund) <- c("Year", "Month", "Site","Abundance")
  ####################### Total month wise abundance - for each order
  order_wise_abund <- aggregate(jnk1$value, by = list(jnk1$Year, jnk1$Month, jnk1$site, jnk1$Group),FUN = sum, na.rm =TRUE)
  colnames(order_wise_abund) <- c("Year", "Month", "Site", "Order","Abundance")
  # merge abundance dataframe to env dfs to generate gam-ready-dfs
  env_df_gam <- read.csv("./IntermediateFiles_NeededForPlotting/Final_Predictor_DF_For_GAM.csv", header = TRUE)
  tot_abund$Year <- as.numeric(tot_abund$Year)
  tot_abund_gam_df <- merge(env_df_gam, tot_abund, by = c("Year", "Site", "Month"))
  order_wise_abund_gam_df <- merge(env_df_gam, order_wise_abund, by = c("Year", "Site", "Month"))
  # transform gam_dfs to ensure classes of individual columns/variables are analysis compatible
  # step 01. add date columns - needed to model months as cyclic covariates
  tot_abund_gam_df$Date <- paste("01", match(tot_abund_gam_df$Month, month.abb), tot_abund_gam_df$Year, sep = "/")
  tot_abund_gam_df$Date <- as.Date.character(tot_abund_gam_df$Date, format = "%d/%m/%Y") # add date column for first of each month - needed to model months as cyclic covariates
  order_wise_abund_gam_df$Date <- paste("01", match(order_wise_abund_gam_df$Month, month.abb), order_wise_abund_gam_df$Year, sep = "/")
  order_wise_abund_gam_df$Date <- as.Date.character(order_wise_abund_gam_df$Date, format = "%d/%m/%Y") # add date column for first of each month - needed to model months as cyclic covariates
  # step 02. transform the remaining columns to numeric or date as appropriate
  tot_abund_gam_df <- transform(tot_abund_gam_df, RH = as.numeric(RH),
                                PET = as.numeric(PET),
                                PPT = as.numeric(PPT),
                                WIND = as.numeric(WIND),
                                Tmean = as.numeric(Tmean),
                                SUN = as.numeric(SUN),
                                Tmax = as.numeric(Tmax),
                                Tmin = as.numeric(Tmin),
                                PSL = as.numeric(PSL),
                                PV = as.numeric(PV),
                                SNOW = as.numeric(SNOW),
                                FROST = as.numeric(FROST),
                                LUMEN = as.numeric(LUMEN),
                                Arable_25km = as.numeric(Arable_25km),
                                Grassland_25km = as.numeric(Grassland_25km),
                                Urban_25km = as.numeric(Urban_25km),
                                Abundance = as.numeric(Abundance),
                                Year  = as.numeric(format(Date, '%Y')),
                                Month = as.numeric(format(Date, '%m')), 
                                Site = as.character(Site),
                                Year_factor = as.factor(as.character(as.numeric(format(Date, '%Y')))),
                                Group = "Total_Except_Aphids")
  
  order_wise_abund_gam_df <- transform(order_wise_abund_gam_df,RH = as.numeric(RH),
                                       PET = as.numeric(PET),
                                       PPT = as.numeric(PPT),
                                       WIND = as.numeric(WIND),
                                       Tmean = as.numeric(Tmean),
                                       SUN = as.numeric(SUN),
                                       Tmax = as.numeric(Tmax),
                                       Tmin = as.numeric(Tmin),
                                       PSL = as.numeric(PSL),
                                       PV = as.numeric(PV),
                                       SNOW = as.numeric(SNOW),
                                       FROST = as.numeric(FROST),
                                       LUMEN = as.numeric(LUMEN),
                                       Arable_25km = as.numeric(Arable_25km),
                                       Grassland_25km = as.numeric(Grassland_25km),
                                       Urban_25km = as.numeric(Urban_25km),
                                       Abundance = as.numeric(Abundance),
                                       Year  = as.numeric(format(Date, '%Y')),
                                       Month = as.numeric(format(Date, '%m')), 
                                       Site = as.character(Site),
                                       Year_factor = as.factor(as.character(as.numeric(format(Date, '%Y')))),
                                       Group = as.character(Order))
  # at the end of this chunk you have: tot_abund_gam_df, order_wise_abund_gam_df
  
  
  
  
  
  ####################### SPECIES RICHNESS
  ####################### Total month wise SPECIES RICHNESS - across total by catch
  ####################### Total month wise species richness - across each order - using taxa top
  ## the chunk below will give you month wise species richness for across total by-catch (all orders combined using taxa_top)
  func_to_generate_vegan_cdfs_monthwise_total <- function(x, taxa_up_or_down){
    jnk1 <- x
    jnk1 <- list.rbind(jnk1)
    jnk1$Month_Year <- paste(jnk1$Month, jnk1$Year, sep = "_")
    vegan_cdfs <- aggregate(jnk1$value, by = list(jnk1[[taxa_up_or_down]], jnk1$Month_Year, jnk1$site),FUN = sum, na.rm =TRUE)
    vegan_cdfs <- split(vegan_cdfs, f = vegan_cdfs$Group.3)
    for(ii in 1:length(vegan_cdfs)){
      print(ii)
      vegan_cdfs[[ii]] <- vegan_cdfs[[ii]][,-3]
      vegan_cdfs[[ii]] <- reshape(vegan_cdfs[[ii]], idvar = "Group.2", timevar = "Group.1", direction = "wide")
      vegan_cdfs[[ii]][is.na(vegan_cdfs[[ii]])] <- 0
      rownames(vegan_cdfs[[ii]]) <- vegan_cdfs[[ii]]$Group.2
      vegan_cdfs[[ii]] <- vegan_cdfs[[ii]][,-1]
    }
    return(vegan_cdfs)   
  }
  tot_vegan_cdfs_taxa_top <- func_to_generate_vegan_cdfs_monthwise_total(x = list_object, taxa_up_or_down = "taxa_top") # family level stuff
  tot_rich_taxa_top <-  lapply(tot_vegan_cdfs_taxa_top, specnumber, MARGIN = 1)
  ## the chunk below will give you month wise species richness for each order individually using taxa_top
  func_to_generate_vegan_cdfs_monthwise_orderwise <- function(x, taxa_up_or_down){
    jnk1 <- x
    list_vegan_cdfs <- list()
    for(ii in 1:length(jnk1)){
      print(paste("ii"))
      print(ii)
      list_vegan_cdfs[[ii]] <- list()
      jnk2 <- jnk1[[ii]]
      jnk2$Month_Year <- paste(jnk2$Year, jnk2$Month, sep = "_")
      jnk2 <- split(jnk2, f = jnk2$site)
      for(jj in 1:length(jnk2)){
        print(paste("jj"))
        print(jj)
        list_vegan_cdfs[[ii]][[jj]] <- aggregate(jnk2[[jj]]$value, by = list(jnk2[[jj]][[taxa_up_or_down]], jnk2[[jj]]$Month_Year),FUN = sum, na.rm =TRUE)
        list_vegan_cdfs[[ii]][[jj]] <- reshape(list_vegan_cdfs[[ii]][[jj]] , idvar = "Group.2", timevar = "Group.1", direction = "wide")
        rownames(list_vegan_cdfs[[ii]][[jj]]) <- list_vegan_cdfs[[ii]][[jj]]$Group.2
        if(ncol(list_vegan_cdfs[[ii]][[jj]]) > 2){
          list_vegan_cdfs[[ii]][[jj]] <- list_vegan_cdfs[[ii]][[jj]][,-1]
          list_vegan_cdfs[[ii]][[jj]][is.na(list_vegan_cdfs[[ii]][[jj]])] <- 0
          colnames(list_vegan_cdfs[[ii]][[jj]]) <- gsub("x.", "", colnames(list_vegan_cdfs[[ii]][[jj]]))
          colnames(list_vegan_cdfs[[ii]][[jj]]) <- gsub(" ", "_", colnames(list_vegan_cdfs[[ii]][[jj]]))
        } else{
          column <- colnames(list_vegan_cdfs[[ii]][[jj]])[2]
          rows <- list_vegan_cdfs[[ii]][[jj]][,1]
          list_vegan_cdfs[[ii]][[jj]] <- list_vegan_cdfs[[ii]][[jj]][,-1]
          list_vegan_cdfs[[ii]][[jj]] <- data.frame(list_vegan_cdfs[[ii]][[jj]])
          colnames(list_vegan_cdfs[[ii]][[jj]]) <- column
          rownames(list_vegan_cdfs[[ii]][[jj]]) <- rows
          colnames(list_vegan_cdfs[[ii]][[jj]]) <- gsub("x.", "", colnames(list_vegan_cdfs[[ii]][[jj]]))
        }
      }
    }
    
    site_names <- sort(unique(jnk1$aphids$site))
    for(ii in 1:length(list_vegan_cdfs)){
      names(list_vegan_cdfs[[ii]]) <- site_names
    }
    
    names(list_vegan_cdfs) <- names(x)
    
    return(list_vegan_cdfs)
  }
  order_wise_vegan_cdfs_taxa_top <- func_to_generate_vegan_cdfs_monthwise_orderwise(x = list_object, taxa_up_or_down = "taxa_top") # family level stuff
  # vegan_cdfs_taxa_down <- func_to_generate_vegan_cdfs_monthwise(x = list_object, taxa_up_or_down = "taxa_down") # lower taxonomic resolution; not using this for now only doing taxa_top for now - it makes more sense
  order_wise_rich_taxa_top <- list()
  for(ii in 1:length(order_wise_vegan_cdfs_taxa_top)){
    order_wise_rich_taxa_top[[ii]] <- list()
    for(jj in 1:length(order_wise_vegan_cdfs_taxa_top[[ii]])){
      order_wise_rich_taxa_top[[ii]][[jj]] <- specnumber(order_wise_vegan_cdfs_taxa_top[[ii]][[jj]], MARGIN = 1)
    }
  }
  site_names <- c("Hereford", "Newcastle", "Rothamsted", "Starcross")
  for(ii in 1:length(order_wise_rich_taxa_top)){
    names(order_wise_rich_taxa_top[[ii]]) <- site_names
  }
  names(order_wise_rich_taxa_top) <- names(order_wise_vegan_cdfs_taxa_top)
  # at the end of this chunk you have: tot_rich_taxa_top, order_wise_rich_taxa_top
  # now moving on to next chunk: 
  # merge richness dataframe to env dfs to generate gam-ready-dfs
  for(ii in 1:length(tot_rich_taxa_top)){
    print(ii)
    tot_rich_taxa_top[[ii]] <- data.frame(Site = names(tot_rich_taxa_top)[[ii]], Month_Year = names(tot_rich_taxa_top[[ii]]), Richness = as.numeric(tot_rich_taxa_top[[ii]]))
    jnk2 <- data.frame(do.call('rbind',strsplit(as.character(tot_rich_taxa_top[[ii]]$Month_Year),'_',fixed = TRUE)))
    tot_rich_taxa_top[[ii]]$Month <- jnk2$X1
    tot_rich_taxa_top[[ii]]$Year <- jnk2$X2
    tot_rich_taxa_top[[ii]] <- tot_rich_taxa_top[[ii]][,-2]
  }
  jnk1 <- tot_rich_taxa_top
  jnk1 <- list.rbind(jnk1)
  rownames(jnk1) <- seq(1, nrow(jnk1))
  tot_rich_taxa_top <- jnk1
  jnk33 <- order_wise_rich_taxa_top
  for(ii in 1:length(jnk33)){
    print(ii)
    for(jj in 1:length(jnk33[[ii]])){
      print(jj)
      jnk33[[ii]][[jj]] <- data.frame(Taxa = names(jnk33)[ii], Site = names(jnk33[[ii]])[jj], Month_Year = names(jnk33[[ii]][[jj]]), Richness = as.numeric(jnk33[[ii]][[jj]]))
      jnk2 <- data.frame(do.call('rbind',strsplit(as.character(jnk33[[ii]][[jj]]$Month_Year),'_',fixed = TRUE)))
      jnk33[[ii]][[jj]]$Month <- jnk2$X2
      jnk33[[ii]][[jj]]$Year <- jnk2$X1
      jnk33[[ii]][[jj]] <- jnk33[[ii]][[jj]][,-3] 
    }
  }
  for(ii in 1:length(jnk33)){
    jnk33[[ii]] <- list.rbind(jnk33[[ii]])
  }
  jnk33 <- list.rbind(jnk33)
  rownames(jnk33) <- seq(1, nrow(jnk33))
  order_wise_rich_taxa_top <- jnk33
  tot_rich_gam_df <- merge(env_df_gam, tot_rich_taxa_top, by = c("Year", "Site", "Month"))
  order_wise_rich_taxa_top <- merge(env_df_gam, order_wise_rich_taxa_top, by = c("Year", "Site", "Month"))
  # transform gam_dfs to ensure classes of individual columns/variables are analysis compatible
  # step 01. add date columns - needed to model months as cyclic covariates
  tot_rich_gam_df$Date <- paste("01", match(tot_rich_gam_df$Month, month.abb), tot_rich_gam_df$Year, sep = "/")
  tot_rich_gam_df$Date <- as.Date.character(tot_rich_gam_df$Date, format = "%d/%m/%Y") # add date column for first of each month - needed to model months as cyclic covariates
  order_wise_rich_taxa_top$Date <- paste("01", match(order_wise_rich_taxa_top$Month, month.abb), order_wise_rich_taxa_top$Year, sep = "/")
  order_wise_rich_taxa_top$Date <- as.Date.character(order_wise_rich_taxa_top$Date, format = "%d/%m/%Y") # add date column for first of each month - needed to model months as cyclic covariates
  # step 02. transform the remaining columns to numeric or date as appropriate
  tot_rich_gam_df <- transform(tot_rich_gam_df, RH = as.numeric(RH),
                               PET = as.numeric(PET),
                               PPT = as.numeric(PPT),
                               WIND = as.numeric(WIND),
                               Tmean = as.numeric(Tmean),
                               SUN = as.numeric(SUN),
                               Tmax = as.numeric(Tmax),
                               Tmin = as.numeric(Tmin),
                               PSL = as.numeric(PSL),
                               PV = as.numeric(PV),
                               SNOW = as.numeric(SNOW),
                               FROST = as.numeric(FROST),
                               LUMEN = as.numeric(LUMEN),
                               Arable_25km = as.numeric(Arable_25km),
                               Grassland_25km = as.numeric(Grassland_25km),
                               Urban_25km = as.numeric(Urban_25km),
                               Richness = as.numeric(Richness),
                               Year  = as.numeric(format(Date, '%Y')),
                               Month = as.numeric(format(Date, '%m')), 
                               Site = as.character(Site),
                               Year_factor = as.factor(as.character(as.numeric(format(Date, '%Y')))),
                               Group = "All_Except_Aphids")
  
  order_wise_rich_taxa_top <- transform(order_wise_rich_taxa_top, RH = as.numeric(RH),
                                        PET = as.numeric(PET),
                                        PPT = as.numeric(PPT),
                                        WIND = as.numeric(WIND),
                                        Tmean = as.numeric(Tmean),
                                        SUN = as.numeric(SUN),
                                        Tmax = as.numeric(Tmax),
                                        Tmin = as.numeric(Tmin),
                                        PSL = as.numeric(PSL),
                                        PV = as.numeric(PV),
                                        SNOW = as.numeric(SNOW),
                                        FROST = as.numeric(FROST),
                                        LUMEN = as.numeric(LUMEN),
                                        Arable_25km = as.numeric(Arable_25km),
                                        Grassland_25km = as.numeric(Grassland_25km),
                                        Urban_25km = as.numeric(Urban_25km),
                                        Richness = as.numeric(Richness),
                                        Year  = as.numeric(format(Date, '%Y')),
                                        Month = as.numeric(format(Date, '%m')), 
                                        Site = as.character(Site),
                                        Year_factor = as.factor(as.character(as.numeric(format(Date, '%Y')))),
                                        Group = as.character(Taxa))
  
  # at the end of this chunk you have: tot_abund_gam_df, order_wise_abund_gam_df, tot_rich_gam_df and order_wise_rich_taxa_top
  
  return(list(tot_abund_gam_df, order_wise_abund_gam_df, tot_rich_gam_df, order_wise_rich_taxa_top))
  
  
}
func_to_produce_aphid_gam_dfs <- function(df){
  jnk1 <- df
  ####################### ABUNDANCES
  ####################### Total month wise abundance - across total by catch
  tot_abund <- aggregate(jnk1$value, by = list(jnk1$Year, jnk1$Month, jnk1$site),FUN = sum, na.rm =TRUE)
  colnames(tot_abund) <- c("Year", "Month", "Site","Abundance")
  # merge abundance dataframe to env dfs to generate gam-ready-dfs
  env_df_gam <- read.csv("./IntermediateFiles_NeededForPlotting/Final_Predictor_DF_For_GAM.csv", header = TRUE)
  tot_abund_gam_df <- merge(env_df_gam, tot_abund, by = c("Year", "Site", "Month"))
  
  # transform gam_dfs to ensure classes of individual columns/variables are analysis compatible
  # step 01. add date columns - needed to model months as cyclic covariates
  tot_abund_gam_df$Date <- paste("01", match(tot_abund_gam_df$Month, month.abb), tot_abund_gam_df$Year, sep = "/")
  tot_abund_gam_df$Date <- as.Date.character(tot_abund_gam_df$Date, format = "%d/%m/%Y") # add date column for first of each month - needed to model months as cyclic covariates
  # step 02. transform the remaining columns to numeric or date as appropriate
  tot_abund_gam_df <- transform(tot_abund_gam_df, RH = as.numeric(RH),
                                PET = as.numeric(PET),
                                PPT = as.numeric(PPT),
                                WIND = as.numeric(WIND),
                                Tmean = as.numeric(Tmean),
                                SUN = as.numeric(SUN),
                                Tmax = as.numeric(Tmax),
                                Tmin = as.numeric(Tmin),
                                PSL = as.numeric(PSL),
                                PV = as.numeric(PV),
                                SNOW = as.numeric(SNOW),
                                FROST = as.numeric(FROST),
                                LUMEN = as.numeric(LUMEN),
                                Arable_25km = as.numeric(Arable_25km),
                                Grassland_25km = as.numeric(Grassland_25km),
                                Urban_25km = as.numeric(Urban_25km),
                                Abundance = as.numeric(Abundance),
                                Year  = as.numeric(format(Date, '%Y')),
                                Month = as.numeric(format(Date, '%m')), 
                                Site = as.character(Site),
                                Year_factor = as.factor(as.character(as.numeric(format(Date, '%Y')))),
                                Group = "Aphids")
  
  
  
  
  
  
  
  
  
  ####################### SPECIES RICHNESS
  ####################### Total month wise species richness - for only aphids using taxa top
  ## the chunk below will give you month wise species richness for across total by-catch (all orders combined using taxa_top)
  func_to_generate_vegan_cdfs_monthwise_total <- function(x, taxa_up_or_down){
    jnk1 <- x
    jnk1$Month_Year <- paste(jnk1$Month, jnk1$Year, sep = "_")
    vegan_cdfs <- aggregate(jnk1$value, by = list(jnk1[[taxa_up_or_down]], jnk1$Month_Year, jnk1$site),FUN = sum, na.rm =TRUE)
    vegan_cdfs <- split(vegan_cdfs, f = vegan_cdfs$Group.3)
    for(ii in 1:length(vegan_cdfs)){
      print(ii)
      vegan_cdfs[[ii]] <- vegan_cdfs[[ii]][,-3]
      vegan_cdfs[[ii]] <- reshape(vegan_cdfs[[ii]], idvar = "Group.2", timevar = "Group.1", direction = "wide")
      vegan_cdfs[[ii]][is.na(vegan_cdfs[[ii]])] <- 0
      rownames(vegan_cdfs[[ii]]) <- vegan_cdfs[[ii]]$Group.2
      vegan_cdfs[[ii]] <- vegan_cdfs[[ii]][,-1]
    }
    return(vegan_cdfs)   
  }
  tot_vegan_cdfs_taxa_top <- func_to_generate_vegan_cdfs_monthwise_total(x = df, taxa_up_or_down = "taxa_top") # family level stuff
  tot_rich_taxa_top <-  lapply(tot_vegan_cdfs_taxa_top, specnumber, MARGIN = 1)
  # at the end of this chunk you have: tot_rich_taxa_top
  # now moving on to next chunk: 
  # merge richness dataframe to env dfs to generate gam-ready-dfs
  for(ii in 1:length(tot_rich_taxa_top)){
    print(ii)
    tot_rich_taxa_top[[ii]] <- data.frame(Site = names(tot_rich_taxa_top)[[ii]], Month_Year = names(tot_rich_taxa_top[[ii]]), Richness = as.numeric(tot_rich_taxa_top[[ii]]))
    jnk2 <- data.frame(do.call('rbind',strsplit(as.character(tot_rich_taxa_top[[ii]]$Month_Year),'_',fixed = TRUE)))
    tot_rich_taxa_top[[ii]]$Month <- jnk2$X1
    tot_rich_taxa_top[[ii]]$Year <- jnk2$X2
    tot_rich_taxa_top[[ii]] <- tot_rich_taxa_top[[ii]][,-2]
  }
  jnk1 <- tot_rich_taxa_top
  jnk1 <- list.rbind(jnk1)
  rownames(jnk1) <- seq(1, nrow(jnk1))
  tot_rich_taxa_top <- jnk1
  tot_rich_gam_df <- merge(env_df_gam, tot_rich_taxa_top, by = c("Year", "Site", "Month"))
  # transform gam_dfs to ensure classes of individual columns/variables are analysis compatible
  # step 01. add date columns - needed to model months as cyclic covariates
  tot_rich_gam_df$Date <- paste("01", match(tot_rich_gam_df$Month, month.abb), tot_rich_gam_df$Year, sep = "/")
  tot_rich_gam_df$Date <- as.Date.character(tot_rich_gam_df$Date, format = "%d/%m/%Y") # add date column for first of each month - needed to model months as cyclic covariates
  # step 02. transform the remaining columns to numeric or date as appropriate
  tot_rich_gam_df <- transform(tot_rich_gam_df, RH = as.numeric(RH),
                               PET = as.numeric(PET),
                               PPT = as.numeric(PPT),
                               WIND = as.numeric(WIND),
                               Tmean = as.numeric(Tmean),
                               SUN = as.numeric(SUN),
                               Tmax = as.numeric(Tmax),
                               Tmin = as.numeric(Tmin),
                               PSL = as.numeric(PSL),
                               PV = as.numeric(PV),
                               SNOW = as.numeric(SNOW),
                               FROST = as.numeric(FROST),
                               LUMEN = as.numeric(LUMEN),
                               Arable_25km = as.numeric(Arable_25km),
                               Grassland_25km = as.numeric(Grassland_25km),
                               Urban_25km = as.numeric(Urban_25km),
                               Richness = as.numeric(Richness),
                               Year  = as.numeric(format(Date, '%Y')),
                               Month = as.numeric(format(Date, '%m')), 
                               Site = as.character(Site),
                               Year_factor = as.factor(as.character(as.numeric(format(Date, '%Y')))),
                               Group = "Aphids")
  
  # at the end of this chunk you have: tot_abund_gam_df, tot_rich_gam_df
  
  return(list(tot_abund_gam_df, tot_rich_gam_df))
}
# calculations
tot_abund_aphids <- func_to_produce_aphid_gam_dfs(df = aphid_df)[[1]]
richness_aphids <- func_to_produce_aphid_gam_dfs(df = aphid_df)[[2]]
tot_abund_remaining <- func_to_produce_gam_dfs(list_object = insect_df_list)[[1]]
orderwise_abund_remaining <- func_to_produce_gam_dfs(list_object = insect_df_list)[[2]]
tot_richness_remaining <- func_to_produce_gam_dfs(list_object = insect_df_list)[[3]]
orderwise_richness_remaining <- func_to_produce_gam_dfs(list_object = insect_df_list)[[4]]

# remove the row numbers that came inadvertantly
tot_abund_aphids <- tot_abund_aphids[,-4]
richness_aphids <-  richness_aphids[,-4]
tot_abund_remaining <-  tot_abund_remaining[,-4]
orderwise_abund_remaining <-  orderwise_abund_remaining[,-4]
tot_richness_remaining <-  tot_richness_remaining[,-4]
orderwise_richness_remaining <-  orderwise_richness_remaining[,-4]

# # in 04a_AbioticDrivers - you selected final variables as 
# final variables = lumen, PET, rainfall, sfcwind, tmean
# urban, grassland and arable 25km
# so remove extra columns from each gam df
tot_abund_aphids <- data.frame(Metric = tot_abund_aphids$Abundance,
                               Year = tot_abund_aphids$Year, 
                               Month = tot_abund_aphids$Month,
                               Site = tot_abund_aphids$Site, 
                               Lumen = tot_abund_aphids$LUMEN,
                               PET = tot_abund_aphids$PET, 
                               PPT = tot_abund_aphids$PPT,
                               WIND = tot_abund_aphids$WIND, 
                               TMean = tot_abund_aphids$Tmean,
                               Arable = tot_abund_aphids$Arable_25km,
                               Urban = tot_abund_aphids$Urban_25km,
                               Grassland = tot_abund_aphids$Grassland_25km,
                               Year_factor = tot_abund_aphids$Year_factor,
                               Group = tot_abund_aphids$Group)

tot_abund_remaining <- data.frame(Metric = tot_abund_remaining$Abundance,
                                  Year = tot_abund_remaining$Year, 
                                  Month = tot_abund_remaining$Month,
                                  Site = tot_abund_remaining$Site, 
                                  Lumen = tot_abund_remaining$LUMEN,
                                  PET = tot_abund_remaining$PET, 
                                  PPT = tot_abund_remaining$PPT,
                                  WIND = tot_abund_remaining$WIND, 
                                  TMean = tot_abund_remaining$Tmean,
                                  Arable = tot_abund_remaining$Arable_25km,
                                  Urban = tot_abund_remaining$Urban_25km,
                                  Grassland = tot_abund_remaining$Grassland_25km,
                                  Year_factor = tot_abund_remaining$Year_factor,
                                  Group = tot_abund_remaining$Group)


abund_remaining_orderwise <- data.frame(Metric = orderwise_abund_remaining$Abundance,
                                        Year = orderwise_abund_remaining$Year, 
                                        Month = orderwise_abund_remaining$Month,
                                        Site = orderwise_abund_remaining$Site, 
                                        Lumen = orderwise_abund_remaining$LUMEN,
                                        PET = orderwise_abund_remaining$PET, 
                                        PPT = orderwise_abund_remaining$PPT,
                                        WIND = orderwise_abund_remaining$WIND, 
                                        TMean = orderwise_abund_remaining$Tmean,
                                        Arable = orderwise_abund_remaining$Arable_25km,
                                        Urban = orderwise_abund_remaining$Urban_25km,
                                        Grassland = orderwise_abund_remaining$Grassland_25km,
                                        Year_factor = orderwise_abund_remaining$Year_factor,
                                        Group = orderwise_abund_remaining$Group)


richness_aphids <- data.frame(Metric = richness_aphids$Richness,
                              Year = richness_aphids$Year, 
                              Month = richness_aphids$Month,
                              Site = richness_aphids$Site, 
                              Lumen = richness_aphids$LUMEN,
                              PET = richness_aphids$PET, 
                              PPT = richness_aphids$PPT,
                              WIND = richness_aphids$WIND, 
                              TMean = richness_aphids$Tmean,
                              Arable = richness_aphids$Arable_25km,
                              Urban = richness_aphids$Urban_25km,
                              Grassland = richness_aphids$Grassland_25km,
                              Year_factor = richness_aphids$Year_factor,
                              Group = richness_aphids$Group)



richness_remaining <- data.frame(Metric = tot_richness_remaining$Richness,
                                     Year = tot_richness_remaining$Year, 
                                     Month = tot_richness_remaining$Month,
                                     Site = tot_richness_remaining$Site, 
                                     Lumen = tot_richness_remaining$LUMEN,
                                     PET = tot_richness_remaining$PET, 
                                     PPT = tot_richness_remaining$PPT,
                                     WIND = tot_richness_remaining$WIND, 
                                     TMean = tot_richness_remaining$Tmean,
                                     Arable = tot_richness_remaining$Arable_25km,
                                     Urban = tot_richness_remaining$Urban_25km,
                                     Grassland = tot_richness_remaining$Grassland_25km,
                                     Year_factor = tot_richness_remaining$Year_factor,
                                     Group = tot_richness_remaining$Group)


richness_remaining_orderwise <- data.frame(Metric = orderwise_richness_remaining$Richness,
                                           Year = orderwise_richness_remaining$Year, 
                                           Month = orderwise_richness_remaining$Month,
                                           Site = orderwise_richness_remaining$Site, 
                                           Lumen = orderwise_richness_remaining$LUMEN,
                                           PET = orderwise_richness_remaining$PET, 
                                           PPT = orderwise_richness_remaining$PPT,
                                           WIND = orderwise_richness_remaining$WIND, 
                                           TMean = orderwise_richness_remaining$Tmean,
                                           Arable = orderwise_richness_remaining$Arable_25km,
                                           Urban = orderwise_richness_remaining$Urban_25km,
                                           Grassland = orderwise_richness_remaining$Grassland_25km,
                                           Year_factor = orderwise_richness_remaining$Year_factor,
                                           Group = orderwise_richness_remaining$Group)




# rbind tot_Abund_remaining and abund_Remaining orderwise (and richness also)
jnk1 <- rbind(tot_abund_remaining, abund_remaining_orderwise)
jnk2 <- rbind(richness_remaining, richness_remaining_orderwise)
list.gam.dfs <- list(tot_abund_aphids, jnk1, richness_aphids, jnk2)
names(list.gam.dfs) <- c("abundance_aphids", "abundance_others", "richness_aphids", "richness_others")
# these are your final GAM READY DATAFRAMES
# just check everything once
ncol.check <- lapply(list.gam.dfs, ncol)
colname.check <- lapply(list.gam.dfs, colnames)

# now one final thing: 
unique(list.gam.dfs$abundance_aphids$Site)
unique(list.gam.dfs$abundance_others$Site)
unique(list.gam.dfs$richness_aphids$Site)
unique(list.gam.dfs$richness_others$Site)
