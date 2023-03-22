# scale covariates
for(ii in 1:length(list.gam.dfs)){
  list.gam.dfs[[ii]] <- list.gam.dfs[[ii]] %>% group_by(Site) %>% mutate(PET_s = scale(PET)[,1], 
                                                                         PPT_s = scale(PPT)[,1],
                                                                         Lumen_s = scale(Lumen)[,1],
                                                                         WIND_s = scale(WIND)[,1],
                                                                         TMean_s = scale(TMean)[,1],
                                                                         Arable_s = scale(Arable)[,1],
                                                                         Urban_s = scale(Urban)[,1],
                                                                         Grassland_s = scale(Grassland)[,1])
  
}

################################################################ BEST GAM FOR APHID ABUNDANCES WITH COVARIATES##################################
# best aphid model
# best aphid model with covariates
jnk1 <- list.gam.dfs[[1]]
jnk1$Site <- as.factor(jnk1$Site)
gam1 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Month, Site, bs = "fs", xt = list(bs = "cc")) + 
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s) + s(TMean_s) + s(Arable_s) + s(Urban_s) + s(Grassland_s) +
              te(Lumen_s, TMean_s) + te(TMean, PPT_s) + te(TMean, Arable_s),
            data = jnk1, knots = list(Month = c(1, 12)),
            family = nb(), method="REML")

gam2 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Month, Site, bs = "fs", xt = list(bs = "cc")) +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s) + s(TMean_s) + s(Arable_s) + s(Urban_s) + s(Grassland_s),
            data = jnk1, knots = list(Month = c(1, 12)),
            family = nb(), method="REML")

gam3 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Month, Site, bs = "fs", xt = list(bs = "cc")) +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s) + s(TMean_s) + s(Arable_s) + s(Urban_s),
            data = jnk1, knots = list(Month = c(1, 12)),
            family = nb(), method="REML")

gam4 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Month, Site, bs = "fs", xt = list(bs = "cc")) +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s) + s(TMean_s) + s(Arable_s),
            data = jnk1, knots = list(Month = c(1, 12)),
            family = nb(), method="REML")

gam5 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Month, Site, bs = "fs", xt = list(bs = "cc")) +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s) + s(TMean_s),
            data = jnk1, knots = list(Month = c(1, 12)),
            family = nb(), method="REML")

gam6 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Month, Site, bs = "fs", xt = list(bs = "cc")) +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s),
            data = jnk1, knots = list(Month = c(1, 12)),
            family = nb(), method="REML")


gam7 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Month, Site, bs = "fs", xt = list(bs = "cc")) +
              s(Lumen_s) + s(PET_s),
            data = jnk1, knots = list(Month = c(1, 12)),
            family = nb(), method="REML")

gam8 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Month, Site, bs = "fs", xt = list(bs = "cc")) +
              s(Lumen_s),
            data = jnk1, knots = list(Month = c(1, 12)),
            family = nb(), method="REML")

min_jnk <- min(AIC(gam1), AIC(gam2),AIC(gam3),AIC(gam4),AIC(gam5),AIC(gam6),AIC(gam7),AIC(gam8))
min_jnk - AIC(gam1)
min_jnk - AIC(gam2)
min_jnk - AIC(gam3)
min_jnk - AIC(gam4)
min_jnk - AIC(gam5)
min_jnk - AIC(gam6)
min_jnk - AIC(gam7)
min_jnk - AIC(gam8)
# MODEL DIAGNOSTICS 
k.check(gam1)
k.check(gam2)
k.check(gam3)
k.check(gam4)
k.check(gam5)
k.check(gam6)
k.check(gam7)
k.check(gam8)
# DEVIANCE EXPLAINED
summary(gam1)$dev.expl * 100
summary(gam2)$dev.expl * 100
summary(gam3)$dev.expl * 100
summary(gam4)$dev.expl * 100
summary(gam5)$dev.expl * 100
summary(gam6)$dev.expl * 100
summary(gam7)$dev.expl * 100
summary(gam8)$dev.expl * 100
# diagnostic plots for supplementary
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM1.jpg")
appraise(gam1, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM2.jpg")
appraise(gam2, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM3.jpg")
appraise(gam3, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM4.jpg")
appraise(gam4, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM5.jpg")
appraise(gam5, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM6.jpg")
appraise(gam6, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM7.jpg")
appraise(gam7, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM8.jpg")
appraise(gam8, method = "simulate")
dev.off()

aphid_abundance_covariate_model <- gam1





################################################################ BEST GAM FOR TOTAL INSECT ABUNDANCES WITH COVARIATES##################################

jnk1 <- list.gam.dfs[[2]]
jnk1 <- jnk1[jnk1$Group == "Total_Except_Aphids",]
jnk1$Site <- as.factor(jnk1$Site)
jnk1$Group <- as.factor(jnk1$Group)
# Month wise model not possible because of too few months
insect_covariate_model <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year) +
                                s(Lumen_s) + s(PET_s) +  s(PPT_s) + 
                                s(WIND_s) + s(TMean_s) + s(Arable_s) + s(Urban_s) + s(Grassland_s) +
                                te(Lumen_s, TMean_s) + te(TMean, PPT_s) + te(TMean, Arable_s),
                              data = jnk1, family = nb(), method="REML")


# group wise
jnk1 <- list.gam.dfs[[2]]
jnk1 <- jnk1[!jnk1$Group == "Total_Except_Aphids",]
jnk1$Site <- as.factor(jnk1$Site)
jnk1$Group <- as.factor(jnk1$Group)

gam1 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year, Group, bs = "fs") +
              s(Lumen_s, Group, bs = "fs") + s(PET_s, Group, bs = "fs") +  s(PPT_s, Group, bs = "fs") + 
              s(WIND_s, Group, bs = "fs") + s(TMean_s, Group, bs = "fs") + s(Arable_s, Group, bs = "fs") + s(Urban_s, Group, bs = "fs") + s(Grassland_s, Group, bs = "fs") +
              te(Lumen_s, TMean_s) + te(TMean, PPT_s) + te(TMean, Arable_s),
            data = jnk1, family = nb(), method="REML")

gam2 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year, Group, bs = "fs") +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s) + s(TMean_s) + s(Arable_s) + s(Urban_s) + s(Grassland_s) +
              te(Lumen_s, TMean_s) + te(TMean, PPT_s) + te(TMean, Arable_s),
            data = jnk1, family = nb(), method="REML")


gam3 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year, Group, bs = "fs") +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s) + s(TMean_s) + s(Arable_s) + s(Urban_s) + s(Grassland_s),
            data = jnk1, family = nb(), method="REML")

gam4 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year, Group, bs = "fs") +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s) + s(TMean_s) + s(Arable_s) + s(Urban_s),
            data = jnk1, family = nb(), method="REML")

gam5 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year, Group, bs = "fs") +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s) + s(TMean_s) + s(Arable_s),
            data = jnk1, family = nb(), method="REML")

gam6 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year, Group, bs = "fs") +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s) + s(TMean_s),
            data = jnk1, family = nb(), method="REML")

gam7 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year, Group, bs = "fs") +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s),
            data = jnk1, family = nb(), method="REML")

gam8 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year, Group, bs = "fs") +
              s(Lumen_s) + s(PET_s), data = jnk1, family = nb(), method="REML")

gam9 <-gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year, Group, bs = "fs") +
             s(Lumen_s), data = jnk1, family = nb(), method="REML")

# AICs
min_jnk <- min(AIC(gam1), AIC(gam2),AIC(gam3),AIC(gam4),AIC(gam5),AIC(gam6),AIC(gam7),AIC(gam8), AIC(gam9))
min_jnk - AIC(gam1)
min_jnk - AIC(gam2)
min_jnk - AIC(gam3)
min_jnk - AIC(gam4)
min_jnk - AIC(gam5)
min_jnk - AIC(gam6)
min_jnk - AIC(gam7)
min_jnk - AIC(gam8)
min_jnk - AIC(gam9)
# MODEL DIAGNOSTICS 
k.check(gam1)
k.check(gam2)
k.check(gam3)
k.check(gam4)
k.check(gam5)
k.check(gam6)
k.check(gam7)
k.check(gam8)
k.check(gam9)
# diagnostic plots for supplementary
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM1_Remaining.jpg")
appraise(gam1, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM2_Remaining.jpg")
appraise(gam2, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM3_Remaining.jpg")
appraise(gam3, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM4_Remaining.jpg")
appraise(gam4, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM5_Remaining.jpg")
appraise(gam5, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM6_Remaining.jpg")
appraise(gam6, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM7_Remaining.jpg")
appraise(gam7, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM8_Remaining.jpg")
appraise(gam8, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM9_Remaining.jpg")
appraise(gam9, method = "simulate")
dev.off()

insect_abundance_corvariate_model <- gam1




# PLotting final gams
################################################################ BEST GAM FOR APHID RICHNESS WITH COVARIATES##################################
jnk1 <- list.gam.dfs[[3]]
jnk1$Site <- as.factor(jnk1$Site)
gam1 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Month, Site, bs = "fs", xt = list(bs = "cc")) + 
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s) + s(TMean_s) + s(Arable_s) + s(Urban_s) + s(Grassland_s) +
              te(Lumen_s, TMean_s) + te(TMean, PPT_s) + te(TMean, Arable_s),
            data = jnk1, knots = list(Month = c(1, 12)),
            family = poisson(), method="REML")

gam2 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Month, Site, bs = "fs", xt = list(bs = "cc")) +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s) + s(TMean_s) + s(Arable_s) + s(Urban_s) + s(Grassland_s),
            data = jnk1, knots = list(Month = c(1, 12)),
            family = poisson(), method="REML")

gam3 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Month, Site, bs = "fs", xt = list(bs = "cc")) +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s) + s(TMean_s) + s(Arable_s) + s(Urban_s),
            data = jnk1, knots = list(Month = c(1, 12)),
            family = poisson(), method="REML")

gam4 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Month, Site, bs = "fs", xt = list(bs = "cc")) +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s) + s(TMean_s) + s(Arable_s),
            data = jnk1, knots = list(Month = c(1, 12)),
            family = poisson(), method="REML")

gam5 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Month, Site, bs = "fs", xt = list(bs = "cc")) +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s) + s(TMean_s),
            data = jnk1, knots = list(Month = c(1, 12)),
            family = poisson(), method="REML")

gam6 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Month, Site, bs = "fs", xt = list(bs = "cc")) +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s),
            data = jnk1, knots = list(Month = c(1, 12)),
            family = poisson(), method="REML")


gam7 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Month, Site, bs = "fs", xt = list(bs = "cc")) +
              s(Lumen_s) + s(PET_s),
            data = jnk1, knots = list(Month = c(1, 12)),
            family = poisson(), method="REML")

gam8 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Month, Site, bs = "fs", xt = list(bs = "cc")) +
              s(Lumen_s),
            data = jnk1, knots = list(Month = c(1, 12)),
            family = poisson(), method="REML")

summary(gam1)
summary(gam2)
summary(gam3)
summary(gam4)
summary(gam5)
summary(gam6)
summary(gam7)
summary(gam8)

min_jnk <- min(AIC(gam1), AIC(gam2),AIC(gam3),AIC(gam4),AIC(gam5),AIC(gam6),AIC(gam7),AIC(gam8))
min_jnk - AIC(gam1)
min_jnk - AIC(gam2)
min_jnk - AIC(gam3)
min_jnk - AIC(gam4)
min_jnk - AIC(gam5)
min_jnk - AIC(gam6)
min_jnk - AIC(gam7)
min_jnk - AIC(gam8)
# MODEL DIAGNOSTICS 
k.check(gam1)
k.check(gam2)
k.check(gam3)
k.check(gam4)
k.check(gam5)
k.check(gam6)
k.check(gam7)
k.check(gam8)
# DEVIANCE EXPLAINED
summary(gam1)$dev.expl * 100
summary(gam2)$dev.expl * 100
summary(gam3)$dev.expl * 100
summary(gam4)$dev.expl * 100
summary(gam5)$dev.expl * 100
summary(gam6)$dev.expl * 100
summary(gam7)$dev.expl * 100
summary(gam8)$dev.expl * 100
# diagnostic plots for supplementary
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM1_Richness.jpg")
appraise(gam1, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM2_Richness.jpg")
appraise(gam2, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM3_Richness.jpg")
appraise(gam3, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM4_Richness.jpg")
appraise(gam4, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM5_Richness.jpg")
appraise(gam5, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM6_Richness.jpg")
appraise(gam6, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM7_Richness.jpg")
appraise(gam7, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM8_Richness.jpg")
appraise(gam8, method = "simulate")
dev.off()

aphid_richness_covariate_model <- gam7
aphid_richness_covariate_model_for_plotting <- gam1







################################################################ BEST GAM FOR TOTAL INSECT RICHNESS WITH COVARIATES##################################
jnk1 <- list.gam.dfs[[4]]
jnk1 <- jnk1[!jnk1$Group == "All_Except_Aphids",]
jnk1$Site <- as.factor(jnk1$Site)
jnk1$Group <- as.factor(jnk1$Group)

gam1 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year, Group, bs = "fs") +
              s(Lumen_s, Group, bs = "fs") + s(PET_s, Group, bs = "fs") +  s(PPT_s, Group, bs = "fs") + 
              s(WIND_s, Group, bs = "fs") + s(TMean_s, Group, bs = "fs") + s(Arable_s, Group, bs = "fs") + s(Urban_s, Group, bs = "fs") + s(Grassland_s, Group, bs = "fs") +
              te(Lumen_s, TMean_s) + te(TMean, PPT_s) + te(TMean, Arable_s),
            data = jnk1, family = poisson(), method="REML")

gam2 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year, Group, bs = "fs") +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s) + s(TMean_s) + s(Arable_s) + s(Urban_s) + s(Grassland_s) +
              te(Lumen_s, TMean_s) + te(TMean, PPT_s) + te(TMean, Arable_s),
            data = jnk1, family = poisson(), method="REML")


gam3 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year, Group, bs = "fs") +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s) + s(TMean_s) + s(Arable_s) + s(Urban_s) + s(Grassland_s),
            data = jnk1, family = poisson(), method="REML")

gam4 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year, Group, bs = "fs") +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s) + s(TMean_s) + s(Arable_s) + s(Urban_s),
            data = jnk1, family = poisson(), method="REML")

gam5 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year, Group, bs = "fs") +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s) + s(TMean_s) + s(Arable_s),
            data = jnk1, family = poisson(), method="REML")

gam6 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year, Group, bs = "fs") +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s) + s(TMean_s),
            data = jnk1, family = poisson(), method="REML")

gam7 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year, Group, bs = "fs") +
              s(Lumen_s) + s(PET_s) +  s(PPT_s) + s(WIND_s),
            data = jnk1, family = poisson(), method="REML")

gam8 <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year, Group, bs = "fs") +
              s(Lumen_s) + s(PET_s), data = jnk1, family = poisson(), method="REML")

gam9 <-gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year, Group, bs = "fs") +
             s(Lumen_s), data = jnk1, family = poisson(), method="REML")

summary(gam1)
summary(gam2)
summary(gam3)
summary(gam4)
summary(gam5)
summary(gam6)
summary(gam7)
summary(gam8)
summary(gam9)

# AICs
min_jnk <- min(AIC(gam1), AIC(gam2),AIC(gam3),AIC(gam4),AIC(gam5),AIC(gam6),AIC(gam7),AIC(gam8), AIC(gam9))
min_jnk - AIC(gam1)
min_jnk - AIC(gam2)
min_jnk - AIC(gam3)
min_jnk - AIC(gam4)
min_jnk - AIC(gam5)
min_jnk - AIC(gam6)
min_jnk - AIC(gam7)
min_jnk - AIC(gam8)
min_jnk - AIC(gam9)
# MODEL DIAGNOSTICS 
k.check(gam1)
k.check(gam2)
k.check(gam3)
k.check(gam4)
k.check(gam5)
k.check(gam6)
k.check(gam7)
k.check(gam8)
k.check(gam9)
# diagnostic plots for supplementary
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM1_Remaining_Richness.jpg")
appraise(gam1, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM2_Remaining_Richness.jpg")
appraise(gam2, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM3_Remaining_Richness.jpg")
appraise(gam3, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM4_Remaining_Richness.jpg")
appraise(gam4, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM5_Remaining_Richness.jpg")
appraise(gam5, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM6_Remaining_Richness.jpg")
appraise(gam6, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM7_Remaining_Richness.jpg")
appraise(gam7, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM8_Remaining_Richness.jpg")
appraise(gam8, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/04_GAMs_WithCovariates/ModelDiagnostics/GAM9_Remaining_Richness.jpg")
appraise(gam9, method = "simulate")
dev.off()

insect_richness_covariate_model <- gam1



