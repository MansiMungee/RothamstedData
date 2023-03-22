# You have four dataframes for fitting GAMs: 
# names(list.gam.dfs) <- c("abundance_aphids", "abundance_others", "richness_aphids", "richness_others")


############################## HIGHER CHUNK 01 - ABUNDANCES THROUGH TIME ###############################
# comparing models G, GS, GI, S and I - to assess whether site specific variation is needed for:
#  1. aphid abundance
#  2. abundance_others
#  3. abundance of each group
################### Models G TO I for aphid abundance ################
# Abundance of aphids through time
jnk1 <- list.gam.dfs[[1]]
jnk1$Site <- as.factor(jnk1$Site)
# Global smoother with site as random effects - Model G
aphid_abund_gam_G <- gam(Metric ~ s(Year) + s(Site, bs = "re") + s(Month, bs = "cc", k = 12), knots = list(c(1,12)), method = "REML", data = jnk1, family = nb())
# global smoother with site-level smoother with same wiggliness - Model GS
aphid_abund_gam_GS <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Month, Site, bs = "fs", xt = list(bs = "cc")), knots = list(c(1,12)), method = "REML", data = jnk1, family = nb())
# global smoother with site-level smoother with same wiggliness - Model GI
aphid_abund_gam_GI <- gam(Metric ~ s(Year) + s(Year, by = Site) + s(Site, bs = "re") + s(Month, Site, bs = "fs", xt = list(bs = "cc")), knots = list(c(1,12)), method = "REML", data = jnk1, family = nb())
# Model S (shared smoothers) is model GS without the global smoother term
aphid_abund_gam_S <- gam(Metric ~ s(Year, Site, bs = "fs") + s(Month, Site, bs = "fs", xt = list(bs = "cc")), knots = list(c(1,12)), method = "REML", data = jnk1, family = nb())
# Model I is model GI without the first term:
aphid_abund_gam_I <- gam(Metric ~ s(Year, by = Site, bs = "tp") + s(Site, bs = "re") + s(Month, Site, bs = "fs", xt = list(bs = "cc")), knots = list(c(1,12)), method = "REML", data = jnk1, family = nb())
# comparing AICs
jnk_min <- min(AIC(aphid_abund_gam_G), AIC(aphid_abund_gam_GS), AIC(aphid_abund_gam_GI), AIC(aphid_abund_gam_S), AIC(aphid_abund_gam_I))
jnk_min - AIC(aphid_abund_gam_G)
jnk_min - AIC(aphid_abund_gam_GS)
jnk_min - AIC(aphid_abund_gam_GI)
jnk_min - AIC(aphid_abund_gam_S)
jnk_min - AIC(aphid_abund_gam_I)
# MODEL DIAGNOSTICS 
k.check(aphid_abund_gam_G)
k.check(aphid_abund_gam_GS)
k.check(aphid_abund_gam_GI)
k.check(aphid_abund_gam_S)
k.check(aphid_abund_gam_I)
# deviance explained
summary(aphid_abund_gam_G)$dev.expl * 100
summary(aphid_abund_gam_GS)$dev.expl * 100
summary(aphid_abund_gam_GI)$dev.expl * 100
summary(aphid_abund_gam_S)$dev.expl * 100
summary(aphid_abund_gam_I)$dev.expl * 100
# diagnostic plots for supplementary
jpeg("./../05_Figures/02_Feb23/03_GAMs_WithoutCovariates/ModelDiagnostics/01_aphid_abund_gam_G.jpg")
appraise(aphid_abund_gam_G, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/03_GAMs_WithoutCovariates/ModelDiagnostics/02_aphid_abund_gam_GS.jpg")
appraise(aphid_abund_gam_GS, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/03_GAMs_WithoutCovariates/ModelDiagnostics/03_aphid_abund_gam_GI.jpg")
appraise(aphid_abund_gam_GI, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/03_GAMs_WithoutCovariates/ModelDiagnostics/04_aphid_abund_gam_S.jpg")
appraise(aphid_abund_gam_S, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/03_GAMs_WithoutCovariates/ModelDiagnostics/05_aphid_abund_gam_I.jpg")
appraise(aphid_abund_gam_I, method = "simulate")
dev.off()



# best model for aphid abundance = S
################### Models G TO I for insect abundance ################
# insects except aphids abundance
jnk1 <- list.gam.dfs[[2]]
jnk1 <- jnk1[!jnk1$Group == "Total_Except_Aphids",]
jnk1$Site <- as.factor(jnk1$Site)
jnk1$Group <- as.factor(jnk1$Group)
# Global smoother with site as random effects - Model G
abund_other_gam_G <- gam(Metric ~ s(Year) + s(Site, bs = "re"), method = "REML", data = jnk1, family = nb())
# global smoother with site-level smoother with same wiggliness - Model GS
abund_other_gam_GS <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year, Group, bs = "fs"), method = "REML", data = jnk1, family = nb())
# global smoother with site-level smoother with same wiggliness - Model GI
abund_other_gam_GI <- gam(Metric ~ s(Year) + s(Year, by = Site, bs = "fs") + s(Year, by = Group, bs = "fs"), method = "REML", data = jnk1, family = nb())

# comparing AICs
jnk_min <- min(AIC(abund_other_gam_G), AIC(abund_other_gam_GS), AIC(abund_other_gam_GI))
jnk_min - AIC(abund_other_gam_G)
jnk_min - AIC(abund_other_gam_GS)
jnk_min - AIC(abund_other_gam_GI)
# MODEL DIAGNOSTICS 
k.check(abund_other_gam_G)
k.check(abund_other_gam_GS)
k.check(abund_other_gam_GI)
# deviance explained
summary(abund_other_gam_G)$dev.expl * 100
summary(abund_other_gam_GS)$dev.expl * 100
summary(abund_other_gam_GI)$dev.expl * 100
# diagnostic plots for supplementary
jpeg("./../05_Figures/02_Feb23/03_GAMs_WithoutCovariates/ModelDiagnostics/01_abund_other_gam_G.jpg")
appraise(abund_other_gam_G, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/03_GAMs_WithoutCovariates/ModelDiagnostics/02_abund_other_gam_GS.jpg")
appraise(abund_other_gam_GS, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/03_GAMs_WithoutCovariates/ModelDiagnostics/02_abund_other_gam_GI.jpg")
appraise(abund_other_gam_GI, method = "simulate")
dev.off()
# best model for total insect abundance = GI








############################## HIGHER CHUNK 02 - RICHNESS THROUGH TIME ###############################
# comparing models G, GS, GI, S and I - to assess whether site specific variation is needed for:
#  1. richness_aphids
#  2. richness_others
#  3. richness of each group
################### Models G TO I for aphid richness ################
# Abundance of aphids through time
jnk1 <- list.gam.dfs[[3]]
jnk1$Site <- as.factor(jnk1$Site)
# Global smoother with site as random effects - Model G
aphid_richness_gam_G <- gam(Metric ~ s(Year) + s(Site, bs = "re") + s(Month, bs = "cc", k = 12), knots = list(c(1,12)), method = "REML", data = jnk1, family = poisson())
# global smoother with site-level smoother with same wiggliness - Model GS
aphid_richness_gam_GS <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Month, Site, bs = "fs", xt = list(bs = "cc")), knots = list(c(1,12)), method = "REML", data = jnk1, family = poisson())
# global smoother with site-level smoother with same wiggliness - Model GI
aphid_richness_gam_GI <- gam(Metric ~ s(Year) + s(Year, by = Site) + s(Site, bs = "re") + s(Month, Site, bs = "fs", xt = list(bs = "cc")), knots = list(c(1,12)), method = "REML", data = jnk1, family = poisson())
# Model S (shared smoothers) is model GS without the global smoother term
aphid_richness_gam_S <- gam(Metric ~ s(Year, Site, bs = "fs") + s(Month, Site, bs = "fs", xt = list(bs = "cc")), knots = list(c(1,12)), method = "REML", data = jnk1, family = poisson())
# Model I is model GI without the first term:
aphid_richness_gam_I <- gam(Metric ~ s(Year, by = Site, bs = "tp") + s(Site, bs = "re") + s(Month, Site, bs = "fs", xt = list(bs = "cc")), knots = list(c(1,12)), method = "REML", data = jnk1, family = poisson())
# comparing AICs
jnk_min <- min(AIC(aphid_richness_gam_G), AIC(aphid_richness_gam_GS), AIC(aphid_richness_gam_GI), AIC(aphid_richness_gam_S), AIC(aphid_richness_gam_I))
jnk_min - AIC(aphid_richness_gam_G)
jnk_min - AIC(aphid_richness_gam_GS)
jnk_min - AIC(aphid_richness_gam_GI)
jnk_min - AIC(aphid_richness_gam_S)
jnk_min - AIC(aphid_richness_gam_I)
# MODEL DIAGNOSTICS 
k.check(aphid_richness_gam_G)
k.check(aphid_richness_gam_GS)
k.check(aphid_richness_gam_GI)
k.check(aphid_richness_gam_S)
k.check(aphid_richness_gam_I)
# deviance explained
summary(aphid_richness_gam_G)$dev.expl * 100
summary(aphid_richness_gam_GS)$dev.expl * 100
summary(aphid_richness_gam_GI)$dev.expl * 100
summary(aphid_richness_gam_S)$dev.expl * 100
summary(aphid_richness_gam_I)$dev.expl * 100
# diagnostic plots for supplementary
jpeg("./../05_Figures/02_Feb23/03_GAMs_WithoutCovariates/ModelDiagnostics/01_aphid_richness_gam_G.jpg")
appraise(aphid_richness_gam_G, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/03_GAMs_WithoutCovariates/ModelDiagnostics/02_aphid_richness_gam_GS.jpg")
appraise(aphid_richness_gam_GS, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/03_GAMs_WithoutCovariates/ModelDiagnostics/03_aphid_richness_gam_GI.jpg")
appraise(aphid_richness_gam_GI, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/03_GAMs_WithoutCovariates/ModelDiagnostics/04_aphid_richness_gam_S.jpg")
appraise(aphid_richness_gam_S, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/03_GAMs_WithoutCovariates/ModelDiagnostics/05_aphid_richness_gam_I.jpg")
appraise(aphid_richness_gam_I, method = "simulate")
dev.off()
# best model for aphid abundance = G


################### Models G TO I for insect richness ################
# insects except aphids richness
jnk1 <- list.gam.dfs[[4]]
jnk1 <- jnk1[!jnk1$Group == "All_Except_Aphids",]
jnk1$Site <- as.factor(jnk1$Site)
jnk1$Group <- as.factor(jnk1$Group)
# Global smoother with site as random effects - Model G
richness_other_gam_G <- gam(Metric ~ s(Year) + s(Site, bs = "re"), method = "REML", data = jnk1, family = poisson())
# global smoother with site-level smoother with same wiggliness - Model GS
richness_other_gam_GS <- gam(Metric ~ s(Year) + s(Year, Site, bs = "fs") + s(Year, Group, bs = "fs"), method = "REML", data = jnk1, family = poisson())
# global smoother with site-level smoother with same wiggliness - Model GI
richness_other_gam_GI <- gam(Metric ~ s(Year) + s(Year, by = Site, bs = "fs") + s(Year, by = Group, bs = "fs"), method = "REML", data = jnk1, family = poisson())

# comparing AICs
jnk_min <- min(AIC(richness_other_gam_G), AIC(richness_other_gam_GS), AIC(richness_other_gam_GI))
jnk_min - AIC(richness_other_gam_G)
jnk_min - AIC(richness_other_gam_GS)
jnk_min - AIC(richness_other_gam_GI)
# MODEL DIAGNOSTICS 
k.check(richness_other_gam_G)
k.check(richness_other_gam_GS)
k.check(richness_other_gam_GI)
# deviance explained
summary(richness_other_gam_G)$dev.expl * 100
summary(richness_other_gam_GS)$dev.expl * 100
summary(richness_other_gam_GI)$dev.expl * 100
# diagnostic plots for supplementary
jpeg("./../05_Figures/02_Feb23/03_GAMs_WithoutCovariates/ModelDiagnostics/01_richness_other_gam_G.jpg")
appraise(richness_other_gam_G, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/03_GAMs_WithoutCovariates/ModelDiagnostics/02_richness_other_gam_GS.jpg")
appraise(richness_other_gam_GS, method = "simulate")
dev.off()
jpeg("./../05_Figures/02_Feb23/03_GAMs_WithoutCovariates/ModelDiagnostics/02_richness_other_gam_GI.jpg")
appraise(richness_other_gam_GI, method = "simulate")
dev.off()
# best model for total insect RCIHNESS = GS










