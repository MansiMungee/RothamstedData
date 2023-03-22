############################ Figure 3 - Aphid Abundance Covriates ########################
gam_model <- aphid_abundance_covariate_model
mod_smooths <- smooths(gam_model)

p1 <- draw(gam_model, select = mod_smooths[5], residuals = TRUE, rug = FALSE, resid_col = "gray30")
p1 <- p1 + theme_bw()
p1 <- p1 + geom_line(size = 1, linetype = "solid")
p1 <- p1 + theme(text = element_text(size = 9))
p1 <- p1 + annotate("text", x = -3, y = 5.5, label = "A)", size = 6)
p1 <- p1 + coord_cartesian(ylim = c(-4, 4), xlim = c(-2, 3), clip = "off")


p2 <- draw(gam_model, select = mod_smooths[7], residuals = TRUE, rug = FALSE, resid_col = "gray30")
p2 <- p2 + theme_bw()
p2 <- p2 + geom_line(size = 1, linetype = "solid")
p2 <- p2 + theme(text = element_text(size = 9))
p2 <- p2 + annotate("text", x = -5, y = 5.5, label = "B)", size = 6)
p2 <- p2 + coord_cartesian(ylim = c(-4, 4), xlim = c(-3, 5), clip = "off")


p3 <- draw(gam_model, select = mod_smooths[13]) + theme_bw() + scale_fill_viridis(option = "magma", alpha = 0.8)
p3 <- p3 + theme(text = element_text(size = 9))
p3 <- p3 + annotate("text", x = -7.5, y = 5, label = "C)", size = 6)
p3 <- p3 + coord_cartesian(ylim = c(-2, 4), xlim = c(-2, 20), clip = "off")


gam_model <- aphid_richness_covariate_model_for_plotting
mod_smooths <- smooths(gam_model)

p4 <- draw(gam_model, select = mod_smooths[5], residuals = TRUE, rug = FALSE, resid_col = "gray30")
p4 <- p4 + theme_bw()
p4 <- p4 + geom_line(size = 1, linetype = "solid")
p4 <- p4 + theme(text = element_text(size = 9))
p4 <- p4 + annotate("text", x = -3.4, y = 2.7, label = "D)", size = 6)
p4 <- p4 + coord_cartesian(ylim = c(-2, 2), xlim = c(-2,3), clip = "off")


p5 <- draw(gam_model, select = mod_smooths[7], residuals = TRUE, rug = FALSE, resid_col = "gray30")
p5 <- p5 + theme_bw()
p5 <- p5 + geom_line(size = 1, linetype = "solid")
p5 <- p5 + theme(text = element_text(size = 9))
p5 <- p5 + annotate("text", x = -4.5, y = 2.6, label = "E)", size = 6)
p5 <- p5 + coord_cartesian(ylim = c(-2,2), xlim = c(-2.75, 5.5), clip = "off")


p6 <- draw(gam_model, select = mod_smooths[13]) + theme_bw()  + scale_fill_viridis(option = "magma", alpha = 0.8)
p6 <- p6 + theme(text = element_text(size = 9))
p6 <- p6 + annotate("text", x = -7.5, y = 5, label = "F)", size = 6)
p6 <- p6 + coord_cartesian(ylim = c(-2, 4), xlim = c(-2, 20), clip = "off")


tiff("./../05_Figures/02_Feb23/04_Figure4.tiff", width = 16, height = 12, unit = "cm", res = 300)
p1 + p2 + p3 + p4 + p5 + p6 +plot_layout(ncol = 3, nrow = 2)
dev.off()
