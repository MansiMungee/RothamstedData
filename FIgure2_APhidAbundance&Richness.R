############################################# Figure2 - Aphid abundances and richness through time  ################################
gam_model <- aphid_abund_gam_GS
mod_smooths <- smooths(gam_model)
mod_estimates <- smooth_estimates(gam_model)
mod_estimates <- add_confint(mod_estimates)

p1 <- draw(gam_model, select = mod_smooths[1], residuals = TRUE, resid_col = mycolor_pallette[5], rug = FALSE)
p1 <- p1 + theme_bw()
p1 <- p1 + geom_line(size = 0.75)
p1 <- p1 + annotate("text", x = 1996, y = 8, label = "A)", size = 6)
p1 <- p1 + coord_cartesian(ylim = c(-4, 6), xlim = c(2000, 2018), clip = "off")
p1 <- p1 + theme(text = element_text(size = 9))


p2 <- draw(gam_model, select = mod_smooths[2], residuals = TRUE, rug = FALSE)
p2 <- p2 + theme_bw()
p2 <- p2 + geom_line(size = 0.75)
p2 <- p2 + scale_color_manual(values = alpha(mycolor_pallette, 0.3))
p2 <- p2 + theme(legend.position = "none")
p2 <- p2 + annotate("text", x = 1996, y = 0.55, label = "B)", size = 6)
p2 <- p2 + coord_cartesian(ylim = c(-0.4, 0.4), xlim = c(2000, 2018), clip = "off")
p2 <- p2 + theme(text = element_text(size = 9))


p3 <- draw(gam_model, select = mod_smooths[3], residuals = TRUE, rug = FALSE)
p3 <- p3 + theme_bw()
p3 <- p3 + geom_line(size = 0.75)
p3 <- p3 + scale_color_manual(values = alpha(mycolor_pallette, 0.3))
p3 <- p3 + theme(legend.position = c(0.62,0.25), legend.title = element_blank(), 
                 legend.key.height = unit(0.5, "line"),
                 legend.background = element_rect(color = NA, fill = NA), text = element_text(size = 9))
p3 <- p3 + annotate("text", x = -3, y = 5.75, label = "C)", size = 6)
p3 <- p3 + coord_cartesian(ylim = c(-6.2, 4), xlim = c(0, 12.5), clip = "off")


gam_model <- aphid_richness_gam_G
mod_smooths <- smooths(gam_model)
mod_estimates <- smooth_estimates(gam_model)
mod_estimates <- add_confint(mod_estimates)

p4 <- draw(gam_model, select = mod_smooths[1], residuals = TRUE, resid_col = mycolor_pallette[5], rug = FALSE)
p4 <- p4 + theme_bw()
p4 <- p4 + geom_line(size = 0.75)
p4 <- p4 + annotate("text", x = 1996, y = 2.25, label = "D)", size = 6)
p4 <- p4 + coord_cartesian(ylim = c(-1.6, 1.5), xlim = c(2000, 2018), clip = "off")
p4 <- p4 + theme(text = element_text(size = 9))



p5 <- draw(gam_model, select = mod_smooths[2])
p5 <- p5 + theme_bw()
p5 <- p5 + annotate("text", x = -1.55, y = 0.166, label = "E)", size = 6)
p5 <- p5 + coord_cartesian(ylim = c(-0.18, 0.1), xlim = c(-1.05, 1.05), clip = "off")
p5 <- p5 + theme(text = element_text(size = 9))

p6 <- draw(gam_model, select = mod_smooths[3])
p6 <- p6 + theme_bw()
p6 <- p6 + annotate("text", x = -3.5, y = 1.5, label = "F)", size = 6)
p6 <- p6 + coord_cartesian(ylim = c(-1.5, 1), xlim = c(0, 12.5), clip = "off")
p6 <- p6 + theme(text = element_text(size = 9))

tiff("./../05_Figures/02_Feb23/02_Figure2.tiff", width = 16, height = 12, unit = "cm", res = 300)
p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(ncol = 3, nrow = 2)
dev.off()





