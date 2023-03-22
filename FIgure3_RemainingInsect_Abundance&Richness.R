gam_model <- abund_other_gam_GS
mod_smooths <- smooths(gam_model)
mod_estimates <- smooth_estimates(gam_model)
mod_estimates <- add_confint(mod_estimates)

p1 <- draw(gam_model, select = mod_smooths[1], residuals = TRUE, resid_col = mycolor_pallette[5], rug = FALSE)
p1 <- p1 + theme_bw()
p1 <- p1 + geom_line(size = 0.75)
p1 <- p1 + theme(text = element_text(size = 9))
p1 <- p1 + annotate("text", x = 1984, y = 7.4, label = "A)", size = 6)
p1 <- p1 + coord_cartesian(ylim = c(-3.5, 6), xlim = c(1990, 2018), clip = "off")



p2 <- draw(gam_model, select = mod_smooths[2], residuals = TRUE, rug = FALSE)
p2 <- p2 + theme_bw()
p2 <- p2 + geom_line(size = 0.75)
p2 <- p2 + scale_color_manual(values = alpha(mycolor_pallette, 0.3))
p2 <- p2 + theme(legend.position = c(0.70,0.88), legend.title = element_blank(), 
                 legend.key.height = unit(0.5, "line"),
                 legend.background = element_rect(color = NA, fill = NA))
p2 <- p2 + theme(text = element_text(size = 9))
p2 <- p2 + annotate("text", x = 1984, y = 0.9, label = "B)", size = 6)
p2 <- p2 + coord_cartesian(ylim = c(-0.7, 0.7), xlim = c(1990, 2018), clip = "off")


coloors <- RColorBrewer::brewer.pal(n = 10, name = "Paired")
coloors <- coloors[c(2,4,6,8)]
p3 <- draw(gam_model, select = mod_smooths[3], residuals = TRUE, rug = FALSE)
p3 <- p3 + theme_bw()
p3 <- p3 + geom_line(size = 0.75)
p3 <- p3 + scale_color_manual(values = coloors)
p3 <- p3 + theme(legend.position = c(0.67,0.75), legend.title = element_blank(), 
                 legend.key.height = unit(0.5, "line"),
                 legend.background = element_rect(color = NA, fill = NA))
p3 <- p3 + theme(text = element_text(size = 9))
p3 <- p3 + annotate("text", x = 1984, y = 3.1, label = "C)", size = 6)
p3 <- p3 + coord_cartesian(ylim = c(-1.7, 2.5), xlim = c(1990, 2018), clip = "off")


gam_model <- richness_other_gam_GS
mod_smooths <- smooths(gam_model)
mod_estimates <- smooth_estimates(gam_model)
mod_estimates <- add_confint(mod_estimates)
p4 <- draw(gam_model, select = mod_smooths[1], residuals = TRUE, resid_col = mycolor_pallette[5], rug = FALSE)
p4 <- p4 + theme_bw()
p4 <- p4 + geom_line(size = 0.75)
p4 <- p4 + theme(text = element_text(size = 9))
p4 <- p4 + annotate("text", x = 1984, y = 3, label = "D)", size = 6)
p4 <- p4 + coord_cartesian(ylim = c(-3.1, 2.2), xlim = c(1990, 2018), clip = "off")



p5 <- draw(gam_model, select = mod_smooths[2], residuals = TRUE, rug = FALSE)
p5 <- p5 + theme_bw()
p5 <- p5 + geom_line(size = 0.75)
p5 <- p5 + scale_color_manual(values = alpha(mycolor_pallette, 0.3))
p5 <- p5 + theme(legend.position = "none")
p5 <- p5 + theme(text = element_text(size = 9))
p5 <- p5 + annotate("text", x = 1984, y = 0.2, label = "E)", size = 6)
p5 <- p5 + coord_cartesian(ylim = c(-0.22, 0.15), xlim = c(1990, 2018), clip = "off")


coloors <- RColorBrewer::brewer.pal(n = 10, name = "Paired")
coloors <- coloors[c(2,4,6,8)]
p6 <- draw(gam_model, select = mod_smooths[3], residuals = TRUE, rug = FALSE)
p6 <- p6 + theme_bw()
p6 <- p6 + geom_line(size = 0.75)
p6 <- p6 + scale_color_manual(values = coloors)
p6 <- p6 + theme(legend.position ="none")
p6 <- p6 + theme(text = element_text(size = 9))
p6 <- p6 + annotate("text", x = 1984, y = 1.5, label = "F)", size = 6)
p6 <- p6 + coord_cartesian(ylim = c(-1.2, 1.2), xlim = c(1990, 2018), clip = "off")



tiff("./../05_Figures/02_Feb23/03_Figure3.tiff", width = 16, height = 12, unit = "cm", res = 300)
p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(ncol = 3, nrow = 2)
dev.off()
