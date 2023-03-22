coloors <- RColorBrewer::brewer.pal(n = 10, name = "Paired")
coloors <- coloors[c(2,4,6,8)]

gam_model <- insect_abundance_corvariate_model
mod_smooths <- smooths(gam_model)
p1 <- draw(gam_model, select = mod_smooths[5], residuals = TRUE, rug = FALSE, resid_col = "gray30")
p1 <- p1 + theme_bw()
p1 <- p1 + geom_line(linewidth = 0.1, linetype = "solid")
p1 <- p1 + scale_color_manual(values = coloors)
p1 <- p1 + theme(legend.position =  "none", 
                 plot.margin = margin(0, 0, 0.2, 0.2, "cm"))
p1 <- p1 + theme(text = element_text(size = 5), 
                 axis.ticks = element_line(linewidth = 0.2), 
                 axis.title.x.top = element_blank(),plot.title = element_blank(),
                 plot.caption = element_blank(),
                 axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")),
                 axis.title.x = element_text(margin = margin(t = 0.02, r = 0, b = 0, l = 0, unit = "cm")))
p1 <- p1 + ggtitle(label = "")


p2 <- draw(gam_model, select = mod_smooths[7], residuals = TRUE, rug = FALSE, resid_col = "gray30")
p2 <- p2 + theme_bw()
p2 <- p2 + geom_line(size = 0.5, linetype = "solid")
p2 <- p2 + theme(text = element_text(size = 5))
p2 <- p2 + scale_color_manual(values = coloors)
p2 <- p2 + theme(legend.position =  "none", plot.margin = margin(0, 0, 0.2, 0.2, "cm"))
p2 <- p2 + theme(text = element_text(size = 5), axis.ticks = element_line(linewidth = 0.2),  
                 axis.title.x.top = element_blank(),plot.title = element_blank(),
                 plot.caption = element_blank(),
                 axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")),
                 axis.title.x = element_text(margin = margin(t = 0.02, r = 0, b = 0, l = 0, unit = "cm")))
p2 <- p2 + ggtitle(label = "")
p2 <- p2 + ylab("")

p3 <- draw(gam_model, select = mod_smooths[8], residuals = TRUE, rug = FALSE, resid_col = "gray30")
p3 <- p3 + theme_bw()
p3 <- p3 + geom_line(size = 0.5, linetype = "solid")
p3 <- p3 + theme(text = element_text(size = 5))
p3 <- p3 + scale_color_manual(values = coloors)
p3 <- p3 + theme(legend.position =  "none", plot.margin = margin(0, 0, 0.2, 0.2, "cm"))
p3 <- p3 + theme(text = element_text(size = 5), axis.ticks = element_line(linewidth = 0.2),                   
                 axis.title.x.top = element_blank(),plot.title = element_blank(),
                 plot.caption = element_blank(),
                 axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")),
                 axis.title.x = element_text(margin = margin(t = 0.02, r = 0, b = 0, l = 0, unit = "cm")))
p3 <- p3 + ggtitle(label = "")
p3 <- p3 + ylab("")


p4 <- draw(gam_model, select = mod_smooths[12]) + theme_bw() + scale_fill_viridis(option = "magma", alpha = 0.8)
p4 <- p4 + theme(text = element_text(size = 5))
p4 <- p4 + theme(legend.title = element_blank(), 
                 legend.text = element_text(size = 5),
                 legend.key.height = unit(0.5, "line"), 
                 legend.box.spacing = unit(0, "cm"), 
                 legend.key.width = unit(0.1, "cm"), plot.margin = margin(0, 0, 0.2, 0.2, "cm"),
                 axis.ticks = element_line(linewidth = 0.2), 
                 plot.title = element_blank(),
                 plot.caption = element_blank(),
                 axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")),
                 axis.title.x = element_text(margin = margin(t = 0.02, r = 0, b = 0, l = 0, unit = "cm")))
p4 <- p4 + ggtitle(label = "")


p5 <- draw(gam_model, select = mod_smooths[14]) + theme_bw() + scale_fill_viridis(option = "magma", alpha = 0.8)
p5 <- p5 + theme(text = element_text(size = 5))
p5 <- p5 + theme(legend.title = element_blank(), 
                   legend.text = element_text(size = 5),
                   legend.key.height = unit(0.5, "line"), 
                   legend.box.spacing = unit(0, "cm"), 
                   legend.key.width = unit(0.1, "cm"), plot.margin = margin(0, 0, 0.2, 0.2, "cm"),
                   axis.ticks = element_line(linewidth = 0.2), 
                   plot.title = element_blank(),
                   plot.caption = element_blank(),
                   axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")),
                   axis.title.x = element_text(margin = margin(t = 0.02, r = 0, b = 0, l = 0, unit = "cm")))
p5 <- p5 + ggtitle(label = "")


gam_model <- insect_richness_covariate_model
mod_smooths <- smooths(gam_model)

gam_model <- insect_abundance_corvariate_model
mod_smooths <- smooths(gam_model)
p6 <- draw(gam_model, select = mod_smooths[5], residuals = TRUE, rug = FALSE, resid_col = "gray30")
p6 <- p6 + theme_bw()
p6 <- p6 + geom_line(size = 0.5, linetype = "solid")
p6 <- p6 + scale_color_manual(values = coloors)
p6 <- p6 + theme(legend.position =  "none", plot.margin = margin(0, 0, 0.2, 0.2, "cm"))
p6 <- p6 + theme(text = element_text(size = 5), axis.ticks = element_line(linewidth = 0.2),  
                 axis.title.x.top = element_blank(),plot.title = element_blank(),
                 plot.caption = element_blank(),
                 axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")),
                 axis.title.x = element_text(margin = margin(t = 0.02, r = 0, b = 0, l = 0, unit = "cm")))
p6 <- p6 + ggtitle(label = "")


p7 <- draw(gam_model, select = mod_smooths[7], residuals = TRUE, rug = FALSE, resid_col = "gray30")
p7 <- p7 + theme_bw()
p7 <- p7 + geom_line(size = 0.5, linetype = "solid")
p7 <- p7 + theme(text = element_text(size = 5))
p7 <- p7 + scale_color_manual(values = coloors)
p7 <- p7 + theme(legend.position =  "none", plot.margin = margin(0, 0, 0.2, 0.2, "cm"))
p7 <- p7 + theme(text = element_text(size = 5), axis.ticks = element_line(linewidth = 0.2),  
                 axis.title.x.top = element_blank(),plot.title = element_blank(),
                 plot.caption = element_blank(),
                 axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")),
                 axis.title.x = element_text(margin = margin(t = 0.02, r = 0, b = 0, l = 0, unit = "cm")))
p7 <- p7 + ggtitle(label = "")
p7 <- p7 + ylab("")

p8 <- draw(gam_model, select = mod_smooths[8], residuals = TRUE, rug = FALSE, resid_col = "gray30")
p8 <- p8 + theme_bw()
p8 <- p8 + geom_line(size = 0.5, linetype = "solid")
p8 <- p8 + theme(text = element_text(size = 5))
p8 <- p8 + scale_color_manual(values = coloors)
p8 <- p8 + theme(legend.position =  "none", plot.margin = margin(0, 0, 0.2, 0.2, "cm"))
p8 <- p8 + theme(text = element_text(size = 5), axis.ticks = element_line(linewidth = 0.2),  
                 axis.title.x.top = element_blank(),plot.title = element_blank(),
                 plot.caption = element_blank(),
                 axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")),
                 axis.title.x = element_text(margin = margin(t = 0.02, r = 0, b = 0, l = 0, unit = "cm")))
p8 <- p8 + ggtitle(label = "")
p8 <- p8 + ylab("")


p9 <- draw(gam_model, select = mod_smooths[12]) + theme_bw() + scale_fill_viridis(option = "magma", alpha = 0.8)
p9 <- p9 + theme(text = element_text(size = 5))
p9 <- p9 + theme(legend.title = element_blank(), 
                   legend.text = element_text(size = 5),
                   legend.key.height = unit(0.5, "line"), 
                   legend.box.spacing = unit(0, "cm"), 
                   legend.key.width = unit(0.1, "cm"), plot.margin = margin(0, 0, 0.2, 0.2, "cm"),
                   axis.ticks = element_line(linewidth = 0.2), 
                   plot.title = element_blank(),
                   plot.caption = element_blank(),
                   axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")),
                   axis.title.x = element_text(margin = margin(t = 0.02, r = 0, b = 0, l = 0, unit = "cm")))
p9 <- p9 + ggtitle(label = "")


p10 <- draw(gam_model, select = mod_smooths[14]) + theme_bw() + scale_fill_viridis(option = "magma", alpha = 0.8)
p10 <- p10 + theme(text = element_text(size = 5))
p10 <- p10 + theme(legend.title = element_blank(), 
                 legend.text = element_text(size = 5),
                 legend.key.height = unit(0.5, "line"), 
                 legend.box.spacing = unit(0, "cm"), 
                 legend.key.width = unit(0.1, "cm"), plot.margin = margin(0, 0, 0.2, 0.2, "cm"),
                 axis.ticks = element_line(linewidth = 0.2), 
                 plot.title = element_blank(),
                 plot.caption = element_blank(),
                 axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")),
                 axis.title.x = element_text(margin = margin(t = 0.02, r = 0, b = 0, l = 0, unit = "cm")))
p10 <- p10 + ggtitle(label = "")


tiff("./../05_Figures/02_Feb23/SupplementaryFigure.tiff", width = 16, height = 8, unit = "cm", res = 300)
p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + plot_layout(ncol = 5, nrow = 2)
dev.off()



