library(vegan)
library(factoextra)
library(readxl)
library(ggplot2)

#ENVIT
#antes tinha feito com a planilha1 agora com aplanilha3
ex2 <-read_excel("family_4spp_leo.xlsx" ,sheet="Planilha3")
com = ex2[,2:425]
#env = df[,3:19]

m_com = as.matrix(com)

#nmds code
set.seed(123)
nmds = metaMDS(m_com, distance = "bray")
nmds
#en = envfit(nmds, env, permutations = 999, na.rm = TRUE)
#en
plot(nmds)
#plot(en)
#extract NMDS scores (x and y coordinates) for sites from newer versions of vegan package
data.scores = as.data.frame(scores(nmds)$sites)
data.species = as.data.frame(scores(nmds)$species)
#add 'season' column as before
data.scores$season = ex2$Family
#en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)
#en_coord_cat = as.data.frame(scores(en, "factors")) * ordiArrowMul(en)

gg = ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = data.scores, aes(colour = season), size = 3, alpha = 0.5) + 
  scale_colour_manual(values = c("orange", "darkorange","green","darkgreen", "blue","darkblue","red","darkred"))  + 
  #  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
  geom_point(data = data.species, aes(x = NMDS1, y = NMDS2), 
             shape = "diamond", size = 2, alpha = 0.5, colour = "navy") +
  # geom_text(data = en_coord_cat, aes(x = NMDS1, y = NMDS2+0.04), 
  #           label = row.names(en_coord_cat), colour = "navy", fontface = "bold") + 
  # geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
  #           fontface = "bold", label = row.names(en_coord_cont)) + 
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30")) + 
  labs(colour = "Fish Specie")  + scale_color_discrete(
    labels = c(
      expression(italic("Acanthurus chirurgus (2016)")),
      expression(italic("Acanthurus chirurgus (2017)")),
      expression(italic("Kyphosus sp. (2016)")),
      expression(italic("Kyphosus sp. (2017)")),
      expression(italic("Scarus trispinosus (2016)")),
      expression(italic("Scarus trispinosus (2017)")),
      expression(italic("Sparisoma axilare (2016)")),
      expression(italic("Scarus axilare (2017)"))))

gg
png(filename = "high_res_plot.png", width = 10, height = 6, units = "in", res = 300)

# Generate the plot
print(gg)

# Close the graphical device
dev.off()