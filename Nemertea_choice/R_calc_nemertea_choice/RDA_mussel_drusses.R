library(vegan)
library(ggplot2)
library(ggvegan)
library(ggrepel)


drus <- read.table("Dolg_drus.csv", sep = ";", header = T)



mean_spec <- apply(drus[,-c(1:3)],  MARGIN = 2, FUN = mean)
qplot(log(mean_spec)) + geom_histogram()


drus2 <- drus[,-c(1:3) ]

drus2 <- drus2[, log(mean_spec) > 0]

library(dplyr)

drus %>%select(-Sample, -Year) %>% group_by(Type) %>% summarise_all(mean) %>% t() %>% as.data.frame()
  
  
  

str(drus2)

rda_drus <- rda(log(drus2+1) ~ type, scale = T)

summary(rda_drus)



gg_rda_drus <- fortify(rda_drus, scaling = "species")

str(gg_rda_drus)

gg_rda_drus$Score

autoplot(rda_drus) + theme_bw()


species <- as.character(gg_rda_drus[gg_rda_drus$Score == "species", ]$Label)


species <- c("Nemertini", "H. imbricata", "S. armiger", "H.filiformis", "D.quadrilobata"  , "Oligochaeta", "H. ulvae", "O.aculeus", "M. balthica", "Mytilus"   )

spec_code <- rep(NA, 10) 
spec_code[species %in% c("Nemertini", "Oligochaeta", "Mytilus") ] <- 1
spec_code[!species %in% c("Nemertini", "Oligochaeta", "Mytilus") ] <- 0
spec_code <- factor(spec_code)


ggplot(gg_rda_drus[gg_rda_drus$Score == "centroids", ],  aes(x = RDA1, y = PC1)) +
  theme_bw() +
  theme(panel.grid = element_blank()) + 
  geom_text_repel(aes(label = c("Между друз", "В друзах")), color = "blue") + 
  geom_point(size = 4, shape = 4) +  
  geom_point(data = gg_rda_drus[gg_rda_drus$Score == "sites", ],  aes(x = RDA1, y = PC1, shape = type), size = 0.8) + 
  geom_segment(data = gg_rda_drus[gg_rda_drus$Score == "species",  ], aes(x = 0, y = 0, xend = RDA1, yend = PC1, color = spec_code, size = spec_code), arrow = arrow(type = "open", length = unit(0.02, "native"))) + 
  geom_text_repel(data = gg_rda_drus[gg_rda_drus$Score == "species", ], aes(x= RDA1, y = PC1, label = species, color = spec_code)) +
  scale_color_manual(values = c("gray", "red"))+
  scale_size_manual(values = c(0.3, 1))+
  guides(shape = "none", color="none", size = "none") + 
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)




+ geom_text_repel(data = gg_rda_holo[gg_rda_holo$Score == "biplot",  ][-1,], aes(x =  RDA1, y = RDA2, label = Label)) + labs(x = "Constrained RDA1", y = "Сonstrained RDA2") + theme(legend.position = "bottom", panel.grid = element_blank() )

