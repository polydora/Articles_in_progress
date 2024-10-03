# Код, позволяющий сверять списки видов с WorMS


library(worms)
# detach(plyr)





# df <- read_excel("Data/west_sound 1995-2024.xls", sheet = "Таблица видов сокращ.")
# df <- 
#   df %>% 
#   filter(!Species %in% c("Hetairus sp.", "Styela rustica", "Rhodophyta нитчатые", "Chlorophyta нитчатые", "Dictyosiphon", "Membranopora sp.", "Actinaria gen.sp."))




# df <- read_excel("Data/Материалы отчетов ЗИН_2005_2011.xlsx", sheet = "Abundance")

df <- read.table("clipboard", sep = "\t", header = T)

sp_df <- wormsbymatchnames(taxon_names = df$Species)

sp_df_short <-
sp_df %>% 
  dplyr::select(valid_name, phylum, class) %>% 
  cbind(., df$Species) 

names(sp_df_short)[4] <- "Species"

write.table(sp_df_short, "clipboard", sep = "\t", row.names = F)
