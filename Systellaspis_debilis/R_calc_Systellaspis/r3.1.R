# install.packages("seqinr")
# install.packages("ade4")
# 
# install.packages("ape")
# install.packages("ggtree")



require(ape)
library(ggtree)
library(seqinr)
library(ade4)


#Translate to protein sequence
getwd()
DNA <- read.fasta("Data/alignment_COI_OURS1.fasta")

alignment <- read.alignment(file="Data/Nucleotide alignment_COI_OURS_without_outgr.fasta", format="fasta")
alignment$seq #showing the sequence of the file

str(alignment)

alignment$nam

#### названия сиквенсов в выравнивании перемешаны, для соспоставления с матрицей расстояний по PC1 и PC2 их нужно выстроить в таком же порядке, как и во второй матрице

alignment$nb #number of sequence

#### сиквенсов 73, из них 4 - ювенили, которые мы удалили из морф анализа для уменьшения шума. Так что и из данной матрицы их нужно удалить.

alignment$nam

include <- row.names(ca_unconstr_pred) 


shrimpdist <- dist.alignment(alignment)


shrimpdist <- as.matrix(dist.alignment(alignment, matrix = "identity" ))



shrimpdist <- (shrimpdist[include, include]) 




#### все получилось, кол-во строк сошлось


rownames(shrimpdist)

cca_unconstr <- scores(mod_cca, choices = 3:5)$sites %>% as.data.frame()

cca_unconstr2 <- cca_unconstr[rownames(cca_unconstr) %in% rownames(shrimpdist), ] 



library(vegan)

distmorph <- vegdist(cca_unconstr2, method = "euclidean")




mantel(xdis = shrimpdist, ydis = distmorph, permutations = 9999)


ord <- metaMDS(comm=shrimpdist)

plot(ord)


#### ГЕНЕТИКА (неудачная попытка)

# 
db_shrimpdist <- dbrda(shrimpdist ~ PC1 + PC2, data = scores_individuals)


plot(db_shrimpdist)

### связь ген расстояний с PC1

anova(db_shrimpdist)
##не значимая связь


# По-другому ген дист визуализироваит не удалось 
