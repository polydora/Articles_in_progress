library(Momocs)
library(ggplot2)
bot

bot$fac
bot$coo


panel(bot, fac="type", names=TRUE)
stack(bot)

coo_oscillo(bot[1], "efourier")

Ptolemy(bot[1], nb.h = 10)

shapes                    # prints a brief summary
panel(shapes, names=TRUE) # base graphics


shp <- shapes[4]
coo_plot(shp)

Ptolemy(shp, nb.h = 20)



shapes_f <- efourier(shapes, nb.h=10)
shapes_f



boxplot(shapes_f, drop=1)

shapes.p <- PCA(shapes_f)
class(shapes.p)        # a PCA object, let's plot it
plot(shapes.p)



coo_oscillo(bot[1], "efourier")

coo_oscillo(shapes[4], "efourier")


bot.f <- efourier(bot)


bot.p <- PCA(bot.f)
class(bot.p)        # a PCA object, let's plot it
plot(bot.p)

CLUST(bot.p, ~type)

KMEANS(bot.p, centers = 10)


# mean shape, per group
bot.ms <- MSHAPES(bot.f, ~type)
# lets rebuild an Out
Out(bot.ms$shp) %>% panel(names=TRUE)


bot %>% efourier %>% PCA %>% as_df

beer   <- bot.ms$shp$beer    %>% coo_plot(border="blue")
whisky <- bot.ms$shp$whisky  %>% coo_draw(border="red")

tps_grid(beer, whisky)
tps_arr(beer, whisky)
tps_iso(beer, whisky)

pile(wings)

w.al <- fgProcrustes(wings)

PCA(w.al) %>% plot_PCA(1)

w.ef <- efourier(wings)
PCA(wings) %>% plot_PCA(1)


hearts
panel(hearts, fac="aut", names="aut")

ht <- measure(hearts, coo_area, coo_circularity, d(1, 3))
class(ht)
ht$coe
ht %>% PCA() %>% plot_PCA(~aut)


scree(bot.p)
scree_plot(bot.p)
boxplot(bot.p, 1)
PCcontrib(bot.p)

bot.p %>% as_df(3) 


TraCoe(iris[, -5], fac=data.frame(sp=iris$Species)) %>%
  PCA() %>% plot_PCA(~sp)


###################

dat <- data.frame(x = seq(0,2*pi, 0.1))

dat %>% 
  mutate(y_sin = sin(x*8*pi), y_cos = cos(x/10), y = y_sin + y_cos) %>% 
  ggplot(aes(x, y)) +
  geom_line()



rnorm(200) %>%
  matrix(ncol = 2) %T>%
  plot %>% # plot usually does not return anything. 
  colSums

data.frame(z = rnorm(100)) %$% 
  ts.plot(.$z)

iris$Sepal.Length %<>% sqrt

####################

import.txt()

library(dplyr)

dat <- read.table("clipboard", sep = "\t")
for(i in unique(dat$V1) ){
  dat %>%
    filter(V1 == i) %>%
    select(V2, V3) -> df

  write.table(df, file = paste("Coordinates/",i,".txt",sep = ""), row.names = FALSE, col.names = FALSE)
}



folder <- paste("Coordinates/", list.files("Coordinates"), sep = "")

coords <- import_txt(txt.paths = folder)

myt <- Out(coords)

str(myt)

panel(myt)

df <- read.table("clipboard", sep = "\t", header = T)


myt$fac <- df

  myt %T>%                    # A toy dataset
  stack() %>%                  # Another picture of aligned outlines
  efourier(6, norm=TRUE) %>%  
  PCA() %T>%                   # Principal Component Analysis
  plot_PCA(~Genotype,points = T, morphospace = T, chull= T, chullfilled =T, labelpoints = F, labelgroups = T) 
  # %>% 
  # LDA(~Gabitus) %>%                # Linear Discriminant Analysis
  # plot_CV()              
