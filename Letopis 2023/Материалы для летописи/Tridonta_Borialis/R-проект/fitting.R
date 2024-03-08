library(readxl)
library(ggplot2)
library(dplyr)
Nomber_Age_size <- read_excel("data/Nomber_Age_size.xlsx", sheet = "Лист1")

ggplot(data = Nomber_Age_size, aes(x = age_years, y = height_mm)) + 
  geom_point()

#max_age = 40
#min_age = 20
max_age = 20

#yangsters <- Nomber_Age_size %>% filter(age_years < max_age & age_years > min_age)
yangsters <- Nomber_Age_size %>% filter(age_years < max_age)
ggplot(data = yangsters, aes(x = age_years, y = height_mm)) + 
  geom_point()

size_fitter <- function(t,A,k,b){
  return(A * (1 - exp(- k * (t + b))))
}

A0 = 10
k0  = 0.11
b0  = 1.863

model <- nls(height_mm ~ size_fitter(age_years,A,k,b),
    data = yangsters,
    start = list(A = A0, k = k0, b = b0),
    #trace = TRUE,
    #control = nls.control(maxiter = 1000, minFactor = 0.000001)
)
 summary(model)

predicted_size = predict(model)
ggplot(data = yangsters, aes(x = age_years, y = height_mm)) + 
  geom_point() + 
  geom_line(data = yangsters, aes(x = age_years, y = predicted_size)
)+
  theme_bw()+
  labs(x = "возраст,года", y = "высота,мм")

