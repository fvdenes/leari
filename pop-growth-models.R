### Lear's Macaw Abundance model ####

library(unmarked)


m1 <- pcount(~times+months ~julian+year,data=umf1, mixture="NB", se=FALSE)
m2 <- pcount(~times+months ~julian+year,data=umf1, mixture="NB",starts=coef(m1))


m2 <- pcount(~times ~year,data=umf1, mixture="NB")

m1