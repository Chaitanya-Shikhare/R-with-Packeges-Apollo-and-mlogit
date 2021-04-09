library(mlogit)

data("Heating")

H <- mlogit.data(Heating, shape = "wide", choice = "depvar", varying = c(3:12))

heat2.mnl <- mlogit(depvar ~ ic + oc | rooms, H, alt.subset =
                      c("er","gc","gr","hp"))

summary(heat2.mnl)

#-----------------------------------------------

heat.mnl <- mlogit(depvar ~ ic + oc | rooms, H)

hmftest(heat.mnl,heat2.mnl)
