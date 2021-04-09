library(mlogit)

data("Heating")
H <- mlogit.data(Heating, shape = "wide", choice = "depvar", varying = c(3:12))

hc <- mlogit(depvar ~ ic + oc | rooms, H)
data("oc.gc50") <= data("oc.gc")*0.5
summary(hc)
#Hn <- H
#Hn[idx(Hn, 2) == "hp","ic"] <- 0.9 * Hn[idx(Hn, 2) == "hp", "ic"]
#apply(predict(hc, newdata = Hn ),2 , mean)

Hn <- H
Hn[idx(Hn, 2) == "ec","oc"] <- 0.5 * Hn[idx(Hn, 2) == "ec","oc"]
apply(predict(hc, newdata = Hn ),2 , mean)

Tn <- H
Tn[idx(Tn, 2) == "er","oc"] <- 0.5 * Tn[idx(Tn, 2) == "er","oc"]
apply(predict(hc, newdata = Tn ),2 , mean)


