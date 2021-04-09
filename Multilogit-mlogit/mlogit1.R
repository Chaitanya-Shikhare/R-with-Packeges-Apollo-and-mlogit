install.packages ("mlogit")
library(mlogit)

library(tidyr)

data("Mode", package = "mlogit")

mode <- mlogit.data(Mode, shape = "wide", choice = "choice", varying = c(2:9))

summary(mode)
#Question 3.1
f2 <- mlogit(choice ~ time + cost | 1, mode)
summary(f2)


