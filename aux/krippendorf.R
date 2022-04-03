library(tidyverse)
library(krippendorffsalpha)


nominal <- matrix(c(1,2,3,3,2,1,4,1,2,NA,NA,NA,
                    1,2,3,3,2,2,4,1,2,5,NA,3,
                    NA,3,3,3,2,3,4,2,2,5,1,NA,
                    1,2,3,3,2,4,4,1,2,5,1,NA), 12, 4)
set.seed(42)
fit.full <- krippendorffs.alpha(nominal, level = "nominal", control = list(parallel = FALSE),verbose = TRUE)
summary(fit.full)
(inf.6 = influence(fit.full, units = 6))
fit.full$alpha.hat - inf.6$dfbeta.units
fit.sub = krippendorffs.alpha(nominal[-6, ], level = "nominal",control = list(parallel = FALSE)) 
confint(fit.sub)
plot(fit.sub, xlim = c(0, 1), xlab = "Bootstrap Estimates", main = "Nominal Data", density = FALSE)
