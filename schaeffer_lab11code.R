################################################
###                 PART 1                   ###
################################################
library(pwr)
library(tidyverse)

#Power analysis
pwr.t.test(d=0.65, sig.level=0.05, type="one.sample", alternative = "two.sided", power=0.8)



