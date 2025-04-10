################################################
###                 PART 1                   ###
################################################
library(pwr)
library(tidyverse)
library(readxl)

#Power analysis
pwr.t.test(d=0.65, 
           sig.level=0.05, 
           type="one.sample", 
           alternative = "two.sided", 
           power=0.8)
#n=20.58 - need at least 21 observations

################################################
###                 PART 2                   ###
################################################
#Pulling data from figure 2
fig2_further = read_csv("further_data.csv", col_names = "Further")
fig2_closer = read_csv("closer_data.csv", col_names = "Closer")

#Combining 2 values into a single tibble
fig2_tibble = bind_cols(fig2_further, fig2_closer)

#Mutating a new column to show the difference between columns
fig2_tibble = fig2_tibble |>
  mutate(Difference = Closer-Further)

#view(fig2_tibble)


################################################
###                 PART 3                   ###
################################################
dopamine_summ = fig2_tibble |>
  summarize(
    further    = mean(Further),
    closer     = mean(Closer),
    difference = mean(Difference)
  )
view(dopamine_summ)


################################################
###                 PART 4                   ###
################################################











