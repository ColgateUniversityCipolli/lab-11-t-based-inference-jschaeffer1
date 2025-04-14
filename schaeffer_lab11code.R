################################################
###                 PART 1                   ###
################################################
library(pwr)
library(tidyverse)
library(effectsize)

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
#view(dopamine_summ)

#Summarizing further
further_summary = ggplot() + 
  geom_histogram(aes(fig2_tibble$Further, y=after_stat(density))) +
  theme_bw() +
  geom_hline(yintercept=0) +
  xlab("Dopamine Compared to Baseline") +
  ylab("Density")

#Summarizing closer
closer_summary = ggplot() + 
  geom_histogram(aes(fig2_tibble$Closer, y=after_stat(density))) +
  theme_bw() +
  geom_hline(yintercept=0) +
  xlab("Dopamine Compared to Baseline") +
  ylab("Density")

#Summarizing difference
difference_summary = ggplot() + 
  geom_histogram(aes(fig2_tibble$Difference, y=after_stat(density))) +
  theme_bw() +
  geom_hline(yintercept=0) +
  xlab("Dopamine Change") +
  ylab("Density")





################################################
###                 PART 4                   ###
################################################

mu0 <- 0
#Calculating for closer (right-sided)
#Manually calculating statistics
x <- fig2_tibble$Closer
xbar <- mean(x)
s <- sd(x)
n <- length(x)
t.stat <- (xbar - mu0)/(s/sqrt(n))
p.val <- pt(q=-abs(t.stat), df = n-1)


#Calculating hedges value
closer_hedges_vals = hedges_g(x = x, mu = mu0, alternative = "greater")


#having t.test calculate values automatically and ensuring they match
closer_t_test = t.test(x=x, mu = mu0, alternative = "greater")
closer_CI = t.test(x=x)$conf.int #Caluclating the CI using a two sided test

#####################
#Calculating for further (left sided)
#Manually calculating statistics
x <- fig2_tibble$Further
xbar <- mean(x)
s <- sd(x)
n <- length(x)
t.stat <- (xbar - mu0)/(s/sqrt(n))
p.val <- pt(q=-abs(t.stat), df = n-1)


#Calculating hedges value
further_hedges_vals = hedges_g(x = x, mu = mu0, alternative = "less")


#having t.test calculate values automatically and ensuring they match
further_t_test = t.test(x=x, mu = mu0, alternative = "less")
further_CI = t.test(x=x)$conf.int #Caluclating the CI using a two sided test

#######################
#Calculating for difference (two-sided)

#Manually calculating statistics
x <- fig2_tibble$Difference
xbar <- mean(x)
s <- sd(x)
n <- length(x)
t.stat <- (xbar - mu0)/(s/sqrt(n))
p.val <- 2*pt(q=-abs(t.stat), df = n-1)


#Calculating hedges value
difference_hedges_vals = hedges_g(x = x, mu = mu0, alternative = "two.sided")


#having t.test calculate values automatically and ensuring they match
difference_t_test = t.test(x=x, mu = mu0, alternative = "two.sided")


################################################
###                 PART 5                   ###
################################################
ggdat.t <- tibble(t=seq(-5,5,length.out=1000))|>
  mutate(pdf.null = dt(t, df=n-1))


#Making plot for closer
#Pulling t value
obs_t=closer_t_test$statistic[[1]]

ggdat.obs <- tibble(t    = obs_t, 
                    y    = 0) # to plot on x-axis

# Resampling to approximate the sampling distribution 
# on the data
R <- 1000
resamples <- tibble(t=numeric(R))
for(i in 1:R){
  curr.sample <- sample(x=fig2_tibble$Closer,
                        size=n,
                        replace=T)
  resamples$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(n))
}


s <- sd(fig2_tibble$Closer)
t.breaks <- c(-5, 0, 
              qt(0.95, df = n-1), 5,  # rejection region (right)
              obs_t)                  # t-statistic observed
xbar.breaks <- t.breaks * s/(sqrt(n)) + mu0



# Create Plot
closer_plot = ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0)+
  # rejection regions
  geom_ribbon(data=subset(ggdat.t, t>=qt(0.95, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # plot p-value (not visible)
  geom_ribbon(data=subset(ggdat.t, t>=t.stat), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="reg", alpha=0.25)+
  # plot observation point
  geom_point(data=ggdat.obs, aes(x=t, y=y), color="red")+
  # Resampling Distribution
  stat_density(data=resamples, 
               aes(x=t),
               geom="line", color="grey")+
  # clean up aesthetics
  theme_bw()+
  ylab("Density")+
  scale_x_continuous("t",
                     breaks = round(t.breaks,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks,
                                         labels = round(xbar.breaks,2)))


#################################################
#################################################


#Making plot for further
#Pulling t value
obs_t=further_t_test$statistic[[1]]

ggdat.obs <- tibble(t    = obs_t, 
                    y    = 0) # to plot on x-axis

# Resampling to approximate the sampling distribution 
# on the data
R <- 1000
resamples <- tibble(t=numeric(R))
for(i in 1:R){
  curr.sample <- sample(x=fig2_tibble$Further,
                        size=n,
                        replace=T)
  resamples$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(n))
}

s <- sd(fig2_tibble$Further)
t.breaks <- c(-5, qt(0.05, df = n-1), # rejection region (left)
              0, 5,
              obs_t)                  # t-statistic observed
xbar.breaks <- t.breaks * s/(sqrt(n)) + mu0


# Create Plot
further_plot = ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0)+
  # rejection regions
  geom_ribbon(data=subset(ggdat.t, t<=qt(0.05, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # plot p-value (not visible)
  geom_ribbon(data=subset(ggdat.t, t>=t.stat), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="reg", alpha=0.25)+
  # plot observation point
  geom_point(data=ggdat.obs, aes(x=t, y=y), color="red")+
  # Resampling Distribution
  stat_density(data=resamples, 
               aes(x=t),
               geom="line", color="grey")+
  # clean up aesthetics
  theme_bw()+
  ylab("Density")+
  scale_x_continuous("t",
                     breaks = round(t.breaks,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks,
                                         labels = round(xbar.breaks,2)))

#################################################
#################################################


#Making plot for difference
#Pulling t value
obs_t=difference_t_test$statistic[[1]]


ggdat.obs <- tibble(t    = obs_t, 
                    y    = 0) # to plot on x-axis

# Resampling to approximate the sampling distribution 
# on the data
R <- 1000
resamples <- tibble(t=numeric(R))
for(i in 1:R){
  curr.sample <- sample(x=fig2_tibble$Difference,
                        size=n,
                        replace=T)
  resamples$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(n))
}

s <- sd(fig2_tibble$Difference)
t.breaks <- c(-5, qt(0.025, df = n-1), # rejection region (left)
              0, 
              qt(0.975, df = n-1), 5,  # rejection region (right)
              obs_t)                  # t-statistic observed
xbar.breaks <- t.breaks * s/(sqrt(n)) + mu0


# Create Plot
difference_plot = ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0)+
  # rejection regions
  geom_ribbon(data=subset(ggdat.t, t>=qt(0.975, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  geom_ribbon(data=subset(ggdat.t, t<=qt(0.025, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # plot p-value (not visible)
  geom_ribbon(data=subset(ggdat.t, t>=t.stat), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="reg", alpha=0.25)+
  # plot observation point
  geom_point(data=ggdat.obs, aes(x=t, y=y), color="red")+
  # Resampling Distribution
  stat_density(data=resamples, 
               aes(x=t),
               geom="line", color="grey")+
  # clean up aesthetics
  theme_bw()+
  ylab("Density")+
  scale_x_continuous("t",
                     breaks = round(t.breaks,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks,
                                         labels = round(xbar.breaks,2)))







