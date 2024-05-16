# Reanalyse arthropod data from Seibold et al (2019) using some
# ideas from Daskalova, Phillimore & Meyers-Smith (2021)
#
# Refs
# Daskalova, G.N., Phillimore, A.B., Myers‐Smith, I.H., 2021. Accounting for
#   year effects and sampling error in temporal analyses of invertebrate
#   population and biodiversity change: a comment on Seibold et al. 2019.
#   *Insect Conserv. Divers.* 14, 149–154. https://doi.org/10.1111/icad.12468
# Seibold, S., Gossner, M.M., Simons, N.K., Blüthgen, N., Müller, J., Ambarlı,
#   D., Ammer, C., Bauhus, J., Fischer, M., Habel, J.C., Linsenmair, K.E.,
#   Nauss, T., Penone, C., Prati, D., Schall, P., Schulze, E.-D., Vogt, J.,
#   Wöllauer, S., Weisser, W.W., 2019. Arthropod decline in grasslands and
#   forests is associated with landscape-level drivers. Nature 574, 671–674.
#   https://doi.org/10.1038/s41586-019-1684-3

# packages
pkgs <- c("here", "readr", "janitor", "mgcv", "gratia", "dplyr", "ggplot2",
          "ggrepel")
vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

# data - from Seibold et al Nature 2019
# https://doi.org/10.25829/bexis.25786-1.3.11
seibold <- read_csv2("/home/jt-miller/Soltis-lab/useful-code-depository/ecological-forecasting/R/data/25786_3_data.csv",
                     col_types = "cccccdndddddcddnddcddcddcddnnnnnnnnnnnn")

# basic data wrangling so we can plot
seibold <- seibold %>%
  janitor::clean_names() %>%
  rename(year = collection_year,
         region = exploratory,
         habitat = habitat_type) %>%
  mutate(across(c(habitat, sampling_regime, region, plot_id_year, plot_id),
                as.factor))

# basic plot
seibold %>%
  ggplot(aes(x = year, y = abundance_identified, group = plot_id)) +
  geom_line() +
  facet_grid(region ~ habitat)

## more wrangling
seibold <- seibold %>%
  mutate(year_f = factor(year), # year as a factor for ranef
         year_c = year - 2012) # centre year

# how many plots
seibold %>%
  group_by(plot_id) %>%
  count()

#
seibold %>%
  group_by(habitat, region, plot_id) %>%
  count() %>%
  arrange(-n)

# filter out the forest sites; many have fewer than 9 observations
seibold <- seibold %>%
  filter(habitat == "grassland")

seibold_end <- seibold %>%
  group_by(plot_id) %>%
  top_n(1, year) %>%
  ungroup() %>%
  mutate(year = max(year))

# abundance
seibold %>%
  filter(plot_id == "AEG1") %>%
  ggplot(aes(x = year, y = abundance_identified)) +
  geom_line(alpha = 0.5) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 9)) +
  labs(y = "Abundance", x = NULL)

site <- seibold %>%
  filter(plot_id == "AEG1")

# Negative binomial with theta (constant) estimated
m_site <- gam(abundance_identified ~ s(year), data = site,
              method = "REML", family = nb())

summary(m_site) # model summary

draw(m_site) # partial effect of smooth

appraise(m_site, method = "simulate") # model diagnostics, residuals qqplot, histogram, deviance, obs vs fitted

k.check(m_site) # check basis size k, the largest value can be is 9, if k-index gets below 1 then the basis may of been set to be too small. 
# this is also indicated by the p-value. Here, we dont have enough datapoints to redemy this situation. 
site %>%
  add_residuals(m_site) %>%
  ggplot(aes(x = year, y = .residual)) +
    geom_line() +
    geom_point()

acf(residuals(m_site)) # plotting the residuals overtime

rootogram(m_site, max_count = 125) %>%
  draw(warn_limits = FALSE, bar_fill = "black") # blue dots are the estimated abundance, the bars are the observed abundance. Here, we are predicting below the maximum abundance. 
# could also use darma package to assess. 
## install.packages("countreg", repos="http://R-Forge.R-project.org") 
autoplot(countreg::rootogram(m_site))

# use multiple threads for fitting
ctrl <- gam.control(nthreads = 4)

# fit to all data
m1 <- gam(abundance_identified ~ s(year) + # smooth of year (trend)
            s(plot_id, bs = "re"), # random effect of plot 
          data = seibold,
          method = "REML",
          family = nb(),
          control = ctrl) # use multiple threads if needed

summary(m1)
draw(m1)
k.check(m1) # indicates there is a problem, we would want to change K as the pvalue is very very low (~0). Random effects does not matter. 

# perhaps region-specific trends?
m2 <- gam(abundance_identified ~ region + # regional means (anytime we use a factor on the by level we need to account for the means)
            s(year, by = region) + # region-specific trends (get a smooth for each region in the dataset)
            s(plot_id, bs = "re"), # random intercept for plot
          data = seibold,
          method = "REML",
          family = tw(),
          control = ctrl)

summary(m2)
draw(m2)

# individual trends?
# about 1 minute on my laptop
system.time(
m3 <- gam(abundance_identified ~
            s(year, plot_id, bs = "fs", k = 5), # plot-specific trends # factor smooth basis (difference is in the by we can have individual smoothness (all individual lambda), while this fs makes it where all have a shared lambda (same wiggliness) but different shapes are allowed) 
          data = seibold,
          method = "REML",
          family = nb(),
          control = ctrl)
  )

summary(m3) 

draw(m3) # shows variations across sites. 

# region-specific trends, plus individual trends 
# about 3 minutes on my laptop
system.time(
m4 <- gam(abundance_identified ~ region + # regional means fixef
            s(year, by = region) + # region-specific smooths (capture the common patterns)
            s(year, plot_id, bs = "fs", k = 5), # plot-specific trends (capture the individual patterns per site)
          data = seibold,
          method = "REML",
          family = nb(),
          control = ctrl)
  )

summary(m4)

draw(m4)

# region-specific trends, plus individual trends, plus year-to-year effects
# about 5 minutes on my laptop
system.time(
m5 <- gam(abundance_identified ~ region + # regional means fixef
            s(year_f, bs = "re") + # year-to-year effects
            s(year, by = region, k = 5) + # region-specific smooths
            s(year, plot_id, bs = "fs", k = 5), # plot-specific trends
          data = seibold,
          method = "REML",
          family = nb(),
          control = ctrl)
  )

# plot smooths
draw(m5)

# model diagnostics
appraise(m5, method = "simulate")

# basis size
k.check(m5)

# rootogram
rootogram(m5, max_count = 300) %>%
  draw() # does pretty well, dips below expected threshold occassionaly (-1 is the Tukey guess limit)

## Alternatives with bam()
## not threaded unless you set up a cluster -- see ?bam
## DON'T RUN THIS IN WEBINAR!! - about 17 minutes
system.time(
b5a <- bam(abundance_identified ~ region + # regional means fixef
             s(year_f, bs = "re") + # year-to-year effects
             s(year, by = region) + # region-specific smooths
             s(year, plot_id, bs = "fs", k = 5), # plot-specific trends
           data = seibold,
           method = "fREML", # <-- fast REML!
           family = nb(),
           control = ctrl)
  )

## Discretise covariates -- algorithm uses `nthreads` threads
system.time(
b5b <- bam(abundance_identified ~ region + # regional means fixef
             s(year_f, bs = "re") + # year-to-year effects
             s(year, by = region) + # region-specific smooths
             s(year, plot_id, bs = "fs", k = 5), # plot-specific trends
           data = seibold,
           method = "fREML", # <-- fast REML!
           discrete = TRUE,  # <-- discretise covariates == smaller basis (this doesnt make sense for this data, but just a showcase) Maps everything to a discretized value (faster at the expense of some accuracy, use cluster instead.)
           family = nb(),
           control = ctrl)
  )

## Location scale models
## about 10 minutes!!
system.time(
m_twlss <- gam(list(
                 abundance_identified ~ region + # regional means fixef
                   s(year_f, bs = "re") + # year-to-year effects
                   s(year, by = region) + # region-specific smooths
                   s(year, plot_id, bs = "fs", k = 5), # plot-specific trends
                 ~ s(plot_id, bs = "re"), # each plot has its own twlss dist
                 ~ s(plot_id, bs = "re") # good way to model overdispersion. 
                ),
              data = seibold,
              method = "REML",
              optimizer = "efs",
              family = twlss(),
              control = ctrl)
  )

summary(m_twlss)

draw(m_twlss)

appraise(m_twlss)

##
smooths(m5)
y2y <- smooth_estimates(m5, "s(year_f)")

y2y %>%
  ggplot(aes(x = year_f, y = est)) +
  geom_pointrange(aes(ymin = est - se,
                      ymax = est + se)) +
  labs(x = NULL)

## other covariates
seibold <- seibold %>%
  mutate(year_s = scale(year),
         landuse_intensity_s = scale(landuse_intensity)[,1],
         mean_winter_temperature_s = scale(mean_winter_temperature)[,1],
         precipitation_sum_growing_preriod_s =
           scale(precipitation_sum_growing_preriod)[,1],
         grassland_cover_1000_s = scale(grassland_cover_1000)[,1],
         arable_cover_1000_s = scale(arable_cover_1000)[,1])

## about 1.5 minutes
system.time(
m_cov <- gam(abundance_identified ~
               s(year_s) + # overall trend
               s(landuse_intensity_s) +
               s(mean_winter_temperature_s) +
               s(precipitation_sum_growing_preriod_s) +
               s(grassland_cover_1000_s) +
               s(arable_cover_1000_s) +
               ti(mean_winter_temperature_s,
                  precipitation_sum_growing_preriod_s) +
               ti(year_s, landuse_intensity_s) +
               ti(year_s, grassland_cover_1000_s) +
               ti(year_s, arable_cover_1000_s) +
               s(year_f, bs = "re") + # year-to-year effects
               s(plot_id, bs = "re"), # site specific mean abundance
             family = nb(),
             method = "REML",
             control = ctrl,
             data = seibold,
             select = TRUE)
  )

# plot the smooths
sms <- smooths(m_cov)

# plot the univariate smooths
draw(m_cov, select = sms[1:6])

# plot the tensor product interation smooths
draw(m_cov, select = sms[7:10], rug = FALSE)

# plot the ranefs
draw(m_cov, select = sms[11:12])

# plot the overall trend effect
draw(m_cov, select = "s(year_s)")

# DON'T RUN IN WEBINAR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# put in site specific trends
# ~17 minutes 
system.time(
m_cov2 <- gam(abundance_identified ~
                s(year_s) +
                s(landuse_intensity_s) +
                s(mean_winter_temperature_s) +
                s(precipitation_sum_growing_preriod_s) +
                s(grassland_cover_1000_s) +
                s(arable_cover_1000_s) +
                ti(mean_winter_temperature_s,
                   precipitation_sum_growing_preriod_s) +
                ti(year_s, landuse_intensity_s) +
                ti(year_s, grassland_cover_1000_s) +
                ti(year_s, arable_cover_1000_s) +
                s(year_f, bs = "re") +
                s(year_s, plot_id, bs = "fs", k = 5), # <-- here
              family = nb(),
              method = "REML",
              control = ctrl,
              data = seibold)
  )

AIC(m_cov, m_cov2)

##
smooths(m_cov2)
y2y <- smooth_estimates(m_cov2, "s(year_f)")

y2y %>%
  ggplot(aes(x = year_f, y = est)) +
  geom_pointrange(aes(ymin = est - se,
                      ymax = est + se)) +
  labs(x = NULL)

draw(m_cov2, select = "s(year_s,plot_id)")

# year, day-of-year

# knots <- list(day_of_year = c(0.5, 366.5)) # for daily data, ;pcatopm pf tje cyclic spline join
# gam(y ~ s(year) + s(day_of_year, bs = "cc") , # assumes we have data across every day of the year, cyclic spine so joins december-jan 
# knots = knots)

# knots <- list(month = c(0.5, 12.5)) # location of the cyclic spline join
# gam(y ~ s(year) + s(month, bs = "cc", k = 12), knots = knots) # 12 is the maximum

# gam(y ~ te(year, month, bs = c("tp", "cc")), knots = knots)
# gam(y ~ s(year) + s(month, bs = "cc", k = 12) + # I want the overall seasonal effect
#       ti(year, month, bs = c("tp", "cc")), knots = knots) # how does the seasonal effect vary with the trend (or vice versa) (rather than fixed seasonal effect)


