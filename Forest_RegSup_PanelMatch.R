### Matching Using TSCS Matching by Imai, Kim and Wang (2021)
install.packages("PanelMatch")
library(PanelMatch)
library(countrycode)

##preparing full data set - coverting into date.frame & converting numeric variable to integer
str(Full_new)
Full_new_df <- Full_new
Full_new_df <- as.data.frame(Full_new_df)
Full_new_df$year <- as.integer(Full_new_df$year)

### Preparing country-yr data - coverting into date.frame & converting numeric variable to integer

#Rename UN M49 country code variable
country_full$UN_ccode <- country_full$un 
VDem_RegSup_All <- rename(VDem_RegSup_All, "UN_ccode" = "country_id")

#Merge data set using dplyr
country_full_new <- left_join(country_full, VDem_RegSup_All, by = c("UN_ccode", "year"))

#Create dummys for country-yr data set

#Adding support group dummies
country_new <- country_full_new %>%
  mutate(Ruralsup = case_when(v2regsupgroups_11 >= 0.5 | v2regsupgroups_12 >= 0.5 ~ 1)) %>%
  mutate(Urbansup = case_when(v2regsupgroups_9 >= 0.5 | v2regsupgroups_10 >= 0.5 ~ 1)) %>%
  mutate(Ruralloc = case_when(v2regsuploc == 3 ~ 1)) %>%
  mutate(Urbanloc = case_when(v2regsuploc == c(1,2) ~ 1)) %>%
  mutate(Agrelite_mimp = case_when(v2regimpgroup == 1 ~ 1)) %>%
  mutate(Busielite_mimp = case_when(v2regimpgroup == 3 ~ 1))

#replacing NA with 0
country_new$Ruralsup[is.na(country_new$Ruralsup)] <- 0 
country_new$Urbansup[is.na(country_new$Urbansup)] <- 0 
country_new$Ruralloc[is.na(country_new$Ruralloc)] <- 0 
country_new$Urbanloc[is.na(country_new$Urbanloc)] <- 0 
country_new$Agrelite_mimp[is.na(country_new$Agrelite_mimp)] <- 0 
country_new$Busielite_mimp[is.na(country_new$Busielite_mimp)] <- 0

#coverting to data.frame
country_new_df <- as.data.frame(country_new)
country_new_df$year <- as.integer(country_new_df$year)

##Visualization of Treatment

#Rural location as treatment (cell-yr)
picRuralloc <- DisplayTreatment(unit.id = "FID",
                                time.id = "year", 
                                xlab = "Year (1982-2016)", ylab = "Cell Code (Entire World)",
                                treatment = "Ruralloc", data = Full_new_df,
                                hide.x.axis.label = TRUE, hide.y.axis.label = TRUE,
                                dense.plot = TRUE
                                )


#Rural Supporters as treatment (cell-yr)
picRuralsup <- DisplayTreatment(unit.id = "FID",
                   time.id = "year", 
                   xlab = "Year (1982-2016)", ylab = "Cell Code (Entire World)",
                   treatment = "Ruralsup", data = Full_new_df,
                   hide.x.axis.label = TRUE, hide.y.axis.label = TRUE,
                   dense.plot = TRUE
                   )


#Rural support treatment (country-yr)
picRuralsup_country <- DisplayTreatment(unit.id = "UN_ccode",
                                time.id = "year", 
                                xlab = "Year (1982-2016)", ylab = "Country Code (Entire World)",
                                treatment = "Ruralsup", data = country_new_df
)

#Rural location treatment (country-yr)
picRuralsup_country <- DisplayTreatment(unit.id = "UN_ccode",
                                        time.id = "year", 
                                        xlab = "Year (1982-2016)", ylab = "Country Code (Entire World)",
                                        treatment = "Ruralloc", data = country_new_df, legend.labels = c("not treated", "treated")
)

#Urban support treatment (country-yr)
picUrbansup_country <- DisplayTreatment(unit.id = "UN_ccode",
                                        time.id = "year", 
                                        xlab = "Year (1982-2016)", ylab = "Country Code (Entire World)",
                                        treatment = "Urbansup", data = country_new_df
)

#Business-elite treatment (country-yr)
picBusimimp_country <- DisplayTreatment(unit.id = "UN_ccode",
                                        time.id = "year", 
                                        xlab = "Year (1982-2016)", ylab = "Country Code (Entire World)",
                                        treatment = "Busielite_mimp", data = country_new_df
)

#Agrarian-elite treatment (country-yr)
picAgrmimp_country <- DisplayTreatment(unit.id = "UN_ccode",
                                        time.id = "year", 
                                        xlab = "Year (1982-2016)", ylab = "Country Code (Entire World)",
                                        treatment = "Agrelite_mimp", data = country_new_df
)



#Democracy as treatment (country-ry)
picDemocracy_country <- DisplayTreatment(unit.id = "UN_ccode",
                                        time.id = "year", 
                                        xlab = "Year (1982-2016)", ylab = "Country Code (Entire World)",
                                        treatment = "democracy_BX", data = country_new_df
)


#Ad-hoc: Combine
library(ggpubr)
ggarrange(picRuralsup, picRuralloc + rremove("x.text"),
          ncol = 2)


##Preparing cell-yr data set for panel matching (cell-yr)
Full_new_df_PMr <- select(Full_new_df, c("x", "y", "year", "FID", "forest", "forest.l", "forest.diff", "democracy_BX", "PCGDP.l", "Pop.growth.l","Ruralsup", "Urbansup",
                                        "Agrelite_mimp", "Busielite_mimp", "v2x_polyarchy"))

Full_new_df_PMr$Ruralsup <- factor(Full_new_df_PMr$Ruralsup, levels = c(0,1)) #make treatment dichotomous
Full_new_df_PMr$year <- Full_new_df_PMr$year
levels(Full_new_df_PMr$Ruralsup)
as.numeric(Full_new_df_PMr)

##Preparing country-yr data set for panel matching (country-yr)
library(tidyverse)
country_new_df_PMr <- select(country_new_df, c("year", "UN_ccode", "forest", "forest.l", "forest.diff", "democracy_BX", "PCGDP.l","Pop.growth.l","Ruralsup", "Urbansup",
                                            "Agrelite_mimp", "Busielite_mimp", "v2x_polyarchy"))

country_new_df_PMr$Ruralsup <- factor(country_new_df_PMr$Ruralsup, levels = c(0,1)) #make treatment dichotomous
country_new_df_PMr$UN_ccode <- as.integer(country_new_df_PMr$UN_ccode)
levels(country_new_df_PMr$Ruralsup)
str(country_new_df_PMr)

##add autocracy to country year set
Add_BMRautocracy_cy <- country_new_df_PMr %>%
  mutate(autocracy_BX = ifelse(democracy_BX == 1, 0,1))
country_new_df_PMr <- Add_BMRautocracy_cy







##USING COUNTRY-YR DATA SET

# THIS Matching on democracy w/mahalanobis covariates (country-yr) # 3-yr lag
PM.results.ma.dem <- PanelMatch(lag = 5, time.id = "year", unit.id = "UN_ccode",
                                treatment = "autocracy_BX", refinement.method = "mahalanobis",
                                data = country_new_df_PMr[, c("year", "UN_ccode", "democracy_BX", "autocracy_BX", "forest", "forest.diff",
                                                              "PCGDP.l", "Pop.growth.l")],
                                covs.formula = ~ PCGDP.l + Pop.growth.l, 
                                match.missing = TRUE,
                                size.match = 10,
                                qoi = "att", outcome.var = "forest.diff",
                                lead = 0:4, forbid.treatment.reversal = FALSE,
                                use.diagonal.variance.matrix = TRUE)
mset.dem.ma <- PM.results.ma.dem$att
PM.results.ma.dem$att
summary(PM.results.ma.dem$att)
DisplayTreatment(unit.id = "UN_ccode",
                 time.id = "year", legend.position = "none",
                 xlab = "year", ylab = "Country Code",
                 treatment = "democracy_BX", data = country_new_df_PMr,
                 matched.set = PM.results.ma.dem, # this way I highlight the particular set
                 show.set.only = TRUE)

PE.results.ma.dem <- PanelEstimate(sets = PM.results.ma.dem, data = country_new_df_PMr, 
                                   se.method = "bootstrap", 
                                   number.iterations = 1000,
                                   confidence.level = .95)
print(mset.dem.ma)
plot(PE.results.ma.dem)
plot(PM.results.ma.dem$att)
PM.results.no.dem
summary(mset.dem.ma)
class(PM.results.no.dem)
PE.results.ma.dem[["estimates"]]
summary(PE.results.ma.dem)


# THIS Matching on democracy w/mahalanobis covariates (country-yr) # 10-yr lag
PM.results.ma.dem.5 <- PanelMatch(lag = 10, time.id = "year", unit.id = "UN_ccode",
                                treatment = "democracy_BX", refinement.method = "mahalanobis",
                                data = country_new_df_PMr[, c("year", "UN_ccode", "democracy_BX", "forest", "forest.diff",
                                                              "PCGDP.l", "Pop.growth.l", "Busielite_mimp")],
                                covs.formula = ~ PCGDP.l + Pop.growth.l ,
                                match.missing = TRUE,
                                size.match = 10,
                                qoi = "att", outcome.var = "forest.diff",
                                lead = 0:5, forbid.treatment.reversal = FALSE,
                                use.diagonal.variance.matrix = TRUE)
mset.dem.ma.5 <- PM.results.ma.dem.5$att
PM.results.ma.dem.5$att
DisplayTreatment(unit.id = "UN_ccode",
                 time.id = "year", legend.position = "none",
                 xlab = "year", ylab = "Country Code",
                 treatment = "democracy_BX", data = country_new_df_PMr,
                 matched.set = PM.results.ma.dem.5, # this way I highlight the particular set
                 show.set.only = TRUE)

PE.results.ma.dem.5 <- PanelEstimate(sets = PM.results.ma.dem.5, data = country_new_df_PMr, 
                                   se.method = "bootstrap", 
                                   number.iterations = 1000,
                                   confidence.level = .95)

PE.results.ma.dem.5.modr <- PanelEstimate(sets = PM.results.ma.dem.5, data = country_new_df_PMr, moderator = "Busielite_mimp")


PE.results.ma.dem.5[["estimates"]]
summary(PE.results.ma.dem.5)
PE.results.ma.dem.5.modr[["estimates"]]

plot(PE.results.ma.dem.5.modr)

plot(PE.results.ma.dem.5)

# (test) matching on democracy w/mahalanobis covariates (country-yr) # 10-yr lag
PM.results.ma.dem.10 <- PanelMatch(lag = 10, time.id = "year", unit.id = "UN_ccode",
                                  treatment = "democracy_BX", refinement.method = "mahalanobis",
                                  data = country_new_df_PMr[, c("year", "UN_ccode", "democracy_BX", "forest", "forest.diff",
                                                                "PCGDP.l", "Pop.growth.l")],
                                  covs.formula = ~ PCGDP.l + Pop.growth.l,
                                  match.missing = TRUE,
                                  size.match = 25,
                                  qoi = "att", outcome.var = "forest.diff",
                                  lead = 0:10, forbid.treatment.reversal = FALSE,
                                  use.diagonal.variance.matrix = TRUE)
mset.dem.ma.10 <- PM.results.ma.dem.10$att
PM.results.ma.dem.10$att
DisplayTreatment(unit.id = "UN_ccode",
                 time.id = "year", legend.position = "none",
                 xlab = "year", ylab = "Country Code",
                 treatment = "democracy_BX", data = country_new_df_PMr,
                 matched.set = PM.results.ma.dem.10, # this way I highlight the particular set
                 show.set.only = TRUE)

PE.results.ma.dem.10 <- PanelEstimate(sets = PM.results.ma.dem.10, data = country_new_df_PMr, 
                                     se.method = "bootstrap", 
                                     number.iterations = 5000,
                                     confidence.level = .95)

PE.results.ma.dem.10[["estimates"]]
print(PM.results.ma.dem.10$att
)

summary(PE.results.ma.dem.10)
plot(PE.results.ma.dem.10)
summary.matched.set

# (THE FIRST ONE IN THESIS) matching on business elties w/mahalanobis covariates (country-yr) # 3-yr lag
PM.results.busi.3 <- PanelMatch(lag = 3, time.id = "year", unit.id = "UN_ccode",
                                   treatment = "Busielite_mimp", refinement.method = "mahalanobis",
                                   data = country_new_df_PMr[, c("year", "UN_ccode", "democracy_BX", "forest", "forest.diff",
                                                                 "PCGDP.l", "Pop.growth.l", "Busielite_mimp")],
                                   covs.formula = ~ PCGDP.l + Pop.growth.l,
                                   match.missing = TRUE,
                                   size.match = 10,
                                   qoi = "att", outcome.var = "forest.diff",
                                   lead = 0:3, forbid.treatment.reversal = FALSE,
                                   use.diagonal.variance.matrix = TRUE)
mset.busi.3 <- PM.results.busi.3$att
PM.results.busi.3$att
DisplayTreatment(unit.id = "UN_ccode",
                 time.id = "year",
                 xlab = "year", ylab = "Country Code",
                 treatment = "Busielite_mimp", data = country_new_df_PMr,
                 matched.set = mset.busi.3[1], # this way I highlight the particular set
                 show.set.only = TRUE)

PE.results.busi.3.95 <- PanelEstimate(sets = PM.results.busi.3, data = country_new_df_PMr, 
                                      se.method = "bootstrap", 
                                      number.iterations = 1000,
                                      confidence.level = .95)


plot(mset.busi.3)
PE.results.busi.3.95[["estimates"]]
plot(PE.results.busi.3.95)
summary(PE.results.busi.3.95, verbose=TRUE)

# (THE SECOND ONE IN THESIS) matching on business elties w/mahalanobis covariates (country-yr) # 5-yr lag
PM.results.busi.5 <- PanelMatch(lag = 5, time.id = "year", unit.id = "UN_ccode",
                                treatment = "Busielite_mimp", refinement.method = "mahalanobis",
                                data = country_new_df_PMr[, c("year", "UN_ccode", "democracy_BX", "forest", "forest.diff",
                                                              "PCGDP.l", "Pop.growth.l", "Busielite_mimp")],
                                covs.formula = ~ PCGDP.l + Pop.growth.l,
                                match.missing = TRUE,
                                size.match = 10,
                                qoi = "att", outcome.var = "forest.diff",
                                lead = 0:3, forbid.treatment.reversal = FALSE,
                                use.diagonal.variance.matrix = TRUE)
mset.busi.5 <- PM.results.busi.5$att
PM.results.busi.5$att
DisplayTreatment(unit.id = "UN_ccode",
                 time.id = "year",
                 xlab = "year", ylab = "Country Code",
                 treatment = "Busielite_mimp", data = country_new_df_PMr,
                 matched.set = mset.busi.5, # this way I highlight the particular set
                 show.set.only = TRUE)

PE.results.busi.5 <- PanelEstimate(sets = PM.results.busi.5, data = country_new_df_PMr, 
                                   se.method = "bootstrap", 
                                   number.iterations = 1000,
                                   confidence.level = .90)

PE.results.busi.5[["estimates"]]
plot(PE.results.busi.5)
summary(PE.results.busi.5)

plot(mset.busi.5)



# (IN THESIS) matching on urban groups w/mahalanobis covariates (country-yr) # 3-yr lag
PM.results.urban.3 <- PanelMatch(lag = 3, time.id = "year", unit.id = "UN_ccode",
                                 treatment = "Urbansup", refinement.method = "mahalanobis",
                                 data = country_new_df_PMr[, c("year", "UN_ccode", "democracy_BX", "forest", "forest.diff",
                                                               "PCGDP.l", "Pop.growth.l", "Urbansup")],
                                 covs.formula = ~ PCGDP.l + Pop.growth.l,
                                 match.missing = TRUE,
                                 size.match = 10,
                                 qoi = "att", outcome.var = "forest.diff",
                                 lead = 0:3, forbid.treatment.reversal = FALSE,
                                 use.diagonal.variance.matrix = TRUE)
mset.urban.3 <- PM.results.urban.5$att
PM.results.urban.3$att
DisplayTreatment(unit.id = "UN_ccode",
                 time.id = "year", legend.position = "none",
                 xlab = "year", ylab = "Country Code",
                 treatment = "democracy_BX", data = country_new_df_PMr,
                 matched.set = PM.results.ma.dem.10, # this way I highlight the particular set
                 show.set.only = TRUE)

PE.results.urban.3 <- PanelEstimate(sets = PM.results.urban.3, data = country_new_df_PMr, 
                                    se.method = "bootstrap", 
                                    number.iterations = 1000,
                                    confidence.level = .95)

PE.results.urban.3[["estimates"]]
plot(PE.results.urban.3)
summary(PE.results.urban.3)

# (IN THESIS) matching on urban groups w/mahalanobis covariates (country-yr) # 5-yr lag
PM.results.urban.5 <- PanelMatch(lag = 5, time.id = "year", unit.id = "UN_ccode",
                                treatment = "Urbansup", refinement.method = "mahalanobis",
                                data = country_new_df_PMr[, c("year", "UN_ccode", "democracy_BX", "forest", "forest.diff",
                                                              "PCGDP.l", "Pop.growth.l", "Urbansup")],
                                covs.formula = ~ PCGDP.l + Pop.growth.l,
                                match.missing = TRUE,
                                size.match = 10,
                                qoi = "att", outcome.var = "forest.diff",
                                lead = 0:3, forbid.treatment.reversal = FALSE,
                                use.diagonal.variance.matrix = TRUE)
@ <- PM.results.urban.5$att
PM.results.urban.5$att
DisplayTreatment(unit.id = "UN_ccode",
                 time.id = "year", legend.position = "none",
                 xlab = "year", ylab = "Country Code",
                 treatment = "democracy_BX", data = country_new_df_PMr,
                 matched.set = PM.results.ma.dem.10, # this way I highlight the particular set
                 show.set.only = TRUE)

PE.results.urban.5 <- PanelEstimate(sets = PM.results.urban.5, data = country_new_df_PMr, 
                                   se.method = "bootstrap", 
                                   number.iterations = 1000,
                                   confidence.level = .95)

PE.results.urban.5[["estimates"]]
plot(PE.results.urban.5)
summary(PE.results.urban.5)


####Covariate balance


get_covariate_balance(PM.results.busi.3$att, #3yrs lag
                      data = country_new_df_PMr,
                      use.equal.weights = TRUE,
                      covariates = c("PCGDP.l", "Pop.growth.l", "forest.diff"),
                      plot = TRUE,
                      ylim = c(-1, 1))

get_covariate_balance(PM.results.busi.3.ps$att, #3yrs lag
                      data = country_new_df_PMr,
                      use.equal.weights = TRUE,
                      covariates = c("PCGDP.l", "Pop.growth.l", "forest.diff"),
                      plot = TRUE,
                      ylim = c(-1, 1))

ggsave("", plot = lastplot)



get_covariate_balance(PM.results.busi.5$att, #5yr lag 
                      data = country_new_df_PMr,
                      covariates = c("PCGDP.l", "Pop.growth.l", "forest.diff"),
                      plot = TRUE,
                      ylim = c(-1, 1))


get_covariate_balance(PM.results.urban.3$att, #3yr lag 
                      data = country_new_df_PMr,
                      covariates = c("PCGDP.l", "Pop.growth.l", "forest.diff"),
                      plot = TRUE,
                      ylim = c(-1, 1))


get_covariate_balance(PM.results.urban.5$att, #5yr lag 
                      data = country_new_df_PMr,
                      covariates = c("PCGDP.l", "Pop.growth.l", "forest.diff"),
                      plot = TRUE,
                      ylim = c(-1, 1))




get_covariate_balance(PM.results.ma.dem.5$att, #10yr lag
                      data = country_new_df_PMr,
                      covariates = c("PCGDP.l", "Pop.growth.l","forest.diff", "democracy_BX"),
                      plot = TRUE,
                      ylim = c(-1, 1))

get_covariate_balance(PM.results.ma.dem$att, #5yr lag
                      data = country_new_df_PMr,
                      covariates = c("PCGDP.l", "Pop.growth.l","forest.diff"),
                      plot = TRUE,
                      ylim = c(-1, 1))



### TEST WITH PROPENSITY SCORE WEIGHTING


# (PS WEIGHTING) matching on business elties w/propensity score weighting (country-yr) # 3-yr lag
PM.results.busi.3.ps <- PanelMatch(lag = 3, time.id = "year", unit.id = "UN_ccode",
                                   treatment = "Busielite_mimp", refinement.method = "ps.weight",
                                   data = country_new_df_PMr[, c("year", "UN_ccode", "democracy_BX", "forest", "forest.diff",
                                                                 "PCGDP.l", "Pop.growth.l", "Busielite_mimp")],
                                   covs.formula = ~ PCGDP.l + Pop.growth.l,
                                   match.missing = TRUE,
                                   size.match = 5,
                                   qoi = "att", outcome.var = "forest.diff",
                                   lead = 0:3, forbid.treatment.reversal = FALSE,
                                   use.diagonal.variance.matrix = TRUE)
mset.busi.3.ps <- PM.results.busi.3.ps$att
PM.results.busi.3.ps$att
DisplayTreatment(unit.id = "UN_ccode",
                 time.id = "year", legend.position = "none",
                 xlab = "year", ylab = "Country Code",
                 treatment = "Busielite_mimp", data = country_new_df_PMr,
                 matched.set = PM.results.busi.3.ps$att, # this way I highlight the particular set
                 show.set.only = TRUE)

PM.results.busi.3.ps <- PanelEstimate(sets = PM.results.busi.3.ps, data = country_new_df_PMr, 
                                      se.method = "bootstrap", 
                                      number.iterations = 500,
                                      confidence.level = .95)


plot(PM.results.busi.3.ps)

# (PS WEIGHTING) matching on business elties w/propensity score weighting (country-yr) # 5-yr lag
PM.results.busi.5.ps <- PanelMatch(lag = 5, time.id = "year", unit.id = "UN_ccode",
                                   treatment = "Busielite_mimp", refinement.method = "ps.weight",
                                   data = country_new_df_PMr[, c("year", "UN_ccode", "democracy_BX", "forest", "forest.diff",
                                                                 "PCGDP.l", "Pop.growth.l", "Busielite_mimp")],
                                   covs.formula = ~ PCGDP.l + Pop.growth.l,
                                   match.missing = TRUE,
                                   size.match = 5,
                                   qoi = "att", outcome.var = "forest.diff",
                                   lead = 0:3, forbid.treatment.reversal = FALSE,
                                   use.diagonal.variance.matrix = TRUE)
mset.busi.5.ps <- PM.results.busi.5.ps$att
PM.results.busi.5.ps$att
DisplayTreatment(unit.id = "UN_ccode",
                 time.id = "year", legend.position = "none",
                 xlab = "year", ylab = "Country Code",
                 treatment = "Busielite_mimp", data = country_new_df_PMr,
                 matched.set = PM.results.busi.5.ps$att, # this way I highlight the particular set
                 show.set.only = TRUE)

PM.results.busi.5.ps <- PanelEstimate(sets = PM.results.busi.5.ps, data = country_new_df_PMr, 
                                      se.method = "bootstrap", 
                                      number.iterations = 500,
                                      confidence.level = .95)


plot(PM.results.busi.5.ps)



