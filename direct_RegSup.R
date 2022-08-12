library(tidyverse)
library(dplyr)
install.packages("countrycode")
library(countrycode)


### Change variables country codes using "countrycodes" package
VDem_RegSup_All$country_id <- countrycode(VDem_RegSup_All$country_id, origin = "vdem", destination = "un")
View(VDem_RegSup_All)

### Rename forest data
Forest_Sanford <- full

### Rename UN M49 country code variable
Forest_Sanford$UN_ccode <- Forest_Sanford$un # adding additional variable for merging
VDem_RegSup_All <- rename(VDem_RegSup_All, "UN_ccode" = "country_id")

### Merge data set using dplyr
Forest_RegSup_Merged_Full <- left_join(Forest_Sanford, VDem_RegSup_All, by = c("UN_ccode", "year"))


###Filter based on different social groups to check obs. ("Rural" and "Urban" categories)

## LOCATION
#Rural
Forest_Ruralloc <- filter(Forest_RegSup_Merged_Full, v2regsuploc == 3) #3 = supporters are located in rural areas
#Urban
Forest_Urbanloc <- filter(Forest_RegSup_Merged_Full, v2regsuploc == c(1,2)) #1,2 = supporters are located in capital/urban areas

## GROUPS IN COALITION

## Support groups
#Rural
Forest_Ruralsup <- filter(Forest_RegSup_Merged_Full, v2regsupgroups_11 >= 0.5 | v2regsupgroups_12 >= 0.5)
Forest_Ruralsupall <- filter(Forest_RegSup_Merged_Full, v2regsupgroups_11 >= 0.5 | v2regsupgroups_12 >= 0.5 | v2regsupgroups_1 >= 0.5) 
Forest_RuralsupAgr <- filter(Forest_RegSup_Merged_Full, v2regsupgroups_1 >= 0.5)
#Urban
Forest_Urbansup <- filter(Forest_RegSup_Merged_Full, v2regsupgroups_9 >= 0.5 | v2regsupgroups_10 >= 0.5)
Forest_Urbansupall <- filter(Forest_RegSup_Merged_Full, v2regsupgroups_9 >= 0.5 | v2regsupgroups_10 >= 0.5 | v2regsupgroups_3 >= 0.5)
Forest_UrbansupBusi <- filter(Forest_RegSup_Merged_Full, v2regsupgroups_3 >= 0.5)

## MOST IMPORTANT GROUP IN COALITION
#Rural
Forest_Ruralimpall <- filter(Forest_RegSup_Merged_Full, v2regimpgroup == c(11,12,1) ) #rural identity
Forest_Agrelite <- filter(Forest_RegSup_Merged_Full, v2regimpgroup == 1 ) #agrarian elite identity
#Urban
Forest_Urbanimpall <- filter(Forest_RegSup_Merged_Full, v2regimpgroup == c(9,10,3) ) #urban identity all 
Forest_Urbanimp <- filter(Forest_RegSup_Merged_Full, v2regimpgroup == c(9,10,) ) #urban identity
Forest_Busielite <- filter(Forest_RegSup_Merged_Full, v2regimpgroup == 3 ) #business elite identity

### Creating dummy-variables for main data set 
library(dplyr)

#Ruralsup
Merged_full <- Forest_RegSup_Merged_Full %>%
  mutate(Ruralsup = case_when(v2regsupgroups_11 >= 0.5 | v2regsupgroups_12 >= 0.5 ~ 1))
Merged_full$Ruralsup[is.na(Merged_full$Ruralsup)] <- 0
#Urbansup
Merged_full_1 <- Merged_full %>%
  mutate(Urbansup = case_when(v2regsupgroups_9 >= 0.5 | v2regsupgroups_10 >= 0.5 ~ 1))
Merged_full_1$Urbansup[is.na(Merged_full_1$Urbansup)] <- 0
#Ruralloc
Merged_full_2 <- Merged_full_1 %>%
  mutate(Ruralloc = case_when(v2regsuploc == 3 ~ 1)) 
Merged_full_2$Ruralloc[is.na(Merged_full_2$Ruralloc)] <- 0
#Urbanloc
Merged_full_3 <- Merged_full_2 %>%
  mutate(Urbanloc = case_when(v2regsuploc == c(1,2) ~ 1)) 
Merged_full_3$Urbanloc[is.na(Merged_full_3$Urbanloc)] <- 0

## Extreme cases

#Agrelitesup_mimp
Merged_full_4 <- Merged_full_3 %>%
  mutate(Agrelite_mimp = case_when(v2regimpgroup == 1 ~ 1)) 
Merged_full_4$Agrelite_mimp[is.na(Merged_full_4$Agrelite_mimp)] <- 0
#Busielitesup_mimp
Merged_full_5 <- Merged_full_4 %>%
  mutate(Busielite_mimp = case_when(v2regimpgroup == 3 ~ 1)) 
Merged_full_5$Busielite_mimp[is.na(Merged_full_5$Busielite_mimp)] <- 0


#Dummy for Robustness checks and TSCS Matching

#Ruralsup.75
Merged_full_6 <- Merged_full_5 %>%
  mutate(Ruralsup.75 = case_when(v2regsupgroups_11 >= 0.75 | v2regsupgroups_12 >= 0.75 ~ 1))
Merged_full_6$Ruralsup.75[is.na(Merged_full_6$Ruralsup.75)] <- 0

#Urbansup.75
Merged_full_7 <- Merged_full_6 %>%
  mutate(Urbansup.75 = case_when(v2regsupgroups_9 >= 0.75 | v2regsupgroups_10 >= 0.75 ~ 1))
Merged_full_7$Urbansup.75[is.na(Merged_full_7$Urbansup.75)] <- 0

#Ruralsup.33
Merged_full_8 <- Merged_full_7 %>%
  mutate(Ruralsup.33 = case_when(v2regsupgroups_11 >= 0.33 | v2regsupgroups_12 >= 0.33 ~ 1))
Merged_full_8$Ruralsup.33[is.na(Merged_full_8$Ruralsup.33)] <- 0

#Urbansup.33
Merged_full_9 <- Merged_full_8 %>%
  mutate(Urbansup.33 = case_when(v2regsupgroups_9 >= 0.33 | v2regsupgroups_10 >= 0.33 ~ 1))
Merged_full_9$Urbansup.33[is.na(Merged_full_9$Urbansup.33)] <- 0


Full_new <- Merged_full_9

#Remove excess data sets
rm(Merged_full_1, Merged_full_2, Merged_full_3, Merged_full_4, Merged_full_5, Merged_full_6, Merged_full_7,
   Merged_full_8, Merged_full_9)
rm(Forest_Urbansupall, Forest_Ruralsupallm, Forest_Ruralloc, Forest_Urbanloc, Forest_Ruralimpall,
   Forest_Urbanimpall, Forest_RuralsupAgr, Forest_UrbansupBusi, Forest_Ruralsupall)

### Democratic transitions
require(dotwhisker)
library(ggplot2)
require(broom)
library(lfe)

#Modified model with interaction variables
install.packages("tidyverse")
library(tidyverse)
to_include = c("democracy_BX","pr","democracy_BX:pr","Ag.emp.l","democracy_BX:Ag.emp.l", "Ruralsup", "Ruralloc", "Urbansup", "Urbanloc", "Agrelite_mimp", "Busielite_mimp")


install.packages("stargazer")
library(stargazer)

#Sanford model (full data set)
m1_org <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|FID + year|0|un + year, data=Full_new)

# models (all)
m1_rl <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralloc + democracy_BX|FID + year|0|un + year, data=Full_new) 
m1_ul <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Urbanloc + democracy_BX|FID + year|0|un + year, data=Full_new) 
m2_rs <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralsup + democracy_BX|FID + year|0|un + year, data=Full_new)  
m2_us <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Urbansup + democracy_BX|FID + year|0|un + year, data=Full_new)  
m3_ae <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Agrelite_mimp + democracy_BX|FID + year|0|un + year, data=Full_new)
m3_be <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Busielite_mimp + democracy_BX|FID + year|0|un + year, data=Full_new)  
m3_all_f <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralsup + Urbansup + Agrelite_mimp + Busielite_mimp + democracy_BX|FID + year|0|un + year, data=Full_new) 
m3_all_ft <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralsup + Urbansup + Agrelite_mimp + Busielite_mimp + democracy_BX|FID + year|0|un + year, data=Full_new_tropical) 
m3_all_f_nc <- felm(forest.diff ~ Ruralsup + Urbansup + Agrelite_mimp + Busielite_mimp + democracy_BX|FID + year|0|un + year, data=Full_new) 


m3_all_f.75 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralsup.75 + Urbansup.75 + Agrelite_mimp + Busielite_mimp + democracy_BX|FID + year|0|un + year, data=Full_new) 
m3_all_f.33 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralsup.33 + Urbansup.33 + Agrelite_mimp + Busielite_mimp + democracy_BX|FID + year|0|un + year, data=Full_new) 


#Main specification / Democracy
stargazer(m3_all_f, m3_all_ft, m3_all_ni, m3_all_ni_t, type = "text", star.cutoffs = c(.05, .01, .001))
stargazer(m3_all_f, m3_all_ft, m3_all_ni, m3_all_ni_t, star.cutoffs = c(.05, .01, .001))

stargazer(m1_org, m1_rl, m1_ul, m2_rs, m2_us, m3_ae, m3_be, type = "text", star.cutoffs = c(.05, .01, .001))
stargazer(m1_org, m2_rs, m2_us, m3_ae, m3_be, type = "text", star.cutoffs = c(.05, .01, .001))
stargazer(m1_org, m2_rs, m2_us, type = "text", star.cutoffs = c(.05, .01, .001))
stargazer(m1_org, m2_rs, m2_us, m3_ae, m3_be, star.cutoffs = c(.05, .01, .001))
stargazer(m3_all, m3_all_t, type = "text", star.cutoffs = c(.05, .01, .001))
stargazer(m3_all_f.75, type = "text", star.cutoffs = c(.05, .01, .001))
stargazer(m3_all_f.33, type = "text", star.cutoffs = c(.05, .01, .001))

stargazer(m1_org, type = "text", star.cutoffs = c(.05, .01, .001))


# ^ only for tropical forests
tropical_sub=c("Tropical rainforest","Subtropical mountain system","Subtropical dry forest","Tropical dry forest","Tropical moist deciduous forest","tropical mountain system","Subtropical humid forest","Tropical shrubland")
Full_new_tropical <- Full_new %>% filter(GEZ_TERM %in% tropical_sub)

tropical_m1_org <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|FID + year|0|un + year, data=Full_new_tropical) 
tropical_m1_rl <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralloc + democracy_BX|FID + year|0|un + year, data=Full_new_tropical)
tropical_m1_ul <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Urbanloc + democracy_BX|FID + year|0|un + year, data=Full_new_tropical)
tropical_m2_rs <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralsup + democracy_BX|FID + year|0|un + year, data=Full_new_tropical) 
tropical_m2_us <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Urbansup + democracy_BX|FID + year|0|un + year, data=Full_new_tropical) 
tropical_m3_ae <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Agrelite_mimp + democracy_BX|FID + year|0|un + year, data=Full_new_tropical)  
tropical_m3_be <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Busielite_mimp + democracy_BX|FID + year|0|un + year, data=Full_new_tropical) 

stargazer(tropical_m1_org, tropical_m1_rl, tropical_m1_ul, tropical_m2_rs, tropical_m2_us, tropical_m3_ae, tropcial_m3_be, type = "text", star.cutoffs = c(.05, .01, .001))
stargazer(tropical_m1_org, tropical_m2_rs, tropical_m2_us, tropical_m3_ae, tropcial_m3_be, type = "text", star.cutoffs = c(.05, .01, .001))
stargazer(tropical_m1_org, tropical_m2_rs, tropical_m2_us, tropical_m3_ae, tropcial_m3_be, star.cutoffs = c(.05, .01, .001))

#Support groups on forest loss
m1_rl_ni <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX + Ruralloc|FID + year|0|un + year, data=Full_new) 
m1_ul_ni <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX + Urbanloc|FID + year|0|un + year, data=Full_new) 
m2_rs_ni <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralsup|FID + year|0|un + year, data=Full_new) 
m2_us_ni <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX + Urbansup|FID + year|0|un + year, data=Full_new) 
m3_ae_ni <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX + Agrelite_mimp|FID + year|0|un + year, data=Full_new)
m3_be_ni <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX +Busielite_mimp|FID + year|0|un + year, data=Full_new)
m3_all_ni <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralsup + Urbansup + Agrelite_mimp + Busielite_mimp|FID + year|0|un + year, data=Full_new) 

stargazer(m2_rs_ni, m2_us_ni, m3_ae_ni, m3_be_ni,type = "text")
stargazer(m2_rs_ni, m2_us_ni, m3_ae_ni, m3_be_ni, type = "text", star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

summary(m2_rs_ni)


m2_us_ni_non <- felm(forest.diff ~ PCGDP.l + PCGDP.change.l + Pop.growth.l + Urbansup|year|0|un + year, data=Full_new) 
stargazer(m2_us_ni_non, type = "text")

#Support groups on tropical forest loss
m1_rl_ni_t <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralloc|FID + year|0|un + year, data=Full_new_tropical) 
m1_ul_ni_t <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Urbanloc|FID + year|0|un + year, data=Full_new_tropical)
m2_rs_ni_t <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralsup|FID + year|0|un + year, data=Full_new_tropical) 
m2_us_ni_t <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Urbansup|FID + year|0|un + year, data=Full_new_tropical) 
m3_ae_ni_t <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Agrelite_mimp|FID + year|0|un + year, data=Full_new_tropical) 
m3_be_ni_t <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Busielite_mimp|FID + year|0|un + year, data=Full_new_tropical) 
m3_all_ni_t <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralsup + Urbansup + Agrelite_mimp + Busielite_mimp|FID + year|0|un + year, data=Full_new_tropical)

stargazer(m3_all_t, m2_rs_ni_t, m2_us_ni_t, m3_ae_ni_t, m3_be_ni_t, type = "text", star.cutoffs = c(.05, .01, .001))
stargazer(m3_all_t, m2_rs_ni_t, m2_us_ni_t, m3_ae_ni_t, m3_be_ni_t, star.cutoffs = c(.05, .01, .001))

m3_be_ni_special_t <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX + Busielite_mimp|FID + year|0|un + year, data=Full_new_tropical) 
m3_be_ni_special_all <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX +Busielite_mimp|FID + year|0|un + year, data=Full_new)
stargazer(m3_be_ni_special, m3_be_ni_special_all, type = "text")

m2_rs_ni_t_d <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX + Ruralsup|FID + year|0|un + year, data=Full_new_tropical) 
m2_us_ni_t_d <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX + Urbansup|FID + year|0|un + year, data=Full_new_tropical) 
m3_ae_ni_t_d <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX + Agrelite_mimp|FID + year|0|un + year, data=Full_new_tropical) 
m3_be_ni_t_d <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX + Busielite_mimp|FID + year|0|un + year, data=Full_new_tropical) 

stargazer(m2_rs_ni_t_d, m2_us_ni_t_d, m3_ae_ni_t_d, m3_be_ni_t_d)


###ROBUSTNESS CHECK - REMOVE CONTROL VARIABLES
#Support groups on forest loss, no covariates
m1_rl_ni_nc <- felm(forest.diff ~ forest.l + Ruralloc|FID + year|0|un + year, data=Full_new)
m1_ul_ni_nc <- felm(forest.diff ~ forest.l + Urbanloc|FID + year|0|un + year, data=Full_new) 
m2_rs_ni_nc <- felm(forest.diff ~ forest.l + Ruralsup|FID + year|0|un + year, data=Full_new)
m2_us_ni_nc <- felm(forest.diff ~ forest.l + Urbansup|FID + year|0|un + year, data=Full_new) 
m3_ae_ni_nc <- felm(forest.diff ~ forest.l + Agrelite_mimp|FID + year|0|un + year, data=Full_new)
m3_be_ni_nc <- felm(forest.diff ~ forest.l + Busielite_mimp|FID + year|0|un + year, data=Full_new)
  m3_all_ni_nc <- felm(forest.diff ~ forest.l + Ruralsup + Urbansup + Agrelite_mimp + Busielite_mimp|FID + year|0|un + year, data=Full_new)  

m2_rs_ni_nc_t <- felm(forest.diff ~ forest.l + Ruralsup|FID + year|0|un + year, data=Full_new_tropical)
m2_us_ni_nc_t <- felm(forest.diff ~ forest.l + Urbansup|FID + year|0|un + year, data=Full_new_tropical) 
m3_ae_ni_nc_t <- felm(forest.diff ~ forest.l + Agrelite_mimp|FID + year|0|un + year, data=Full_new_tropical)
m3_be_ni_nc_t <- felm(forest.diff ~ forest.l + Busielite_mimp|FID + year|0|un + year, data=Full_new_tropical)

stargazer(m2_rs_ni_nc, m2_us_ni_nc, m3_ae_ni_nc, m3_be_ni_nc)
stargazer(m2_rs_ni_nc_t, m2_us_ni_nc_t, m3_ae_ni_nc_t, m3_be_ni_nc_t)


###Marginal effects?
ggplot(Full_new, aes(forest, forest.diff)) +
  geom_point() 


###VIF tests for different models
library(performance)
install.packages("see")
library(see)

#mod_dem_VIF <- check_collinearity(m3_all)
#plot(mod_dem)
#check_collinearity(m3_all_f)
#check_collinearity(m3.3_ce90_all)

check_collinearity(m2_rs)
check_collinearity(m2_us)
check_collinearity(m3_ae)
check_collinearity(m3_all_ni)
check_collinearity(m3_all_ni_t)


check_collinearity(m2_rs_ni_t)
check_collinearity(m2_us_ni_t)
check_collinearity(m3_ae_ni_t)
check_collinearity(m3_be_ni_t)

check_collinearity(m2_rs_ni_t_d)
check_collinearity(m2_us_ni_t_d)
check_collinearity(m3_ae_ni_t_d)
check_collinearity(m3_be_ni_t_d)




check_collinearity(m3_all_ft)
check_collinearity(m3_be_ni_special)
check_collinearity(m3_be_ni_special_all)

## QQ-Plots

library(car)

ggplot(full, aes(sample=forest.diff)) +
  stat_qq() +
  theme_bw()

ggplot(country_full, aes(sample=forest.diff)) +
  stat_qq() +
  stat_qq_line() +
  theme_bw()



qqPlot(country_full_new$forest.diff)


## Kernel Density Estimation (KDE) Plot
install.packages("kdensity")
library(kdensity)

#cy level
density_forestchange_cy = kdensity(country_full_new$forest.diff, kernel = "gaussian", na.rm = TRUE)
plot(density_forestchange_cy)

#cell level
density_forestchange_cell = kdensity(full$forest.diff, kernel = "gaussian", na.rm = TRUE)
plot(density_forestchange_cell)

density_forestchange_cell = kdensity(full$forest.diff, kernel = "gaussian", na.rm = TRUE)



#Dotwhiskers plot
dwplot(list(m1_org, m3_all_f, m3_all_ft))
dwplot(m3_all_f_nc)

dwplot(list(m1_org, m3_all_f, m3_all_ft), 
       vline = geom_vline(
         xintercept = 0,
         colour = "grey60",
         linetype = 2),
       vars_order = c("democracy_BX", "Ruralsup", "Urbansup", "Agrelite_mimp", "Busielite_mimp"),
       model_order = c("m1_org", "m3_all_f", "m3_all_ft")
) %>% # plot line at zero _behind_coefs
  relabel_predictors(
    c(
      democracy_BX = "Democracy (BMR)",
      Ruralsup = "Rural Supporters",
      Urbansup = "Urban Supporters",
      Agrelite_mimp = "Agrarian elites (mimp)",
      Busielite_mimp = "Business elites (mimp)",
      
    )
  ) +
  theme_bw(base_size = 4) + 
  # Setting `base_size` for fit the theme
  # No need to set `base_size` in most usage
  xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0,
             colour = "grey60",
             linetype = 2) +
  ggtitle("Estimated Effect of Democracy and Support Groups")
) 


#Country-yr analysis
m1_org_cy <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|FID + year|0|un + year, data=country_new)   
m1_rl_cy <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralloc + democracy_BX|FID + year|0|un + year, data=country_new)
m1_ul_cy <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Urbanloc + democracy_BX|FID + year|0|un + year, data=country_new)  
m2_rs_cy <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralsup|FID + year|0|un + year, data=country_new) 
m2_us_cy <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Urbansup|FID + year|0|un + year, data=country_new)
m3_ae_cy <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Agrelite_mimp|FID + year|0|un + year, data=country_new)
m3_be_cy <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Busielite_mimp|FID + year|0|un + year, data=country_new) 
m3_all_cy <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralsup + Urbansup + Agrelite_mimp + Busielite_mimp + democracy_BX|FID + year|0|un + year, data=country_new)


stargazer(m2_rs_cy, m2_us_cy, m3_ae_cy, )

m2_rs_cy_nd <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralsup|FID + year|0|un + year, data=country_new) 
m2_us_cy_nd <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Urbansup|FID + year|0|un + year, data=country_new)
m3_ae_cy_nd <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Agrelite_mimp|FID + year|0|un + year, data=country_new)
m3_be_cy_nd <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Busielite_mimp|FID + year|0|un + year, data=country_new) 

stargazer(m2_rs_cy_nd, m2_us_cy_nd, m3_ae_cy_nd, m3_be_cy_nd, type = "text")

### 0.1 level included, no democracy in equation
m2_rs_01 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralsup|FID + year|0|un + year, data=Full_new) 
m2_us_01 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Urbansup|FID + year|0|un + year, data=Full_new)
m3_ae_01 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Agrelite_mimp|FID + year|0|un + year, data=Full_new)
m3_be_01 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Busielite_mimp|FID + year|0|un + year, data=Full_new) 


m2_rs_01_t <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralsup|FID + year|0|un + year, data=Full_new_tropical) 
m2_us_01_t <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Urbansup|FID + year|0|un + year, data=Full_new_tropical)
m3_ae_01_t <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Agrelite_mimp|FID + year|0|un + year, data=Full_new_tropical)
m3_be_01_t <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Busielite_mimp|FID + year|0|un + year, data=Full_new_tropical) 


stargazer(m2_rs_01, m2_us_01, m3_ae_01,m3_be_01)



stargazer(m2_rs_01_t, m2_us_01_t, m3_ae_01_t,m3_be_01_t)

library(dotwhisker)
library(broom)

#MAKING PLOT

to_include = c("forest.l", "PCGDP.l", "PCGDP.change.l", "Pop.growth.l")

#all forest
m1_rs_dw <- tidy(m2_rs_01) %>%
  filter(term == "Ruralsup") %>% mutate(model = "M1 Ru")
m1_us_dw <- tidy(m2_us_01) %>%
  filter(term == "Urbansup") %>% mutate(model = "M2 Ur")
m3_ae_dw <- tidy(m3_ae_01) %>%
  filter(term == "Agrelite_mimp") %>% mutate(model = "M3 Ae")
m3_be_dw <- tidy(m3_be_01) %>%
  filter(term == "Busielite_mimp") %>% mutate(model = "M4 Be")
#tropical forest
m1_rs_t_dw <- tidy(m2_rs_01_t) %>%
  filter(term == "Ruralsup") %>% mutate(model = "M1T Ru")
m1_us_t_dw <- tidy(m2_us_01_t) %>%
  filter(term == "Urbansup") %>% mutate(model = "M2T Ur")
m3_ae_t_dw <- tidy(m3_ae_01_t) %>%
  filter(term == "Agrelite_mimp") %>% mutate(model = "M3T Ae")
m3_be_t_dw <- tidy(m3_be_01_t) %>%
  filter(term == "Busielite_mimp") %>% mutate(model = "M4T Be")


direct_mods_dw <- rbind(m1_rs_dw, m1_us_dw, m3_ae_dw, m3_be_dw, 
            m1_rs_t_dw, m1_us_t_dw, m3_ae_t_dw, m3_be_t_dw)


direct_allforest_dw <- rbind(m1_rs_dw, m1_us_dw, m3_ae_dw, m3_be_dw)
direct_tropforest_dw <- rbind(m1_rs_t_dw, m1_us_t_dw, m3_ae_t_dw, m3_be_t_dw)

devtools::install_github(repo = "shandiya/feathers", ref = "main")
library(feathers)
library(viridis)

dwplot(direct_mods_dw,
              vline = geom_vline(
                xintercept = 0,
                colour = "grey60",
                linetype = 2),
       vars_order = c("Ruralsup", "Urbansup", "Agrelite_mimp", "Busielite_mimp"),
       model_order = c("M1 Ru", "M1T Ru", "M2 Ur", "M2T Ur", "M3 Ae", "M3T Ae", "M4 Be", "M4T Be")
      ) %>%
       relabel_predictors(
         Ruralsup = "Rural supporters",
         Urbansup = "Urban supporters",
         Agrelite_mimp = "Agrarian elites (most important)",
         Busielite_mimp = "Business elites (most important)"
       ) +
  theme_bw() + 
  xlab("Percentage Point Forest Cover Change") + ylab("") +
  scale_colour_manual(values = get_pal("rose_crowned_fruit_dove"))


ggsave("dotwhiskers_direct.png", plot = last_plot(), dpi = 200)



### Urban or rural location
m2_rl <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralloc|FID + year|0|un + year, data=Full_new) 
m2_ul <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Urbanloc|FID + year|0|un + year, data=Full_new)

stargazer(m2_rl, m2_ul, m2_rl_t, m2_ul_t,   type="text")

m2_rl_t <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralloc|FID + year|0|un + year, data=Full_new_tropical) 
m2_ul_t <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Urbanloc|FID + year|0|un + year, data=Full_new_tropical)

stargazer(m2_rl_t, m2_ul_t,  type="text")

### Remove year-fixed effects 
m2_rs_cfe <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralsup|FID|0|un + year, data=Full_new)  
m2_us_cfe <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Urbansup|FID|0|un + year, data=Full_new)  
m3_ae_cfe <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Agrelite_mimp|FID|0|un + year, data=Full_new)
m3_be_cfe <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Busielite_mimp|FID|0|un + year, data=Full_new)  
stargazer(m2_rs_cfe, m2_us_cfe, m3_ae_cfe, m3_be_cfe, type = "text")

### using forest / no first differences (using forest - similar results?)
m2_rs_forest <- felm(forest ~  PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralsup|FID + year|0|un + year, data=Full_new)  
m2_us_forest <- felm(forest ~  PCGDP.l + PCGDP.change.l + Pop.growth.l + Urbansup|FID + year|0|un + year, data=Full_new)  
m3_ae_forest <- felm(forest ~  PCGDP.l + PCGDP.change.l + Pop.growth.l + Agrelite_mimp|FID + year |0|un + year, data=Full_new)
m3_be_forest <- felm(forest ~  PCGDP.l + PCGDP.change.l + Pop.growth.l + Busielite_mimp|FID + year|0|un + year, data=Full_new)  

stargazer(m2_rs_forest, m2_us_forest, m3_ae_forest, m3_be_forest, type = "text")



##Correlation between predictors 
cor.test(Full_new$democracy_BX, Full_new$Urbansup, method = "pearson")
cor.test(Full_new$democracy_BX, Full_new$Busielite_mimp, method = "pearson")
cor.test(Full_new$Urbansup, Full_new$Busielite_mimp, method = "pearson")
cor.test(Full_new$Ruralsup, Full_new$Agrelite_mimp, method = "pearson")
cor.test(Full_new$Ruralsup, Full_new$democracy_BX, method = "pearson")


#Pairs plot for controls
pairs_data <- select(country_new, c("forest.diff", "PCGDP.l", "PCGDP.change.l", "Pop.growth.l", "democracy_BX", "v2x_polyarchy", "Ruralsup", "Urbansup", "Ruralloc", "Urbanloc", "Agrelite_mimp", "Busielite_mimp"))

pairs(pairs_data, col = "blue", labels = c("Country", "Forest Change (t-1 to t+0)", "PCGDP (t-1)", "Delta PCGDP (t-1)", "Delta Pop. growth (t-1)"), lower.panel = panel.cor,
      upper.panel = upper.panel)

#Correlation matrix
library(corrplot)
rquery.cormat(pairs_data)

cor_pairs_data <- cor(pairs_data, method = "pearson", use = "pairwise.complete.obs")
colnames(cor_pairs_data) <- c("Country", "Forest Change", "PCGDP", "PCGDP Change", "Pop. Growth", "Democracy (BMR)", "Polyarchy (V-Dem)", "Rural sup.", "Urban sup. ", "Rural loc.", "Urban loc.", "Agrarian elites", "Business elites")
rownames(cor_pairs_data) <- c("Country", "Forest Change", "PCGDP", "PCGDP Change", "Pop. Growth", "Democracy (BMR)", "Polyarchy (V-Dem)", "Rural sup.", "Urban sup. ", "Rural loc.", "Urban loc.", "Agrarian elites", "Business elites")

corrplot <- corrplot(cor_pairs_data,addCoef.col = 'black', type = 'lower')
saveas(corrplot,'corrplot.png')


##tree
install.packages("tree")
library(tree)

t1 <- tree(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ruralsup, data = Full_new)



