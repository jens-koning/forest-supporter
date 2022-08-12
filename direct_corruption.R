### Corruption test script 
library(dplyr)
install.packages("countrycode")
library(countrycode)

#change countrycode
Vdem_corruption$country_id <- countrycode(Vdem_corruption$country_id, origin = "vdem", destination = "un")
Vdem_corruption <- rename(Vdem_corruption, "UN_ccode" = "country_id")
Full_new <- left_join(Full_new, Vdem_corruption, by = c("UN_ccode", "year"))
Full_new_tropical <- left_join(Full_new_tropical, Vdem_corruption, by = c("UN_ccode", "year"))


#"v2xnp_client", "v2x_corr", "v2x_execorr"
require(zoo)
library(stargazer)

#lagging corruption
Full_new_corr <- Full_new %>% 
  group_by(FID) %>%
  mutate(v2x_corr.l = lag(v2x_corr), v2x_execorr.l = lag(v2x_execorr))

Full_new_tropical_corr <- Full_new_tropical %>% 
  group_by(FID) %>%
  mutate(v2x_corr.l = lag(v2x_corr), v2x_execorr.l = lag(v2x_execorr))


#corruption and log.GDP - global
library(lfe)
m2_rs_cli_ln <- felm(forest.diff ~ forest.l + log(PCGDP.l) + PCGDP.change.l + Ag.emp.l + Pop.growth.l + v2x_execorr.l + Ruralsup|FID + year|0|un + year, data=Full_new_corr) 
m2_us_cli_ln <- felm(forest.diff ~ forest.l + log(PCGDP.l) + PCGDP.change.l + Ag.emp.l + Pop.growth.l + v2x_execorr.l + Urbansup|FID + year|0|un + year, data=Full_new_corr) 
m3_ae_cli_ln <- felm(forest.diff ~ forest.l + log(PCGDP.l) + PCGDP.change.l + Ag.emp.l + Pop.growth.l + v2x_execorr.l + Agrelite_mimp|FID + year|0|un + year, data=Full_new_corr)
m3_be_cli_ln <- felm(forest.diff ~ forest.l + log(PCGDP.l) + PCGDP.change.l + Ag.emp.l + Pop.growth.l + v2x_execorr.l + Busielite_mimp|FID + year|0|un + year, data=Full_new_corr)

stargazer(m2_rs_cli_ln, m2_us_cli_ln, m3_ae_cli_ln, m3_be_cli_ln,  type = "text") #Busielite_mimp
stargazer(m2_rs_cli, type = "text")

#corruption and log.GDP - tropical
m2_rs_cli_ln_t <- felm(forest.diff ~ forest.l + log(PCGDP.l) + PCGDP.change.l + Pop.growth.l + v2x_corr.l + v2x_execorr.l + Ruralsup|FID + year|0|un + year, data=Full_new_tropical_corr) 
m2_us_cli_ln_t <- felm(forest.diff ~ forest.l + log(PCGDP.l) + PCGDP.change.l + Pop.growth.l + v2x_corr.l + v2x_execorr.l + Urbansup|FID + year|0|un + year, data=Full_new_tropical_corr) 
m3_ae_cli_ln_t <- felm(forest.diff ~ forest.l + log(PCGDP.l) + PCGDP.change.l + Pop.growth.l + v2x_corr.l + v2x_execorr.l + Agrelite_mimp|FID + year|0|un + year, data=Full_new_tropical_corr)
m3_be_cli_ln_t <- felm(forest.diff ~ forest.l + log(PCGDP.l) + PCGDP.change.l + Pop.growth.l + v2x_corr.l + v2x_execorr.l + Busielite_mimp|FID + year|0|un + year, data=Full_new_tropical_corr)

stargazer(m2_rs_cli_ln_t, m2_us_cli_ln_t, m3_ae_cli_ln_t, m3_be_cli_ln_t) #Busielite_mimp



#environmental EKC hypothesis. 
m2_rs_cli_ln_EKC <- felm(forest.diff ~ forest.l + log(PCGDP.l)^2 + PCGDP.change.l + Pop.growth.l + Ruralsup|FID + year|0|un + year, data=Full_new_corr) 
m2_us_cli_ln_EKC <- felm(forest.diff ~ forest.l + log(PCGDP.l)^2 + PCGDP.change.l + Pop.growth.l + Urbansup|FID + year|0|un + year, data=Full_new_corr) 
m3_ae_cli_ln_EKC <- felm(forest.diff ~ forest.l + log(PCGDP.l)^2 + PCGDP.change.l + Pop.growth.l + Agrelite_mimp|FID + year|0|un + year, data=Full_new_corr)
m3_be_cli_ln_EKC <- felm(forest.diff ~ forest.l + log(PCGDP.l)^2 + PCGDP.change.l + Pop.growth.l + Busielite_mimp|FID + year|0|un + year, data=Full_new_corr)



##interaction - global
m2_rs_cli_ln_int <- felm(forest.diff ~ forest.l + log(PCGDP.l) + PCGDP.change.l + Pop.growth.l + v2x_corr.l + v2x_execorr.l + democracy_BX*Ruralsup|FID + year|0|un + year, data=Full_new_corr) 
m2_us_cli_ln_int <- felm(forest.diff ~ forest.l + log(PCGDP.l) + PCGDP.change.l + Pop.growth.l + v2x_corr.l + v2x_execorr.l + democracy_BX*Urbansup|FID + year|0|un + year, data=Full_new_corr) 
m3_ae_cli_ln_int <- felm(forest.diff ~ forest.l + log(PCGDP.l) + PCGDP.change.l + Pop.growth.l + v2x_corr.l + v2x_execorr.l + democracy_BX*Agrelite_mimp|FID + year|0|un + year, data=Full_new_corr)
m3_be_cli_ln_int <- felm(forest.diff ~ forest.l + log(PCGDP.l) + PCGDP.change.l + Pop.growth.l + v2x_corr.l + v2x_execorr.l + democracy_BX*Busielite_mimp|FID + year|0|un + year, data=Full_new_corr)

stargazer(m2_rs_cli_ln_int , m2_us_cli_ln_int , m3_ae_cli_ln_int , m3_be_cli_ln_int) #Busielite_mimp

##interaction - tropical
m2_rs_cli_ln_int_t <- felm(forest.diff ~ forest.l + log(PCGDP.l) + PCGDP.change.l + Pop.growth.l + v2x_corr.l + v2x_execorr.l + democracy_BX*Ruralsup|FID + year|0|un + year, data=Full_new_tropical_corr) 
m2_us_cli_ln_int_t <- felm(forest.diff ~ forest.l + log(PCGDP.l) + PCGDP.change.l + Pop.growth.l + v2x_corr.l + v2x_execorr.l + democracy_BX*Urbansup|FID + year|0|un + year, data=Full_new_tropical_corr) 
m3_ae_cli_ln_int_t <- felm(forest.diff ~ forest.l + log(PCGDP.l) + PCGDP.change.l + Pop.growth.l + v2x_corr.l + v2x_execorr.l + democracy_BX*Agrelite_mimp|FID + year|0|un + year, data=Full_new_tropical_corr)
m3_be_cli_ln_int_t <- felm(forest.diff ~ forest.l + log(PCGDP.l) + PCGDP.change.l + Pop.growth.l + v2x_corr.l + v2x_execorr.l + democracy_BX*Busielite_mimp|FID + year|0|un + year, data=Full_new_tropical_corr)

stargazer(m2_rs_cli_ln_int_t , m2_us_cli_ln_int_t , m3_ae_cli_ln_int_t , m3_be_cli_ln_int_t) #Busielite_mimp



#agriculture
m2_rs_cli <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ag.emp.l + Ruralsup|FID + year|0|un + year, data=Full_new) 
m2_us_cli <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ag.emp.l + Urbansup|FID + year|0|un + year, data=Full_new) 
m3_ae_cli <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ag.emp.l + Agrelite_mimp|FID + year|0|un + year, data=Full_new)
m3_be_cli <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ag.emp.l + Busielite_mimp|FID + year|0|un + year, data=Full_new)

