
library(tidyverse)
library(dplyr)
install.packages("countrycode")
library(countrycode)


## Change variables country codes using "countrycodes" package
VDem_RegSup_All$country_id <- countrycode(VDem_RegSup_All$country_id, origin = "vdem", destination = "un")
View(VDem_RegSup_All)

## Rename forest data
Forest_Sanford <- full

## Rename UN M49 country code variable
Forest_Sanford$UN_ccode <- Forest_Sanford$un # adding additional variable for merging
VDem_RegSup_All <- rename(VDem_RegSup_All, "UN_ccode" = "country_id")

## Merge data set using dplyr
Forest_RegSup_Merged_Full <- left_join(Forest_Sanford, VDem_RegSup_All, by = c("UN_ccode", "year"))


## Filter based on different social groups to check obs. 

