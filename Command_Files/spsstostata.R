library(ezpickr)
library(tidyverse)
library(lubridate)
library(broom)
TSC <- pick("TSC_Quantitative_Dataset%5B1%5D.sav")
df <- TSC %>% mutate(
  startday = day(v29),
  startmonth = month(v29),
  startyear = year(v29),
  endday = day(v30),
  endmonth = month(v30),
  endyear = year(v30)
)
df.sum <- df %>% select(Observations, startyear, v16, v18, v20, v48, v49, v50, v52,
                    v53, v57, v60, v61, v65, v66, v70, v73, v74, v78, v79, v83, 
                    v86, v87, v106, v107, v109, v111, v112, v117, v122, v132, 
                    v144, v282, v287, v292)

df.sum <- df.sum %>% separate(Observations, c("country", "epi")) %>% select(-epi)
library(countrycode)

df.sum$country <- countrycode(df.sum$country, 
                              origin = "country.name", destination = "cown")

df.sum <- df.sum %>% mutate(
  country = case_when(
    country=="Côte" ~ 437L,
    country=="FRY" ~ 345L,
    country=="CAR" ~ 482L,
    T ~ country
  )) %>% drop_na(country)
df.sum <- df.sum %>% mutate(
  year = startyear,
  idyearA = paste0(startyear, country),
  idyearA_destring = as.numeric(idyearA))

foreign::write.dta(df.sum, "tsc.dta")
