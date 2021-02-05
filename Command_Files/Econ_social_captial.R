##############################################################################
### Project: Social capital and the success of economic sanctions
### Updates: 7th January 2021
###    logs: 06/12/2017
###          02/09/2016
### Data Collection
### - Variables - IV and DV
### Analysis
### Author: 
### - Jayoung Hur
### - Sanghoon Park
### - Haena Kim 
### - Taehee Whang
### 
##############################################################################

pacman::p_load(ezpickr, readxl, lubridate, broom, countrycode, psych,
               stargazer, tidyverse)


supply <- Hmisc::bezier(x = c(4, 6, 8),
                        y = c(8, 6, 4)) %>%
  as_data_frame()
demand <- Hmisc::bezier(c(4, 6, 8),
                        c(4, 6, 8)) %>%
  as_data_frame()

plot_labels <- tibble(label = c("Opposition effect", "Rally effect"),
                          x = c(8, 8),
                          y = c(4, 8))

ggplot(mapping = aes(x = x, y = y)) +
  geom_path(data = supply, color = "black", size = 1) +
  geom_path(data = demand, color = "black", size = 1) +
  ggrepel::geom_label_repel(
    data = plot_labels,
                aes(x = x, y = y, label = label, family = "serif"),
    fill = "white", direction='y', position = position_dodge(0.5)) + 
  xlim(3, 9) + ylim(3, 9) + 
  labs(x = "Social capital", y = "Pr(Sanction Success)") + 
  theme_classic() +
  coord_equal() + 
  theme(axis.title = element_text(family = "serif"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(
          arrow = arrow(angle = 30, length = unit(0.1,"inches"), type = "closed")))
ggsave("Documents/figures/equilibrium.png", width = 5, height = 4, dpi = 400)



### Main Dataset: merge_data.dta
### Import from STATA
getwd()


### WVS data
load("Datasets/WVS_TimeSeries_R_v1_5")

main_df <- pick("Analytic_Files/merge_data.dta")

### Ordering necessary variables

main_df <- main_df %>% dplyr::select(
  identifier, idyearA, idyearA_destring, sanctionoutcome,
  trust, m_polparty1, m_profassociation1, m_humanrights1, 
  m_religious1, c_government1, c_parliament1, c_polparty1,
  c_justicelegalsyscourts1, c_armedforces1, c_churches1, c_police1,
  contig, distance, lntarget_gdppc_gle, salience_dummy2, alliance2, 
  targetdem
)

### V-dem Dataset

vdem <- pick("Datasets/Country_Year_V-Dem_Core_STATA_v10/V-Dem-CY-Core-v10.dta")

vdem <- vdem %>% dplyr::select(
  COWcode, year, v2x_polyarchy, v2x_libdem, v2x_partipdem, v2x_delibdem,
  v2x_egaldem, v2x_freexp_altinf, v2x_suffr, v2x_civlib, v2csreprss,
  v2csprtcpt, v2mecenefm, v2mecenefi, v2meslfcen, v2xnp_client, v2x_corr
) %>% drop_na(COWcode)

vdem <- vdem %>% mutate(
  idyearA = paste0(year, COWcode),
  idyearA_destring = as.numeric(idyearA)
  )

######################################################
###  Main Data set + V-dem + Quality of Governance ###
######################################################

merge_df <- left_join(
  main_df, vdem, by = c("idyearA_destring")
)
names(merge_df)
merge_df <- merge_df %>% dplyr::select(
  identifier, idyearA=idyearA.x, idyearA_destring, 
  sanctionoutcome, trust, m_polparty1, m_profassociation1, m_humanrights1, m_religious1, 
  c_government1, c_parliament1, c_polparty1, c_justicelegalsyscourts1, 
  c_armedforces1, c_churches1, c_police1, contig, distance, lntarget_gdppc_gle,
  salience_dummy2, alliance2, targetdem, v2x_polyarchy, v2x_libdem, 
  v2x_partipdem, v2x_delibdem, v2x_egaldem, v2x_freexp_altinf, v2x_suffr,
  v2csreprss, v2csprtcpt, v2mecenefm, v2mecenefi, v2meslfcen, v2xnp_client,
  v2x_civlib, v2x_corr
)

### Import QoG
qog <- pick("Datasets/qog_std_ts_jan20.dta")
qog_cowna <- qog %>% drop_na(ccodecow)
qog_cowna <- qog_cowna %>% mutate(
  idyearA = paste0(year, ccodecow),
  idyearA_destring = as.numeric(idyearA)
) %>% dplyr::select(
  idyearA, idyearA_destring, ccodecow, year, wdi_internet, gle_trade, wdi_trade
  )


######################################################
###  Main Data set + V-dem + Quality of Governance ###
######################################################

merge_df <- left_join(merge_df, qog_cowna, by = c("idyearA_destring"))


### Targeted Sanction Data 

TSC <- pick("Datasets/TSC_Quantitative_Dataset%5B1%5D.sav")


TSC_v2 <- TSC %>% mutate(
  startday = day(v29),
  startmonth = month(v29),
  startyear = year(v29),
  endday = day(v30),
  endmonth = month(v30),
  endyear = year(v30)
) %>% select(Observations, startyear, v16, v18, v20, v48, v49, v50, v52,
             v53, v57, v60, v61, v65, v66, v70, v73, v74, v78, v79, v83, 
             v86, v87, v106, v107, v109, v111, v112, v117, v122, v132, 
             v144, v282, v287, v292)


TSC_v2 <- TSC_v2 %>% separate(Observations, c("country", "epi")) %>% 
  select(-epi)

TSC_v2$country <- countrycode(TSC_v2$country, 
                              origin = "country.name", destination = "cown")

TSC_v2 <- TSC_v2 %>% mutate(
  country = case_when(
    country=="Côte" ~ 437,
    country=="FRY" ~ 345,
    country=="CAR" ~ 482,
    T ~ country
  )) %>% drop_na(country) %>%
  mutate(
    year = startyear,
    idyearA = paste0(startyear, country),
    idyearA_destring = as.numeric(idyearA))

############################################################
###  Main Data set + V-dem + Quality of Governance + TSC ###
############################################################

merge_df <- left_join(merge_df, TSC_v2, by = c("idyearA_destring"))

############################################
### Analysis: Replicate from KimH_prj.do ###      
############################################

### Descriptive statistics: All observations 

sample <- merge_df %>% 
  dplyr::select(
    sanctionoutcome, trust,
    m_polparty1, m_profassociation1, m_humanrights1, m_religious1,
    c_government1, c_parliament1, c_polparty1, c_justicelegalsyscourts1,
    c_armedforces1, c_churches1, c_police1, contig, distance, lntarget_gdppc_gle,
    salience_dummy2, alliance2, v2x_polyarchy, v2x_libdem,v2x_partipdem, v2x_freexp_altinf, gle_trade,
    wdi_trade, targetdem, wdi_internet
  )

sample <- sample %>% mutate(
  lndistance = log(distance + 1))
descriptive1 <- sample %>% dplyr::select(
  sanctionoutcome, trust,
  contig, lndistance, lntarget_gdppc_gle, salience_dummy2, alliance2, v2x_polyarchy
) %>% drop_na() %>% describe() %>% dplyr::select(
  c(2, 3, 4, 8, 9)
)
descriptive1
descriptive2 <- sample %>% dplyr::select(
  sanctionoutcome, 
  c_justicelegalsyscourts1,
  contig, lndistance, lntarget_gdppc_gle, salience_dummy2, alliance2, v2x_polyarchy
) %>% drop_na() %>% describe() %>% dplyr::select(
  c(2, 3, 4, 8, 9)
)
descriptive2

descriptive %>% drop_na(sanctionoutcome) %>% describe() %>% dplyr::select(
  c(2, 3, 4, 8, 9)
)
describe(descriptive)[,c(2, 3, 4, 8, 9)]

psych::describe(drop_na(test))[,c(2, 3, 4, 8, 9)]


model <- glm(sanctionoutcome ~ 
               m_polparty1 +  
                contig + distance +
                lntarget_gdppc_gle + salience_dummy2 + 
                alliance2 + v2x_polyarchy, 
              family = binomial(link="probit"), data = sample)
stargazer::stargazer(model, type = "text")


model1 <- glm(sanctionoutcome ~ 
                   trust +  
                   contig + lndistance +
                   lntarget_gdppc_gle + salience_dummy2 + 
                   alliance2 + v2x_polyarchy, 
              family = binomial(link="probit"), data = sample)

stargazer::stargazer(model1, type = "text")
model2 <- glm(sanctionoutcome ~ 
                m_polparty1 +  
                contig + lndistance +
                lntarget_gdppc_gle + salience_dummy2 + 
                alliance2 + v2x_polyarchy,
              family = binomial(link="probit"), data = sample)
stargazer::stargazer(model2, type = "text")
model3 <- glm(sanctionoutcome ~ 
                m_profassociation1 + 
                contig + lndistance +
                lntarget_gdppc_gle + salience_dummy2 + 
                alliance2 + v2x_polyarchy,
              family = binomial(link="probit"), data = sample)
stargazer::stargazer(model1, model2, model3, type="text",
                     keep.stat = c("n","ll"))
print(lmtest::coeftest(model1, vcov=vcovHC(model1, "HC1")), digits = 2)
print(lmtest::coeftest(model2, vcov=vcovHC(model2, "HC1")), digits = 1)
print(lmtest::coeftest(model3, vcov=vcovHC(model3, "HC1")), digits = 1)

library("sandwich")
library("lmtest")
install.packages("blorr")



1-(model3$deviance/model3$null.deviance)


####################################################################################
## PREDICTED PROBABILITIES (for key variables; here I show the results for trust) ##
####################################################################################
library(mvtnorm)

beta_trust <- rmvnorm(n=1000, mean = coef(model1), sigma=vcov(model1))
trust <- cbind(
  1, seq(4.9, 64.8, by = 3.5),
  mean(sample$contig, na.rm = T), mean(sample$lndistance, na.rm = T),
  mean(sample$lntarget_gdppc_gle, na.rm = T), mean(sample$salience_dummy2, na.rm = T),
  mean(sample$alliance2, na.rm = T), mean(sample$v2x_polyarchy, na.rm = T)
)


pr.trust <- t(pnorm(trust %*% t(beta_trust)))
trust_m <- apply(pr.trust, 2, mean)
trust_se <- apply(pr.trust, 2, quantile, c(0.05, 0.95))

trust.df <- data.frame(M=trust_m,
                           Low=trust_se[1,],
                           High= trust_se[2,],
                           Trust = seq(4.9, 64.8, by = 3.5))
library(extrafont)
loadfonts(device = "win")

plot1 <- trust.df %>% 
  ggplot(aes(x=Trust, y=M)) + 
  geom_ribbon(aes(y = M, ymin = Low, ymax = High), 
              alpha = 0, 
              color = futurevisions::futurevisions("mars")[1],
              size = 1.5)  + 
  geom_line(color = futurevisions::futurevisions("mars")[1], size = 1) + 
  geom_point(color = futurevisions::futurevisions("mars")[1], 
             size = 4) +
  scale_x_continuous(breaks = seq(5, 65, by = 14)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
    labels = scales::percent) + 
  theme(axis.text.x  = element_text(vjust=0.5)) + 
  labs(x="Trust", y="Pr(Sanction success)",
       caption = "") +
  geom_hline(yintercept=0, color="red", linetype = "dashed") + 
  theme_bw() + 
  theme(
    text=element_text(family="serif"),
    axis.title = element_text(family="serif"))
plot1
ggsave("Documents/figures/figure2.png",plot1, width = 6, height = 3.5, dpi = 500)

beta_polpar <- rmvnorm(n=1000, mean = coef(model2), sigma=vcov(model2))

polpar <- cbind(
  1, seq(0, 50, by = 5),
  mean(sample$contig, na.rm = T), mean(sample$lndistance, na.rm = T),
  mean(sample$lntarget_gdppc_gle, na.rm = T), mean(sample$salience_dummy2, na.rm = T),
  mean(sample$alliance2, na.rm = T), mean(sample$v2x_polyarchy, na.rm = T)
)


pr.polpar <- t(pnorm(polpar %*% t(beta_polpar)))
polpar_m <- apply(pr.polpar, 2, mean)
polpar_se <- apply(pr.polpar, 2, quantile, c(0.05, 0.95))

polpar.df <- data.frame(M=polpar_m,
                       Low=polpar_se[1,],
                       High= polpar_se[2,],
                       Polpar = seq(0, 50, by = 5))

plot2 <- polpar.df %>% 
  ggplot(aes(x= Polpar, y=M)) + 
  geom_ribbon(aes(y = M, ymin = Low, ymax = High), 
              alpha = 0, 
              color = futurevisions::futurevisions("mars")[2],
              size = 1.5)  + 
  geom_line(color = futurevisions::futurevisions("mars")[2], size = 1) + 
  geom_point(color = futurevisions::futurevisions("mars")[2], 
             size = 4) +
  scale_x_continuous(breaks = seq(0, 50, by = 10)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) + 
  theme(axis.text.x  = element_text(vjust=0.5)) + 
  labs(x="Membership:\nPolitical party", y="Pr(Sanction success)",
       caption = "") +
  geom_hline(yintercept=0, color="red", linetype = "dashed") + 
  theme_bw() + 
  theme(
    text=element_text(family="serif"),
    axis.title = element_text(family="serif"))


summary(sample$m_profassociation1)

beta_prof <- rmvnorm(n=1000, mean = coef(model3), sigma=vcov(model3))

prof <- cbind(
  1, seq(0, 35, by = 4),
  mean(sample$contig, na.rm = T), mean(sample$lndistance, na.rm = T),
  mean(sample$lntarget_gdppc_gle, na.rm = T), mean(sample$salience_dummy2, na.rm = T),
  mean(sample$alliance2, na.rm = T), mean(sample$v2x_polyarchy, na.rm = T)
)


pr.prof <- t(pnorm(prof %*% t(beta_prof)))
prof_m <- apply(pr.prof, 2, mean)
prof_se <- apply(pr.prof, 2, quantile, c(0.05, 0.95))

prof.df <- data.frame(M=prof_m,
                        Low=prof_se[1,],
                        High= prof_se[2,],
                        prof = seq(0, 35, by = 4))

plot3 <- prof.df %>% 
  ggplot(aes(x= prof, y=M)) + 
  geom_ribbon(aes(y = M, ymin = Low, ymax = High), 
              alpha = 0, 
              color = futurevisions::futurevisions("mars")[3],
              size = 1.5)  + 
  geom_line(color = futurevisions::futurevisions("mars")[3], size = 1) + 
  geom_point(color = futurevisions::futurevisions("mars")[3], 
             size = 4) +
  scale_x_continuous(breaks = seq(0, 35, by = 5)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) + 
  theme(axis.text.x  = element_text(vjust=0.5)) + 
  labs(x="Membership:\nProfessional association", y = "",
       caption = "") +
  geom_hline(yintercept=0, color="red", linetype = "dashed") + 
  theme_bw() + 
  theme(
    text=element_text(family="serif"),
    axis.title = element_text(family="serif"))

plot2 + plot3 + patchwork::plot_layout(ncol = 2)

ggsave("Documents/figures/figure3.png", width = 7, height = 3.5, dpi = 500)


model4 <- glm(sanctionoutcome ~ 
                c_polparty1 + 
                contig + lndistance +
                lntarget_gdppc_gle + salience_dummy2 + 
                alliance2 + v2x_polyarchy,
              family = binomial(link="probit"), data = sample)


model5 <- glm(sanctionoutcome ~ 
                c_government1 + 
                contig + lndistance +
                lntarget_gdppc_gle + salience_dummy2 + 
                alliance2 + v2x_polyarchy,
              family = binomial(link="probit"), data = sample)

model6 <- glm(sanctionoutcome ~ 
                c_parliament1 +
                contig + lndistance +
                lntarget_gdppc_gle + salience_dummy2 + 
                alliance2 + v2x_polyarchy,
              family = binomial(link="probit"), data = sample)

model7 <- 
  glm(sanctionoutcome ~ 
                c_justicelegalsyscourts1 +  
                contig + distance +
                lntarget_gdppc_gle + salience_dummy2 + 
                alliance2 + targetdem,
              family = binomial(link="probit"), data = sample)

stargazer::stargazer(model4, model5, model6, model7, type = "text")
print(lmtest::coeftest(model4, vcov=vcovHC(model4, "HC1")), digits = 1)
print(lmtest::coeftest(model5, vcov=vcovHC(model5, "HC1")), digits = 1)
print(lmtest::coeftest(model6, vcov=vcovHC(model6, "HC1")), digits = 1)
print(lmtest::coeftest(model7, vcov=vcovHC(model7, "HC1")), digits = 1)
library("sandwich")
library("lmtest")
install.packages("blorr")



1-(model4$deviance/model4$null.deviance)
1-(model5$deviance/model5$null.deviance)
1-(model6$deviance/model6$null.deviance)
1-(model7$deviance/model7$null.deviance)
table1 <- list(
  model1, model2, model3
)
summary(model1)
library(sandwich)
library(modelsummary)
library(tidyverse)
msummary(table1, conf_map = label,
             fmt = 3,
             estimate  = c("{estimate} ({std.error}){stars}"),
             vcov = "robust",
             statistic = NULL,
             coef_omit = "Intercept",
             conf_level = 0.90)

label <- c("Trust" = "trust",
           "contig" = "Contig",
           "lndistance" = "Distance",
           "lntarget_gdppc_gle" = "lnGDPpc",
           "salience_dummy2" = "Salience",
           "alliance2" = "Alliance",
           "v2x_polyarchy" = "Target Democracy",
           "m_polparty1" = "Membership: Political Party",
           "m_profassociation1" = "Membership: Prof. Association")
sample %>% dplyr::select(c_polparty1, c_government1, c_parliament1,
                         c_justicelegalsyscourts1) %>% summary()

beta.m4 <- rmvnorm(n=1000, mean = coef(model4), sigma=vcov(model4))

conf.m4 <- cbind(
  1, seq(0, 55, by = 11),
  mean(sample$contig, na.rm = T), mean(sample$lndistance, na.rm = T),
  mean(sample$lntarget_gdppc_gle, na.rm = T), mean(sample$salience_dummy2, na.rm = T),
  mean(sample$alliance2, na.rm = T), mean(sample$v2x_polyarchy, na.rm = T)
)


pr.m4 <- t(pnorm(conf.m4 %*% t(beta.m4)))
m4_m <- apply(pr.m4, 2, mean)
m4_se <- apply(pr.m4, 2, quantile, c(0.05, 0.95))

m4.df <- data.frame(M=m4_m,
                      Low=m4_se[1,],
                      High= m4_se[2,],
                      conf =  seq(0, 55, by = 11))

plot4 <- m4.df %>% 
  ggplot(aes(x= conf, y=M)) + 
  geom_ribbon(aes(y = M, ymin = Low, ymax = High), 
              alpha = 0, 
              color = futurevisions::futurevisions("jupiter")[1],
              size = 1.5)  + 
  geom_line(color = futurevisions::futurevisions("jupiter")[1], size = 1) + 
  geom_point(color = futurevisions::futurevisions("jupiter")[1], 
             size = 4) +
  scale_x_continuous(breaks = seq(0, 55, by = 11)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) + 
  theme(axis.text.x  = element_text(vjust=0.5)) + 
  labs(x="Confidence:\nPolitical party", y = "Pr(Sanction success",
       caption = "") +
  geom_hline(yintercept=0, color="red", linetype = "dashed") + 
  theme_bw() + 
  theme(
    text=element_text(family="serif"),
    axis.title = element_text(family="serif"))


beta.m5 <- rmvnorm(n=1000, mean = coef(model5), sigma=vcov(model5))

conf.m5 <- cbind(
  1, seq(1, 80, by = 16),
  mean(sample$contig, na.rm = T), mean(sample$lndistance, na.rm = T),
  mean(sample$lntarget_gdppc_gle, na.rm = T), mean(sample$salience_dummy2, na.rm = T),
  mean(sample$alliance2, na.rm = T), mean(sample$v2x_polyarchy, na.rm = T)
)


pr.m5 <- t(pnorm(conf.m5 %*% t(beta.m5)))
m5_m <- apply(pr.m5, 2, mean)
m5_se <- apply(pr.m5, 2, quantile, c(0.05, 0.95))

m5.df <- data.frame(M=m5_m,
                    Low=m5_se[1,],
                    High= m5_se[2,],
                    conf =  seq(1, 80, by = 16))

plot5 <- m5.df %>% 
  ggplot(aes(x= conf, y=M)) + 
  geom_ribbon(aes(y = M, ymin = Low, ymax = High), 
              alpha = 0, 
              color = futurevisions::futurevisions("jupiter")[2],
              size = 1.5)  + 
  geom_line(color = futurevisions::futurevisions("jupiter")[2], size = 1) + 
  geom_point(color = futurevisions::futurevisions("jupiter")[2], 
             size = 4) +
  scale_x_continuous(breaks = seq(1, 80, by = 16)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) + 
  theme(axis.text.x  = element_text(vjust=0.5)) + 
  labs(x="Confidence:\nGovernment", y = "",
       caption = "") +
  geom_hline(yintercept=0, color="red", linetype = "dashed") + 
  theme_bw() + 
  theme(
    text=element_text(family="serif"),
    axis.title = element_text(family="serif"))


beta.m6 <- rmvnorm(n=1000, mean = coef(model6), sigma=vcov(model6))

conf.m6 <- cbind(
  1, seq(0, 75, by = 15),
  mean(sample$contig, na.rm = T), mean(sample$lndistance, na.rm = T),
  mean(sample$lntarget_gdppc_gle, na.rm = T), mean(sample$salience_dummy2, na.rm = T),
  mean(sample$alliance2, na.rm = T), mean(sample$v2x_polyarchy, na.rm = T)
)


pr.m6 <- t(pnorm(conf.m6 %*% t(beta.m6)))
m6_m <- apply(pr.m6, 2, mean)
m6_se <- apply(pr.m6, 2, quantile, c(0.05, 0.95))

m6.df <- data.frame(M=m6_m,
                    Low=m6_se[1,],
                    High= m6_se[2,],
                    conf =  seq(0, 75, by = 15))

plot6 <- m6.df %>% 
  ggplot(aes(x= conf, y=M)) + 
  geom_ribbon(aes(y = M, ymin = Low, ymax = High), 
              alpha = 0, 
              color = futurevisions::futurevisions("jupiter")[3],
              size = 1.5)  + 
  geom_line(color = futurevisions::futurevisions("jupiter")[3], size = 1) + 
  geom_point(color = futurevisions::futurevisions("jupiter")[3], 
             size = 4) +
  scale_x_continuous(breaks = seq(0, 75, by = 15)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) + 
  theme(axis.text.x  = element_text(vjust=0.5)) + 
  labs(x="Confidence:\nParliament", y = "Pr(Sanction success)",
       caption = "") +
  geom_hline(yintercept=0, color="red", linetype = "dashed") + 
  theme_bw() + 
  theme(
    text=element_text(family="serif"),
    axis.title = element_text(family="serif"))



beta.m7 <- rmvnorm(n=1000, mean = coef(model7), sigma=vcov(model7))

conf.m7 <- cbind(
  1, seq(0, 40, by = 10),
  mean(sample$contig, na.rm = T), mean(sample$lndistance, na.rm = T),
  mean(sample$lntarget_gdppc_gle, na.rm = T), mean(sample$salience_dummy2, na.rm = T),
  mean(sample$alliance2, na.rm = T), mean(sample$v2x_polyarchy, na.rm = T)
)


pr.m7 <- t(pnorm(conf.m7 %*% t(beta.m7)))
m7_m <- apply(pr.m7, 2, mean)
m7_se <- apply(pr.m7, 2, quantile, c(0.05, 0.95))

m7.df <- data.frame(M=m7_m,
                    Low=m7_se[1,],
                    High= m7_se[2,],
                    conf =  seq(0, 40, by = 10))

plot7 <- m7.df %>% 
  ggplot(aes(x= conf, y=M)) + 
  geom_ribbon(aes(y = M, ymin = Low, ymax = High), 
              alpha = 0, 
              color = futurevisions::futurevisions("jupiter")[4],
              size = 1.5)  + 
  geom_line(color = futurevisions::futurevisions("jupiter")[4], size = 1) + 
  geom_point(color = futurevisions::futurevisions("jupiter")[4], 
             size = 4) +
  scale_x_continuous(breaks = seq(0, 40, by = 10)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) + 
  theme(axis.text.x  = element_text(vjust=0.5)) + 
  labs(x="Confidence:\nCourts", y = "",
       caption = "") +
  geom_hline(yintercept=0, color="red", linetype = "dashed") + 
  theme_bw() + 
  theme(
    text=element_text(family="serif"),
    axis.title = element_text(family="serif"))


plot.sum <- plot4 + plot5 + plot6 + plot7 + patchwork::plot_layout(ncol = 2)
ggsave("Documents/figures/figure4.png", plot.sum, width = 7, height = 7, dpi = 450)
m4.df[1,1]-m4.df[4,1]
m5.df[1,1]-m5.df[4,1]
m6.df[1,1]-m6.df[4,1]
m7.df[1,1]-m7.df[4,1]


library(modelsummary)
modelsummary(model1, "markdown")









sum sanctionoutcome trust ///
  m_polparty1 m_profassociation1 m_humanrights1 m_religious1 /// 
  c_government1 c_parliament1 c_polparty1 c_justicelegalsyscourts1 c_armedforces1 c_churches1 c_police1 ///
  contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem

corr sanctionoutcome trust ///
  m_polparty1 m_profassociation1 m_humanrights1 m_religious1 /// 
  c_government1 c_parliament1 c_polparty1 c_justicelegalsyscourts1 c_armedforces1 c_churches1 c_police1 ///
  contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem

* Descriptive statistics: Observations in our results 
sum sanctionoutcome trust ///
  m_polparty1 m_profassociation1 m_humanrights1 m_religious1 /// 
  c_government1 c_parliament1 c_polparty1 c_justicelegalsyscourts1 c_armedforces1 c_churches1 c_police1 ///
  contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem ///
  if sanctionoutcome != . & trust != . & contig != . & distance != . & lntarget_gdppc_gle != . & salience_dummy2 != . & alliance2 != . & targetdem != .

corr sanctionoutcome trust ///
  m_polparty1 m_profassociation1 m_humanrights1 m_religious1 /// 
  c_government1 c_parliament1 c_polparty1 c_justicelegalsyscourts1 c_armedforces1 c_churches1 c_police1 ///
  contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem ///
  if sanctionoutcome != . & contig != . & distance != . & lntarget_gdppc_gle != . & salience_dummy2 != . & alliance2 != . & targetdem != .

* IV
sum $membership1 if sanctionoutcome != . & contig != . & distance != . & lntarget_gdppc_gle != . & salience_dummy2 != . & alliance2 != . & targetdem != .
sum $membership2 if sanctionoutcome != . & contig != . & distance != . & lntarget_gdppc_gle != . & salience_dummy2 != . & alliance2 != . & targetdem != .

* DV
tab finaloutcome if sanctionoutcome != . & contig != . & distance != . & lntarget_gdppc_gle != . & salience_dummy2 != . & alliance2 != . & targetdem != .
tab sanctionoutcome if sanctionoutcome != . & contig != . & distance != . & lntarget_gdppc_gle != . & salience_dummy2 != . & alliance2 != . & targetdem != .


***********
  ** Trust **
  ***********
  
  *probit sanctionoutcome trust salience_dummy2 alliance2 targetdem , robust /*political model*/
  *probit sanctionoutcome trust contig distance lntarget_gdppc_gle , robust /*economic model*/
  probit sanctionoutcome trust contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem , robust /*full model*/
  est store reg1
* possible regressor: targettradeopen

margins, at(trust=(4.9(3)59.4)) 


****************
  ** Membership **
  ****************
  
  probit sanctionoutcome m_polparty1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
est store reg2

margins, at(m_polparty1=(0(3)45.5)) 

probit sanctionoutcome m_profassociation1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
est store reg3

margins, at(m_profassociation1=(0(1.5)25.7))

*probit sanctionoutcome m_humanrights1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
*probit sanctionoutcome m_religious1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust


esttab reg1 reg2 reg3 using "C:\Users\user\Dropbox\KimH_prj\data\turst_membership", ///
  replace modelwidth(10) addnote ("Probit Models Using Robust Standard Error. DV = Sanction Outcome.")  ///
  title("Effect of Social Capital (Trust and Membership) on the Success of Econoimc Sanctions") starlevels (* 0.10 ** 0.05)  ///
  numbers("Model ")  mlabels("Trust" "Membership: Political Party" "Membership: Prof. Association")  ///
  coeflabels(Trust "Trust" m_profassociation1 "Membership: Prof. Association" m_polparty1 "Membership: Political Party" ///
               contig "Contiguity" distance "Distance" lntarget_gdppc_gle "Target ln(GDPPC)" /// 
               salience_dummy2 "Issue Salience" alliance2 "Alliance" targetdem "Target Democracy" _constant "Constant")  ///
  legend cells(b(star fmt(2)) se(par)) stats(N pr2, labels  ("Observations" "Pseudo R-squared")) label varwidth(30) compress 


****************
  ** Confidence **
  ****************
  
  probit sanctionoutcome c_polparty1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
est store reg4

margins, at(c_polparty1=(0.9(3)31.1))

probit sanctionoutcome c_government1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
est store reg5

margins, at(c_government1=(1.9(3)54.5))

probit sanctionoutcome c_parliament1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
est store reg6

margins, at(c_parliament1=(0.9(3)46.1))

probit sanctionoutcome c_justicelegalsyscourts1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
est store reg7

margins, at(c_justicelegalsyscourts1=(6.7(2)37.3))

*probit sanctionoutcome c_armedforces1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
*probit sanctionoutcome c_churches1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
*probit sanctionoutcome c_police1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust


esttab reg4 reg5 reg6 reg7 using "C:\Users\user\Dropbox\KimH_prj\data\confidence", ///
  replace modelwidth(10) addnote ("Probit Models Using Robust Standard Error. DV = Sanction Outcome.")  ///
  title("Effect of Social Capital (Confidence) on the Success of Econoimc Sanctions") starlevels (* 0.10 ** 0.05)  ///
  numbers("Model ")  mlabels("Confidence: Political Party" "Confidence: Government" "Confidence: Parliament" "Confidence: Courts")  ///
  coeflabels(c_polparty1 "Confidence in Political Party" c_government1 "Confidence in Government" c_parliament1 "Confidence in Parliament" c_justicelegalsyscourts1 "Confidence in Courts" ///
               contig "Contiguity" distance "Distance" lntarget_gdppc_gle "Target ln(GDPPC)" /// 
               salience_dummy2 "Issue Salience" alliance2 "Alliance" targetdem "Target Democracy" _constant "Constant")  ///
  legend cells(b(star fmt(2)) se(par)) stats(N pr2, labels  ("Observations" "Pseudo R-squared")) label varwidth(30) compress 


** END











