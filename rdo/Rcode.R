#   Social capital and the success of economic sanctions
# 
# Data Collection
# Variables - IV and DV
# Analysis
# 
# Taehee whang
# 02/09/2016
# 06/12/2017


library(foreign)
library(haven)

## Data  = sanctions + sc + KimYW_NGO data = sanctions_sc_KimYW_merge.dta
sanctions_sc_KimYW_merge <- read_dta("C:/Users/thwha/Dropbox/KimH_prj/data/sanctions_sc_KimYW_merge.dta")
attach(sanctions_sc_KimYW_merge)

## Analysis  

## Descriptive statistics: All observations 

summary.data.frame(sanctions_sc_KimYW_merge)
dim(sanctions_sc_KimYW_merge)

newdata <- data.frame(sanctions_sc_KimYW_merge$sanctionoutcome, 
                      sanctions_sc_KimYW_merge$trust, 
                      sanctions_sc_KimYW_merge$m_polparty1, 
                      sanctions_sc_KimYW_merge$m_profassociation1, 
                      sanctions_sc_KimYW_merge$m_humanrights1, 
                      sanctions_sc_KimYW_merge$m_religious1, 
                      sanctions_sc_KimYW_merge$c_government1, 
                      sanctions_sc_KimYW_merge$c_parliament1,  
                      sanctions_sc_KimYW_merge$c_polparty1,  
                      sanctions_sc_KimYW_merge$c_justicelegalsyscourts1,  
                      sanctions_sc_KimYW_merge$c_armedforces1,  
                      sanctions_sc_KimYW_merge$c_churches1,  
                      sanctions_sc_KimYW_merge$c_police1, 
                      sanctions_sc_KimYW_merge$contig,  
                      sanctions_sc_KimYW_merge$distance,  
                      sanctions_sc_KimYW_merge$lntarget_gdppc_gle, 
                      sanctions_sc_KimYW_merge$salience_dummy2,  
                      sanctions_sc_KimYW_merge$alliance2,  
                      sanctions_sc_KimYW_merge$targetdem)

stargazer(newdata, type="text", out="newdata.txt")
cor(newdata, use="complete.obs")

summary(sanctions_sc_KimYW_merge$trust)
mean(sanctions_sc_KimYW_merge$trust, na.rm=T)

###########
## TRUST ##
###########

# probit sanctionoutcome trust contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem , robust /*full model*/
# margins, at(trust=(4.9(3)59.4))                                       

trust <- glm(sanctionoutcome ~ trust + contig + distance + lntarget_gdppc_gle + salience_dummy2 + alliance2 + targetdem, family=binomial(link='logit'), data=newdata)
summary(trust)
stargazer(trust, type="text", out="trust.txt")

# probit version
trust_p <- glm(sanctionoutcome ~ trust + contig + distance + lntarget_gdppc_gle + salience_dummy2 + alliance2 + targetdem, family=binomial(link='probit'), data=sanctions_sc_KimYW_merge)
summary(trust_p)
stargazer(trust_p, type="text", out="trust.txt")

################
## MEMBERSHIP ##
################

# probit sanctionoutcome m_polparty1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
# margins, at(m_polparty1=(0(3)45.5)) 
# Probit sanctionoutcome m_profassociation1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
# margins, at(m_profassociation1=(0(1.5)25.7))

membership1 <- glm(sanctionoutcome ~ m_polparty1 + contig + distance + lntarget_gdppc_gle + salience_dummy2 + alliance2 + targetdem, family=binomial(link='logit'), data=sanctions_sc_KimYW_merge)
summary(membership1)
stargazer(membership1, type="text", out="membership1.txt")

membership2 <- glm(sanctionoutcome ~ m_profassociation1 + contig + distance + lntarget_gdppc_gle + salience_dummy2 + alliance2 + targetdem, family=binomial(link='logit'), data=sanctions_sc_KimYW_merge)
summary(membership2)
stargazer(membership2, type="text", out="membership2.txt")

stargazer(trust, membership1, membership2, type="text", out="trust_membership.txt")

################
## CONFIDENCE ##
################

# probit sanctionoutcome c_polparty1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
# margins, at(c_polparty1=(0.9(3)31.1))
# 
# probit sanctionoutcome c_government1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
# margins, at(c_government1=(1.9(3)54.5))
#                                        
# probit sanctionoutcome c_parliament1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
# margins, at(c_parliament1=(0.9(3)46.1))
#                                        
# probit sanctionoutcome c_justicelegalsyscourts1 contig distance lntarget_gdppc_gle salience_dummy2 alliance2 targetdem, robust
# margins, at(c_justicelegalsyscourts1=(6.7(2)37.3))

confidence1 <- glm(sanctionoutcome ~ c_polparty1 + contig + distance + lntarget_gdppc_gle + salience_dummy2 + alliance2 + targetdem, family=binomial(link='logit'), data=sanctions_sc_KimYW_merge)
summary(confidence1)

confidence2 <- glm(sanctionoutcome ~ c_government1 + contig + distance + lntarget_gdppc_gle + salience_dummy2 + alliance2 + targetdem, family=binomial(link='logit'), data=sanctions_sc_KimYW_merge)
summary(confidence2)

confidence3 <- glm(sanctionoutcome ~ c_parliament1 + contig + distance + lntarget_gdppc_gle + salience_dummy2 + alliance2 + targetdem, family=binomial(link='logit'), data=sanctions_sc_KimYW_merge)
summary(confidence3)

confidence4 <- glm(sanctionoutcome ~ c_justicelegalsyscourts1 + contig + distance + lntarget_gdppc_gle + salience_dummy2 + alliance2 + targetdem, family=binomial(link='logit'), data=sanctions_sc_KimYW_merge)
summary(confidence4)

stargazer(confidence1, confidence2, confidence3, confidence4, type="text", out="confidence5.txt")

###############################################
## PREDICTED PROBABILITIES (for data points) ##
###############################################

pred_pr_trust <- predict(trust, data=newdata, type="response", se.fit=TRUE)
pred_pr_membership1 <- predict(membership1, data=newdata, type="response", se.fit=TRUE)
pred_pr_membership2 <- predict(membership2, data=newdata, type="response", se.fit=TRUE)
pred_pr_confidence1 <- predict(confidence1, data=newdata, type="response", se.fit=TRUE)
pred_pr_confidence2 <- predict(confidence2, data=newdata, type="response", se.fit=TRUE)
pred_pr_confidence3 <- predict(confidence3, data=newdata, type="response", se.fit=TRUE)
pred_pr_confidence4 <- predict(confidence4, data=newdata, type="response", se.fit=TRUE)

cbind(pred_pr_trust$fit, pred_pr_trust$se.fit)

####################################################################################
## PREDICTED PROBABILITIES (for key variables; here I show the results for trust) ##
####################################################################################

allmean <- data.frame(trust = seq(4.9, 59.4, length.out=100),
                      contig = rep(mean(newdata$sanctions_sc_KimYW_merge.contig,na.rm = T),100),
                      distance = rep(mean(newdata$sanctions_sc_KimYW_merge.distance,na.rm = T),100),
                      lntarget_gdppc_gle = rep(mean(newdata$sanctions_sc_KimYW_merge.lntarget_gdppc_gle,na.rm = T),100),
                      salience_dummy2 = rep(mean(newdata$sanctions_sc_KimYW_merge.salience_dummy2,na.rm = T),100),
                      alliance2 = rep(mean(newdata$sanctions_sc_KimYW_merge.alliance2,na.rm = T),100),
                      targetdem = rep(mean(newdata$sanctions_sc_KimYW_merge.targetdem,na.rm = T),100))

pred_pr_trust <- predict(trust, newdata=allmean, type="response", se.fit=TRUE)

pred_pr_trust_result <- cbind(allmean, pred_pr_trust$fit, pred_pr_trust$se.fit)

plot(pred_pr_trust_result$trust,pred_pr_trust$fit, xlim = c(4.9,59.4), ylim = c(0,0.4), lty=1, xlab="trust", ylab = "Pr(success)")
