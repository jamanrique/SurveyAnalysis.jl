rm(list=ls())
### Libraries ###
library(haven)
library(foreign)
library(tidyverse)
library(survey)
### Data Upload ###
setwd("C:/Users/jaman/Dropbox/Sampling Program for Survey Statisticians/SurvMeth 614/Final Project/ENAHO/850")
ENAHO_01B_1 <- read_sav("Enaho_01B_1-alteryx.sav")
ENAHO_01B_1$departamento <- substr(ENAHO_01B_1$UBIGEO,1,2)
ENAHO_01B_1 %>% mutate(rural = case_when(ESTRATO == 7 ~ 1, ESTRATO == 8 ~ 1, TRUE ~ 0)) -> ENAHO_01B_1
ENAHO_01B_1 %>% filter(!is.na(FAMIEGOB07)) -> ENAHO_01B_1
ENAHO_01B_1$P208_01 = ENAHO_01B_1$P208_01-mean(ENAHO_01B_1$P208_01,na.rm=T)

### Survey Design ###
svy_enaho <- svydesign(data=ENAHO_01B_1,ids = ~UBIGEO+NCONGLOME+VIVIENDA,strata=~interaction(departamento,rural,drop = T),weights = ~FAMIEGOB07,nest=T)
colnames(ENAHO_01B_1)

### Descriptive Statistics ###
confidence <- svymean(~cf_pj+cf_congreso+cf_procac+cf_anticorr,svy_enaho,na.rm=T,deff="replace")
table_1 <- tibble(confidence,SE(confidence),confint(confidence),deff(confidence)) %>% as.data.frame()
colnames(table_1) <- c("Mean","Standard_Error","Confidence_Interval","Design_Effect")
rownames(table_1) <- c("Poder Judicial","Congreso de la República","Procudaruría","Fiscalía Anticorrupción")
table_1

table_2 <- svyby(~cf_pj+cf_congreso+cf_procac+cf_anticorr,by = ~DOMINIO,design = svy_enaho,FUN=svymean,na.rm=T,deff="replace"); table_2
confint(table_2)

bivariate_analysis <- c("P301_01","P207_01")

table <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Predictor", "Rao_Scott_F_stat","Rao_Scott_P_value"))
table_final <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Predictor", "Rao_Scott_F_stat","Rao_Scott_P_value","Category","mean","se"))

for (i in 1:length(bivariate_analysis)) {
  reg <- svyglm(str2lang(paste("cf_anticorr~as.factor(",bivariate_analysis[i],")")),svy_enaho)
  RS_test <- regTermTest(reg,as.formula(str2lang(paste("~as.factor(",bivariate_analysis[i],")"))))
  df <- data.frame(Predictor=bivariate_analysis[i],Rao_Scott_F_stat=RS_test$Ftest,Rao_Scott_P_value=RS_test$p)
  table <- rbind(table,df)
  df_by <-  svyby(~cf_anticorr,by = as.formula(str2lang(paste("~",bivariate_analysis[i],sep=""))),FUN = svymean,design = svy_enaho,na.rm=T)
  df_by <- cbind(df_by, Predictor=bivariate_analysis[i])
  table_new <- merge(table,df_by,by="Predictor")
  colnames(table_new) <- c("Predictor", "Rao_Scott_F_stat","Rao_Scott_P_value","Category","mean","se")
  table_final <- rbind(table_final,table_new)
}

tibble(table_final)

olr_pj <- survey::svyolr(as.factor(cf_pj) ~as.factor(P301_01)+P208_01,design = svy_enaho)
summary(olr_pj)
svy_null <- svydesign(data = ENAHO_01B_1,ids = ~VIVIENDA)

glm_pj <- survey::svyglm(cf_pj ~ as.factor(P301_01)+P208_01,design = svy_enaho,deff=T)
glm_ols_pl <- svyglm(cf_pj ~ as.factor(P301_01)+P208_01,design = svy_null,deff=T)

glm_congreso <- survey::svyglm(cf_congreso ~ as.factor(P301_01)+P208_01,design = svy_enaho)
glm_ols_congreso <- svyglm(cf_congreso ~ as.factor(P301_01)+P208_01,design = svy_null,deff=T)

glm_procac <- survey::svyglm(cf_procac ~ as.factor(P301_01)+as.factor(P207_01)+P208_01,design = svy_enaho)
glm_ols_procac <- svyglm(cf_procac ~ as.factor(P301_01)+as.factor(P207_01)+P208_01,design = svy_null,deff=T)

glm_anticorr <- survey::svyglm(cf_anticorr ~ as.factor(P301_01)+as.factor(P207_01)+P208_01,design = svy_enaho)
glm_ols_anticorrc <- svyglm(cf_anticorr ~ as.factor(P301_01)+as.factor(P207_01)+P208_01,design = svy_null,deff=T)

summary(glm_pj)
round(confint(glm_pj),3)

round(summary(glm_pj)$coef[,2]^2/summary(glm_ols_pl)$coef[,2]^2,3) ## deff glm_pj

summary(glm_congreso)
round(confint(glm_congreso),3)
round(summary(glm_congreso)$coef[,2]^2/summary(glm_ols_congreso)$coef[,2]^2,3) ## deff glm_congreso

summary(glm_procac)
round(confint(glm_procac),3)
round(summary(glm_procac)$coef[,2]^2/summary(glm_ols_procac)$coef[,2]^2,3) ## deff glm_procac

summary(glm_anticorr)
round(confint(glm_anticorr),3)
round(summary(glm_anticorr)$coef[,2]^2/summary(glm_ols_anticorrc)$coef[,2]^2,3) ## deff glm_procac

sum(!is.na(ENAHO_01B_1$cf_anticorr))/length(ENAHO_01B_1$cf_anticorr)

regTermTest(glm_pj,~ as.factor(P301_01)+P208_01)
regTermTest(glm_congreso,cf_congreso ~ as.factor(P301_01)+P208_01)
regTermTest(glm_procac,~ as.factor(P301_01)+as.factor(P207_01)+P208_01)
regTermTest(glm_anticorr,~ as.factor(P301_01)+as.factor(P207_01)+P208_01)
