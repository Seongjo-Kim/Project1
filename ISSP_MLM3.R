setwd("D://DATA/")
library("foreign")
rm(list = ls( ))

ISSP <- read.spss("ZA7600_v3-0-0.sav", header=T, use.value.label= F, to.data.frame=TRUE)


library(tidyverse)
library(poliscidata)
library(psych)
library(moonBook)
library(MASS)
library(stargazer)
library(interactions)
library(nnet)
library(lme4)
library(lmerTest)
library(lmtest)
library(ordinal)

#COUNTRY#
ISSP$Country  <- ISSP$country
ISSP$Country [ISSP$country == '36'] <- 'AU'
ISSP$Country [ISSP$country == '40'] <- 'AT'
ISSP$Country [ISSP$country == '100'] <- 'BG'
ISSP$Country [ISSP$country == '152'] <- 'CL'
ISSP$Country [ISSP$country == '158'] <- 'TW'
ISSP$Country [ISSP$country == '191'] <- 'HR'
ISSP$Country [ISSP$country == '203'] <- 'CZ'
ISSP$Country [ISSP$country == '208'] <- 'DK'
ISSP$Country [ISSP$country == '246'] <- 'FI'
ISSP$Country [ISSP$country == '250'] <- 'FR'
ISSP$Country [ISSP$country == '276'] <- 'DE'
ISSP$Country [ISSP$country == '352'] <- 'IS'
ISSP$Country [ISSP$country == '376'] <- 'IL'
ISSP$Country [ISSP$country == '380'] <- 'IT'
ISSP$Country [ISSP$country == '392'] <- 'JP'
ISSP$Country [ISSP$country == '440'] <- 'LT'
ISSP$Country [ISSP$country == '554'] <- 'NZ'
ISSP$Country [ISSP$country == '578'] <- 'NO'
ISSP$Country [ISSP$country == '608'] <- 'PH'
ISSP$Country [ISSP$country == '643'] <- 'RU'
ISSP$Country [ISSP$country == '705'] <- 'SI'
ISSP$Country [ISSP$country == '710'] <- 'ZA'
ISSP$Country [ISSP$country == '740'] <- 'SR'
ISSP$Country [ISSP$country == '752'] <- 'SE'
ISSP$Country [ISSP$country == '756'] <- 'CH'
ISSP$Country [ISSP$country == '764'] <- 'TH'
ISSP$Country [ISSP$country == '826'] <- 'GB'
ISSP$Country [ISSP$country == '840'] <- 'US'
ISSP$Country [ISSP$country == '862'] <- 'VE'

ISSP <- ISSP %>% 
  filter(!(Country == 'TW' | Country == 'SI' |  Country == 'NZ'| Country == 'BG' 
           | Country == 'HR' | Country == 'PH' | Country == 'RU'| Country == 'ZA' 
           | Country == 'SR'| Country == 'TH'| Country == 'VE' ))



#gender#
ifelse(ISSP$SEX==2,1,0) ->ISSP$female


#Differences in income in [COUNTRY] are too large#
ifelse(ISSP$v21 == 'NA', NA, 
       (5- ISSP$v21)*25) ->ISSP$gap

ifelse(ISSP$v21 == 'NA', NA, 
       6- ISSP$v21) ->ISSP$gap2

#국가책임#
ifelse(ISSP$v22 == 'NA', NA, 
       (5- ISSP$v22)*25) ->ISSP$respn

table(ISSP$respn)

#Hard Work#
ifelse(ISSP$v4 == 'NA', NA, 
       (5- ISSP$v4)*25) ->ISSP$hardwork


#계급#
ifelse(ISSP$v61 == 1|ISSP$v61 == 2, 'Working', 
       ifelse(ISSP$v61 == 3|ISSP$v61 == 4, 'Middle', 'Upper')) ->ISSP$Social_class

as.factor(ISSP$Social_class)-> ISSP$Social_Class
ISSP$Social_Class <- relevel(ISSP$Social_Class, ref="Upper")


#종교활동#
9- ISSP$ATTEND ->ISSP$Religion

#RELIGGRP#
table(ISSP$RELIGGRP)


#분노#
freqC(ISSP$v32)
ISSP$v32->ISSP$anger


#접촉#
ISSP$v51 -> ISSP$contactpoor
ISSP$v52 -> ISSP$contactrich

##10년 후 상태와 현재 상태 비교##
ISSP$v43 - ISSP$v41 -> ISSP$mob.pro


#  연봉추측
ISSP$v12 -> ISSP$CEO_guess

#  연봉당위
ISSP$v17 -> ISSP$CEO_should


# unskilled 연봉추측
ISSP$v14 -> ISSP$unskilled_guess

# unskilled 연봉당위
ISSP$v19 -> ISSP$unskilled_should

#######CEO는 노동자는 몇 배를 벌까까?######

ISSP$CEO_guess/ISSP$unskilled_guess ->ISSP$gapguess1
ISSP$unskilled_should/ISSP$unskilled_guess -> ISSP$gapguess2


#필요한항목만#

ISSP <- ISSP %>% dplyr::select(Country, female, AGE, Social_Class, DEGREE, Religion,
                               gap, hardwork, anger,
                               contactpoor, contactrich, mob.pro,
                               gapguess1, gapguess2)


ISSP <- ISSP %>% dplyr::select(Country, female, AGE, Social_Class, DEGREE, Religion,
                               anger, hardwork, 
                               contactpoor, contactrich, mob.pro)




#결측 제거#
ISSP <- ISSP %>% na.omit(ISSP)



#개인수준에서 측정된 변수들의 기술통계
mydescriptive <- function(myvariable){
  mysize <- length(myvariable)
  mymean <- round(mean(myvariable),3)
  mysd <- round(sd(myvariable),3)
  mymin <- round(min(myvariable),3)
  mymax <- round(max(myvariable),3)
  mydes <- matrix(c(mysize,mymean,mysd,mymin,mymax),ncol=5)
  colnames(mydes) <- c('n','mean','sd','min','max')
  mydes
}
mydescriptive(ISSP$female)
mydescriptive(ISSP$AGE)
mydescriptive(ISSP$DEGREE)
mydescriptive(ISSP$respn)
mydescriptive(ISSP$tax)
mydescriptive(ISSP$contactpoor)

mydescriptive(ISSP$Religion)
mydescriptive(ISSP$mob.pro)
mydescriptive(ISSP$hardwork)
mydescriptive(ISSP$anger)
mydescriptive(ISSP$gap)



#2레벨조인
library(readxl) 
LV2 <- read_excel("Gini_OECD.xlsx")
LV2 <- LV2 %>% mutate(am.Gini = Gini - mean(Gini))
LV2 <- LV2 %>% mutate(am.logGDP = log_GDP - mean(log_GDP))
ISSP <- inner_join(ISSP, LV2, by = "Country")


#평균화#
ISSP <- group_by(ISSP, Country) %>% 
  mutate(gm.AGE = AGE - mean(AGE), 
         gm.DEGREE = DEGREE -mean(DEGREE), 
         gm.hardwork = hardwork-mean(hardwork), 
         gm.Religion = Religion-mean(Religion),
         gm.contactpoor = contactpoor -mean(contactpoor), 
         gm.contactrich = contactrich -mean(contactrich),
         gm.mob.pro = mob.pro -mean(mob.pro))



ISSP <- group_by(ISSP, Country) %>% 
  mutate(gm.AGE = AGE - mean(AGE), 
         gm.DEGREE = DEGREE -mean(DEGREE), 
         gm.hardwork = hardwork-mean(hardwork),
         gm.Religion = Religion-mean(Religion),
         gm.contactpoor = contactpoor -mean(contactpoor), 
         gm.mob.pro = mob.pro -mean(mob.pro))


# 변수간 관계 그림그리기#

ggplot(data=ISSP, aes(y=anger, x=contactpoor))+
  geom_smooth(method='lm')+
  facet_wrap(~ Country) + 
  labs(x='Contacting poor', y='Anger toward inequality')

ggplot(data=ISSP, aes(y=anger, x=contactrich))+
  geom_smooth(method='lm')+
  facet_wrap(~ Country) + 
  labs(x='Contacting rich', y='Anger toward inequality')


ggplot(data=ISSP, aes(y=respn, x=gap))+
  geom_smooth(method='lm')+
  facet_wrap(~ Country) + 
  labs(x='freqency_of_contacting_poor', y='Differences in income are too large.')

ggplot(data=ISSP, aes(y=respn, x=contactpoor))+
  geom_smooth(method='lm')+
  facet_wrap(~ Country) + 
  labs(x='freqency_of_contacting_poor', y='governmental_responsibility')



ggplot(data=ISSP, aes(y=anger, x=contactpoor))+
  geom_smooth(method='lm')+
  facet_wrap(~ Country) + 
  labs(x='freqency_of_contacting_poor', y='LevelofAnger')

ggplot(data=ISSP, aes(y=gapguess1, x=contactpoor))+
  geom_smooth(method='lm')+
  facet_wrap(~ Country) + 
  labs(x='freqency_of_contacting_poor', y='gapguess1')

##########Anger_Anger2로 논문에 사용##########

summary(Anger.model0<-lmer(anger~ 1+ (1| Country),  data=ISSP))

summary(Anger.model1<-lmer(anger~ gm.contactpoor+gm.contactrich+female+gm.AGE+gm.DEGREE+Social_Class+ 
                             gm.hardwork+ gm.Religion+gm.mob.pro+ 
                             am.Gini+ am.logGDP+ (1| Country),data=ISSP))

summary(Anger.model1.2<-lmer(anger~ gm.contactrich+female+gm.AGE+
                               gm.DEGREE+Social_Class*gm.contactpoor+
                               gm.hardwork+ gm.Religion+gm.mob.pro+ 
                               (1| Country),data=ISSP))

summary(Anger.model1.3<-lmer(anger~ gm.contactpoor+gm.contactrich+
                               female*gm.DEGREE+gm.AGE+Social_Class+ 
                               gm.hardwork+ gm.Religion+ mob.pro
                             + am.Gini+ am.logGDP+(1| Country),  data=ISSP))

summary(Anger2<-lmer(anger~ female+gm.AGE+Social_Class+ 
                       gm.hardwork+ gm.Religion+ gm.DEGREE*gm.mob.pro+
                       gm.contactpoor+gm.contactrich+
                       am.Gini+ am.logGDP + (1| Country),  data=ISSP))

summary(Anger2.0<-lmer(anger~ female+gm.AGE+Social_Class+ 
                       gm.hardwork+ gm.Religion+ +gm.mob.pro+
                       gm.contactpoor*gm.DEGREE+gm.contactrich+
                       am.Gini+ am.logGDP + (1| Country),  data=ISSP))


summary(Anger2.1<-lmer(anger~ female+gm.AGE+gm.DEGREE+
                         Social_Class*(gm.contactpoor+ gm.contactrich)+
                         gm.hardwork+ gm.Religion+ mob.pro+
                         am.Gini+ am.logGDP + (1| Country),  data=ISSP))

summary(Anger2.2<-lmer(anger~ female+gm.AGE+Social_Class+ 
                       gm.hardwork+ gm.Religion+ gm.DEGREE*gm.mob.pro*am.Gini+
                       gm.contactpoor+gm.contactrich+
                       am.logGDP + (1| Country),  data=ISSP))


summary(Anger3.1<-lmer(anger~ female+gm.AGE+gm.DEGREE+Social_Class+
                         gm.hardwork+ gm.Religion+ mob.pro+
                         (gm.contactpoor+gm.contactrich)*am.Gini+
                         am.logGDP + (1| Country),  data=ISSP))


summary(Anger.model3.4<-lmer(anger~ (gm.contactpoor+gm.contactrich)*gm.Religion+
                               female+gm.AGE+gm.DEGREE+Social_Class+
                               gm.hardwork+ mob.pro+am.Gini+ am.logGDP + (1| Country),  data=ISSP))

summary(Anger3.1<-lmer(anger~ female+gm.AGE++Social_Class+
                         gm.hardwork+ gm.Religion+ mob.pro+
                         gm.contactpoor+gm.contactrich+gm.DEGREE *am.Gini+
                         am.logGDP + (1| Country),  data=ISSP))



#상호작용그림#
interactions::interact_plot(Anger.model1.4, pred= gm.contactpoor, modx=  gm.Religion,
                            interval = T, int.type =  c("confidence", "prediction"),
                            int.width = 0.95) 

interactions::interact_plot(Anger2.0, pred= gm.contactpoor, modx=  gm.DEGREE,
                            interval = T, int.type =  c("confidence", "prediction"),
                            int.width = 0.95) 



interactions::interact_plot(Anger3.1, pred= gm.DEGREE, modx= am.Gini ,
                            interval = T, int.type =  c("confidence", "prediction"),
                            int.width = 0.95) 



##### 연봉 추측측####

summary(incomega2<-lmer(gapguess2 ~ female+gm.AGE+gm.DEGREE+Social_Class+
                       gm.hardwork+gm.Religion+gm.mob.pro+
                         gm.contactpoor+  (1| Country), data= ISSP))


##########gap로 논문에 사용##########

summary(gap1<-lmer(gap~ female+gm.AGE+gm.DEGREE+gm.contactpoor+gm.contactrich+
                    Social_Class+
                     gm.hardwork+ gm.Religion+gm.mob.pro+ 
                     am.Gini+ (1| Country),data=ISSP))


summary(gap2<-lmer(gap~ female*gm.DEGREE+gm.AGE+Social_Class+
                    gm.hardwork+ gm.Religion+gm.mob.pro+ 
                    am.Gini+ (1| Country),data=ISSP))


summary(gap3<-lmer(gap~ female+gm.AGE+gm.DEGREE+gm.contactpoor+Social_Class+
                     gm.hardwork+ gm.Religion+gm.mob.pro+ +
                     am.Gini+ (1| Country),data=ISSP))


interactions::interact_plot(gap2, pred= gm.contactpoor, modx=  Social_Class,
                            interval = T, int.type =  c("confidence", "prediction"),
                            int.width = 0.95) 


##########repn로 논문에 사용##########



summary(rp1<-lmer(respn~ female+gm.AGE+gm.DEGREE+Social_Class+ gm.contactpoor+
                             gm.hardwork+ gm.Religion+gm.mob.pro+ 
                             am.Gini+ (1| Country),data=ISSP))


summary(rp2<-lmer(respn~ female+gm.AGE+gm.DEGREE+gm.contactpoor*Social_Class+
                    gm.hardwork+ gm.mob.pro+  
                    am.Gini+ (1| Country),data=ISSP))


summary(rp3<-lmer(respn~ female+gm.AGE+gm.DEGREE+Social_Class+
                    gm.contactpoor*gm.hardwork+ gm.Religion+gm.mob.pro+  
                    am.Gini+ (1| Country),data=ISSP))


interactions::interact_plot(rp2, pred= gm.contactpoor, modx=  Social_Class,
                            interval = T, int.type =  c("confidence", "prediction"),
                            int.width = 0.95) 




####### OLS ######

summary(anger0 <- lm(anger~ female+gm.AGE+Social_class+ 
                       gm.hardwork+ gm.Religion+gm.mob.pro+gm.DEGREE+ gm.contactpoor+Country,
                     data = ISSP))

summary(anger1 <- lm(anger~ female+gm.AGE+Social_class+ 
                       gm.hardwork+ gm.Religion+gm.mob.pro*gm.DEGREE+ gm.contactpoor+Country,
                     data = ISSP))


summary(anger2 <- lm(anger~ female+gm.AGE+ 
                       gm.hardwork+ gm.Religion+gm.mob.pro+gm.DEGREE+ 
                       Social_class*gm.contactpoor+gm.contactrich+Country,
                     data = ISSP))


summary(anger3 <- lm(anger~ female+gm.AGE+ 
                        gm.Religion+gm.mob.pro+gm.DEGREE+ 
                       Social_Class+gm.hardwork*gm.contactpoor+gm.contactrich+Country,
                     data = ISSP))

summary(anger4 <- lm(anger~ female+gm.AGE+ 
                       gm.Religion+Social_class+gm.hardwork+
                       gm.DEGREE*gm.mob.pro+gm.contactpoor+gm.contactrich+Country,
                     data = ISSP))

summary(ST1 <- lm(structure~ female+gm.AGE+Social_Class+ gm.DEGREE+
                       gm.hardwork+ gm.Religion+gm.mob.pro+
                     gm.contactpoor+gm.contactrich+ Country,
                     data = ISSP))



###로지스틱회귀분석#############
##격차가너무크다##
#접촉과클라스그리고접촉과하드워킹간상호작용확인#
summary(gap1 <- polr(as.ordered(gap)~ female+gm.AGE+Social_Class+ gm.DEGREE+
                         gm.hardwork+gm.Religion+gm.mob.pro+
                         gm.contactpoor+gm.contactrich+Country, 
                       data = ISSP, Hess=TRUE))

lmtest::coeftest(gap1)

summary(gap2 <- polr(as.ordered(gap)~ female+gm.AGE+ gm.DEGREE+
                         gm.hardwork+gm.Religion+gm.mob.pro+
                         gm.contactpoor*Social_class+gm.contactrich+Country, 
                       data = ISSP, Hess=TRUE))
lmtest::coeftest(gap2)

summary(gap3 <- polr(as.ordered(gap)~ gm.AGE+gm.DEGREE+ Social_class+
                         gm.hardwork+gm.Religion+gm.mob.pro+
                         gm.contactpoor*female+gm.contactrich+Country, 
                       data = ISSP, Hess=TRUE))
lmtest::coeftest(gap3)

summary(gap4 <- polr(as.ordered(gap)~ gm.AGE+female+gm.DEGREE+ Social_class+
                         gm.Religion+gm.mob.pro+
                         gm.contactpoor*gm.hardwork+gm.contactrich+Country, 
                       data = ISSP, Hess=TRUE))
lmtest::coeftest(gap4)

summary(gap5 <- polr(as.ordered(gap)~ gm.AGE+female+gm.DEGREE+ Social_class+
                         gm.Religion+gm.hardwork+
                         gm.contactpoor*gm.mob.pro+gm.contactrich+Country, 
                       data = ISSP, Hess=TRUE))
lmtest::coeftest(gap5)

# 하드워크 컨택리치만 파지티브 영향#
summary(hardwork1 <- polr(as.ordered(hardwork)~ female+gm.AGE+Social_Class+ gm.DEGREE+
                       gm.Religion+gm.mob.pro+
                       gm.contactpoor+gm.contactrich+Country, 
                     data = ISSP, Hess=TRUE))

lmtest::coeftest(hardwork1)



##### 구조주의적 관점####

summary(lmer.ST1<-lmer(structure~ female+gm.AGE+gm.DEGREE+Social_Class+
                       gm.Religion+gm.mob.pro+
                         gm.contactpoor+ am.Gini+ 
                         am.logGDP+(1| Country), data= ISSP))

summary(lmer.ST2<-lmer(structure~ female+gm.AGE+gm.DEGREE+Social_Class+
                         gm.Religion+gm.mob.pro+
                         gm.contactpoor*am.Gini+ 
                         am.logGDP+(1| Country), data= ISSP))

summary(lmer.ST3<-lmer(structure~ female+gm.AGE+gm.DEGREE +  
                         gm.Religion+
                         gm.contactpoor*Social_Class+
                         am.Gini+ am.logGDP+(1| Country), data= ISSP))


#상호작용그림#

interactions::interact_plot(lmer.ST3, pred= gm.contactpoor, modx=  Social_Class,
                            interval = T, int.type =  c("confidence", "prediction"),
                            int.width = 0.95) 


interactions::interact_plot(lmer.ST2, pred= gm.contactpoor, modx=  am.Gini,
                            interval = T, int.type =  c("confidence", "prediction"),
                            int.width = 0.95) 


### 다층순서형로지스틱회귀분석##################

#####gap 격차가너무크다#####

#기본모형 랜덤효과는 절편에서만 발생#
summary(model0<-clmm(as.factor(gap)~ 1 + (1| Country), data= ISSP))


#Random slope#
summary(model0.1<-clmm(as.factor(gap)~ female+gm.AGE+gm.DEGREE+Social_Class+ gm.contactpoor+
                         gm.hardwork+ gm.Religion+gm.mob.pro+ 
                         (1| Country),data=ISSP))


summary(gap1.0<-clmm(as.factor(gap)~ female+gm.AGE+gm.DEGREE+Social_Class+ gm.contactpoor+
                         gm.hardwork+ gm.Religion+gm.mob.pro+ 
                         + am.Gini+am.logGDP+
                         (1| Country),data=ISSP))


summary(gap2<-clmm(as.factor(gap)~ female+gm.AGE+gm.DEGREE+Social_Class*gm.contactpoor+
                       gm.hardwork+ gm.Religion+gm.mob.pro+ 
                       + am.Gini+am.logGDP+
                       (1| Country),data=ISSP))

summary(gap3<-clmm(as.factor(gap)~ female+gm.AGE+gm.DEGREE+Social_Class+
                     gm.contactpoor*am.Gini+
                     gm.hardwork+ gm.Religion+gm.mob.pro+ 
                     + am.logGDP+
                     (1| Country),data=ISSP))



##### 다층 로지스틱 family ##### 

summary(model.f1<-clmm(as.factor(family)~ female+gm.AGE+gm.DEGREE+Social_Class+ 
                         gm.Religion+gm.mob.pro+ gm.contactpoor+gm.contactrich+
                         (1| Country),data=ISSP))

summary(model.h1<-clmm(as.factor(hardwork)~ female+gm.AGE+gm.DEGREE+Social_Class+ 
                         gm.Religion+gm.mob.pro+ gm.contactpoor+gm.contactrich+
                         am.Gini+am.logGDP+(1| Country),data=ISSP))




##접촉과능력주의관계##
##Random slope##
summary(hw1<-clmm(as.factor(hardwork)~ female+gm.AGE+gm.DEGREE+Social_Class+ 
                    gm.Religion+gm.mob.pro+ 
                    gm.contactpoor+am.Gini+am.logGDP+ 
                    (1| Country),data=ISSP))

summary(hw1.1<-clmm(as.factor(hardwork)~ female+gm.AGE+gm.DEGREE+Social_Class+ 
                      gm.Religion+gm.mob.pro+ 
                      gm.contactpoor+am.Gini+am.logGDP+ 
                      (1| Country),data=ISSP))


summary(hw1.2<-clmm(as.factor(hardwork)~ female+gm.AGE+gm.DEGREE+ 
                      gm.Religion+gm.mob.pro+ 
                      gm.contactpoor*Social_Class+ 
                      (1| Country),data=ISSP))


summary(hw2.1<-clmm(as.factor(hardwork)~ female+gm.AGE+gm.DEGREE+Social_Class+ 
                      gm.Religion+gm.mob.pro+ 
                      gm.contactpoor+am.Gini+am.logGDP+ 
                      (1| Country),data=ISSP))



#Random slope#

summary(lmer.model1<-lmer(hardwork2~ female+gm.AGE+gm.DEGREE+Social_Class+ 
                            gm.Religion+gm.mob.pro+ gm.contactpoor+gm.contactrich+
                            (1| Country),data=ISSP))


summary(lmer.model1.1<-lmer(hardwork2~ female+gm.AGE+gm.DEGREE+Social_Class+ 
                              gm.Religion+gm.mob.pro+ gm.contactpoor+gm.contactrich+
                              am.Gini+am.logGDP+ (1| Country),data=ISSP))


summary(lmer.model1.2<-lmer(hardwork2~ female+gm.AGE+gm.DEGREE+Social_Class*gm.contactpoor+ 
                              gm.mob.pro+ gm.contactrich+am.Gini+am.logGDP+
                              (1| Country),data=ISSP))

summary(lmer.model1.3<-lmer(hardwork2~ female+gm.AGE+gm.DEGREE+Social_Class+ 
                              gm.Religion+gm.mob.pro+gm.contactrich*am.Gini+ 
                              gm.contactpoor+am.logGDP+
                              (1| Country),data=ISSP))

#Random intercept#
summary(lmer.model2.1<-lmer(hardwork2~ female+gm.AGE+gm.DEGREE+Social_Class+ 
                              gm.Religion+gm.mob.pro+ gm.contactpoor+gm.contactrich+
                              am.Gini+am.logGDP+ (gm.contactrich|Country),data=ISSP))




###재분배 국가책임####


#기본모형 랜덤효과는 절편에서만 발생#
summary(model0<-clmm(as.factor(respn)~ 1 + (1| Country), data= ISSP))

summary(lmer.model0<-lmer(respn~ 1 + (1| Country), data= ISSP))


#Random slope#
summary(model0.1<-clmm(as.factor(respn)~ female+gm.AGE+gm.DEGREE+Social_Class+ 
                         gm.hardwork+ gm.Religion+gm.mob.pro+ gm.contactpoor+
                         (1| Country),data=ISSP))


summary(model0.2<-clmm(as.factor(respn)~ female+gm.AGE+gm.DEGREE+Social_Class+ 
                         gm.hardwork+ gm.Religion+gm.mob.pro+ gm.contactpoor+
                         am.Gini+am.logGDP+ (1| Country),data=ISSP))


summary(model0.3<-clmm(as.factor(respn)~ female+gm.AGE+gm.DEGREE+
                         gm.hardwork+ gm.Religion+gm.mob.pro+ 
                         gm.contactpoor*Social_Class+
                         am.Gini+am.logGDP+ (1| Country),data=ISSP))

summary(model1<-clmm(as.factor(respn)~ female+gm.AGE+gm.DEGREE+Social_Class+ 
                         gm.hardwork+ gm.Religion+gm.mob.pro+ gm.contactpoor*am.Gini+
                         am.logGDP+ (1| Country),data=ISSP))



summary(lmer.model0<-lmer(respn~ female+gm.AGE+gm.DEGREE+Social_Class+ 
                            gm.hardwork+ gm.Religion+gm.mob.pro+ gm.contactpoor+
                            am.Gini+am.logGDP+
                            (1| Country),data=ISSP))


exp(model0.1$coef)


#랜덤효과는 절편과 컨택 기울기에서 발생#
summary(model1.1<-clmm(as.factor(respn)~ female+gm.AGE+gm.DEGREE+gm.class+ 
                         gm.hardwork+ gm.Religion+gm.mob.pro+ gm.contactpoor+
                         (gm.contactpoor| Country),data=ISSP))

exp(model1.1$coef)


summary(lmer.model1.1<-lmer(respn~ female+gm.AGE+gm.DEGREE+gm.class+ gm.log.USINC+
                              gm.hardwork+ gm.Religion+gm.mob.pro+ gm.contactpoor+
                              (gm.contactpoor| Country),data=ISSP))


summary(lmer.model1<-lmer(respn~ 1 + female+gm.AGE+gm.DEGREE+gm.class+ gm.log.USINC+
                            gm.Religion+gm.mob.pro+ gm.contactpoor*gm.hardwork+
                            (gm.contactpoor| Country),data=ISSP))



summary(lmer.model1<-lmer(respn~ female+gm.AGE+gm.DEGREE+Social_class+ 
                            gm.Religion+gm.mob.pro+ gm.contactpoor*gm.hardwork+
                            (gm.contactpoor| Country),data=ISSP))



## 재분배 세금 ##
summary(tax.model1<-clmm(as.factor(tax)~ female+gm.AGE+gm.DEGREE+gm.class+ gm.log.USINC+
                           gm.hardwork+ gm.Religion+gm.mob.pro+ gm.contactpoor+
                           (gm.contactpoor| Country),data=ISSP))


#Random slope#
summary(tax.model0.1<-clmm(as.factor(tax)~ female+gm.AGE+gm.DEGREE+gm.class+ 
                             gm.hardwork+ gm.Religion+gm.mob.pro+ gm.contactpoor+
                             (1| Country),data=ISSP))
#Random intercept#
summary(tax.model1.1<-clmm(as.factor(tax)~ female+gm.AGE+gm.DEGREE+gm.class+ 
                             gm.hardwork+ gm.Religion+gm.mob.pro+ gm.contactpoor+
                             (gm.contactpoor| Country),data=ISSP))

exp(tax.model0.1$coef)


summary(tax.model2<-clmm(as.factor(tax)~ female+Unemp+gm.AGE+gm.DEGREE+gm.class+ 
                           gm.hardwork+ gm.Religion+gm.mob.pro+ gm.contactpoor*am.Gini+
                           (1| Country),  data=ISSP))



# 국가별 평균비교#

mean(ISSP$respn, na.rm = T)

ISSP %>% 
  filter(!is.na(anger)) %>% 
  group_by(Country) %>% 
  summarise(mean(anger))


ISSP %>% 
  filter(!is.na(respn)) %>% 
  group_by(Country) %>% 
  summarise(mean(respn)) ->vis_Res



ISSP %>% 
  mutate(across(.cols=where(is.double),
                .fns=function(x){ifelse(x < 0, NA, x)})) ->mydata1

ISSP %>% 
  filter(!is.na(respn)) %>% 
  group_by(Country) %>% 
  summarise(M_res=mean(respn)) -> Mydata2


Mydata2 %>% drop_na() %>% 
  ggplot(aes(x=reorder(Country,M_res), y=M_res))+
  geom_bar(stat="identity")+
  labs(x='Country',y='govt_responsible') 

ISSP %>% 
  group_by(Country) %>% 
  summarise(M_tax=mean(tax, na.rm = T)) -> Mydata3


Mydata3 %>% drop_na() %>% 
  ggplot(aes(x=reorder(Country,M_tax), y=M_tax))+
  geom_bar(stat="identity")+
  labs(x='Country',y='Rich_tax') 

ISSP %>% 
  group_by(Country) %>% 
  summarise(M_anger=mean(anger, na.rm = T)) %>% 
  drop_na() %>% 
  ggplot(aes(x=Country, y=M_anger))+
  geom_bar(stat="identity")+
  labs(x='Country',y='Anger') 


ISSP %>% 
  filter(Country == "DK") %>% 
  count(Gen, mob.pro3) %>% 
  group_by(Gen) %>%   
  mutate(P=n/sum(n)) %>% 
  ggplot(aes(x=Gen, y=P, fill = mob.pro3))+
  geom_bar(stat="identity")+
  labs(x='age_group',y='mobility_expection') 

ISSP %>% 
  filter(Country == "DK") %>% 
  count(contactpoor, Anger) %>% 
  drop_na() %>%
  group_by(contactpoor) %>%   
  mutate(P=n/sum(n)) %>% 
  ggplot(aes(x=contactpoor, y=P, fill = Anger))+
  geom_bar(stat="identity")+
  labs(x='contact',y='Anger') 

ISSP %>% 
  filter(Country == "DE") %>% 
  filter(v41 < 4) %>% 
  count(Gen, v43) %>% 
  drop_na() %>%
  group_by(Gen) %>%   
  ggplot(aes(x=Gen, y=n, fill = v43))+
  geom_bar(stat="identity")+
  labs(x='age_group',y='mobility_expection') 

ISSP %>% 
  filter(Country == "FR") %>% 
  count(class, hardwork) %>% 
  drop_na() %>%
  group_by(class) %>% 
  mutate(P=n/sum(n)) %>% 
  ggplot(aes(x=class, y=P, fill = hardwork))+
  geom_bar(stat="identity")+
  labs(x='class',y='proportion', fill="hardwork")

ISSP %>% 
  filter(Country == "GB") %>% 
  count(Gen, hardwork) %>% 
  drop_na() %>%
  group_by(Gen) %>% 
  mutate(P=n/sum(n)) %>% 
  ggplot(aes(x=Gen, y=P, fill = hardwork))+
  geom_bar(stat="identity")+
  labs(x='Gen',y='proportion', fill="hardwork")


ISSP %>% 
  count(Country, mob.experience3) %>% 
  drop_na() %>% 
  group_by(Country) %>%   
  mutate(P=n/sum(n)) %>% 
  filter(!is.na(mob.experience3)) %>% 
  ggplot(aes(x= as_factor(Country), y=P, fill=mob.experience3))+
  geom_bar(stat="identity")+
  labs(x="Country",y="proportion", fill="Mobility type") 



ISSP %>% 
  group_by(Country) %>% 
  summarise(M_anger=mean(anger, na.rm = T)) %>% 
  drop_na() %>% 
  ggplot(aes(x=reorder(Country,M_anger), y=M_anger))+
  geom_bar(stat="identity")+
  labs(x='Country',y='Anger') 


ISSP %>% 
  group_by(Country) %>% 
  summarise(M_hardwork=mean(hardwork, na.rm = T)) %>% 
  drop_na() %>% 
  ggplot(aes(x=reorder(Country,M_hardwork), y=M_hardwork))+
  geom_bar(stat="identity")+
  labs(x='Country',y='Hardwork') 

ISSP %>% 
  group_by(Gen) %>% 
  summarise(M_hardwork=mean(hardwork, na.rm = T)) %>% 
  drop_na() %>% 
  ggplot(aes(x=Gen, y=M_hardwork))+
  geom_bar(stat="identity")+
  labs(x='Age_groups',y='hardwork') 


ISSP %>% 
  group_by(Country) %>% 
  summarise(M_wealthy=mean(wealthy, na.rm = T)) %>% 
  drop_na() %>% 
  ggplot(aes(x=reorder(Country,M_wealthy), y=M_wealthy))+
  geom_bar(stat="identity")+
  labs(x='Country',y='Wealthy_family') 

ISSP %>% 
  group_by(Country) %>% 
  summarise(M_edu_P=mean(v2, na.rm = T)) %>% 
  drop_na() %>% 
  ggplot(aes(x=reorder(Country,M_edu_P), y=M_edu_P))+
  geom_bar(stat="identity")+
  labs(x='Country',y='well-educated_Parents') 


ISSP %>% 
  group_by(Country) %>% 
  summarise(M_edu_s=mean(v3, na.rm = T)) %>% 
  drop_na() %>% 
  ggplot(aes(x=reorder(Country,M_edu_s), y=M_edu_s))+
  geom_bar(stat="identity")+
  labs(x='Country',y='edu_yourself') 


ISSP %>% 
  group_by(Country) %>% 
  summarise(M_knowpeople=mean(knowpeople, na.rm = T)) %>% 
  drop_na() %>% 
  ggplot(aes(x=reorder(Country,M_knowpeople), y=M_knowpeople))+
  geom_bar(stat="identity")+
  labs(x='Country',y='knowpeople') 


ISSP %>% 
  group_by(Country) %>% 
  summarise(M_contactpoor=mean(contactpoor, na.rm = T)) %>% 
  drop_na() %>% 
  ggplot(aes(x=reorder(Country,M_contactpoor), y=M_contactpoor))+
  geom_bar(stat="identity")+
  labs(x='Country',y='Contact_poor') 

ISSP %>% 
  group_by(Country) %>% 
  summarise(M_contactrich=mean(contactrich, na.rm = T)) %>% 
  drop_na() %>% 
  ggplot(aes(x=reorder(Country,M_contactrich), y=M_contactrich))+
  geom_bar(stat="identity")+
  labs(x='Country',y='Contact_rich') 

ISSP %>% 
  group_by(Country) %>% 
  summarise(M_Unbalance=mean(Unbalance, na.rm = T)) %>% 
  drop_na() %>% 
  ggplot(aes(x=reorder(Country,M_Unbalance), y=M_Unbalance))+
  geom_bar(stat="identity")+
  labs(x='Country',y='Unbalance') 


ISSP %>% 
  group_by(Country) %>% 
  summarise(M_conflict1=mean(v36, na.rm = T)) %>% 
  drop_na() %>% 
  ggplot(aes(x=reorder(Country,M_conflict1), y=M_conflict1))+
  geom_bar(stat="identity")+
  labs(x='Country',y='conflict1') 


ISSP %>% 
  count(Country, mob.pro4) %>% 
  drop_na() %>% 
  mutate(mob.pro4=mob.pro4) %>% 
  pivot_wider(names_from = mob.pro4, values_from = "n") ->myfreq

ISSP %>% 
   count(Country, mob.pro3) %>% 
   drop_na()  ->freq

freq

freq %>% 
   group_by(Country) %>% 
   mutate(P=n/sum(n)) -> freq2

freq2

freq2 %>% 
  filter(!is.na(mob.pro3)) %>% 
  ggplot(aes(x= as_factor(Country), y=P, fill=mob.pro3))+
  geom_bar(stat="identity")+
  labs(x="Country",y="proportion", fill="Mobility type") 
 
   
ISSP %>% 
   count(Country, mob.experience3) %>% 
   drop_na() %>% 
   group_by(Country) %>%   
   mutate(P=n/sum(n)) %>% 
   filter(!is.na(mob.experience3)) %>% 
   ggplot(aes(x= as_factor(Country), y=P, fill=mob.experience3))+
   geom_bar(stat="identity")+
   labs(x="Country",y="proportion", fill="Mobility type") 

ISSP %>% 
  count(Country, contactpoor) %>% 
  drop_na() %>% 
  group_by(Country) %>%   
  mutate(P=n/sum(n)) %>% 
  filter(!is.na(contactpoor)) %>% 
  ggplot(aes(x= as_factor(Country), y=P, fill=contactpoor))+
  geom_bar(stat="identity")+
  labs(x="Country",y="proportion", fill="contactpoor") 

