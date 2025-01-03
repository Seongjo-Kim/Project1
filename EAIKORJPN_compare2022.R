###unfied전체적분석##################################################

setwd("F:/STAT_DATA/EAIDATA/")
library("foreign")

EAI2022 <-read.csv("merged2022.csv", header = T)

EAI2022.Jpn <- subset(EAI2022, EAI2022$country == 'Japan')
EAI2022.Kor <- subset(EAI2022, EAI2022$country == 'Korea')


#age groups_Japan#
freqC(EAI2022.Jpn$AGE)
cutpoints_age1 = c(3, 5)
EAI2022.Jpn$GEN = cut2(EAI2022.Jpn$AGE, cutpoints_age1)
levels(EAI2022.Jpn$GEN) = c("2030s","4050s","60plus")
relevel(EAI2022.Jpn$GEN, "60plus" ) -> EAI2022.Jpn$GENE


#age groups#
freqC(EAI2022.Kor$AGE)
cutpoints_age1 = c(40, 60)
EAI2022.Kor$GEN = cut2(EAI2022.Kor$AGE, cutpoints_age1)
levels(EAI2022.Kor$GEN) = c("2030s","4050s","60plus")
relevel(EAI2022.Kor$GEN, "60plus" ) -> EAI2022.Kor$GENE
freqC(EAI2022.Kor$GENE)

#Status_japan#
ifelse(EAI2022.Jpn$Q14 == 1, 1, 0) -> EAI2022.Jpn$status2

#Status-Kor#
ifelse(EAI2022.Kor$Q14 == 1, 1, 0) -> EAI2022.Kor$status2

#성별#
freqC(EAI2022.Jpn$GENDER)
ifelse(EAI2022.Jpn$GENDER  == 3, NA, EAI2022.Jpn$GENDER-1) -> EAI2022.Jpn$Female

EAI2022.Kor$GENDER -1 -> EAI2022.Kor$Female
freqC(EAI2022.Kor$Female)

#income#
EAI2022.Jpn$INC -> EAI2022.Jpn$Income

EAI2022.Jpn$Income[EAI2022.Jpn$INC == 1] <- "Low"
EAI2022.Jpn$Income[EAI2022.Jpn$INC == 2 |EAI2022.Jpn$INC == 3 | EAI2022.Jpn$INC == 4] <- "Mid"
EAI2022.Jpn$Income[ EAI2022.Jpn$INC == 5 |  EAI2022.Jpn$INC == 6] <- "High"
as.factor(EAI2022.Jpn$Income) ->EAI2022.Jpn$Income2
relevel(EAI2022.Jpn$Income2, "Low" ) -> EAI2022.Jpn$Income3
freqC(EAI2022.Jpn$Income3)


EAI2022.Kor$INC -> EAI2022.Kor$Income
cutpoints_inc = c(4, 7)
EAI2022.Kor$inc1 = cut2(EAI2022.Kor$Income, cutpoints_inc)
levels(EAI2022.Kor$inc1) = c("Low","Mid","High")
relevel(EAI2022.Kor$inc1, "Low" ) -> EAI2022.Kor$Income3
freqC(EAI2022.Kor$Income3)

#edu#
freq(EAI2022.Jpn$EDU)
ifelse(EAI2022.Jpn$EDU == 6, NA, EAI2022.Jpn$EDU) -> EAI2022.Jpn$Education
freq(EAI2022.Jpn$Education)

ifelse(EAI2022.Jpn$EDU == 4 | EAI2022.Jpn$EDU == 5, 1, 0) -> EAI2022.Jpn$Univ
freq(EAI2022.Jpn$Univ)

ifelse(EAI2022.Kor$EDU == 5 | EAI2022.Kor$EDU == 6, 1, 0) -> EAI2022.Kor$Univ
freq(EAI2022.Kor$Univ)

#Covid#
freq(EAI2022.Kor$Q48)
ifelse(EAI2022.Kor$Q48 == 5, NA, 5-EAI2022.Kor$Q48) -> EAI2022.Kor$covid
freq(EAI2022.Kor$covid)
describe(EAI2022.Kor$covid)

EAI2022.Kor$Covid.K <- EAI2022.Kor$Q48
EAI2022.Kor$Covid.K[EAI2022.Kor$Q48 == 1] <- 5
EAI2022.Kor$Covid.K[EAI2022.Kor$Q48 == 2] <- 4
EAI2022.Kor$Covid.K[EAI2022.Kor$Q48 == 5] <- 3
EAI2022.Kor$Covid.K[EAI2022.Kor$Q48 == 3] <- 2
EAI2022.Kor$Covid.K[EAI2022.Kor$Q48 == 4] <- 1
freq(EAI2022.Kor$Covid.K)


freq(EAI2022.Jpn$Q48)
ifelse(EAI2022.Jpn$Q48 == 5, NA, 5-EAI2022.Jpn$Q48) -> EAI2022.Jpn$covid
EAI2022.Jpn$Covid.J <- EAI2022.Jpn$Q48
EAI2022.Jpn$Covid.J[EAI2022.Jpn$Q48 == 1] <- 5
EAI2022.Jpn$Covid.J[EAI2022.Jpn$Q48 == 2] <- 4
EAI2022.Jpn$Covid.J[EAI2022.Jpn$Q48 == 5] <- 3
EAI2022.Jpn$Covid.J[EAI2022.Jpn$Q48 == 3] <- 2
EAI2022.Jpn$Covid.J[EAI2022.Jpn$Q48 == 4] <- 1
freq(EAI2022.Jpn$Covid.J)

#호감도##
freqC(EAI2022.Jpn$Q5)
EAI2022.Jpn$Q5 -> EAI2022.Jpn$fav

EAI2022.Jpn$fav[EAI2022.Jpn$Q5 == 1] <- 5
EAI2022.Jpn$fav[EAI2022.Jpn$Q5 == 2] <- 4
EAI2022.Jpn$fav[EAI2022.Jpn$Q5 == 5] <- 3
EAI2022.Jpn$fav[EAI2022.Jpn$Q5 == 3] <- 2
EAI2022.Jpn$fav[EAI2022.Jpn$Q5 == 4] <- 1

freq(EAI2022.Jpn$fav)
as.numeric(EAI2022.Jpn$fav) -> EAI2022.Jpn$Favor
mean(EAI2022.Jpn$Favor, na.rm = T)

freqC(EAI2022.Kor$Q5)
EAI2022.Kor$Q5 -> EAI2022.Kor$fav
EAI2022.Kor$fav[EAI2022.Kor$Q5 == 1] <- 5
EAI2022.Kor$fav[EAI2022.Kor$Q5 == 2] <- 4
EAI2022.Kor$fav[EAI2022.Kor$Q5 == 5] <- 3
EAI2022.Kor$fav[EAI2022.Kor$Q5 == 3] <- 2
EAI2022.Kor$fav[EAI2022.Kor$Q5 == 4] <- 1
freq(EAI2022.Kor$fav)
as.numeric(EAI2022.Kor$fav) -> EAI2022.Kor$Favor

describe(EAI2022.Kor$Favor, na.rm = T)

#상호의존#
freqC(EAI2022.Kor$Q33)

freqC(EAI2022.Jpn$Q33)
ifelse(EAI2022.Jpn$Q33 == 5, NA, 5-EAI2022.Jpn$Q33) -> EAI2022.Jpn$EID
EAI2022.Jpn$Q33 -> EAI2022.Jpn$eid
EAI2022.Jpn$eid[EAI2022.Jpn$Q33 == 1] <- 5
EAI2022.Jpn$eid[EAI2022.Jpn$Q33 == 2] <- 4
EAI2022.Jpn$eid[EAI2022.Jpn$Q33 == 5] <- 3
EAI2022.Jpn$eid[EAI2022.Jpn$Q33 == 3] <- 2
EAI2022.Jpn$eid[EAI2022.Jpn$Q33 == 4] <- 1

freq(EAI2022.Jpn$eid)
as.numeric(EAI2022.Jpn$eid) -> EAI2022.Jpn$EID5


#상호의존 Korea#

freqC(EAI2022.Kor$Q33)
ifelse(EAI2022.Kor$Q33 == 5, NA, 5-EAI2022.Kor$Q33) -> EAI2022.Kor$EID
EAI2022.Kor$Q33 -> EAI2022.Kor$eid
EAI2022.Kor$eid[EAI2022.Kor$Q33 == 1] <- 5
EAI2022.Kor$eid[EAI2022.Kor$Q33 == 2] <- 4
EAI2022.Kor$eid[EAI2022.Kor$Q33 == 5] <- 3
EAI2022.Kor$eid[EAI2022.Kor$Q33 == 3] <- 2
EAI2022.Kor$eid[EAI2022.Kor$Q33 == 4] <- 1
freq(EAI2022.Kor$eid)
as.numeric(EAI2022.Kor$eid) -> EAI2022.Kor$EID5
describe(EAI2022.Kor$EID5)


#미디어#
ifelse(EAI2022.Jpn$Q43 == 1, 3, EAI2022.Jpn$Q43-1) -> EAI2022.Jpn$media

ifelse(EAI2022.Kor$Q43 == 1, 3, EAI2022.Kor$Q43-1) -> EAI2022.Kor$media


#visit#
ifelse(EAI2022.Kor$Q1_SQ3 == 4, 0, 1) -> EAI2022.Kor$Visit
freq(EAI2022.Kor$Visit)
EAI2022.Kor$Visit -> EAI2022.Kor$VISIT
EAI2022.Kor$VISIT[is.na(EAI2022.Kor$VISIT)] <-0
freq(EAI2022.Kor$VISIT)

ifelse(EAI2022.Jpn$Q1_SQ3 == 4, 0, 1) -> EAI2022.Jpn$Visit
freq(EAI2022.Jpn$Visit)
EAI2022.Jpn$Visit -> EAI2022.Jpn$VISIT
EAI2022.Jpn$VISIT[is.na(EAI2022.Jpn$VISIT)] <-0
freq(EAI2022.Jpn$VISIT)

#대중문화#
freqC(EAI2022.Jpn$Q42)
EAI2022.Jpn$Q42 -> EAI2022.Jpn$Cul
ifelse(EAI2022.Jpn$Q42 == 1 |EAI2022.Jpn$Q42 == 2 , 1, 0) -> EAI2022.Jpn$Culture

EAI2022.Kor$Q42 -> EAI2022.Kor$Cul
ifelse(EAI2022.Kor$Q42 == 1 |EAI2022.Kor$Q42 == 2 , 1, 0) -> EAI2022.Kor$Culture



#필요한항목만#

EAI2022.Jpn2 <- EAI2022.Jpn %>% dplyr::select(status2, GENE, Female, Univ, Income3, Favor, EID5,
                                              covid, media, Culture, VISIT)

EAI2022.Kor2 <- EAI2022.Kor %>% dplyr::select(status2, GENE, Female, Univ, Income3, Favor, EID5,
                                              covid, media, Culture, VISIT)



#결측 제거#
EAI2022.Jpn3 <- EAI2022.Jpn2 %>% na.omit(EAI2022.Jpn2)

EAI2022.Kor3 <- EAI2022.Kor2 %>% na.omit(EAI2022.Kor2)


#이분형Japan#

summary(St.Japan1 <- glm(status2 ~  GENE+Female+Univ+Income3++
                           Favor+ EID5+ covid+media+Culture+
                         VISIT, 
                         data = EAI2022.Jpn3, family = binomial("logit")))

summary(St.Japan1.1 <- glm(status2 ~  GENE+Female*Favor+Univ+Income3+
                             EID5+covid+media+Culture+
                             VISIT, 
                           data = EAI2022.Jpn3, family = binomial("logit")))

summary(St.Japan2.1 <- glm(status2 ~  GENE+Favor*Female+Univ+Income3+
                            EID5+media+covid+Culture+
                         VISIT, 
                         data = EAI2022.Jpn3, family = binomial("logit")))

summary(St.Japan2.2 <- glm(status2 ~  GENE+Female*EID5+Univ+Income3+
                             Favor+media+covid+Culture+
                             VISIT, 
                           data = EAI2022.Jpn3, family = binomial("logit")))

summary(St.Japan2.3 <- glm(status2 ~ GENE+Female+Univ+Income3+
                             Favor+ EID5+media+Covid.J+VISIT+Culture,
                           data = EAI2022.Jpn3, family = binomial("logit")))

sd(EAI2022.Jpn3$covid)

BIC(St.Korea1)

stargazer(St.Japan1,St.Japan1.1,
          keep.stat = c("n", "rsq","AIC", "BIC"),
          type = "text")

interact_plot(St.Japan2.1, pred= Favor, modx= Female, 
              interval = TRUE, int.type = 'confidence', int.width = 0.95)+
labs(x="Favorable opinion of South Korea", y="Predicted Probability")

## 시각화###
library(ggeffects)
ggpredict(St.Japan1, terms= "Favor") %>% 
  ggplot(mapping = aes(x = x, y=predicted))+
  geom_smooth(se =FALSE)+
  geom_ribbon(aes(ymin =conf.low, ymax=conf.high), alpha=.2)+
  scale_x_continuous(limits = c(1,5),breaks = c(1:5))+
  labs(x="Favorable opinion of South Korea", y="Predicted Probability")+
  theme_minimal()

library(margins)
library(prediction)

cdat <- cplot(St.Japan1, "EID5", what = "prediction",
              main = "Predicted Probability", draw = F)

ggplot(cdat, aes(x = xvals)) +
  geom_line(aes(y = yvals)) +
  geom_line(aes(y = upper), linetype = 2)+
  geom_line(aes(y = lower), linetype = 2) +
  labs(x = "Win-win",
       y = "Predicted Probability")+ 
   theme(text = element_text(size = 20))


#이분형KOrea#

summary(St.Korea1 <- glm(status2 ~  GENE+Female+Univ+Income3+
                           Favor+ EID5+
                           media+Culture+covid+
                           VISIT, 
                         data = EAI2022.Kor, family = binomial("logit")))


summary(St.Korea1.1 <- glm(status2 ~  GENE+Favor*Female+Univ+Income3+
                              EID5+Covid.K+
                             media+
                             Culture+VISIT, 
                           data = EAI2022.Kor, family = binomial("logit")))

summary(St.Korea2 <- glm(status2 ~ GENE+Female+Univ+Income3+
                           Favor+ EID5+
                           media+Culture+Covid.K+
                           VISIT, 
                         data = EAI2022.Kor3, family = binomial("logit")))

summary(St.Korea2.1 <- glm(status2 ~ GENE+Female+Univ+Income3+
                             Favor+ EID5+
                             media*Covid.K+Culture+VISIT, 
                           data = EAI2022.Kor, family = binomial("logit")))



## 시각화###
library(ggeffects)
ggpredict(St.Japan1, terms= "Favor") %>% 
  ggplot(mapping = aes(x = x, y=predicted))+
  geom_smooth(se =FALSE)+
  geom_ribbon(aes(ymin =conf.low, ymax=conf.high), alpha=.2)+
  scale_x_continuous(limits = c(1,5),breaks = c(1:5))+
  labs(x="Favorable opinion of South Korea", y="Predicted Probability")+
  theme_minimal()

library(margins)
library(prediction)

cdat <- cplot(St.Korea1, "covid", what = "prediction",
              main = "Predicted Probability", draw = F)

ggplot(cdat, aes(x = xvals)) +
  geom_line(aes(y = yvals)) +
  geom_line(aes(y = upper), linetype = 2)+
  geom_line(aes(y = lower), linetype = 2) +
  labs(x = "Covid Response",
       y = "Predicted Probability")+ 
  theme(text = element_text(size = 20))

lmtest::coeftest(St.Korea1)
exp(coef(St.Korea1))  %>% round(3)


ggcoef_model(St.Japan2, 
             add_reference_rows = TRUE,
             signif_stars = TRUE)

#stargazer보고하기#
stargazer(J.Recover2,
          type = "text",
          keep.stat = c("n", "rsq","AIC", "BIC"))

## 보고하기##
lmtest::coeftest(St.Japan1.1) 
exp(coef(St.Japan1.1))  %>% round(3)
DescTools::PseudoR2(St.Japan, which = "all") %>% round(3)

lmtest::coeftest(J.Recover2) 
exp(coef(J.Recover2))  %>% round(3)
DescTools::PseudoR2(J.Recover2, which = "all") %>% round(3)
