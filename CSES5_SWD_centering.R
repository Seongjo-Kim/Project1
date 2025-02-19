setwd("F://STAT_DATA/CSES5/")

rm(list = ls( ))

CSES5 <-read.csv("1356.csv", head=T, na.strings=c("NA", ""))

library(tidyverse)
library(poliscidata)
library(stargazer)
library(interactions)

library(MASS)

## 민주주의 만족도 #######################
freqC(CSES5$Q23)
CSES5$Sat.demo <- ifelse(CSES5$Q23 == 7 | CSES5$Q23 == 8, 
                         NA, 5 - CSES5$Q23)
freqC(CSES5$Sat.demo)
as.numeric(CSES5$Sat.demo) ->CSES5$Sat.demo4

CSES5$Sat.demo.ord <- as.ordered(CSES5$Sat.demo)

#이항변수화#
CSES5$SWD <- ifelse(CSES5$Sat.demo == 4 | CSES5$Sat.demo == 3, 1, 0)

freqC(CSES5$SWD)
# 여당의 정권담당능력# 
ifelse(CSES5$Q25_1 == 1 | CSES5$Q25_3 == 1, 1, 0) -> CSES5$Ruling.gov
freqC(CSES5$Ruling.gov)

# 야당의 정권담당능력# 
freqC(CSES5$Q25_8)
ifelse(CSES5$Q25_2 == 1 | CSES5$Q25_4 == 1 | CSES5$Q25_5 == 1 | 
         CSES5$Q25_6 == 1  | CSES5$Q25_7 == 1 | CSES5$Q25_8 == 1 | 
         CSES5$Q25_9 == 1, 1, 0) -> CSES5$Opp.gov
freqC(CSES5$Opp.gov)
class(CSES5$Opp.gov)

xtp (CSES5, Opp.gov, Ruling.gov)

## ctg##

#pr선거투표정당은##

CSES5$gov.ctg[CSES5$Ruling.gov==1&CSES5$Opp.gov==1] <- 'both' 
CSES5$gov.ctg[CSES5$Ruling.gov==1&CSES5$Opp.gov==0] <- 'onlyruling'
CSES5$gov.ctg[CSES5$Ruling.gov==0&CSES5$Opp.gov==1] <- 'onlyopp'
CSES5$gov.ctg[CSES5$Ruling.gov==0&CSES5$Opp.gov==0] <- 'neither'

freqC(CSES5$gov.ctg)

CSES5$gov.ctg2 <- relevel(as.factor(CSES5$gov.ctg), ref="onlyopp")

#whether vote or not ##
freqC(CSES5$Q1A) 

CSES5$Vote17 <- ifelse(CSES5$Q1A == 1 | CSES5$Q1A == 2, 1, 0)
freqC(CSES5$Vote17) 

#2017 PR vote#
freqC(CSES5$Q1C)

CSES5$Q1Cn = ifelse(CSES5$Q1C == 94 | CSES5$Q1C == 97 | CSES5$Q1C == 98| CSES5$Q1C == 99,
                    NA, CSES5$Q1C)

freqC(CSES5$Q1Cn)
CSES5$pr.winner = ifelse(CSES5$Q1Cn == '1'| CSES5$Q1Cn== '3', 1, 0)


freqC(CSES5$pr.winner)

as.factor(CSES5$pr.winner) -> CSES5$pr.winner2
ifelse(CSES5$pr.winner == '1', 'yes', 'No') -> CSES5$pr.winner3
class(CSES5$pr.winner3)
as.factor(CSES5$pr.winner3) -> CSES5$pr.winner4


#pr선거투표정당은##
freqC(CSES5$Q1Cn)
CSES5$voting.pr[CSES5$Q1Cn==1] <- 'LDP' 
CSES5$voting.pr[CSES5$Q1Cn==2] <- 'Hope'
CSES5$voting.pr[CSES5$Q1Cn==3] <- 'Komeito'
CSES5$voting.pr[CSES5$Q1Cn==4] <- 'JCP'
CSES5$voting.pr[CSES5$Q1Cn==5] <- 'CDP'
CSES5$voting.pr[CSES5$Q1Cn==6] <- 'JRP'
CSES5$voting.pr[CSES5$Q1Cn==7] <- 'SDP'
CSES5$voting.pr[CSES5$Q1Cn==8 | CSES5$Q1Cn ==9 |
                   CSES5$Q1Cn ==10] <- 'etc'


freqC(CSES5$voting.pr)



##2017 비례선거투표 기권포함##
CSES5$PW17 = ifelse(CSES5$Q1C == '1'| CSES5$Q1C== '3', 1, 0)
freqC(CSES5$PW17)

ifelse(CSES5$PW17 == '1', 'yes', 'No') -> CSES5$PW17.f
class(CSES5$PW17.f)
as.factor(CSES5$PW17.f) -> CSES5$Winner

#2017 SMD vote#
freqC(CSES5$Q1B)
CSES5$Q1Bn = ifelse(CSES5$Q1B == 94 | CSES5$Q1B == 97 | CSES5$Q1B == 98| CSES5$Q1B == 99,
                    NA, CSES5$Q1B)

CSES5$smd.winner = ifelse(CSES5$Q1Bn == '1'| CSES5$Q1Bn== '3', 1, 0)

CSES5$smd.winner2 = ifelse(CSES5$Q1B == '1'| CSES5$Q1B== '3', 1, 0)


##double winner##

CSES5 <- CSES5 %>% 
  mutate(vote_win = case_when( pr.winner =="1"& smd.winner=="1" ~ "Double Winner",
                               pr.winner =="1"| smd.winner=="1" ~ "Single Winner",
                               pr.winner =="0"& smd.winner=="0" ~ "Double loser"))
  
  

#2014 PR vote#
freqC(CSES5$Q2C)
CSES5$Q2Cn = ifelse(CSES5$Q2C == 94 | CSES5$Q2C == 97 | CSES5$Q2C == 98| CSES5$Q2C == 99,
                    NA, CSES5$Q2C)

CSES5$pr.winner2014 = ifelse(CSES5$Q2Cn == '1'| CSES5$Q2Cn== '4', 1, 0)


##성별##
CSES5$Female <- CSES5$Q48 -1

#교육3단계 졸업 수료 등 계산#
CSES5$Q49n = ifelse(CSES5$Q49 == 97,
                    NA, CSES5$Q49)

CSES5$Q49n -> CSES5$edu
freqC(CSES5$edu)

CSES5$Edu3[CSES5$Q49n == 1 | CSES5$Q49n == 2] <- '1' 
CSES5$Edu3[CSES5$Q49n == 3 | CSES5$Q49n == 4& 
             CSES5$Q50 == 2|CSES5$Q50 == 3] <- '1' 

CSES5$Edu3[CSES5$Q49n == 3 | CSES5$Q49n == 4& CSES5$Q50 == 1] <- '2' 
CSES5$Edu3[CSES5$Q49n == 5 & CSES5$Q50 == 1] <- '3' 
CSES5$Edu3[CSES5$Q49n == 6 | CSES5$Q49n == 7] <- '3' 

class(CSES5$Edu3) 
as.numeric(CSES5$Edu3) -> CSES5$EDU3
freqC(CSES5$EDU3)

##income##
freqC(CSES5$Q61)
CSES5$Q61n = ifelse(CSES5$Q61 ==94 | CSES5$Q61 == 97 |
                      CSES5$Q61 == 98,
                    NA, CSES5$Q61)
CSES5$Q61n -> CSES5$Income


##세대##
freqC(CSES5$Q47OLD)
CSES5$Q47OLD -> CSES5$Age

## Left-right ID##
freqC(CSES5$Q45)
CSES5$LR = ifelse(CSES5$Q22A == 94 | CSES5$Q22A == 95 |
                    CSES5$Q22A == 97 | CSES5$Q22A == 98 
                  ,NA, CSES5$Q22A)

#자민당 정당이 좌우에 어느정도 위칭하는지 평가#
CSES5$LR.LDP = ifelse(CSES5$Q21A == 94 | CSES5$Q21A == 95 |
                        CSES5$Q21A == 96 | CSES5$Q21A == 97 | CSES5$Q21A == 98 
                      ,NA, CSES5$Q21A)

freqC(CSES5$LR.LDP)

CSES5 %>% 
  filter(!is.na(LR)) %>% 
  filter(!is.na(LR.LDP)) %>% 
  group_by(LR) %>% 
  summarise(mean(LR.LDP))


#CDP 정당이 LR에 어느정도 위칭하는지 평가#
freqC(CSES5$Q21E)
CSES5$LR.CDP = ifelse(CSES5$Q44E == 95 |
                        CSES5$Q44E == 96 | CSES5$Q44E == 97 | CSES5$Q44E == 98 
                      ,NA, CSES5$Q44E)


#자민당 CDP의 거리#
CSES5$Dist.LR <- abs(CSES5$LR.LDP - CSES5$LR.CDP)


## Liberal-Con ID##
freqC(CSES5$Q45)
CSES5$LC = ifelse(CSES5$Q45 == 94 | CSES5$Q45 == 95 |
                    CSES5$Q45 == 97 | CSES5$Q45 == 98 
                  ,NA, CSES5$Q45)

freqC(CSES5$LC)
mean(CSES5$LC)
mean(CSES5$LC, na.rm = T) 

#정치이데올로기2#
cutpoints_lc = c(4,7)
CSES5$Idg3 = cut2(CSES5$LC, cutpoints_lc)
levels(CSES5$Idg3) = c("Progressive","Moderate","Conservative")
freq(CSES5$Idg3)
relevel(CSES5$Idg3, "Moderate") -> CSES5$Idg4

cutpoints_lc2 = c(5,6)
CSES5$Idg5 = cut2(CSES5$LC, cutpoints_lc2)
levels(CSES5$Idg5) = c("Progressive","Moderate","Conservative")
freq(CSES5$Idg5)
class(CSES5$Idg5)

#자민당 정당이 LC에 어느정도 위칭하는지 평가#
freqC(CSES5$Q44A)
CSES5$LC.LDP = ifelse(CSES5$Q44A == 95 |
                        CSES5$Q44A == 96 | CSES5$Q44A == 97 | CSES5$Q44A == 98 
                      ,NA, CSES5$Q44A)
freqC(CSES5$LC.LDP)
#komeito 정당이 LC에 어느정도 위칭하는지 평가#
freqC(CSES5$Q44C)
CSES5$LC.KMT = ifelse(CSES5$Q44C == 95 |
                        CSES5$Q44C == 96 | CSES5$Q44C == 97 | CSES5$Q44C == 98 
                      ,NA, CSES5$Q44C)


#CDP 정당이 LC에 어느정도 위칭하는지 평가#
freqC(CSES5$Q44E)
CSES5$LC.CDP = ifelse(CSES5$Q44E == 95 |
                        CSES5$Q44E == 96 | CSES5$Q44E == 97 | CSES5$Q44E == 98 
                      ,NA, CSES5$Q44E)

#abe LC에 어느정도 위칭하는지 평가#
freqC(CSES5$Q46)
CSES5$LC.ABE = ifelse(CSES5$Q46 == 95 |
                        CSES5$Q46 == 94 | CSES5$Q46 == 97 | CSES5$Q46 == 98 
                      ,NA, CSES5$Q46)

freqC(CSES5$LC.ABE)

#내각의 이념적 위치 방법1 캐비넷수 19:1##
CSES5$Cabinet <- (CSES5$LC.LDP*19 + CSES5$LC.KMT*1)/20

#캐비넷과 자신의 거리 ##
CSES5$Dist.c <- CSES5$Cabinet - CSES5$LC
freqC(CSES5$Dist.c)
CSES5$Dist.Abe <- CSES5$LC.ABE - CSES5$LC


plot(SWD~Dist.c,data=CSES5)
abs(CSES5$Dist.c) -> CSES5$Dist.c_ab


## gap between LDP and CDP##
abs(CSES5$LC.LDP - CSES5$LC.CDP) -> CSES5$Dist.gap
CSES5$LC.LDP - CSES5$LC.CDP -> CSES5$Dist.gap1

freqC(CSES5$Dist.gap1)

#정치관심도#
CSES5$IntPol = ifelse(CSES5$Q7 == 6 | CSES5$Q7 == 7 | CSES5$Q7 == 8,
                      NA,5- CSES5$Q7)
describe(CSES5$IntPol)


#정치정보획득하는데 얼마나 열심인가#
CSES5$Info = ifelse(CSES5$Q8 == 6 | CSES5$Q8 == 7 | CSES5$Q8 == 8,
                    NA, 5- CSES5$Q8)
describe(CSES5$Info)

#정치쟁점을잘알고있니#
CSES5$Int.efficacy = ifelse(CSES5$Q9 == 6 | CSES5$Q9 == 7 | CSES5$Q9 == 8,
                            NA,6- CSES5$Q9)


#정치지식테스트#
freqC(CSES5$Q41)
CSES5$Q41n2 = ifelse(CSES5$Q41 == 1,
                     1, 0)
describe(CSES5$Q41n2)

freqC(CSES5$Q42)
CSES5$Q42n2 = ifelse(CSES5$Q42 == 1,
                     1, 0)
describe(CSES5$Q42n2)


freqC(CSES5$Q43)
CSES5$Q43n2 = ifelse(CSES5$Q43 == 1,
                     1, 0)
describe(CSES5$Q43n2)
CSES5$Pol.Knowledge = (CSES5$Q41n2 + CSES5$Q42n2 + CSES5$Q43n2)/3
psych::describe(CSES5$Pol.Knowledge)

#정치가불신뢰#
freqC(CSES5$Q10C)
CSES5$UnTrust_p = ifelse(CSES5$Q10C == 7 | CSES5$Q10C == 8 | CSES5$Q10C == 94 ,
                         NA, CSES5$Q10C)
6-CSES5$UnTrust_p -> CSES5$Trust_p

#gov.efficacy#
freqC(CSES5$Q18A)
CSES5$gov.efficacy = ifelse(CSES5$Q18A == 97 | CSES5$Q18A == 98, 
                            NA, CSES5$Q18A)
describe(CSES5$gov.efficacy)

#vote.effi#
freqC(CSES5$Q18B)
CSES5$vote.efficacy = ifelse(CSES5$Q18B == 97 | CSES5$Q18B == 98, 
                             NA, CSES5$Q18B)

                           
#정당일체감#
freqC(CSES5$Q24C)
CSES5$Q24Cn = ifelse(CSES5$Q24C ==94 | CSES5$Q24C == 97 |
                       CSES5$Q24C == 98,
                     NA, CSES5$Q24C)

CSES5$party[CSES5$Q24Cn==1] <- 'LDP' 
CSES5$party[CSES5$Q24Cn==2] <- 'Hope' 
CSES5$party[CSES5$Q24Cn==3] <- 'KMT'
CSES5$party[CSES5$Q24Cn==4] <- 'JCP'
CSES5$party[CSES5$Q24Cn==5] <- 'CDP'
CSES5$party[CSES5$Q24Cn==6] <- 'JRP'
CSES5$party[CSES5$Q24Cn==99] <- 'Ind'
CSES5$party[CSES5$Q24Cn==7 | CSES5$Q24Cn==8 | 
              CSES5$Q24Cn==9 |CSES5$Q24Cn== 11  ] <- 'etc'
CSES5$Party.id = as.factor(CSES5$party)
CSES5$PartyID <- relevel(CSES5$Party.id, "JCP")
CSES5$PartyID2 <- relevel(CSES5$Party.id, "LDP")
freqC(CSES5$PartyID)

xtp(CSES5, voting.pr, PartyID)

#정당일체감3단계#
CSES5$party3[CSES5$Q24Cn==1 |CSES5$Q24Cn==3] <- 'Coalition' 
CSES5$party3[CSES5$Q24Cn==2 | CSES5$Q24Cn==4 | CSES5$Q24Cn==5|
               CSES5$Q24Cn==6 |CSES5$Q24Cn==7 | CSES5$Q24Cn==8 | 
               CSES5$Q24Cn==9 |CSES5$Q24Cn== 10|CSES5$Q24Cn== 11  ] <- 'Opposition' 
CSES5$party3[CSES5$Q24Cn==99] <- 'Ind'
as.factor(CSES5$party3)-> CSES5$Party3
relevel(CSES5$Party3, "Coalition" ) -> CSES5$PartyID4

##내가찍은정당이집권능력이있는가##

ifelse(CSES5$Q1Cn == '1'& CSES5$Q25_1== '1', 1,
       ifelse(CSES5$Q1Cn == '2' & CSES5$Q25_2=='1', 1,
              ifelse(CSES5$Q1Cn == '3' & CSES5$Q25_3=='1',1,
                     ifelse(CSES5$Q1Cn == '4' & CSES5$Q25_4=='1', 1,
                            ifelse(CSES5$Q1Cn == '5' & CSES5$Q25_5=='1', 1,
                                   ifelse(CSES5$Q1Cn == '6' & CSES5$Q25_6=='1', 1,
                                          ifelse(CSES5$Q1Cn == '7' & CSES5$Q25_7=='1', 1,
                                                 ifelse(CSES5$Q1Cn == '8' | 
                                                          CSES5$Q1Cn == '9'| 
                                                          CSES5$Q1Cn == '10' & CSES5$Q25_8=='1', 1,0)))))))) ->CSES5$Gov.Con2


                                   

freqC(CSES5$Gov.Con2)
as.factor(CSES5$Gov.Con2) -> CSES5$V_gov 

ifelse(CSES5$Gov.Con2 == '1', 'yes', 'No') -> CSES5$Gov.Con3
as.factor(CSES5$Gov.Con3) -> CSES5$Gov.Con4

head(CSES5$Q44B)


##stat##

CSES5 %>% 
  count(vote_win)

CSES5 %>% 
  filter(!is.na(SWD)) %>% 
  filter(!is.na(vote_win)) %>% 
  group_by(vote_win) %>% 
  summarise(mean(SWD))


mean(CSES5$LC, na.rm=T)
mean(CSES5$LC.LDP, na.rm=T)
mean(CSES5$LC.CDP, na.rm=T)
mean(CSES5$Cabinet, na.rm=T)
mean(CSES5$LC.ABE, na.rm=T)

CSES5 %>% 
  filter(!is.na(SWD)) %>% 
  filter(!is.na(Dist.c)) %>% 
  group_by(Dist.c) %>% 
  summarise(mean(SWD))

qplot(CSES5$Dist.c, Sat.demo4)

library(ggplot2)
ggplot(CSES5, aes(x=CSES5$LC, y=CSES5$Sat.demo4)) + theme_bw() +
  geom_point(color="skyblue", size=2, shape=20) + ggtitle("Box Office") +
  geom_smooth(method=lm, fullrange=T) + xlab("Dist.c") + ylab("Sat.demo4") +
  geom_jitter()


CSES5 %>% 
  filter(!is.na(SWD)) %>% 
  filter(!is.na(SIZE)) %>% 
  group_by(SIZE) %>% 
  summarise(mean(SWD))


CSES5 %>% 
  filter(!is.na(LC)) %>% 
  filter(!is.na(LC.LDP)) %>% 
  group_by(LC) %>% 
  summarise(mean(LC.LDP))

CSES5 %>% 
  group_by(voting.pr) %>% 
  summarise(M_SWD=mean(SWD, na.rm = T)) %>% 
  drop_na() %>% 
  ggplot(aes(x=voting.pr, y=M_SWD))+
  geom_bar(stat="identity")+
  labs(x='vote_PR',y='SWD') 

CSES5 %>% 
  filter(!is.na(SWD)) %>% 
  group_by(voting.pr) %>% 
  summarise(M_SWD=mean(SWD, na.rm = T)) %>% 
  drop_na() %>% 
  ggplot(aes(x=reorder(voting.pr, -M_SWD), y=M_SWD, fill = M_SWD))+
  geom_bar(stat="identity")+
  labs(x='vote_PR',y='SWD') 


CSES5 %>% 
  filter(!is.na(SWD)) %>% 
  group_by(voting.pr) %>% 
  summarise(mean(SWD))

freqC(CSES5$gov.ctg)

CSES5 %>% 
  filter(!is.na(SWD)) %>% 
  group_by(PartyID) %>% 
  summarise(mean(SWD))

CSES5 %>% 
  filter(!is.na(SWD)) %>% 
  filter(PartyID == "LDP") %>% 
  group_by(voting.pr) %>% 
  summarise(mean(SWD))


CSES5 %>% 
  filter(!is.na(Gov.Con2)) %>% 
  group_by(LC) %>% 
  summarise(mean(Gov.Con2))

xtp(CSES5, LC, voting.pr)


japandem2 %>% 
  filter(!is.na(Dist)) %>% 
  group_by(pr.winner) %>% 
  summarise(mean(Dist))

####필요한항목만###

japandem <- CSES5 %>% dplyr::select(SWD, pr.winner, Female, Age, Income, EDU3,
                                    LC.LDP, LC,Info,
                                    Pol.Knowledge,IntPol,
                                    Int.efficacy,gov.efficacy, vote.efficacy,
                                    Gov.Con2, Ruling.gov, Opp.gov)

jswd <- CSES5 %>% dplyr::select(SWD, PW17, Dist.gap, Dist.gap1,
                                LC, Gov.Con2,Gov.Con4, 
                                Ruling.gov,  
                                Female, Age, Income, EDU3, SIZE, Vote17,
                                vote.efficacy)



jswd_DS <- CSES5 %>% dplyr::select(SWD, PW17, Dist.gap, 
                                LC, 
                                Ruling.gov,  
                                Female, Age, Income, EDU3, SIZE, Vote17,
                                vote.efficacy)


jswd_a <- CSES5 %>% dplyr::select(SWD, PW17, vote_win, LR, Dist.LR, Gov.Con2,Gov.Con4, 
                                Ruling.gov,  
                                Female, Age, Income, EDU3, SIZE, 
                                vote.efficacy)




japandem2 <- japandem %>% na.omit(japandem)

jswd_DS2 <- jswd_DS %>% na.omit(jswd_DS)
stargazer(jswd_DS2, type = "text", title="Descriptive statistics2", digits=3, out="table1.txt")

jswd_b <- jswd_a %>% na.omit(jswd_a)

jswd4 <- jswd3 %>% na.omit(jswd3)

stargazer(jswd2, type = "text", title="Descriptive statistics_SWD", 
          digits=3, out="table1.txt")

## 중심평균화 전자료##

##중심평균화 진행##

#평균화#
jswd_ct <- jswd4 %>% 
  mutate(across(
    .cols=c(Dist.gap, Dist.gap1, LC, Age, Income,EDU3, SIZE,vote.efficacy),
    .fns=function(x){x-mean(x, na.rm = T)}
  ))


jswd_ct2 <- jswd %>% 
  mutate(across(
    .cols=c(Dist.gap, Dist.gap1, LC, Age, Income,EDU3, SIZE,vote.efficacy),
    .fns=function(x){x-mean(x, na.rm = T)}
  ))
stargazer(jswd, type = "text", title="Descriptive statistics_ct2", digits=3, out="table1.txt")



jswd_ct3 <- jswd_b %>% 
  mutate(across(
    .cols=c(Dist.LR, LR, Age, Income,EDU3, SIZE,vote.efficacy),
    .fns=function(x){x-mean(x, na.rm = T)}
  ))

jswd_a

###b1.1를 중심으로 볼것 ########


summary(ld1 <- lm(Sat.demo4 ~ Female + Age.c + Income + EDU3 +
                    vote_win+ LC+IntPol+vote.efficacy+Trust_p, data=CSES5))
## correlation####
car::vif(ld1)
install.packages("corrr")
library('corrr')

jswd_ct %>% 
  dplyr::select (PW17,Dist.c_ab, Dist.gap, Dist.gap1, LC, 
                 Ruling.gov, Opp.gov) %>% 
  correlate(use ="complete.obs")


### binary CSES###

###How can I export regression outputs to CSV or XLSX files ####
model_summary <- summary(b1)
coefficients <- as.data.frame(model_summary$coefficients)
write.csv(coefficients, "regression_results2.csv", row.names = TRUE)
# Additional model metrics
num_observations <- nobs(b1)
metrics <- data.frame(
  Metric = c("Observations", "log_likelihood", "AIC","BIC", "Null Deviance", 
              "Residual Deviance"),
  Value = c( nobs(b1), logLik(b1), AIC(b1), BIC(b1), 
             model_summary$null.deviance, model_summary$deviance)
)

# Save metrics to a CSV file
write.csv(metrics, "regression_metrics2.csv", row.names = FALSE)

# Combine coefficients and metrics into a list
results_list <- list(
  Coefficients = coefficients,
  Metrics = metrics
)

# Save as separate sheets in a single Excel file (requires `writexl` or `openxlsx` package)
library(writexl)
write_xlsx(results_list, "regression_results.xlsx")

summary(m1 <-  glm(SWD~ pr.winner+LC+People_Decision+
                       Female + Age.c + Income + EDU3+SIZE,
                     data=CSES5, family = binomial(), na.action = na.omit))


summary(m2 <-  glm(SWD~ PW17+LC+People_Decision+Ruling.gov+Opp.gov+
                     Female + Age.c + Income + EDU3 +SIZE,
                   data=CSES5, family = binomial(), na.action = na.omit))


summary(m3 <-  glm(SWD~PW17+LC+People_Decision+Gov.Con4+
                       Female + Age.c + Income + EDU3+SIZE,
            data=CSES5, family = binomial(), na.action = na.omit))


summary(m4 <-  glm(SWD~ PW17+LC*People_Decision+Ruling.gov+Opp.gov+Vote17+
                    Female + Age.c + Income + EDU3 +SIZE,
                     data=CSES5, family = binomial(), na.action = na.omit))


summary(m5 <-  glm(SWD~ PW17*Gov.Con4+LC+People_Decision+
                     Female + Age.c + Income + EDU3 +SIZE,
                   data=CSES5, family = binomial(), na.action = na.omit))



########################


summary(v0 <-  glm(SWD~ PW17+LC+
                     Female + Age + Income + EDU3+SIZE+
                     Vote17+vote.efficacy,
                   data=jswd_ct2, family = binomial(), na.action = na.omit))


summary(v1 <-  glm(SWD~ PW17+Ruling.gov+Dist.gap+
                     LC+Female + Age + Income + EDU3+SIZE+
                     Vote17+vote.efficacy,
                   data=jswd_ct2, family = binomial(), na.action = na.omit))

summary(v2 <-  glm(SWD~PW17*Ruling.gov+Dist.gap+LC+
                     Female + Age + Income + EDU3+
                     SIZE+Vote17+vote.efficacy,
                   data=jswd_ct2, family = binomial(), na.action = na.omit))


summary(v2.1 <-  glm(SWD~PW17+LC+SIZE*Ruling.gov+Dist.gap+
                       Female + Age + Income + EDU3+
                       Vote17+vote.efficacy,
                     data=jswd_ct2, family = binomial(), na.action = na.omit))

summary(v3 <-  glm(SWD~ PW17+Gov.Con4+Dist.gap+ LC+
                     Female + Age + Income + EDU3+SIZE+
                     vote.efficacy,
                   data=jswd_ct2, family = binomial(), na.action = na.omit))


summary(v4.1 <-  glm(SWD~ PW17+LC*Gov.Con4+Dist.gap+ 
                       Female + Age + Income + EDU3+SIZE+
                       vote.efficacy,
                     data=jswd_ct2, family = binomial(), na.action = na.omit))


summary(v4 <-  glm(SWD~ PW17*Gov.Con4+Dist.gap+ LC+
                     Female + Age + Income + EDU3+SIZE+
                     vote.efficacy,
                   data=jswd_ct2, family = binomial(), na.action = na.omit))



stargazer(v0,v1,v2,v3,v4,
          type = "text",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))

output0218 <- capture.output(stargazer(v0,v1,v2,v3,v4,
                                    type = "text",
                                    star.char = c("*", "**", "***"),
                                    star.cutoffs = c(.05, .01, .001)))



writeLines(output0218, "SWD_table2.csv")


# 로그우도 카이제곱 테스트: v1 v2 v3

(LL_chi5=v2$null.deviance - v2$deviance)
(LL_df=v2$df.null - v2$df.residual)
pchisq(LL_chi5,LL_df,lower.tail=F)

# 임의 R2
DescTools::PseudoR2(v4,which="all") %>% round(3)
DescTools::PseudoR2(v7,which="all") %>% round(3)



pseudoR2(v1)

bic_values <- c(round(BIC(v2), 3), round(BIC(v3), 3), round(BIC(v4), 3), 
                round(BIC(v5), 3), round(BIC(v6), 3), round(BIC(v7), 3))

print(bic_values)
################################# Double winner ###



summary(v0 <-  glm(SWD~ vote_win+LC+ 
                     Female + Age + Income + EDU3+SIZE+
                     vote.efficacy,
                   data=jswd_ct2, family = binomial(), na.action = na.omit))


summary(v1 <-  glm(SWD~ vote_win+LC+Dist.gap+ Ruling.gov+
                     Female + Age + Income + EDU3+SIZE+
                     vote.efficacy,
                   data=jswd_ct2, family = binomial(), na.action = na.omit))


summary(v1.0 <-  glm(SWD~ vote_win+LR+Dist.LR+ Ruling.gov+ 
                     Female + Age + Income + EDU3+SIZE+
                     vote.efficacy,
                   data=jswd_ct2, family = binomial(), na.action = na.omit))


summary(v1.1 <-  glm(SWD~ vote_win+LC+Dist.gap+ Ruling.gov*SIZE+
                     Female + Age + Income + EDU3+
                     vote.efficacy,
                   data=jswd_ct2, family = binomial(), na.action = na.omit))

summary(v2 <-  glm(SWD~ vote_win*Ruling.gov+LC+Dist.gap+ 
                     Female + Age + Income + EDU3+SIZE+
                     vote.efficacy,
                   data=jswd_ct2, family = binomial(), na.action = na.omit))


summary(v3 <-  glm(SWD~ vote_win+LC+Dist.gap++ Gov.Con2+
                     Female + Age + Income + EDU3+SIZE+
                     vote.efficacy,
                   data=jswd_ct2, family = binomial(), na.action = na.omit))

summary(v4 <-  glm(SWD~ vote_win*Gov.Con2+LC+Dist.gap+ 
                     Female + Age + Income + EDU3+SIZE+
                     vote.efficacy,
                   data=jswd_ct2, family = binomial(), na.action = na.omit))




stargazer(v0,v1,v2,v3,v4,
          type = "text",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))

output4 <- capture.output(stargazer(v1,v2,v3,v4,v5,v6,
                                    type = "text",
                                    star.char = c("*", "**", "***"),
                                    star.cutoffs = c(.05, .01, .001)))
## 과산포##
pchisq(q=1046.21  -920.18, df=10, lower.tail=FALSE)
deviance(v3)/df.residual(v3)




stargazer(v1,v2,v3,v4,
          type = "text",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))

output4 <- capture.output(stargazer(v1,v2,v3,v4,v5,v6,
                                    type = "text",
                                              star.char = c("*", "**", "***"),
                                              star.cutoffs = c(.05, .01, .001)))


writeLines(output4, "Table0204_v2.csv")


exp(1.005715)

stargazer(model1, model2, model3, model4,
          type = "text",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))



pseudoR2(v1)

bic_values <- c(round(BIC(v2), 3), round(BIC(v3), 3), round(BIC(v4), 3), 
                round(BIC(v5), 3), round(BIC(v6), 3), round(BIC(v7), 3))

print(bic_values)


# 로그우도 카이제곱 테스트: v1 v2 v3

(LL_chi5=v2$null.deviance - v2$deviance)
(LL_df=v2$df.null - v2$df.residual)
pchisq(LL_chi5,LL_df,lower.tail=F)

# 임의 R2
DescTools::PseudoR2(v2,which="all") %>% round(3)
DescTools::PseudoR2(v7,which="all") %>% round(3)

#상호작용그림#

interactions::interact_plot(v3, pred= LC , modx=  Dist.gap,
                            interval = T, int.type =  c("confidence", "prediction"),
                            int.width = 0.95) 

interactions::interact_plot(v7, pred=  SIZE   , modx= PW17 ,
                            interval = T, int.type =  c("confidence", "prediction"),
                            int.width = 0.95) 


interact_plot(v5, pred= Dist.gap1, modx= LC, 
              interval = TRUE, int.type = 'confidence', int.width = 0.95)+
  labs(x="Perceived Gap between LDP and CDP", y="Predicted Probability")


interact_plot(v5, pred= LC, modx= Dist.gap1, 
              interval = TRUE, int.type = 'confidence', int.width = 0.95)+
  labs(x="Liberal_Conservative", y="Predicted Probability", 
       modx.label="Perceived Gap")


interact_plot(v5, pred= LC, modx= Dist.gap, 
              interval = TRUE, int.type = 'confidence', int.width = 0.95)+
  labs(x="Liberal_Conservative", y="Predicted Probability", 
       modx.label="Perceived Gap")



interact_plot(v5, pred= Dist.gap , modx=LC , 
              interval = TRUE, int.type = 'confidence', int.width = 0.95)+
  labs(x="Dist.gap", y="Predicted Probability", 
       modx.label="LC")



##보고##

# Calculate BIC
bic_values <- c(round(BIC(m1), 3), round(BIC(m2), 3), round(BIC(m3), 3), 
                round(BIC(m4), 3), round(BIC(m5), 3))

print(bic_values)

stargazer(m1,m2, m3, m4, m5,
          type = "text",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))

stargazer_output3 <- capture.output(stargazer(m1,m2, m3, m4, m5, 
                                             type = "text",
                                             star.char = c("*", "**", "***"),
                                             star.cutoffs = c(.05, .01, .001)))
  
  
writeLines(stargazer_output3, "stargazer_table3.csv")



##시각화 방법1:이것이 더 깔끔함##


library(margins)
library(prediction)

cdat <- cplot(v2, "LC", what = "prediction",
              main = "Predicted Probability", draw = F)

ggplot(cdat, aes(x = xvals)) +
  geom_line(aes(y = yvals)) +
  geom_line(aes(y = upper), linetype = 2)+
  geom_line(aes(y = lower), linetype = 2) +
  labs(x = "Liberal-Conservative",
       y = "Predicted Probability")+ 
  theme(text = element_text(size = 15))

##
cdat2 <- cplot(v1, "LC", what = "prediction",
              main = "Predicted Probability", draw = F)

ggplot(cdat2, aes(x = xvals)) +
  geom_line(aes(y = yvals)) +
  geom_line(aes(y = upper), linetype = 2)+
  geom_line(aes(y = lower), linetype = 2) +
  labs(x = "Liberal_Conservative",
       y = "Predicted Probability")+ 
  theme(text = element_text(size = 15))

## 시각화2###
library(ggeffects)
ggpredict(v2, terms= "Dist.gap1") %>% 
  ggplot(mapping = aes(x = x, y=predicted))+
  geom_smooth(se =FALSE)+
  geom_ribbon(aes(ymin =conf.low, ymax=conf.high), alpha=.2)+
  scale_x_continuous(limits = c(1,7),breaks = c(1:7))+
  labs(x="Perceived_Gap", y="Predicted Probability")+
  theme_minimal()


# 필요한 패키지 설치 및 로드

# ggeffects로 상호작용 효과 계산


# 두 설명변수가 모두 범주형인 상호작용 효과 시각화#

ggpredict(v5, terms = c("Gov.Con4", "PW17")) %>% plot()+
  labs(x="Party_Governing", y="Predicted Probability")

interaction_effect <- ggpredict(v5, terms = c( "PW17", "Gov.Con4"))

ggpredict(v5, terms = c("Gov.Con4", "PW17")) %>% plot()+
ggplot(interaction_effect, aes(x = x, y = predicted, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.3), width = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(width = 0.3), width = 0.1) +
  labs(title = "Interaction Effect",
    x = "Winner",
    y = "Predicted Probability",
    fill = "Party_Governing"
  ) +
  theme_minimal()


# 두 설명변수가 모두 범주형인 상호작용 효과 시각화2#
ggplot(interaction_effect, aes(x = x, y = predicted, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.3), width = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(width = 0.3), width = 0.1) +
  labs(
    title = "Interaction Effect: group1 * group2",
    x = "Winner",
    y = "Predicted Probability",
    fill = "governing capability"
  ) +
  theme_minimal()

ggpredict(interaction_effect3, terms = c("pr.winner[0:15 by = 5]",
                                         "infant_quantiles")) %>%
  plot()



exp(coef(v5))  %>% round(3)




################# ordinal ######################

summary(ord1 <-  polr(Sat.demo.ord~ vote_win+LC++
                     Female + Age.c + Income + EDU3+SIZE+
                     vote.efficacy,
                   data=CSES5, Hess=TRUE))

lmtest::coeftest(ord1) %>% round(3)
exp(coef(ord1))  %>% round(4)
DescTools::PseudoR2(ord1, which = "all") %>% round(4)



summary(ord2 <-  polr(Sat.demo.ord~ vote_win+LC+Ruling.gov+
                     Female + Age.c + Income + EDU3 +SIZE+
                     vote.efficacy,     data=CSES5, Hess=TRUE))

summary(ord3 <-  polr(Sat.demo.ord~ vote_win+LC+Opp.gov+
                     Female + Age.c + Income + EDU3 +SIZE+
                     vote.efficacy,
                   data=CSES5, Hess=TRUE))

summary(ord4<-  polr(Sat.demo.ord~ vote_win*Ruling.gov+LC+
                    Female + Age.c + Income + EDU3 +SIZE+vote.efficacy,
                  data=CSES5, Hess=TRUE))

summary(ord5 <-  polr(Sat.demo.ord~vote_win+LC+Gov.Con4+
                     Female + Age.c + Income + EDU3+SIZE+vote.efficacy,
                   data=CSES5, Hess=TRUE))


summary(ord6 <-  polr(Sat.demo.ord~ vote_win*Gov.Con4+LC+
                     Female + Age.c + Income + EDU3 +SIZE+vote.efficacy,
                   data=CSES5, Hess=TRUE))


summary(ord7 <-  polr(Sat.demo.ord~ vote_win+SIZE*Ruling.gov +LC+
                        Female + Age.c + Income + EDU3 +vote.efficacy,
                      data=CSES5, Hess=TRUE))


stargazer(ord1, ord2, ord3, ord4, ord5, ord6,ord7,
          type = "text",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))


### binary###
###bi.swd2 & bi.swd2.1를 중심으로 볼것 ########

summary(bi.swd0 <-  glm(SWD~ Female + Age.c + Income + EDU3 +
                          pr.winner+smd.winner, 
                        data=japandem2, family = binomial(), na.action = na.omit))

summary(bi.swd0.1 <-  glm(SWD~ Female + Age.c + Income + EDU3 +
                          pr.winner+Ruling.gov+Opp.gov, 
                        data=CSES5, family = binomial(), na.action = na.omit))


summary(bi.swd0.2 <-  glm(SWD~ pr.winner+Gov.Con2+
                            Female + Age.c + Income + EDU3, 
                        data=CSES5, family = binomial(), na.action = na.omit))

summary(bi.swd0.3<-  glm(SWD~ Female + Age.c + Income + EDU3 +
                          pr.winner+Ruling.gov+Opp.gov+Pol.Knowledge, 
                        data=japandem2, family = binomial(), na.action = na.omit))

summary(bi.swd1 <-  glm(SWD~ pr.winner+Gov.Con2+
                          Female + Age.c + Income + EDU3,
                          data=japandem2, family = binomial(), na.action = na.omit))

summary(bi.swd2 <-  glm(SWD~ pr.winner+Gov.Con2+Dist.c+
                          Female + Age.c + Income + EDU3,
                        data=japandem2, family = binomial(), na.action = na.omit))

summary(bi.swd2.0 <-  glm(SWD~ pr.winner*(Gov.Con2+Dist2)+
                          Female + Age.c + Income + EDU3,
                        data=japandem2, family = binomial(), na.action = na.omit))

summary(bi.swd2.1.1 <-  glm(SWD~ pr.winner*Gov.Con2+
                            Female + Age.c + Income + EDU3+
                            vote.efficacy,
                          data=jswd2, family = binomial(), na.action = na.omit))


summary(bi.swd2.2 <-  glm(SWD~ pr.winner*Opp.gov+Ruling.gov+Dist+
                            Female + Age.c + Income + EDU3,
                        data=japandem2, family = binomial(), na.action = na.omit))

summary(bi.swd2.3 <-  glm(SWD~ pr.winner*Dist+Gov.Con2+
                            Female + Age.c + Income + EDU3,
                          data=japandem2, family = binomial(), na.action = na.omit))


summary(bi.swd3 <-  glm(SWD~ Female + Age.c + Income + EDU3 +
                          Pol.Knowledge+vote.efficacy+Dist.LC2 +IntPol+PartyID4+
                          pr.winner*Gov.Con2, 
                        data=japandem2, family = binomial(), na.action = na.omit))



summary(bi.swd4 <-  glm(SWD~ Female + Age.c + Income + EDU3 +
                          pr.winner*Opp.gov+Ruling.gov, 
                          data=japandem2, family = binomial(), na.action = na.omit))


summary(bi.swd4.1<-  glm(SWD~ Female + Age.c + Income + EDU3 +Pol.Knowledge+
                            pr.winner*(Opp.gov+Ruling.gov+Dist), 
                        data=japandem2, family = binomial(), na.action = na.omit))


summary(bi.swd5 <-  glm(SWD~ Female + Age.c + Income + EDU3 +
                          pr.winner*Dist +
                          pr.winner*Gov.Con2, 
                        data=CSES5, family = binomial(), na.action = na.omit))


summary(bi.swd6 <-  glm(SWD~ pr.winner*Gov.Con2+
                            Female + Age.c + Income + EDU3,
                          data=jswd2, family = binomial(), na.action = na.omit))

summary(bi.swd6 <-  glm(SWD~ pr.winner*(Opp.gov+Ruling.gov)+
                          Female + Age.c + Income + EDU3,
                        data=jswd2, family = binomial(), na.action = na.omit))


#상호작용그림#

interactions::interact_plot(bi.swd2.0, pred= Gov.Con2, modx= pr.winner ,
                            interval = T, int.type =  c("confidence", "prediction"),
                            int.width = 0.95) 

interactions::interact_plot(b0, pred=  pr.winner , modx=  smd.winner,
                            interval = T, int.type =  c("confidence", "prediction"),
                            int.width = 0.95) 






##
# 민주주의만족도# 

summary(sat.demo1 <- polr(Sat.demo.ord ~ pr.winner+Gov.Con2+Dist+
                            Female + Age.c + Income + Education, 
                          data=japandem2, Hess=TRUE))

summary(sat.demo2<- polr(Sat.demo.ord ~ vote_win*Gov.Con2+LC+
                           Female + Age.c + Income + EDU3+
                           vote.efficacy,
                         data=CSES5, Hess=TRUE))

summary(sat.demo3 <- polr(Sat.demo.ord ~ pr.winner*(Gov.Con2+Dist)+
                            Female + Age.c + Income + Education, 
                          data=japandem2, Hess=TRUE))


## 보고하기##
lmtest::coeftest(sat.demo2) %>% round(3)
exp(coef(sat.demo5))  %>% round(4)
DescTools::PseudoR2(sat.demo5, which = "all") %>% round(4)


##여당만 돌려보기##
ruling.dem <- CSES5 %>% 
  filter((pr.winner == '1'))

summary(rule1 <-  glm(SWD~ Female + Age.c + Income + EDU3 +LC+
                      LC+Ruling.gov+vote_win+
                      vote.efficacy,
                    data=ruling.dem, family = binomial(), na.action = na.omit))

summary(rule2 <-  glm(SWD~ Female + Age.c + Income + EDU3 +
                        LC+Opp.gov+vote_win+
                        vote.efficacy,
                      data=ruling.dem, family = binomial(), na.action = na.omit))


summary(rule3 <-  glm(SWD~ Female + Age.c + Income + EDU3 + SIZE+
                        LC*Opp.gov+vote_win+Pol.Knowledge+
                        IntPol+gov.efficacy+ vote.efficacy,
                      data=ruling.dem, family = binomial(), na.action = na.omit))

interactions::interact_plot(rule3, pred=  LC , modx=  Opp.gov,
                            interval = T, int.type =  c("confidence", "prediction"),
                            int.width = 0.95) 


##야당만 돌려보기##

opp.dem <- CSES5 %>% 
  filter((pr.winner == '0' ))

opp.dem$voting.pr2 <- relevel(as.factor(opp.dem$voting.pr), ref="JCP")

freqC(opp.dem$voting.pr)
xtp(opp.dem,  Gov.Con4, voting.pr)

xtp(opp.dem,voting.pr, LC)


opp.dem %>% 
  filter(!is.na(SWD)) %>% 
  group_by(LC) %>% 
  summarise(mean(SWD))

opp.dem %>% 
  filter(!is.na(SWD)) %>% 
  group_by(LC) %>% 
  summarise(mean(Gov.Con2))


opp.dem %>% 
  filter(voting.pr2 == "JRP") %>% 
  filter(!is.na(SWD)) %>% 
  group_by(LC) %>% 
  summarise(mean(SWD))


opp.dem %>% 
  filter(voting.pr2 == "Hope") %>% 
  filter(!is.na(SWD)) %>% 
  count(LC)



ifelse(opp.dem$voting.pr2 == "CDP" | opp.dem$voting.pr2 == "JCP" | 
         opp.dem$voting.pr2 == "SDP"|opp.dem$voting.pr2 == "etc", 0, 1) -> opp.dem$voting.pr3

opp.dem %>% 
  filter(!is.na(LR)) %>% 
  group_by(voting.pr3) %>% 
  summarise(mean(LR))



summary(op1 <-  glm(SWD~ Female + Age.c + Income + EDU3 +PartyID+
                      LC+Ruling.gov+vote_win+gov.efficacy+
                      vote.efficacy+Trust_p+Info+IntPol,
                    data=opp.dem, family = binomial(), na.action = na.omit))

summary(op1.1 <-  glm(SWD~ Female + Age.c + Income + EDU3 + Ruling.gov+vote.efficacy+
                        voting.pr2*Gov.Con4, 
                    data=opp.dem, family = binomial(), na.action = na.omit))

summary(op1.2 <-  glm(SWD~ Female + Age.c + Income + EDU3 + 
                        Gov.Con4*voting.pr3,
                      data=opp.dem, family = binomial(), na.action = na.omit))


 summary(op2 <-  glm(SWD~ Female + Age.c + Income + EDU3 +
                       Dist.c*Gov.Con4+vote_win, 
                    data=opp.dem, family = binomial(), na.action = na.omit))

 summary(op2.1 <-  glm(SWD~ Female + Age.c + Income + EDU3 + LC+
                       Dist.c*Gov.Con4+Ruling.gov+vote_win+IntPol+vote.efficacy, 
                     data=opp.dem, family = binomial(), na.action = na.omit))
 
 summary(op2.2 <-  glm(SWD~ Female + Age.c + Income + EDU3 +
                        LC+Gov.Con4*vote_win, 
                      data=opp.dem, family = binomial(), na.action = na.omit))

 
 summary(op2.3 <-  glm(SWD~ Female + Age.c + Income + EDU3 +
                         Gov.Con4*(voting.pr +Dist.c)+vote_win, 
                       data=opp.dem, family = binomial(), na.action = na.omit))
 
 
freqC(opp.dem$LC)
freqC(opp.dem$Dist.c)


#상호작용그림#

interactions::interact_plot(op2, pred= Dist.c , modx=  Gov.Con4,
                            interval = T, int.type =  c("confidence", "prediction"),
                            int.width = 0.95) 



interactions::interact_plot(op2.1, pred= Dist.c , modx=  Gov.Con4,
                            interval = T, int.type =  c("confidence", "prediction"),
                            int.width = 0.95) 



interactions::interact_plot(op2.3, pred= Dist.c , modx=  Gov.Con4,
                            interval = T, int.type =  c("confidence", "prediction"),
                            int.width = 0.95) 

ggpredict(op2.1, terms = c("Gov.Con4", "Ruling.gov")) %>% plot()
