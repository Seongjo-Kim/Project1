

# ================================================================= #
# 2단계 다층 로지스틱 회귀 분석용 데이터 전처리
# ================================================================= #

# ================================================================= #
# 13. [Level-2] 각 정당의 이념 위치 산출 (당파적 지지자 이념 평균)
# ================================================================= #

# 1. 정당별 지지자들의 이념 평균(mean_ideo) 계산
party_ideo_summary <- df %>%
  filter(!is.na(ideo_score), party_id != "Independent", party_id != "Others") %>%
  group_by(party_id) %>%
  summarise(
    partner_ideo = mean(ideo_score, na.rm = TRUE),
    .groups = "drop"
  )

# 2. 자민당(LDP)의 평균 이념 점수만 따로 추출 (정당 간 이념 거리 계산용)
ldp_mean_ideo <- party_ideo_summary %>% 
  filter(party_id == "LDP") %>% 
  pull(partner_ideo)

print("=== 각 정당 지지자들의 평균 이념 위치 ===")
print(party_ideo_summary)


# ================================================================= #
# 14. 연립 대상 정당 변수 정비 및 데이터 스택 (Wide to Long)
# ================================================================= #

# 1. Q10 문항을 바탕으로 각 정당과의 연립 찬성(1/0) 변수 명확화
df <- df %>%
  mutate(
    ID = row_number(), # 응답자 고유 식별자(Random Intercept용)
    coalition_CDP        = ifelse(Q10_1 == 1, 1, 0),
    coalition_Ishin      = ifelse(Q10_2 == 1, 1, 0), # JRP를 Ishin으로 통일
    coalition_DPP        = ifelse(Q10_3 == 1, 1, 0),
    coalition_Komeito    = ifelse(Q10_4 == 1, 1, 0),
    coalition_Reiwa      = ifelse(Q10_5 == 1, 1, 0),
    coalition_JCP        = ifelse(Q10_6 == 1, 1, 0),
    coalition_Sanseito   = ifelse(Q10_7 == 1, 1, 0),
    coalition_Conservative   = ifelse(Q10_8 == 1, 1, 0),
    coalition_SDP   = ifelse(Q10_9 == 1, 1, 0)
  )

# 2. 데이터 스택 (Pivot_longer)
df_long <- df %>%
  # 원본 연속형 변수(표준화 전)도 반드시 가져옵니다. 롱폼에서 다시 표준화하는 것이 정확합니다.
  select(ID, pol_distrust, female, age_decadal, age_group,
         living_std, College, ideo_score, pol_interest, 
         starts_with("coalition_")) %>%
  pivot_longer(
    cols = starts_with("coalition_"),
    names_to = "Partner",              # 파트너 정당 이름이 들어갈 열
    names_prefix = "coalition_",       # "coalition_" 접두사 제거
    values_to = "Accept"               # 연립 찬성 여부 (1/0)
  ) %>%
  filter(!is.na(Accept)) # 결측치 제거


# ================================================================= #
# 15. Level-2 (정당 수준) 특성 병합, 거리 변수 생성 및 일괄 표준화
# ================================================================= #

df_long <- df_long %>%
  # 1. 파트너 정당의 이념 점수 병합
  left_join(party_ideo_summary, by = c("Partner" = "party_id")) %>%
  
  # 2. 분석용 조합 특성 및 거리 계산
  mutate(
    # [Level-2] 자민당과 파트너 정당 간의 이념 거리
    dist_ldp_partner = abs(ldp_mean_ideo - partner_ideo),
    
    # [Level-1 & 2] 응답자 본인과 파트너 정당 간의 이념 거리
    dist_self_partner = abs(ideo_score - partner_ideo),
    
    # [Level-2] 파트너 정당 유형 분류 (현상 유지 vs 보수/진보 대안)
    Partner_Type = case_when(
      Partner == "Komeito" ~ "1_Status_Quo",                     
      Partner %in% c("Ishin", "Sanseito","Conservative") ~ "2_Conservative_Alt", 
      Partner %in% c("DPP") ~ "3_Moderate_Alt", 
      Partner %in% c("CDP", "JCP", "Reiwa","SDP") ~ "4_Progressive_Alt",        
      TRUE ~ "5_Other"
    ),
    
    # 포퓰리즘 여부 더미 변수 (상호작용 분석용)
    is_populist = ifelse(Partner %in% c("Ishin", "Sanseito", "Reiwa"), 1, 0)
  ) %>%
  
  # 공명당(현상 유지)을 Reference(기준 범주)로 설정
  mutate(Partner_Type = factor(Partner_Type, levels = c("1_Status_Quo", "2_Conservative_Alt", "3_Moderate_Alt",
                                                        "4_Progressive_Alt", "5_Other"))) %>%
  
  # 3. ★ 연속형 변수 일괄 표준화 (롱폼 데이터 기준 Z-score) ★
  mutate(
    pol_distrust_std      = as.numeric(scale(pol_distrust)),
    living_std_std        = as.numeric(scale(living_std)),
    pol_interest_std      = as.numeric(scale(pol_interest)),
    dist_ldp_partner_std  = as.numeric(scale(dist_ldp_partner)),
    dist_self_partner_std = as.numeric(scale(dist_self_partner))
  )


# ================================================================= #
# 16. 다층 로지스틱 회귀 분석 (Hierarchical Logistic Regression)
# ================================================================= #
library(lme4)


cat("\n[1] is_populist 없이 기본  모형\n")
model0 <- glmer(
  Accept ~ female+age_group + living_std_std +  College + 
    pol_distrust_std + dist_ldp_partner_std + dist_self_partner_std + 
    (1 | ID), 
  data = df_long, 
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)) # 수렴 최적화
)
summary(model0)



cat("\n[1] 기본 다층 모형 (통제변수 및 거리 변수 효과 확인)\n")
model1 <- glmer(
  Accept ~ female+age_group + living_std_std +  College + 
    pol_distrust_std + dist_ldp_partner_std + dist_self_partner_std + is_populist+
    (1 | ID), 
  data = df_long, 
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)) # 수렴 최적화
)
summary(model1)

# 5. 오즈비(Odds Ratio) 확인 (표준화된 계수 기반)
exp(coef(model1))

cat("\n[6] pol_distrust*is_populist 상호작용 다층 모형 (정치 불신에 따른 연립 파트너 선호 차이)\n")
model2 <- glmer(
  Accept ~ female+ living_std_std + age_group + College + 
    pol_distrust_std*is_populist+ 
    dist_ldp_partner_std+dist_self_partner_std + 
    (1 | ID), 
  data = df_long, 
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model2)



cat("\n[7] 포퓰리즘 정당과 연령 상호작용 다층 모형 (정치 불신에 따른 연립 파트너 선호 차이)\n")

model3 <- glmer(
  Accept ~ female+ living_std_std + age_group*is_populist + College + 
    pol_distrust_std + dist_ldp_partner_std+dist_self_partner_std + 
    (1 | ID), 
  data = df_long, 
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model3)

# 계수, 표준오차, 오즈비, 95% 신뢰구간을 포함한 표 출력
fix_eff <- summary(model3)$coefficients
or_table <- exp(fix_eff[,1]) # 오즈비 계산
ci_table <- exp(confint(model3, method="Wald")) # 신뢰구간 계산 (속도를 위해 Wald 사용)

result_table <- cbind(Estimate = fix_eff[,1], OR = or_table, ci_table[-1,]) # Intercept 제외 결합
print(round(result_table, 3))


# ================================================================= #
# 상호작용 시각화화
# ================================================================= #
library(lme4)
library(sjPlot)
library(ggplot2)

# 상호작용 효과 그래프 생성
# type = "int"는 상호작용(interaction) 항을 자동으로 찾아 시각화합니다.
plot_model(model2, 
           type = "int", 
           mdl.term = "pol_distrust_std:is_populist",
           title = "Interaction Effect of Political Distrust and Populist Parties",
           axis.title = c("Political Distrust", "Predicted Probability of Approval"),
           legend.title = "Party Type") +
  scale_color_discrete(labels = c("Established", "Populist")) +
  theme_minimal()

library(ggeffects)



# ================================================================= #
# 모델1 기준 기초통게
# ================================================================= #

library(dplyr)
library(tidyr)

# 1. 모델에 투입된 최종 데이터프레임 추출 (결측치 제거된 상태)
df_l2_sample <- model.frame(model2)

# 2. 연속형 변수 요약 (표준화 전 원본 변수명을 사용하여 raw value 추출)
# 주의: df_long 생성 시 원본 변수를 select에 포함했어야 함
desc_l2_continuous <- df_long %>%
  filter(row_number() %in% row.names(df_l2_sample)) %>% # 모델 사용 행만 필터링
  select(pol_distrust, dist_ldp_partner, dist_self_partner, living_std) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    N_obs = n(),
    Mean = round(mean(Value, na.rm = TRUE), 3),
    SD = round(sd(Value, na.rm = TRUE), 3),
    Min = round(min(Value, na.rm = TRUE), 3),
    Max = round(max(Value, na.rm = TRUE), 3)
  )

# 2. 범주형 및 이항 변수 요약 (타입 변환 추가)
desc_l2_categorical <- df_long %>%
  filter(row_number() %in% row.names(df_l2_sample)) %>%
  select(Accept, female, College, age_group, is_populist) %>%
  # 모든 열을 문자형으로 변환하여 타입 충돌 방지
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Category") %>%
  group_by(Variable, Category) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(Variable) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 1))

print(desc_l2_continuous)
print(desc_l2_categorical)
# ================================================================= #
#강건성check 2
# ================================================================= #

# [Robustness Check] 무작위 기울기(Random Slopes)를 포함한 다층 모형
library(lme4)

model2_rs1 <- glmer(
  Accept ~ female+ living_std_std + age_group*is_populist + College + 
    pol_distrust_std + dist_ldp_partner_std+dist_self_partner_std + 
    # (1 | ID)를 (1 + is_populist | ID)로 변경하여 무작위 기울기 추가
    (1 + is_populist | ID), 
  data = df_long, 
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

# 결과 확인
summary(model2_rs1)

##############

model2_rs2 <- glmer(
  Accept ~ female+ living_std_std + age_group+ pol_distrust_std*is_populist + College + 
    dist_ldp_partner_std+dist_self_partner_std + 
    # (1 | ID)를 (1 + is_populist | ID)로 변경하여 무작위 기울기 추가
    (1 + is_populist | ID), 
  data = df_long, 
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

# 결과 확인
summary(model2_rs2)

