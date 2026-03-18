
rm(list = ls( ))  

# ================================================================= #
# 1. 환경 설정 및 패키지 로드
# ================================================================= #

setwd("D://STAT_DATA/Tani/")

# 필요한 패키지 한 번에 로드
if(!require(pacman)) install.packages("pacman")
pacman::p_load(poLCA, nnet, dplyr, tidyr, ggplot2, forcats)

# ================================================================= #
# 2. 데이터 불러오기 및 기본 전처리
# ================================================================= #
df_raw <- read.csv("2025UTASV20250812.csv")

df <- df_raw %>%
  # 전체 데이터에서 99를 NA로 일괄 처리
  mutate(across(everything(), ~ifelse(. == 99, NA, .))) %>%
  
  # [Q1] 내각 지지 (1:지지, 0:미지지)
  mutate(support_ishiba = ifelse(Q1 == 1, 1, 0)) %>%
  
  # [Q6] 정치 불신 (Political Distrust)
  # 5번(기타/답변 안 함) 응답은 결측치(NA) 처리하고, 
  # 기존 척도(1:신뢰 ~ 4:불신)를 그대로 유지하여 값이 클수록 불신이 높게 설정
  mutate(
    Q6_clean = ifelse(Q6 == 5, NA, Q6),
    pol_distrust = Q6_clean
  ) %>%
  
  # [Q5] 정치 관심도
  mutate(
    Q5_clean = ifelse(Q5 == 5, NA, Q5),
    # 역코딩: 4(대단히 관심) ~ 1(전혀 없음)
    pol_interest = 5 - Q5_clean
  )

# ================================================================= #
# 3. 주요 정치 현안 변수 및 이념적 역코딩 (High = Conservative/Positive)
# ================================================================= #
df <- df %>%
  mutate(
    # 이분법적 변수: 1이 보수/긍정이 되도록 유지
    const_9_change     = ifelse(Q18 == 1, 1, 0), # 개정 찬성
    const_general_need = ifelse(Q20 == 1, 1, 0), # 개정 필요
    usa_security_trust = ifelse(Q28 == 1, 1, 0), # 미국 방위 신뢰
    
    # Q25 미국 의존도: 높을수록 의존 (3:대단히 ~ 0:전혀)
    usa_reliance = 4 - Q25
  ) %>%
  # [Q43 정책 의견] 5점(반대)이 보수인 문항은 그대로, 1점(찬성)이 보수인 문항은 역코딩
  mutate(
    defense_strengthen = 6 - Q43_1,  # 찬성(1)이 보수 -> 역코딩 (5점: 매우 찬성)
    nk_pressure        = 6 - Q43_2,  # 찬성(1)이 보수 -> 역코딩
    non_nuclear_3      = Q43_3,      # 유지(1)가 진보 -> 그대로 (5점: 반대/보수)
    small_government   = 6 - Q43_4,  # 찬성(1)이 보수 -> 역코딩
    public_works_jobs  = Q43_5,      # 그대로 (5점: 반대/보수)
    fiscal_stimulus    = Q43_6,      # 그대로 (5점: 반대/보수)
    tax_increase_ok    = Q43_7,  # 논쟁적이나 작은 정부보수로 맞춤춤
    rich_tax           = Q43_8,      # 찬성(1)이 진보 -> 그대로 (5점: 반대/보수)
    burden_increase    = Q43_9,  # 찬성(1)이 큰 정부 ->  그대로
    privacy_limit      = 6 - Q43_10, # 찬성(1)이 보수 -> 역코딩
    same_sex_marriage  = Q43_11,     # 찬성(1)이 진보 -> 그대로 (5점: 반대/보수)
    separate_surname   = Q43_12      # 찬성(1)이 진보 -> 그대로 (5점: 반대/보수)
  ) %>%
  # [Q44 쟁점 대립] B(5)가 보수인 문항은 그대로, A(1)가 보수인 문항은 역코딩
  mutate(
    china_threat     = 6 - Q44_1, # A(위협/보수) -> 역코딩 (5점: 위협)
    econ_inequality  = 6 - Q44_2,     # A(격차용인/보수) -> 그대로
    nuclear_keep     = Q44_3,     # B(유지/보수) -> 그대로
    climate_lifestyle = Q44_4,    # B(생활우선/보수) -> 그대로
    merged_districts  = Q44_5, # B(해소/보수) -> 그대로
    corp_donation    = 6-Q44_6      # A(기부허용/보수) -> 그대로
  ) %>%
  # ================================================================= #
  # [Q45] 가장 우선적으로 다뤄야 할 정치 과제 (명목 변수 코딩)
  # ================================================================= #
  mutate(
    # 범주형 분석 및 시각화를 위해 Factor로 변환 (9번은 기타/무응답)
    priority_issue = factor(Q45,
                            levels = 1:9,
                            labels = c("Diplomacy_Security",    # 1. 외교·안전보장
                                       "Economy_Employment",    # 2. 경기·고용
                                       "Fiscal_Reconstruction", # 3. 재정재건
                                       "Pension_Healthcare",    # 4. 연금·의료·개호
                                       "Education_Childcare",   # 5. 교육·육아
                                       "Nuclear_Energy",        # 6. 원자력발전·에너지
                                       "Constitution",          # 7. 헌법(호헌 또는 개헌)
                                       "Political_Reform",      # 8. 정치·행정개혁
                                       "Other_NA")),            # 9. 기타·답변 안 함
    
    # 만약 특정 이슈(예: 경제/고용 vs 그 외)를 우선하는지 보는 이분법 변수가 필요하다면 아래처럼 더미변수 추가 가능
    priority_economy_dummy  = ifelse(Q45 == 2, 1, 0), # 경기·고용 우선 여부
    priority_welfare_dummy  = ifelse(Q45 == 4, 1, 0), # 연금·의료·개호 우선 여부
    priority_security_dummy = ifelse(Q45 == 1, 1, 0)  # 외교·안보 우선 여부
  ) %>%
 # ================================================================= #
 # [Q46] 우선 과제를 가장 잘 처리할 정당 (이슈 소유권 및 정당 효능감)
 # ================================================================= #
mutate(
  # 1. 전체 정당 명목 변수화 (99 무응답은 결측치 처리 또는 별도 범주화)
  # nnet이나 poLCA 분석 시 보기 편하게 영문 라벨링
  best_party = factor(Q46,
                      levels = c(1:12),
                      labels = c("LDP",            # 1. 자민당
                                 "CDP",            # 2. 입헌민주당
                                 "Ishin",          # 3. 일본유신회
                                 "DPP",            # 4. 국민민주당
                                 "Komeito",        # 5. 공명당
                                 "Reiwa",          # 6. 레이와신센구미
                                 "JCP",            # 7. 공산당
                                 "Sanseito",       # 8. 참정당
                                 "Conservative",   # 9. 일본보수당
                                 "SDP",            # 10. 사민당
                                 "Other",          # 11. 기타 정당
                                 "None")),         # None
  
  # 2. 분석 목적에 맞춘 더미 및 그룹 변수 생성
  # (1) 자민당 이슈 소유권 (자민당 단독 정권 지지 분석 시 핵심)
  Technical_competence_ldp  = ifelse(Q46 == 1, 1, 0),
  
  # (2) 정치 불신 / 무당파 성향 변수 (어떤 정당도 내 우선순위 과제를 해결할 수 없다고 봄)
  Technical_competence_none = ifelse(Q46 == 12, 1, 0),
  
  # (3) 주요 야당 이슈 소유권 (입헌, 유신, 국민 등 야당 지지층 묶음 / 1~10번 중 자민, 공명 제외)
  Technical_competence_oppo = ifelse(Q46 %in% c(2, 3, 4, 6, 7, 8, 9, 10), 1, 0),
  # ================================================================= #
  # 3. 분석용 통합 범주 변수 및 통제 변수 설정
  # ================================================================= #
  # 야당 지지자 대비 무당파의 효과를 직관적으로 보기 위해 'Opposition'을 기준(Reference)으로 설정
  Technical_competence_cat = factor(
    case_when(
      Q46 == 1 ~ "LDP",
      Q46 %in% c(2, 3, 4, 6, 7, 8, 9, 10, 11) ~ "Opposition",
      Q46 == 12 ~ "None",
      TRUE ~ NA_character_
    ),
    levels = c("Opposition", "None", "LDP") 
  ))



# ================================================================= #
# 4. 인구사회학적 변수 및 연립/단독 의향 코딩
# ================================================================= #
df <- df %>%
  mutate(
    female = Q47 - 1,
    age_decadal = Q48 / 10,
    age_group = factor(case_when(
      Q48 <= 5 ~ "18-39",
      Q48 >= 6 & Q48 <= 9 ~ "40-59",
      Q48 >= 10 ~ "60 or older"   # 60세 이상
    ), levels = c("60 or older", "18-39", "40-59")),
    College = ifelse(Q51 %in% c(5, 6), 1, 0),
    living_std = case_when(Q54 %in% 1:3 ~ 3, Q54 %in% 4:5 ~ 2, TRUE ~ 1)
  ) %>%
  # 연립 의향 더미 변수 생성
  mutate(
    coalition_CDP = ifelse(Q10_1 == 1, 1, 0), 
    coalition_Ishin = ifelse(Q10_2 == 1, 1, 0),
    coalition_DPP = ifelse(Q10_3 == 1, 1, 0), 
    coalition_Komeito = ifelse(Q10_4 == 1, 1, 0)
  ) %>%
  # 자민 단독 정권 지지 (Partner를 하나도 고르지 않은 경우)
  mutate(ldp_solo = ifelse(rowSums(select(., Q10_1:Q10_10) == 1, na.rm = TRUE) == 0, 1, 0))

# ================================================================= #
#정당당파성
# ================================================================= #

df <- df %>%
  mutate(
    # 1. 개별 정당 ID 생성
    party_id = case_when(
      Q41 == 1  ~ "LDP",
      Q41 == 2  ~ "CDP",
      Q41 == 3  ~ "Ishin",
      Q41 == 4  ~ "DPP",
      Q41 == 5  ~ "Komeito",
      Q41 == 6  ~ "Reiwa",
      Q41 == 7  ~ "JCP",
      Q41 == 8  ~ "Sanseito",
      Q41 == 9  ~ "Conservatives",
      Q41 == 10 ~ "SDP",
      Q41 == 12 ~ "Independent",
      TRUE      ~ "Others"
    ),
    
    # 2. 분석용 그룹화 생성
    party_group = case_when(
      Q41 == 1 ~ "LDP",
      Q41 %in% c(2, 3, 4, 5, 6, 7, 8, 9, 10,11) ~ "Non_LDP", 
      Q41 == 12 ~ "Independent"
    ),
    
    # 3. Non_LDP를 기준(Reference)으로 설정
    # levels의 첫 번째 항목이 통계 분석 시 기준점이 됩니다.
    party_group = factor(party_group, levels = c("Non_LDP", "Independent", "LDP", "Others"))
  )

# ================================================================= #
# 감정온도
# ================================================================= #

# Q42 감정온도 변수 재코딩 (1~6번 전체)
df <- df %>%
  rename(
    ft_ldp    = Q42_1,  # (1) 자민당 [cite: 80]
    ft_cdp    = Q42_2,  # (2) 입헌민주당 [cite: 82]
    ft_ishin  = Q42_3,  # (3) 일본유신회 [cite: 84]
    ft_dpp    = Q42_4,  # (4) 국민민주당 [cite: 86]
    ft_ishiba = Q42_5,  # (5) 이시바 시게루 [cite: 88]
    ft_noda   = Q42_6   # (6) 노다 요시히코 [cite: 90]
  ) %>%
  # 이미 앞에서 99를 NA로 처리했다면 아래 과정은 확인용입니다.
  mutate(across(starts_with("ft_"), ~ifelse(. > 10, NA, .)))

# 코딩 결과 확인 (평균값 산출)
df %>%
  summarise(across(starts_with("ft_"), ~mean(., na.rm = TRUE)))


# ================================================================= #
# 6. 정책 이슈 기반 Blackbox Scaling (minscale=1 설정)
# ================================================================= #

# 패키지 로드
library(basicspace)

# 1. 이념 추출에 사용할 정책 변수군 (역코딩 완료된 1~5점 척도)
ideo_policy_vars <- c(
  "defense_strengthen", "nk_pressure", "non_nuclear_3", 
  "small_government",   "rich_tax", 
  "privacy_limit",   "same_sex_marriage", 
  "separate_surname",   "china_threat",    "nuclear_keep"
)

# 2. 분석용 서브셋 생성 및 결측치 제거
# Blackbox는 응답이 완전히 채워진 행을 기반으로 좌표를 계산합니다.
df_ideo_sub <- df %>%
  select(all_of(ideo_policy_vars)) %>%
  na.omit()

# 3. Blackbox 실행 (1~5점 리커트 척도이므로 minscale = 1)
# dims = 1로 설정하여 '진보-보수' 단일 축을 먼저 추출합니다.
bb_result <- blackbox(df_ideo_sub, dims = 1, minscale = 1, verbose = TRUE)


# ── bb_result 구조 진단 ──────────────────────────────────────────
str(bb_result)          # 전체 구조 확인
names(bb_result)        # 어떤 이름의 슬롯이 있는지 확인



# ================================================================= #
# 7. Blackbox 이념 점수 추출 및 데이터프레임 병합
# ================================================================= #

# 1. individuals[[1]]$c1 으로 접근 (리스트 > 데이터프레임 > 열)
ideo_scores <- bb_result$individuals[[1]]$c1

cat("추출된 점수 수:", length(ideo_scores), "\n")  # 1781이어야 함
cat("df 결측 제거 후 행 수:", nrow(df_ideo_sub), "\n")  # 동일해야 함

# 2. 원본 df에서 결측 없는 행의 인덱스 추적
ideo_index <- df %>%
  select(all_of(ideo_policy_vars)) %>%
  complete.cases() %>%
  which()

cat("인덱스 수:", length(ideo_index), "\n")  # ideo_scores와 동일해야 함

# 3. df에 이념 점수 병합
df$ideo_score <- NA_real_
df$ideo_score[ideo_index] <- ideo_scores

cat("\n이념 점수 요약:\n")
summary(df$ideo_score)
cat("유효 응답자:", sum(!is.na(df$ideo_score)), "명\n")


# ================================================================= #
# 8. 이념 점수 기술통계 및 분포 확인
# ================================================================= #

# 1. 기본 기술통계
cat("=== 이념 점수 기술통계 ===\n")
summary(df$ideo_score)
cat("표준편차:", sd(df$ideo_score, na.rm = TRUE), "\n\n")

# 2. 이념 점수 분포 히스토그램
p_hist <- ggplot(df %>% filter(!is.na(ideo_score)), aes(x = ideo_score)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "steelblue", color = "white", alpha = 0.8) +
  geom_density(color = "firebrick", linewidth = 1.0, adjust = 1.2) +
  geom_vline(xintercept = mean(df$ideo_score, na.rm = TRUE),
             linetype = "dashed", color = "darkred", linewidth = 0.8) +
  labs(title = "이념 점수 분포 (Blackbox Scaling, 1차원)",
       subtitle = "점선: 전체 평균 / 우측이 보수, 좌측이 진보",
       x = "이념 점수", y = "밀도") +
  theme_bw(base_family = "AppleGothic")  # Windows: "malgun gothic"

print(p_hist)


# ================================================================= #
# 9. 집단별 이념 점수 비교
# ================================================================= #

# ── 9-1. Mean Ideological Scores by Party Identification ─────────────────────────
party_ideo <- df %>%
  filter(!is.na(ideo_score), party_id != "Others") %>%
  group_by(party_id) %>%
  summarise(
    n         = n(),
    mean_ideo = mean(ideo_score, na.rm = TRUE),
    se        = sd(ideo_score, na.rm = TRUE) / sqrt(n),
    .groups   = "drop"
  ) %>%
  # Sort from Conservative to Liberal (Descending order of mean_ideo)
  arrange(desc(mean_ideo)) %>%
  mutate(party_id = factor(party_id, levels = party_id))

print(party_ideo)

p_party <- ggplot(party_ideo, aes(x = party_id, y = mean_ideo, fill = party_id)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_ideo - 1.96 * se,
                    ymax = mean_ideo + 1.96 * se),
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Mean Ideological Scores by Party Identification (95% CI)",
       x = NULL, 
       y = "Mean Ideology Score (↑Conservative / ↓Liberal)") +
  theme_bw() + 
  theme(text = element_text(family = "serif")) # Academic standard font (Times New Roman style)

print(p_party)


# ── 9-2. 연령대별 이념 점수 분포 ──────────────────────────────────
p_age <- df %>%
  filter(!is.na(ideo_score), !is.na(age_group)) %>%
  ggplot(aes(x = ideo_score, fill = age_group, color = age_group)) +
  geom_density(alpha = 0.35, linewidth = 0.9, adjust = 1.2) +
  geom_vline(data = df %>%
               filter(!is.na(ideo_score), !is.na(age_group)) %>%
               group_by(age_group) %>%
               summarise(m = mean(ideo_score, na.rm = TRUE)),
             aes(xintercept = m, color = age_group),
             linetype = "dashed", linewidth = 0.8) +
  labs(title = "연령대별 이념 점수 분포",
       x = "이념 점수", y = "밀도", fill = "연령대", color = "연령대") +
  theme_bw(base_family = "AppleGothic")

print(p_age)


# ── 9-3. 내각 지지 여부별 이념 점수 t-검정 ───────────────────────
t_result <- t.test(ideo_score ~ support_ishiba, data = df)
print(t_result)

df %>%
  filter(!is.na(ideo_score), !is.na(support_ishiba)) %>%
  mutate(지지여부 = ifelse(support_ishiba == 1, "지지", "미지지")) %>%
  group_by(지지여부) %>%
  summarise(평균 = mean(ideo_score), SD = sd(ideo_score), n = n())


# ================================================================= #
# 10. 이념 점수와 주요 변수 간 상관 분석
# ================================================================= #

# 필요한 패키지 한 번에 로드
if(!require(ltm)) install.packages("ltm")
library(ltm)

# 1. 데이터 확인 및 결측치 제거
# 두 변수에 결측치가 없는 행만 추출합니다.
clean_df <- na.omit(df[, c("pol_distrust", "Technical_competence_none")])

# 2. 점-이연 상관계수 계산
# biserial.cor(연속형_변수, 이항_변수, use = "complete.obs")
# level = 2는 이항 변수의 두 번째 레벨(보통 1)을 기준으로 한다는 뜻입니다.
cor_result <- biserial.cor(clean_df$pol_distrust , 
                           clean_df$Technical_competence_none, 
                           level = 2)


print(cor_result)

## 상관관계 ##



cor_vars <- c("ideo_score", "political_trust", "usa_reliance",
              "usa_security_trust", "china_threat", "defense_strengthen",
              "same_sex_marriage", "separate_surname",
              "ft_ldp", "ft_cdp", "ft_ishiba")

cor_matrix <- df %>%
  select(all_of(cor_vars)) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3)

print(cor_matrix[, "ideo_score", drop = FALSE])


# ================================================================= #
# 11. 이념 점수 → 내각 지지 로지스틱 회귀 (통제변수 포함)
# ================================================================= #

logit_ideo <- glm(
  support_ishiba ~ ideo_score + political_trust + age_scale +
    College + living_std + party_id,
  data   = df,
  family = binomial(link = "logit")
)

summary(logit_ideo)

# 오즈비(OR) 및 95% CI 출력
OR_table <- exp(cbind(OR = coef(logit_ideo), confint(logit_ideo)))
print(round(OR_table, 3))



# ================================================================= #
# 가설 검증: 자민당 단독 정권 지지 결정 요인
# ================================================================= #


# ================================================================= #
# 모형 1: 각 가설 단순 로지스틱 회귀 (가설별 단변량 검증)
# ================================================================= #

hypo_vars <- list(
  "가설1_정치불신"   = "pol_distrust",
  "가설2_중국위협"   = "china_threat",
  "가설3_호감gap"   = "ft_gap",
  "가설4_고학력"     = "College",
  "가설5_이념중도"   = "ideology_center",
)

cat("=== 단변량 로지스틱 회귀 결과 ===\n")
uni_results <- purrr::map_dfr(names(hypo_vars), function(label) {
  var <- hypo_vars[[label]]
  formula <- as.formula(paste("ldp_solo ~", var))
  m <- glm(formula, data = df, family = binomial(link = "logit"))
  s <- summary(m)$coefficients
  tibble::tibble(
    가설     = label,
    변수     = var,
    OR       = exp(coef(m)[2]),
    CI_lo    = exp(confint(m)[2, 1]),
    CI_hi    = exp(confint(m)[2, 2]),
    p_value  = s[2, 4]
  )
})

print(uni_results %>% mutate(across(where(is.numeric), ~round(., 3))))


# ================================================================= #
# 모형 2: 전체 가설 동시 투입 (다변량 로지스틱 회귀)
# ================================================================= #

model_f <- glm(ldp_solo ~ pol_distrust+Technical_competence_cat + priority_security_dummy + 
                 age_scale+party_id ,
  data   = df,
  family = binomial(link = "logit")
)

cat("\n=== 다변량 로지스틱 회귀 요약 ===\n")
summary(model_f)

# OR 및 95% CI 정리
OR_f <- exp(cbind(OR = coef(model_f), confint(model_f))) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("변수") %>%
  rename(CI_lo = `2.5 %`, CI_hi = `97.5 %`) %>%
  mutate(
    p_value = summary(model_f)$coefficients[, 4],
    sig     = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE            ~ ""
    )
  ) %>%
  filter(변수 != "(Intercept)")

cat("\n=== OR 및 신뢰구간 ===\n")
print(OR_f %>% mutate(across(where(is.numeric), ~round(., 3))))


# ================================================================= #
# 모형 3: 모형 적합도 비교 (통제변수 추가)
# ================================================================= #

# 인구사회학적 통제변수 추가
model_ctrl0 <- glm(
  ldp_solo ~ female+age_scale + living_std + 
    College +  
    party_id+ ideo_score+
    pol_distrust + pol_interest+
    priority_security_dummy,
  data   = df,
  family = binomial(link = "logit")
)


summary(model_ctrl0)


# party_id+ Technical_competence_cat 추가
model_ctrl <- glm(
  ldp_solo ~ female+age_group + living_std + 
    College +  
    party_id+ ideo_score+
    pol_distrust + Technical_competence_cat+pol_interest+
    priority_security_dummy,
  data   = df_scaled,
  family = binomial(link = "logit")
)

summary(model_ctrl)

# 최종 모델: 인구사회학적 통제변수 Technical_competence_cat 추가
model_ctrl2 <- glm(
  ldp_solo ~ female+age_group + living_std + 
    College +  
    party_group+ ideo_score+
    pol_distrust + Technical_competence_cat+pol_interest+
    priority_security_dummy,
  data   = df,
  family = binomial(link = "logit")
)

summary(model_ctrl2)



# OR 및 95% CI 정리
OR_full <- exp(cbind(OR = coef(model_ctrl2), confint(model_ctrl2))) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("변수") %>%
  rename(CI_lo = `2.5 %`, CI_hi = `97.5 %`) %>%
  mutate(
    p_value = summary(model_ctrl2)$coefficients[, 4],
    sig     = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE            ~ ""
    )
  ) %>%
  filter(변수 != "(Intercept)")

cat("\n=== OR 및 신뢰구간 ===\n")
print(OR_full %>% mutate(across(where(is.numeric), ~round(., 3))))


# AIC / BIC / pseudo-R2 비교
cat("\n=== 모형 적합도 비교 ===\n")
fit_compare <- tibble::tibble(
  모형    = c("가설변수만", "통제변수 포함"),
  AIC     = c(AIC(model_f), AIC(model_ctrl)),
  BIC     = c(BIC(model_f), BIC(model_ctrl)),
  # McFadden pseudo-R2
  pR2     = c(
    1 - model_f$deviance / model_f$null.deviance,
    1 - model_f$deviance / model_f$null.deviance
  )
)
print(fit_compare %>% mutate(across(where(is.numeric), ~round(., 3))))

# 우도비 검정 (두 모형 비교)
cat("\n=== 우도비 검정 (가설모형 vs 통제모형) ===\n")
anova(model_f, model_ctrl, test = "Chisq")


# ================================================================= #
# 8. 변수 표준화 및 회귀분석 (model_ctrl2)
# ================================================================= #

# 1. 표준화할 연속형 변수 리스트 정의
# (주의: 더미변수나 요인형 변수는 표준화에서 제외하는 것이 해석에 용이함)
vars_to_scale <- c("pol_distrust", "pol_interest","living_std")

# 2. 데이터 프레임 복사본 생성 및 표준화 적용
df_scaled <- df %>%
  mutate(across(all_of(vars_to_scale), ~ as.numeric(scale(.)), .names = "{.col}_std"))

# 3. 표준화된 변수를 사용한 로지스틱 회귀분석
# 기존 변수명 뒤에 '_std'를 붙여 분석을 진행합니다.
model_ctrl2_std <- glm(
  ldp_solo ~ female + age_group + living_std_std + 
    College +  
    party_group + ideo_score +
    pol_distrust_std + Technical_competence_cat + pol_interest_std +
    priority_security_dummy,
  data   = df_scaled,
  family = binomial(link = "logit")
)

# 4. 분석 결과 출력
summary(model_ctrl2_std)

# 5. 오즈비(Odds Ratio) 확인 (표준화된 계수 기반)
exp(coef(model_ctrl2_std))



# OR 및 95% CI 정리
OR_std <- exp(cbind(OR = coef(model_ctrl2_std), confint(model_ctrl2_std))) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("변수") %>%
  rename(CI_lo = `2.5 %`, CI_hi = `97.5 %`) %>%
  mutate(
    p_value = summary(model_ctrl2_std)$coefficients[, 4],
    sig     = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE            ~ ""
    )
  ) %>%
  filter(변수 != "(Intercept)")

cat("\n=== OR 및 신뢰구간 ===\n")
print(OR_std %>% mutate(across(where(is.numeric), ~round(., 3))))





# ================================================================= #
# 시각화: OR Forest Plot .. Plot for Odds Ratios for Supporting LDP-only Government
# ================================================================= #

library(ggplot2)
library(dplyr)
library(tidyr)

# # 1. 데이터 필터링 및 유의성 라벨(3단계) 재설정
OR_plot_data <- OR_std %>%
  filter(!변수 %in% c("party_idConservatives", "party_idOthers", "party_idSDP")) %>%
  # 가독성 정리
  mutate(변수 = gsub("party_group", "PID: ", 변수),
         변수 = gsub("age_group", "Age: ", 변수),
         변수 = gsub("Technical_competence_cat", "Competence: ", 변수)) %>%
  # p-value에 따른 3단계 별표 생성 (0.05 이상은 빈 문자열)
  mutate(sig_stars = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01  ~ "**",
    p_value < 0.05  ~ "*",
    TRUE            ~ ""
  )) %>%
  # 숫자와 새로운 별표를 합친 라벨 생성
  mutate(label_text = paste0(round(OR, 2), sig_stars))

# 2. Odds Ratio Plot 생성
ggplot(OR_plot_data, aes(x = reorder(변수, OR), y = OR)) +
  # Odds Ratio 점과 95% 신뢰구간 선
  geom_pointrange(aes(ymin = CI_lo, ymax = CI_hi, color = sig_stars != ""), size = 0.8) +
  
  # 점 바로 위에 숫자와 별표 표시
  geom_text(aes(label = label_text), 
            vjust = -1.5,    # 텍스트가 점과 겹치지 않게 위로 조정
            size = 3.5, 
            color = "black",
            fontface = "bold") + 
  
  # 기준선 (OR = 1)
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50", size = 0.6) +
  
  # 축 반전 (변수명을 왼쪽에 배치)
  coord_flip() +
  
  # 유의미성 여부에 따른 색상 설정
  scale_color_manual(values = c("TRUE" = "firebrick", "FALSE" = "steelblue")) +
  
  # 축 라벨 설정 (제목은 저널 요청대로 제외)
  labs(x = "", 
       y = "Odds Ratios") +
  
  # 로그 스케일 및 여백 확보
  scale_y_log10(breaks = c(0.2, 0.5, 1, 2, 3, 4),
                limits = c(0.15, 6), # 텍스트 공간 확보를 위해 상한 소폭 확대
                expand = expansion(mult = c(0.1, 0.1))) + 
  
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        # 왼쪽 변수명 글자 크기 확대
        axis.text.y = element_text(size = 13, color = "black", face = "bold"), 
        axis.title.x = element_text(margin = margin(t = 10), face = "bold"),
        plot.margin = margin(10, 20, 10, 10))

# ================================================================= #
# 모형 4: interaction
# ================================================================= #


# Independent* pol_interest
model_it3 <- glm(
  ldp_solo ~ female+age_group + living_std + 
    College +  
    party_group*pol_interest+pol_distrust+ ideo_score+
    priority_security_dummy,
  data   = df,
  family = binomial(link = "logit")
)

summary(model_it3)



# 나이와 정치불신 
model_it1 <- glm(
  ldp_solo ~ female+age_group*Technical_competence_cat + living_std + 
    College +ideo_score+
    party_group+ pol_interest+Technical_competence_cat+
    priority_security_dummy,
  data   = df,
  family = binomial(link = "logit")
)

summary(model_it1)


# 나이와 pol_interest
model_it2 <- glm(
  ldp_solo ~ female+age_group*pol_interest+ living_std + 
    College +ideo_score+party_group+
    pol_distrust+priority_security_dummy,
  data   = df,
  family = binomial(link = "logit")
)

summary(model_it2)





# PID* Technical_competence_cat 
model_it4 <- glm(
  ldp_solo ~ female+age_group + living_std + 
    College +  
    party_group*Technical_competence_cat+pol_interest+pol_distrust+ ideo_score+
    priority_security_dummy,
  data   = df,
  family = binomial(link = "logit")
)

summary(model_it4)


# PID* Technical_competence_cat 
model_it5 <- glm(
  ldp_solo ~ female+age_group + living_std + 
    College +  
    party_group*pol_distrust+pol_interest+ideo_score+
    priority_security_dummy,
  data   = df,
  family = binomial(link = "logit")
)
summary(model_it5)


# pol_distrust*pol_interest
model_it6 <- glm(
  ldp_solo ~ female+age_group + living_std + 
    College +  
    party_group+pol_distrust*pol_interest++ideo_score+
    priority_security_dummy,
  data   = df,
  family = binomial(link = "logit")
)
summary(model_it6)


############# Interaction Effect of Political Interest and Party Identification

# Load required packages
library(ggeffects)
library(ggplot2)

# 1. 예측 확률 계산
pred_it3 <- ggpredict(model_it3, terms = c("pol_interest", "party_group"))

# 2. 학술지용 그래프 생성 및 폰트 통일
interaction_plot <- plot(pred_it3) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Political Interest (1 = Low, 4 = High)",
    y = "Predicted Probability of Single-Party Govt.",
    color = "Party Group",
    fill = "Party Group" # 리본(신뢰구간) 색상 매칭을 위해 추가
  ) +
  scale_color_manual(
    values = c("LDP" = "#D55E00", "Non_LDP" = "#0072B2", "Independent" = "#009E73"),
    labels = c("LDP" = "LDP", "Non_LDP" = "Non-LDP", "Independent" = "Independent")
  ) +
  scale_fill_manual(
    values = c("LDP" = "#D55E00", "Non_LDP" = "#0072B2", "Independent" = "#009E73"),
    labels = c("LDP" = "LDP", "Non_LDP" = "Non-LDP", "Independent" = "Independent")
  ) +
  # base_family를 빈칸으로 두거나 "sans"로 설정하여 다른 그림들과 통일합니다.
  theme_minimal(base_size = 14, base_family = "") + 
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    axis.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "grey80") # 테두리를 살짝 주어 더 깔끔하게 만듭니다.
  ) +
  scale_x_continuous(breaks = c(1, 2, 3, 4)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) # Y축을 %로 표시하여 가독성 높임

# Display the plot
print(interaction_plot)

######## 정치 관심도& 정치 불신 상호작용 효과 interaciton
library(ggplot2)
library(ggeffects)

# 1. 예측 확률 계산 (조절 변수 pol_interest를 범주별로 명확히 시각화)
pred_it6 <- ggpredict(model_it6, terms = c("pol_distrust", "pol_interest"))

# 2. 논문용 그래프 생성
plot(pred_it6) +
  labs(
    x = "Political Distrust",
    y = "Predicted Probability",
    color = "Political Interest"
  ) +
  # 제목과 부제목 제거
  ggtitle(NULL) +
  # 학술적 톤을 위한 테마 수정
  theme_minimal() + 
  theme(
    text = element_text(family = "serif"), # 논문 표준인 Serif(Times New Roman 계열) 설정
    axis.title = element_text(size = 12, face = "plain"),
    legend.position = "bottom",
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5), # 외곽선 추가
    strip.text = element_blank() # 만약 상단에 잔여 텍스트가 있다면 제거
  ) +
  # Y축을 0-100% 범위로 고정하여 시각적 왜곡 방지 (필요 시)
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.8))




# ================================================================= #
# 2단계 다층모형형
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
# (이름을 party_ideo_summary의 party_id와 정확히 일치시켜야 나중에 병합 가능)
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
    coalition_SDP   = ifelse(Q10_9 == 1, 1, 0),
    # 필요시 Q10_8(Conservative), Q10_9(SDP) 등 추가
  )

# 2. 데이터 스택 (Pivot_longer)
df_long <- df %>%
  select(ID, pol_distrust, pol_distrust, female, age_decadal, age_group,
         living_std, living_std, College, ideo_score, 
         starts_with("coalition_")) %>%
  pivot_longer(
    cols = starts_with("coalition_"),
    names_to = "Partner",              # 파트너 정당 이름이 들어갈 열
    names_prefix = "coalition_",       # "coalition_" 접두사 제거 (CDP, Ishin 등만 남음)
    values_to = "Accept"               # 연립 찬성 여부 (1/0)
  ) %>%
  filter(!is.na(Accept)) # 결측치 제거


# ================================================================= #
# 15. Level-2 (정당 수준) 특성 병합 및 거리 변수 생성
# ================================================================= #

df_long <- df_long %>%
  # 1. 파트너 정당의 이념 점수 병합
  left_join(party_ideo_summary, by = c("Partner" = "party_id")) %>%
  
  # 2. 분석용 조합 특성 및 거리 계산
  mutate(
    # [Level-2] 자민당과 파트너 정당 간의 이념 거리 (이질성, Programmatic heterogeneity)
    dist_ldp_partner = abs(ldp_mean_ideo - partner_ideo),
    
    # [Level-1 & 2] 응답자 본인과 파트너 정당 간의 이념 거리
    dist_self_partner = abs(ideo_score - partner_ideo),
    
    # [Level-2] 파트너 정당 유형 분류 (현상 유지 vs 보수/진보 대안)
    Partner_Type = case_when(
      Partner == "Komeito" ~ "1_Status_Quo",                     # 공명당 (기준/현상유지)
      Partner %in% c("Ishin", "Sanseito","Conservative") ~ "2_Conservative_Alt", # 보수 대안
      Partner %in% c("DPP") ~ "3_Moderate_Alt", # 보수 대안
      Partner %in% c("CDP", "JCP", "Reiwa","SDP") ~ "4_Progressive_Alt",       # 진보 대안
      TRUE ~ "5_Other"
    ),
    # [선택사항] 포퓰리즘 여부만 따로 추출한 더미 변수 (상호작용 분석용)
    is_populist = ifelse(Partner %in% c("Ishin","Sanseito", "Reiwa"), 1, 0)
  ) %>%
  # 공명당(현상 유지)을 Reference(기준 범주)로 설정
  mutate(Partner_Type = factor(Partner_Type, levels = c("1_Status_Quo", "2_Conservative_Alt", "3_Moderate_Alt",
                                                        "4_Progressive_Alt", "5_Other")))


# ================================================================= #
# 16. 다층 로지스틱 회귀 분석 (Hierarchical Logistic Regression)
# ================================================================= #
library(lme4)


cat("\n[1] 정당명 다층 모형 (정당명)\n")
# 1. Partner 변수를 Factor로 변환하고, 기준점(Reference)을 공명당(Komeito)으로 설정
df_long$Partner <- factor(df_long$Partner)
df_long$Partner <- relevel(df_long$Partner, ref = "Komeito")

# 2. 이념 거리 변수를 제외한 정당 팩터 모형 실행
model0 <- glmer(
  Accept ~ female+age_scale + living_std +  College + 
    pol_distrust + College + Partner + 
    (1 | ID), 
  data = df_long, 
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

# 3. 결과 확인
summary(model0)

cat("\n[2] 기본 다층 모형 (통제변수 및 거리 변수 효과 확인)\n")
model1 <- glmer(
  Accept ~ female+age_group + living_std +  College + 
    pol_distrust + dist_ldp_partner + dist_self_partner + 
    (1 | ID), 
  data = df_long, 
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)) # 수렴 최적화
)
summary(model1)

# 2. 패키지 활용 (performance 패키지 설치 필요)
# install.packages("performance")
library(performance)

# model2에 대한 ICC 추출
icc_result <- icc(model2)
print(icc_result)

library(sjPlot)
library(ggplot2)

# type = "eff"는 특정 변수의 한계 효과를 계산합니다.
plot_model(model2, 
           type = "eff", 
           terms = "pol_distrust",
           title = "Marginal Effect of Political Distrust",
           axis.title = c("Political Distrust (1: High, 4: Low)", "Probability of Approval")) +
  theme_minimal()

cat("\n[2_1] Partner_Type 투입입 다층 모형 (통제변수 및 거리 변수 효과 확인)\n")
model2_1 <- glmer(
  Accept ~  female+age_scale + living_std +  College + 
    pol_distrust +
    dist_ldp_partner + dist_self_partner + Partner_Type + 
    (1 | ID), 
  data = df_long, 
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)) # 수렴 최적화
)
summary(model2_1)


cat("\n[4] pol_distrust * Partner_Type 상호작용 다층 모형 (정치 불신에 따른 연립 파트너 선호 차이)\n")
# 핵심 가설: "정치 불신이 높을수록 공명당(Status_Quo)보다 유신회 등 대안(Conservative_Alt)을 선호하는가?"
model_int1 <- glmer(
  Accept ~ female+ living_std + age_scale + College + 
    pol_distrust * Partner_Type + 
    dist_self_partner + dist_ldp_partner+
    (1 | ID), 
  data = df_long, 
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model_int1)


cat("\n[5] 상호작용 다층 모형 (정치 불신에 따른 연립 파트너 선호 차이)\n")
# 핵심 가설: "정치 불신이 높을수록 공명당(Status_Quo)보다 유신회 등 대안(Conservative_Alt)을 선호하는가?"
model_int2 <- glmer(
  Accept ~ female+ living_std +age_scale*Partner_Type + College + 
    pol_distrust+ dist_ldp_partner + dist_self_partner + 
    (1 | ID), 
  data = df_long, 
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model_int2)


cat("\n[6] pol_distrust*is_populist 상호작용 다층 모형 (정치 불신에 따른 연립 파트너 선호 차이)\n")
model_int3 <- glmer(
  Accept ~ female+ living_std + age_group + College + 
    pol_distrust*is_populist+ 
    dist_ldp_partner+dist_self_partner + 
    (1 | ID), 
  data = df_long, 
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model_int3)


cat("\n[7] 포퓰리즘 정당과 연령 상호작용 다층 모형 (정치 불신에 따른 연립 파트너 선호 차이)\n")

model_int4 <- glmer(
  Accept ~ female+ living_std + age_group*is_populist + College + 
    pol_distrust + dist_ldp_partner+dist_self_partner + 
    (1 | ID), 
  data = df_long, 
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model_int4)

# 오즈비(Odds Ratio) 변환 출력 (옵션)
exp(fixef(model_int))

# 필요한 패키지 로드
library(sjPlot)
library(ggplot2)


# ================================================================= #
# 상호작용 효과 그래프 생성
# ================================================================

# type = "int"는 상호작용(interaction) 항을 자동으로 찾아 시각화합니다.
library(sjPlot)
library(ggplot2)


# 저널 제출용 상호작용 그래프 (색상 원복 및 텍스트 확대)
plot_model(model_int3, 
           type = "int", 
           mdl.term = "pol_distrust_std:is_populist", 
           title = "",                               # 제목 제거
           axis.title = c("Political Distrust (std)", "Predicted Probability of Approval"),
           legend.title = "Party Type",
           line.size = 1.5) +                        # 선 굵기 대폭 강화
  scale_color_discrete(labels = c("Established", "Populist")) +
  theme_minimal() +
  theme(
    # 축 제목 및 텍스트 크기 상향
    axis.title = element_text(size = 18, face = "bold"), 
    axis.text = element_text(size = 15, color = "black"),
    
    # 범례 크기 및 위치 조정
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 15),
    legend.position = "bottom",
    
    # 그래프 내부 여백 및 격자 조절
    panel.grid.major = element_line(size = 0.5),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )
# ================================================================

library(sjPlot)
library(ggplot2)

# 저널 제출용 연령대 상호작용 그래프 (제목 제거 및 텍스트 확대)
plot_model(model_int4, 
           type = "int", 
           terms = c("age_group", "is_populist"),
           title = "",                                # 제목 제거
           axis.title = c("Age Group", "Probability of Approval"),
           legend.title = "Party Type",
           dot.size = 3.5,                            # 점 크기 확대
           line.size = 1.2) +                         # 선 굵기 확대
  scale_color_discrete(labels = c("Established", "Populist")) +
  theme_minimal() +                                   # 깔끔한 테마 적용
  theme(
    # 축 제목 및 텍스트 크기 대폭 상향
    axis.title = element_text(size = 18, face = "bold"), 
    axis.text = element_text(size = 15, color = "black"),
    
    # 범례 크기 및 위치 조정
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 15),
    legend.position = "bottom",
    
    # 격자선 및 여백 조절
    panel.grid.major.x = element_blank(),             # X축 수직선 제거로 가독성 향상
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 15, 15, 15)
  )


###########################################################
# ── 기술통계 확인 ────────────────────────────────────────────────
cat("=== 종속변수: 자민당 단독 지지 분포 ===\n")
table(df$ldp_solo, useNA = "ifany")
cat("단독 지지 비율:", mean(df$ldp_solo, na.rm = TRUE) %>% round(3), "\n\n")


# ================================================================= #
# [Appendix] 기초통계량 산출 코드
# ================================================================= #

# 필요 패키지 로드
if(!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, tidyr, stargazer, summarytools)

library(stargazer)
library(summarytools)

# 1. 분석에 사용된 주요 변수 선택 (1단계 및 2단계 공통)
# -----------------------------------------------------------------
# 1단계(Individual level) 데이터셋 기준
desc_df <- df %>%
  select(
    # 종속변수
    ldp_solo, 
    # 주요 독립변수
    pol_distrust, pol_interest, ideo_score,
    # 인구사회학적/통제변수
    female, College, living_std, priority_security_dummy,
    # 범주형(Factor) 변수들
    age_group, party_group, Technical_competence_cat
  )

# 2. 연속형 및 이분형 변수 통계량 (Table A1: Continuous & Binary Variables)
# -----------------------------------------------------------------
# stargazer를 이용하면 논문에 바로 넣을 수 있는 깔끔한 표(LaTeX/Text/HTML)가 나옵니다.
cat("### Table A1. Descriptive Statistics for Continuous and Binary Variables\n")
stargazer(as.data.frame(desc_df), 
          type = "text", # 논문 작성용은 "latex" 또는 "html"로 변경 가능
          title = "Descriptive Statistics",
          digits = 3,
          covariate.labels = c(
            "LDP Single-party Pref. (0/1)",
            "Political Distrust (1-4)",
            "Political Interest (1-4)",
            "Ideology Score (Blackbox)",
            "Female (0/1)",
            "Highly Educated (0/1)",
            "Living Standard (1-3)",
            "Priority: National Security (0/1)"
          ))

# 3. 범주형 변수 빈도 분석 (Table A2: Categorical Variables)
# -----------------------------------------------------------------
cat("\n### Table A2. Frequency Distribution for Categorical Variables\n")

# 함수를 이용해 범주형 변수들의 빈도와 비율 정리
get_freq_table <- function(data, var_name) {
  data %>%
    group_by(!!sym(var_name)) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(percent = round(n / sum(n) * 100, 1),
           Variable = var_name) %>%
    rename(Category = !!sym(var_name)) %>%
    select(Variable, Category, n, percent)
}

bind_rows(
  get_freq_table(desc_df, "age_group"),
  get_freq_table(desc_df, "party_group"),
  get_freq_table(desc_df, "Technical_competence_cat")
) %>% print()

# 4. 2단계 다층모형용 변수 통계 (Level-2: Partner specific)
# 1. 대상 변수만 선택하고, 완벽한 숫자형(numeric) 벡터로 강제 변환
desc_level2 <- df_long %>%
  select(Accept, dist_ldp_partner, dist_self_partner, is_populist) %>%
  mutate(across(everything(), as.numeric)) 

# 2. 기초통계량 계산 (N, Mean, SD, Min, Max)
table_a3_stats <- data.frame(
  N = sapply(desc_level2, function(x) sum(!is.na(x))),
  Mean = sapply(desc_level2, function(x) mean(x, na.rm = TRUE)),
  SD = sapply(desc_level2, function(x) sd(x, na.rm = TRUE)),
  Min = sapply(desc_level2, function(x) min(x, na.rm = TRUE)),
  Max = sapply(desc_level2, function(x) max(x, na.rm = TRUE))
)

# 3. 소수점 셋째 자리까지 반올림하여 출력
print(round(table_a3_stats, 3))



# ================================================================= #
# [Appendix] 이념 측정 정책 문항 기초통계량 산출
# ================================================================= #

# 1. 분석에 사용된 10개 정책 변수 지정
ideo_policy_vars <- c(
  "defense_strengthen", "nk_pressure", "non_nuclear_3", 
  "small_government",   "rich_tax", 
  "privacy_limit",      "same_sex_marriage", 
  "separate_surname",   "china_threat",    "nuclear_keep"
)

# 2. 데이터 추출 및 숫자형 변환 확인
desc_ideo <- df %>%
  select(all_of(ideo_policy_vars)) %>%
  mutate(across(everything(), as.numeric))

# 3. 기초통계량 계산 (N, Mean, SD, Min, Max)
table_ideo_stats <- data.frame(
  N = sapply(desc_ideo, function(x) sum(!is.na(x))),
  Mean = sapply(desc_ideo, function(x) mean(x, na.rm = TRUE)),
  SD = sapply(desc_ideo, function(x) sd(x, na.rm = TRUE)),
  Min = sapply(desc_ideo, function(x) min(x, na.rm = TRUE)),
  Max = sapply(desc_ideo, function(x) max(x, na.rm = TRUE))
)

# 4. 결과 출력 (소수점 셋째 자리까지)
cat("### Table. Descriptive Statistics for Ideology Policy Variables\n")
print(round(table_ideo_stats, 3))


# ================================================================= #

# 로지스틱 회귀 모델에 대한 VIF 확인
library(car)
vif_results <- vif(model_ctrl2)
print(vif_results)

# ================================================================= #

# 다층 모형(glmer) 전용 공선성 검사
if(!require(performance)) install.packages("performance")
library(performance)

# model2에 대한 VIF 산출
m2_vif <- check_collinearity(model2)
print(m2_vif)

# 시각화 (선택 사항)
plot(m2_vif)

# ================================================================= #
#강건성check 1
# ================================================================= #

# 0. party_id 넣은 강건성 모델
model_rob0 <- glm(ldp_solo ~ female + age_group + living_std + 
                    College + party_id + ideo_score + 
                    pol_distrust + Technical_competence_cat,
                  data = df, family = binomial(link = "logit"))

model_rob1 <- glm(ldp_solo ~ female + age_group + living_std + 
                    College + party_id + ideo_score + 
                    pol_distrust + Technical_competence_cat,
                  data = df, family = binomial(link = "logit"))


# 1. 연령을 연속형으로 넣은 강건성 모델
model_rob2 <- glm(ldp_solo ~ female + age_decadal + living_std + 
                    College + party_group + ideo_score + 
                    pol_distrust + Technical_competence_cat,
                  data = df, family = binomial(link = "logit"))

# 2. 결과 비교 (두 모델의 계수가 비슷한지 확인)
summary(model_ctrl2) # 기존 모델
summary(model_rob0)   # 강건성 체크 모델
summary(model_rob1)
summary(model_rob2)



# ================================================================= #
# 8. 변수 표준화 및 회귀분석 (model_ctrl2)
# ================================================================= #

# 1. 표준화할 연속형 변수 리스트 정의
# (주의: 더미변수나 요인형 변수는 표준화에서 제외하는 것이 해석에 용이함)
vars_to_scale <- c("pol_distrust", "pol_interest","living_std")

# 2. 데이터 프레임 복사본 생성 및 표준화 적용
df_scaled <- df %>%
  mutate(across(all_of(vars_to_scale), ~ as.numeric(scale(.)), .names = "{.col}_std"))

# 3. 표준화된 변수를 사용한 로지스틱 회귀분석
# 기존 변수명 뒤에 '_std'를 붙여 분석을 진행합니다.
model_std2 <- glm(
  ldp_solo ~ female + age_group + living_std_std + 
    College +  
    party_group + ideo_score +
    pol_distrust_std + Technical_competence_cat + pol_interest_std +
    priority_security_dummy,
  data   = df_scaled,
  family = binomial(link = "logit")
)

# 4. 분석 결과 출력
summary(model_std2)

# 5. 오즈비(Odds Ratio) 확인 (표준화된 계수 기반)
exp(coef(model_std2))


# OR 및 95% CI 정리
OR_std <- exp(cbind(OR = coef(model_std2), confint(model_std2))) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("변수") %>%
  rename(CI_lo = `2.5 %`, CI_hi = `97.5 %`) %>%
  mutate(
    p_value = summary(model_ctrl2_std)$coefficients[, 4],
    sig     = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE            ~ ""
    )
  ) %>%
  filter(변수 != "(Intercept)")

cat("\n=== OR 및 신뢰구간 ===\n")
print(OR_std %>% mutate(across(where(is.numeric), ~round(., 3))))


# ================================================================= #
# 8. 표준화 상호작용  
# ================================================================= #


# party_id+ Technical_competence_cat 추가
model_std1 <- glm(
  ldp_solo ~ female+age_group + living_std_std + 
    College +  
    party_id+ ideo_score+
    pol_distrust_std + Technical_competence_cat+pol_interest_std+
    priority_security_dummy,
  data   = df_scaled,
  family = binomial(link = "logit")
)

summary(model_std1)

##
model_std2 <- glm(
  ldp_solo ~ female + age_group + living_std_std + 
    College +  
    party_group + ideo_score +
    pol_distrust_std + Technical_competence_cat + pol_interest_std +
    priority_security_dummy,
  data   = df_scaled,
  family = binomial(link = "logit")
)

# 4. 분석 결과 출력
summary(model_std2)

# Independent* pol_interest
model_std3 <- glm(
  ldp_solo ~ female+age_group + living_std_std + 
    College +  
    party_group*pol_interest_std+ideo_score+
    pol_distrust_std+ Technical_competence_cat+
    priority_security_dummy,
  data   = df_scaled,
  family = binomial(link = "logit")
)

summary(model_std3)


#################

cat("\n[4] pol_distrust * Partner_Type 상호작용 다층 모형 (정치 불신에 따른 연립 파트너 선호 차이)\n")
# 핵심 가설: "정치 불신이 높을수록 공명당(Status_Quo)보다 유신회 등 대안(Conservative_Alt)을 선호하는가?"
model2_std <- glmer(
  Accept ~ female+ living_std + age_scale + College + 
    pol_distrust * Partner_Type + 
    dist_self_partner + dist_ldp_partner+
    (1 | ID), 
  data = df_long, 
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(model2_std)

######################## NA 없는 기술통계량 추출 R 코드

library(dplyr)
library(tidyr)

# 1. model_ctrl2에 투입된 변수만 선택 후 결측치 행 제거 (Listwise Deletion)
df_model_sample <- df %>%
  select(
    ldp_solo, female, age_group, living_std, College, 
    party_group, ideo_score, pol_distrust, 
    Technical_competence_cat, pol_interest, priority_security_dummy
  ) %>%
  drop_na() # 결측치가 하나라도 있는 응답자 제외 -> 실제 회귀분석 N 수와 동일해짐

# 2. 연속형 및 이분형(더미) 변수만 골라서 기술통계 산출
desc_stats <- df_model_sample %>%
  # 범주형 변수(age_group, party_group 등)는 평균/표준편차가 없으므로 제외
  select(
    ldp_solo, pol_distrust, pol_interest, ideo_score, 
    female, College, living_std, priority_security_dummy
  ) %>%
  # 데이터를 길게 변환하여 요약하기 쉽게 만듦
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    N = n(),
    Mean = round(mean(Value), 3),
    `St. Dev.` = round(sd(Value), 3),
    Min = round(min(Value), 3),
    Max = round(max(Value), 3)
  ) %>%
  # 출력을 위해 변수명을 깔끔하게 변경
  mutate(Variable = case_when(
    Variable == "ldp_solo" ~ "LDP Single-party Pref. (0/1)",
    Variable == "pol_distrust" ~ "Political Distrust (1-4)",
    Variable == "pol_interest" ~ "Political Interest (1-4)",
    Variable == "ideo_score" ~ "Ideology Score (Blackbox)",
    Variable == "female" ~ "Female (0/1)",
    Variable == "College" ~ "College (0/1)",
    Variable == "living_std" ~ "Living Standard (1-3)",
    Variable == "priority_security_dummy" ~ "Priority: Nat'l Security (0/1)",
    TRUE ~ Variable
  )) %>%
  # 논문 표 순서에 맞게 정렬 (옵션)
  arrange(match(Variable, c(
    "LDP Single-party Pref. (0/1)", "Political Distrust (1-4)", 
    "Political Interest (1-4)", "Ideology Score (Blackbox)", 
    "Female (0/1)", "College (0/1)", "Living Standard (1-3)", 
    "Priority: Nat'l Security (0/1)"
  )))

# 결과 확인
print(desc_stats)


# 범주형 변수 요약 테이블 생성
cat_summary <- df_model_sample %>%
  select(age_group, party_group, Technical_competence_cat) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Category") %>%
  group_by(Variable, Category) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(Variable) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 1))

print(cat_summary)