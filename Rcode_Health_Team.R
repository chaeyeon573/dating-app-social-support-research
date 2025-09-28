
# Section : Import data

library(readxl)
library(dplyr)
library(writexl)
library(stringr)
library(tidyr)
library(corrplot)
library(DT)
library(Hmisc)
library(gt)
library(car)

df <- read_xlsx("[25-1] 연세대학교 사회조사분석 Health조 설문조사(응답) (5).xlsx")


# Section: Data Manipulation

# 열 이름 short name으로 변환 -> 행개수 맞추기 위해 dating_freq 항목별로 따로 변수 처리
colnames(df)  # 전체 컬럼명 확인
column_name <- c("time_stamp",
                 "point",
                 "gender",
                 "birth",
                 "living",
                 "edu",
                 "eco_lvl",
                 "income",
                 "ties_contact",
                 "ties_emotion",
                 "support_who",
                 "ask_help",
                 "get_help",
                 "convo_comfort",
                 "friend_diff",
                 "criticized",
                 "advice",
                 "social_sat",
                 "companionship",
                 "rel_sat",
                 "lonely",
                 "hopeless",
                 "dating_use",
                 "dating_purpose",
                 "dating_freq_login",
                 "dating_freq_profile",
                 "dating_freq_like",
                 "dating_freq_match",
                 "dating_freq_chat",
                 "dating_freq_call",
                 "dating_freq_meet",
                 "dating_freq_love",
                 "dating_freq_continue",
                 "dating_meetings",
                 "no_dating_reason",
                 "percep_less_lonely",
                 "percep_happy",
                 "percep_surface",
                 "percep_reject",
                 "dating_opinion",
                 "contact")


colnames(df) <- column_name
df <- df |> slice(-1)
df


temp <- df |> 
  # 기본 정보 및 인간관계, 데이팅 앱 사용 정보 처리
  mutate(
    # 성별
    gender = factor(ifelse(gender == "여성", 2, ifelse(gender == "남성", 1, 3)),
                    levels = c(1, 2, 3), labels = c("Man", "Woman", "Other")),
    
    # 나이 계산
    age = 2025 - birth + 1,
    
    # 거주형태
     living = factor(living, levels = c("혼자 거주", "가족과 함께", "룸메이트와 함께", "기타"),
                    labels = c("Alone", "With Family", "With Roommates(s)",
                    "Etc")),
    
    # 학력
    edu = factor(edu,
                 levels = c("초등학교 졸업 이하", "중학교 졸업", "고등학교 졸업",
                            "전문대학 재학 (2~3년제)", "전문대학 졸업 (2~3년제)",
                            "대학교 재학 (4년제)", "대학교 졸업 (4년제)", "대학원 재학 또는 졸업"),
                 labels = c("초졸 이하", "중졸", "고졸", "전문대 재학", "전문대 졸업",
                            "대학 재학", "대학 졸업", "대학원")),
    
    # 경제 수준
    eco_lvl = case_when(
      eco_lvl == "상 (경제적으로 매우 여유있음)" ~ 5,
      eco_lvl == "중상 (다소 여유 있음)" ~ 4,
      eco_lvl == "중 (평균 수준)" ~ 3,
      eco_lvl == "중하 (다소 어려움)" ~ 2,
      eco_lvl == "하층 (경제적으로 매우 어려움)" ~ 1,
      TRUE ~ NA_real_
    ),
    
    # 소득 수준
    income = case_when(
      income == "약 130만 원 이하 (1분위)" ~ 1,
      income == "약 130만 ~ 220만 원 (2분위)" ~ 2,
      income == "약 220만 ~ 280만 원 (3분위)" ~ 3,
      income == "약 280만 ~ 340만 원 (4분위)" ~ 4,
      income == "약 340만 ~ 410만 원 (5분위)" ~ 5,
      income == "약 410만 ~ 490만 원 (6분위)" ~ 6,
      income == "약 490만 ~ 580만 원 (7분위)" ~ 7,
      income == "약 580만 ~ 690만 원 (8분위)" ~ 8,
      income == "약 690만 ~ 860만 원 (9분위)" ~ 9,
      income == "약 860만 원 이상 (10분위)" ~ 10,
      TRUE ~ NA_real_
    ),
    
    # 인간관계 변수
    ties_contact = case_when(
      ties_contact == "없음" ~ 0,
      ties_contact == "1명" ~ 1,
      ties_contact == "2~3명" ~ 2.5,
      ties_contact == "4~5명" ~ 4.5,
      ties_contact == "6~9명" ~ 7.5,
      ties_contact == "10명 이상" ~ 10,
      TRUE ~ NA_real_
    ),
    
    ties_emotion = case_when(
      ties_emotion == "없음" ~ 0,
      ties_emotion == "1명" ~ 1,
      ties_emotion == "2~3명" ~ 2.5,
      ties_emotion == "4~5명" ~ 4.5,
      ties_emotion == "6~9명" ~ 7.5,
      ties_emotion == "10명 이상" ~ 10,
      TRUE ~ NA_real_
    ),
     #social_support 
    criticized_rev = 6 - criticized,
    social_support = rowMeans(cbind(ask_help,
                                    get_help,
                                    advice,
                                    companionship, rel_sat,criticized_rev
                                   ),na.rm = FALSE),
    social_functioning = rowMeans(cbind(6 - friend_diff, convo_comfort), na.rm = TRUE),
    
    
    # 데이팅 앱 이진화 및 사회적 지지/기능
    
    dating_use_bin = case_when(
      dating_use == "예" ~ 1,
      dating_use == "아니오" ~ 0,
      TRUE ~ NA_real_
    ),
    
    social_support = rowMeans(cbind(ask_help, get_help, advice, companionship, rel_sat, criticized_rev), na.rm = FALSE),
    social_functioning = rowMeans(cbind(6 - friend_diff, convo_comfort), na.rm = TRUE),
    
    # 데이팅 앱 활동 빈도
    dating_meetings = case_when(
      dating_meetings == "없음" ~ 0,
      dating_meetings == "1회" ~ 1,
      dating_meetings == "2–3회" ~ 2.5,
      dating_meetings == "4–5회" ~ 4.5,
      dating_meetings == "6회 이상" ~ 6,
      TRUE ~ NA_real_
    ),
    
    depressed = rowMeans(cbind(lonely, hopeless), na.rm = TRUE)
  ) |>
  
  # 활동 빈도 across로 처리
  mutate(across(
    .cols = all_of(c("dating_freq_login", "dating_freq_profile", "dating_freq_like", "dating_freq_match",
                     "dating_freq_chat", "dating_freq_call", "dating_freq_meet", "dating_freq_love", "dating_freq_continue")),
    .fns = ~ case_when(
      .x == "전혀 안 함" ~ 0,
      .x == "월 1회" ~ 1,
      .x == "월 2~3회" ~ 2.5,
      .x == "주 1~2회" ~ 1.5 * 4,
      .x == "주 3회 이상" ~ 3 * 4,
      TRUE ~ NA_real_
    )
  ))


# 데이팅 앱 사용 목적
dating_purpose_raw <- c("연애/결혼", "성적 관계", "친구 만들기", "심심함 해소", "기타")
dating_purpose_var <- c("연애_결혼", "성적관계", "친구만들기", "심심함해소", "기타")

for (i in seq_along(dating_purpose_raw)) {
  temp[[paste0("purpose_", dating_purpose_var[i])]] <- ifelse(str_detect(temp$dating_purpose, fixed(dating_purpose_raw[i])), 1, 0)
}

# 3. 데이팅 앱 미사용 이유
no_dating_raw <- c("충분한 인간관계", "연애/만남에 대한 필요성 부족", "온라인 만남에 대한 거부감",
                   "앱 사용자에 대한 부정적 이미지", "외모 중심의 문화에 불편함을 느낀다", "주변 시선이 신경 쓰인다",
                   "사용법이 어렵거나 복잡하게 느껴진다", "개인정보 유출이나 사기 등의 위험이 걱정된다",
                   "종교적/가치관적 이유로 사용하지 않는다", "앱을 사용할 시간이 부족하다", "기타")

no_dating_en_vars <- c("sufficient_relationships", "no_need", "discomfort_online", "negative_image",
                       "looks_oriented", "social_concern", "difficult_to_use", "privacy_scam_risk",
                       "religious_reasons", "no_time", "other")

for (i in seq_along(no_dating_raw)) {
  temp[[paste0("no_dating_", no_dating_en_vars[i])]] <- ifelse(str_detect(temp$no_dating_reason, fixed(no_dating_raw[i])), 1, 0)
}




# Section: Graph 1- dating app us and social support

library(gtsummary)
library(ggplot2)


# 레이블 영어로 바꾸기 + NA 제거
temp_clean <- temp %>%
  filter(!is.na(dating_use), !is.na(social_support)) %>%
  mutate(dating_use = ifelse(dating_use == "예", "Yes",
                             ifelse(dating_use == "아니오", "No", NA)))

# 요약 통계 계산
summary_df <- temp_clean %>%
  group_by(dating_use) %>%
  dplyr::summarise(
    mean_support = mean(social_support),
    sd_support = sd(social_support),
    n = n(),
    se_support = sd_support / sqrt(n)
  )

# 그래프

ggplot(summary_df, aes(x = dating_use, y = mean_support, fill = dating_use)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean_support - se_support, ymax = mean_support + se_support), width = 0.2) +
  geom_text(aes(label = round(mean_support, 2)), vjust = 5.5, size = 5) +  # 막대 위에 평균값 표시
  scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
  labs(
    title = "Dating App Use and Social Support",
    x = "Dating App Use",
    y = "Social Support (1–5 Likert)"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "none"
  )


# Section: Graph2 - dating app use and depression


# NA 제거 및 레이블 영어로 변환
temp_clean <- temp %>%
  filter(!is.na(dating_use), !is.na(depressed)) %>%
  mutate(dating_use = ifelse(dating_use == "예", "Yes",
                             ifelse(dating_use == "아니오", "No", NA)))

# 요약 통계 계산
summary_df <- temp_clean %>%
  group_by(dating_use) %>%
  dplyr::summarise(
    mean_depressed = mean(depressed),
    sd_depressed = sd(depressed),
    n = n(),
    se_depressed = sd_depressed / sqrt(n)
  )

# 그래프 그리기
ggplot(summary_df, aes(x = dating_use, y = mean_depressed, fill = dating_use)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean_depressed - se_depressed, ymax = mean_depressed + se_depressed), width = 0.2) +
  geom_text(aes(label = round(mean_depressed, 2)), vjust = 5.5, size = 5) +
  scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
  labs(
    title = "Dating App Use and Depression",
    x = "Dating App Use",
    y = "Depression (1–5 Likert)"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "none"
  )


# Section: Graph3 - Dating App Use and Feeling Criticized

# NA 제거 및 레이블 영어로 변환
temp_clean <- temp %>%
  filter(!is.na(dating_use), !is.na(criticized)) %>%
  mutate(dating_use = ifelse(dating_use == "예", "Yes",
                             ifelse(dating_use == "아니오", "No", NA)))

# 요약 통계 계산
summary_df <- temp_clean %>%
  group_by(dating_use) %>%
  dplyr::summarise(
    mean_criticized = mean(criticized),
    sd_criticized = sd(criticized),
    n = n(),
    se_criticized = sd_criticized / sqrt(n)
  )

# 그래프 그리기
ggplot(summary_df, aes(x = dating_use, y = mean_criticized, fill = dating_use)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean_criticized - se_criticized, ymax = mean_criticized + se_criticized), width = 0.2) +
  geom_text(aes(label = round(mean_criticized, 2)), vjust = 5.5, size = 5) +
  scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
  labs(
    title = "Dating App Use and Feeling Criticized",
    x = "Dating App Use",
    y = "Criticized (1–5 Likert)"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "none"
  )


# Section: Table1- Purposes of Using Dating Apps 


# 1. 변수명과 영어 레이블 설정
purpose_labels <- c(
  "Romantic\nrelationships",
  "Sexual Relationship",
  "Making Friends",
  "Boredom Relief",
  "Other"
)
purpose_vars <- c("purpose_연애_결혼", "purpose_성적관계", "purpose_친구만들기", "purpose_심심함해소", "purpose_기타")


# 2. 데이터 변환 (long format)
purpose_long <- user_df %>%
  dplyr::select(all_of(purpose_vars)) %>%
  pivot_longer(cols = everything(), names_to = "purpose", values_to = "selected") %>%
  filter(selected == 1)

# 3. 영문 라벨 적용
purpose_long$purpose <- factor(purpose_long$purpose,
                               levels = purpose_vars,
                               labels = purpose_labels)



# 4. 시각화
ggplot(purpose_summary, aes(x = reorder(purpose, -percent), y = percent)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_text(aes(label = paste0(percent, "%")), vjust = -0.5, size = 5) +
  labs(title = "Purposes of Using Dating Apps",
       x = NULL, y = "Percentage (%)") +
  ylim(0, max(purpose_summary$percent) + 10) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),
        axis.title = element_text(size = 13, face = "bold"),
        plot.title = element_text(size = 15, face = "bold"))



# Section: Correlation Table 2 -Correlation Between Prior Dating Apps Experience and Social Support and Depression


# 루프를 돌며 상관관계 계산
library(corrplot)

# 결과 저장할 데이터프레임 초기화
results <- data.frame(
  variable = character(),
  correlation = numeric(),
  p_value = numeric()
)
# 변수 목록
vars <- c("ask_help", "get_help", "convo_comfort", "friend_diff", "criticized", 
          "advice", "social_sat", "companionship", "rel_sat", "lonely", "hopeless")


# 루프를 돌며 상관관계 계산
for (v in vars) {
  temp_df <- temp[, c("dating_use_bin", v)] |> na.omit()
  r <- cor(temp_df$dating_use_bin, temp_df[[v]], method = "pearson")
  p <- cor.test(temp_df$dating_use_bin, temp_df[[v]], method = "pearson")$p.value
  results <- rbind(results, data.frame(variable = v, correlation = r, p_value = p))
}

# 상관계수 순으로 정렬
results <- results |> arrange(desc(abs(correlation)))


# 변수 설명 매핑 (영어 요약)
name_map_eng <- c(
  ask_help = "Can ask for help",
  get_help = "Has someone to help",
  convo_comfort = "Comfortable in conversations",
  friend_diff = "Hard to make new friends",
  criticized = "Feels criticized by others",
  advice = "Has someone for advice",
  social_sat = "Satisfied with social interactions",
  companionship = "Has someone when feeling lonely",
  rel_sat = "Satisfied with relationships",
  lonely = "Felt lonely recently",
  hopeless = "Felt hopeless recently"
)

# 테이블 생성
pretty_results_eng <- results %>%
  filter(!is.na(correlation)) %>%
  mutate(
    Variable = name_map_eng[variable],
    Correlation = round(correlation, 3),
    P_value = round(p_value, 3)
  ) %>%
  filter(P_value < 0.5) %>%    
  arrange(desc(abs(Correlation))) %>%
  select(Variable, Correlation, P_value)

# DT 테이블 출력
datatable(
  pretty_results_eng,
  caption = "Correlation Between Prior Dating Apps Experience and Social Support and Depression",
  options = list(pageLength = 15, dom = 't'),
  rownames = FALSE
)


#Section: table 1- The Relationship Between Dating App Activities and Psychosocial Factors
library(Hmisc)
library(DT)

# 변수 이름 매핑
pretty_names <- c(
  dating_freq_love     = "Developed into relationship",
  dating_freq_call     = "Phone or video call",
  percep_lonely        = "App lowers loneliness",
  percep_surface       = "App feels superficial",
  percep_reject        = "Rejection in apps lowers self-esteem",
  percep_happy         = "App makes me feel happier",
  lonely               = "Felt lonely recently",
  hopeless             = "Felt hopeless recently",
  social_support       = "Received social support",
  advice               = "Has someone for advice"
)


colnames(user_df)

# 분석할 변수쌍 리스트
pair_list <- list(
  c("dating_freq_love", "percep_lonely"),
  c("dating_freq_call", "percep_lonely"),
  c("dating_freq_call", "percep_surface"),
  c("percep_happy", "lonely"),
  c("dating_freq_love", "percep_reject"),
  c("percep_happy", "advice")
)

# 결과 저장용 빈 데이터프레임
cor_results <- data.frame(
  Variable1 = character(),
  Variable2 = character(),
  Correlation = numeric(),
  P_value = numeric()
)

# 루프 한 번만 수행
for (pair in pair_list) {
  v1 <- pair[1]
  v2 <- pair[2]
  
  temp_df <- user_df[, c(v1, v2)] %>% 
    mutate(across(everything(), as.numeric)) %>%
    drop_na()
  
  # 유효한 데이터가 적어도 3개 이상일 때만 분석
  if (nrow(temp_df) >= 3) {
    cor_test <- rcorr(as.matrix(temp_df))
    cor_val <- cor_test$r[1, 2]
    p_val <- cor_test$P[1, 2]
    
    cor_results <- rbind(cor_results, data.frame(
      Variable1 = pretty_names[[v1]],
      Variable2 = pretty_names[[v2]],
      Correlation = round(cor_val, 3),
      P_value = round(p_val, 3)
    ))
  }
}

#Table 출력
datatable(
  cor_results,
  caption = "The Relationship Between Dating App Activities and Psychosocial Factors",
  options = list(dom = 't', pageLength = 10),
  rownames = FALSE
)


#Section: T-test/ Chi -Sqaure test - non users vs users
library(gt)
library(gtsummary)
library(plyr)
library(dplyr)
library(webshot2)


# 매핑
edu_levels_english <- c(
  "전문대 재학" = "Some College(2yr)",
  "대학 재학"   = "Currently in University",
  "대학 졸업"   = "University Graduate",
  "대학원"     = "Graduate School"
)

eco_levels_english <- c(
  "2" = "Lower-middle",
  "3" = "Middle",
  "4" = "Upper-middle",
  "5" = "High"
)

dating_use_english <- c(
  "아니오" = "Non-User",
  "예"    = "User"
)

# 전처리
temp_clean <- temp %>%
  mutate(
    edu = trimws(as.character(edu)),
    eco_lvl = trimws(as.character(eco_lvl)),
    dating_use = trimws(as.character(dating_use))
  ) %>%
  filter(
    edu %in% names(edu_levels_english),
    eco_lvl %in% names(eco_levels_english),
    dating_use %in% names(dating_use_english)
  ) %>%
  mutate(
    education = revalue(edu, edu_levels_english),
    education = factor(education, levels = edu_levels_english),
    
    economic_status = revalue(eco_lvl, eco_levels_english),
    economic_status = factor(economic_status, levels = eco_levels_english),
    
    dating_use = revalue(dating_use, dating_use_english),
    dating_use = factor(dating_use, levels = c("Non_User", "User"))
  )

# 연속형 변수
continuous_vars <- c("criticized", "social_support", "depressed")

# 테이블 생성 및 이미지 저장
summary_table <- temp_clean %>%
  dplyr::select(gender, age, economic_status, education, all_of(continuous_vars), dating_use) %>%
  tbl_summary(
    by = dating_use,
    label = list(
      gender ~ "Gender",
      age ~ "Age (Years)",
      education ~ "Education Level",
      economic_status ~ "Economic Status",
      criticized ~ "Feels Criticized",
      social_support~ "Social Support",
      depressed ~"Depressed"),
    type = all_of(continuous_vars) ~ "continuous",
    statistic = list(all_continuous() ~ "{mean} ± {sd}"),
    digits = all_continuous() ~ 2,
    missing = "no"
  ) %>%
  add_p(test = all_continuous() ~ "t.test") %>% 
  modify_header(label ~ "**Variable**") %>%
  bold_labels() %>%
  as_gt() %>%
  tab_header(
    title = md("**Relationship between Prior Dating App Experience and<br> Feeling Criticized, Depressed, or Supported**"),
    subtitle = md("T-test and Chi-square Test: Dating App Users vs. Non-users")
  )
summary_table
gtsave(summary_table, filename = "Descriptive Statistics2.png")


# Section: Descriptive Statistics (users vs non-users)


summary_table2 <- temp_clean %>%
  dplyr::select(
    gender, age, living, education, economic_status,  ties_contact, ties_emotion,
    ask_help, get_help, criticized, advice, social_support,
    social_sat, companionship, rel_sat, depressed, dating_use,  convo_comfort, friend_diff,social_functioning
  ) %>%
  tbl_summary(
    by = dating_use,
    type = list(
      c("ties_contact", "ties_emotion", "ask_help", "get_help", "convo_comfort",
        "friend_diff", "criticized", "advice", "social_support", "social_sat",
        "companionship", "rel_sat", "depressed", "social_functioning") ~ "continuous"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      gender ~ "Gender",
      age ~ "Age (Years)",
      education ~ "Education Level",
      living ~ "Living",
      economic_status ~ "Economic Status",
      ties_contact       ~ "No. of Regular Contacts",
      ties_emotion       ~ "No. of Emotional Contacts",
      ask_help           ~ "Can Ask for Help",
      get_help           ~ "Someone Willing to Help",
      criticized         ~ "Feels Criticized",
      advice             ~ "Has Someone to Advise",
      social_support     ~ "Overall Social Support",
      social_sat         ~ "Meeting Frequency Satisfaction",
      companionship      ~ "Feels Accompanied",
      rel_sat            ~ "Relationship Satisfaction",
      depressed          ~ "Depression",
      convo_comfort      ~ "Comfort in Conversations",
      friend_diff        ~ "Difficulty Making Friends",
      social_functioning ~ "Social Functioning"
    ),
    digits = all_continuous() ~ 2,
    missing = "no"
  ) %>%
  add_p(
    test = list(
      all_continuous() ~ "t.test",
      all_categorical() ~ "chisq.test"
    ),
    test.args = all_categorical() ~ list(simulate.p.value = TRUE)
  ) %>%
  modify_header(label ~ "**Variable**") %>%
  bold_labels() %>%
  as_gt() %>%
  tab_header(
    title = md("**Descriptive Statistics by Dating App Use**"),
    subtitle = md("Comparison of demographic, social, and psychological variables")
  )

gtsave(summary_table2, filename = "Descriptive Statistics1.png")

library(dplyr)
library(tidyr)
library(DT)

# 1. continuous 변수 정의
continuous_vars <- c(
  "ties_contact", "ties_emotion", "ask_help", "get_help", "convo_comfort",
  "friend_diff", "criticized", "advice", "social_support", "social_sat",
  "companionship", "rel_sat", "depressed", "social_functioning"
)

desc_stats <- temp %>%
  dplyr::summarise(across(all_of(continuous_vars),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", ".value"),
    names_pattern = "^(.*)_(mean|sd)$"
  ) %>%
  mutate(`Mean ± SD` = sprintf("%.2f ± %.2f", mean, sd)) %>%
  select(Variable, `Mean ± SD`)


# 3. 라벨 적용 (원하는 순서와 이름으로)
label_dict <- c(
  ties_contact       = "No. of Regular Contacts",
  ties_emotion       = "No. of Emotional Contacts",
  ask_help           = "Can Ask for Help",
  get_help           = "Someone Willing to Help",
  convo_comfort      = "Comfort in Conversations",
  friend_diff        = "Difficulty Making Friends",
  criticized         = "Feels Criticized",
  advice             = "Has Someone to Advise",
  social_support     = "Overall Social Support",
  social_sat         = "Meeting Frequency Satisfaction",
  companionship      = "Feels Accompanied",
  rel_sat            = "Relationship Satisfaction",
  depressed          = "Depression",
  social_functioning = "Social Functioning"
)

desc_stats$Variable <- label_dict[desc_stats$Variable]

# 4. DT 테이블 출력
datatable(desc_stats,
          caption = "Descriptive Statistics (Mean ± SD)",
          options = list(
            dom = 't',           # 검색창, 페이지네이션 등 제거
            ordering = FALSE     # 정렬 비활성화
          ),
          rownames = FALSE)



# 필요한 패키지 불러오기
library(dplyr)
library(DT)

# 평균 ± 표준편차를 원하는 변수 지정
vars <- c(
  "income", "ties_contact", "ties_emotion",
  "ask_help", "get_help", "convo_comfort", "friend_diff",
  "criticized", "advice", "social_support", "social_sat",
  "companionship", "rel_sat", "depressed", "social_functioning"
)

# 예쁜 이름 매핑
pretty_names <- c(
  income = "Income",
  ties_contact = "Contact with Ties",
  ties_emotion = "Emotional Closeness with Ties",
  ask_help = "Asked for Help",
  get_help = "Received Help",
  convo_comfort = "Comfortable Conversation",
  friend_diff = "Friendship Difficulty",
  criticized = "Criticism",
  advice = "Received Advice",
  social_support = "Social Support",
  social_sat = "Satisfaction with Social Life",
  companionship = "Companionship",
  rel_sat = "Relationship Satisfaction",
  depressed = "Depression Level",
  social_functioning = "Social Functioning"
)

temp %>%
  dplyr::summarise(across(all_of(vars),
                          list(Mean = ~mean(.x, na.rm = TRUE),
                               SD = ~sd(.x, na.rm = TRUE)),
                          .names = "{.col}_{.fn}")) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c("Variable", ".value"),
    names_pattern = "^(.*)_(Mean|SD)$"
  ) %>%
  mutate(
    Variable = pretty_names[Variable],
    `Mean ± SD` = sprintf("%.2f ± %.2f", Mean, SD)
  ) %>%
  select(Variable, `Mean ± SD`) %>%
  DT::datatable(
    caption = "Descriptive Statistics (Mean ± SD)",
    options = list(
      dom = 't',        # 검색창, 페이지 숨기기
      ordering = FALSE  # 정렬 비활성화
    )
  )



library(dplyr)
library(tidyr)
library(gtsummary)


#Section: Table- Perceptions of Dating App among Users

# 1. 변수 지정
vars <- c("percep_lonely", "percep_happy", "percep_surface", "percep_reject")

# 2️. 깔끔한 Summary Table 코드 (에러 없는 버전)
percep_summary <- user_df %>%
  dplyr::summarise(across(all_of(vars),
                          list(mean = ~mean(.x, na.rm = TRUE),
                               sd = ~sd(.x, na.rm = TRUE)),
                          .names = "{.col}_{.fn}")) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c("variable", "stat"),
    names_pattern = "^(.*)_(mean|sd)$"
  ) %>%
  tidyr::pivot_wider(names_from = stat, values_from = value)

# 3. Label English version
percep_summary$variable <- factor(percep_summary$variable,
                                  levels = vars,
                                  labels =  c("Loneliness↓", "Happiness↑", "Superficial", "Self-Esteem↓"))
# 4. Plot with mean score labels
ggplot(percep_summary, aes(x = variable, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.6) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2, color = "black") +
  geom_text(aes(label = round(mean, 2)), vjust = 13.5, size = 4) +  # 점수 표시
  labs(title = "Perceptions of Dating Apps Among Users (Mean ± SD)",
       x = NULL, y = "Mean Score") +
  ylim(0, 5.5) +  # ylim을 높여 텍스트 안 잘리게
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),
        axis.title = element_text(size = 13, face = "bold"),
        plot.title = element_text(size = 15, face = "bold"))



# Section: Reasons for not using dating aps

library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)

# 1. 관련 변수만 선택 (영문 변수만)
no_dating_vars <- temp %>%
  dplyr::select(matches("^no_dating_[a-z_]+$")) %>%
  dplyr::select(-no_dating_reason)  # 이 줄 추가


# 2. 변수별 빈도 집계
reason_counts <- no_dating_vars %>%
  dplyr::summarise(across(everything(), ~ sum(.x == 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "reason", values_to = "count") %>%
  mutate(reason = str_remove(reason, "no_dating_"))

# 3. 영어 라벨 (언더바 제거 및 보기 좋게 정리)
reason_labels <- c(
  "sufficient_relationships" = "Sufficient Relationships",
  "no_need"                  = "No Need",
  "discomfort_online"        = "Discomfort with Online Dating",
  "negative_image"           = "Negative Image of App Users",
  "looks_oriented"           = "Appearance-Focused Culture",
  "social_concern"           = "Concern About Social Perception",
  "difficult_to_use"         = "Too Difficult to Use",
  "privacy_scam_risk"        = "Privacy or Scam Risk",
  "religious_reasons"        = "Religious or Value Reasons",
  "no_time"                  = "Lack of Time",
  "other"                    = "Other"
)

# 4. 라벨 적용 및 정렬
reason_counts <- reason_counts %>%
  dplyr::mutate(
    reason = as.character(reason),  
    reason_label = dplyr::recode(reason, !!!reason_labels),
    reason_label = forcats::fct_reorder(reason_label, count, .desc = FALSE)
  )

# 5. 가로형 막대그래프
ggplot(reason_counts, aes(x = reason_label, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_text(aes(label = count), hjust = -0.1, size = 4) +
  labs(
    title = "Reasons for Not Using Dating Apps",
    x = NULL, y = "Number of Respondents"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 13, face = "bold"),
    plot.title = element_text(size = 15, face = "bold")
  )


#Section: Regression - model 1
 


#회귀분석 실행하기 전, 데이터 NA 처리
complete_df_1 <- temp |> 
  dplyr::select(dating_use_bin,gender,age,eco_lvl,edu) |> drop_na() 

model1 <- glm(dating_use_bin~gender+age+eco_lvl+edu, data = complete_df_1,
              family = binomial())

summary(model1)
vif(model1)

#Section: Regression - model 2

complete_df_2 <- temp %>%
  dplyr::select(dating_use_bin, gender, age, eco_lvl, get_help) %>%
  drop_na()

model2 <- glm(dating_use_bin~gender+age+eco_lvl+get_help, data = complete_df_2)
summary(model2)
vif(model2)

#Section: Regression - model3 

complete <- temp |> select(gender, age,eco_lvl, criticized, lonely,dating_use_bin,get_help) |> 
  drop_na()

model3 <- glm(dating_use_bin ~ gender + age + eco_lvl + criticized+lonely+get_help,
              data = complete,
              family = binomial())
summary(model3)
vif(model3)


# Section: Regression - model4
used_df <- temp |> 
  filter(dating_use_bin==1)



complete_df_4 <- used_df |> select(gender, age,eco_lvl, percep_less_lonely,percep_happy, percep_surface,percep_reject,rel_sat,
                  lonely) |> drop_na()

model4 <-  lm(lonely~eco_lvl+ gender+age+percep_less_lonely+ percep_happy+
                percep_surface+ percep_reject+rel_sat, data = complete_df_4 )

summary(model4)
vif(model4)


# Section: 이미지 출력
library(broom)
library(dplyr)
library(tidyr)
library(purrr)
library(DT)

# 모델 리스트 (Model 1~3)
models <- list(
  "Model 1" = model1,
  "Model 2" = model2,
  "Model 3" = model3
)

# 변수 이름 매핑
name_map <- c(
  "(Intercept)"     = "Intercept",
  "genderWoman"     = "Gender: Woman",
  "age"             = "Age",
  "eco_lvl"         = "Economic Level",
  "get_help"        = "Has Someone to Help",
  "criticized"      = "Feels Criticized",
  "lonely"          = "Felt Lonely"
)

# ★ 별표 함수
add_stars <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.1 ~ ".",
    TRUE ~ ""
  )
}

extract_summary <- function(model, model_name) {
  tidy(model) %>%
    mutate(
      Variable = name_map[term],
      `Coefficient (SE)` = paste0(
        sprintf("%.3f", estimate), add_stars(p.value),
        " (", sprintf("%.3f", std.error), ")"
      ),
      `p-value` = sprintf("%.3f", p.value)
    ) %>%
    select(Variable, `Coefficient (SE)`, `p-value`) %>%
    rename_with(~ paste(model_name, c("Coef. (SE)", "p-value")), -Variable)
}


# 1~3 모델 요약 병합
summary_list <- map2(models, names(models), extract_summary)

final_model_123 <- reduce(summary_list, full_join, by = "Variable") %>%
  arrange(match(Variable, name_map))  # 순서 정리


# 변수 이름 매핑 (Model 4 전용)
name_map4 <- c(
  "(Intercept)"       = "Intercept",
  "eco_lvl"           = "Economic Level",
  "genderWoman"       = "Gender: Woman",
  "age"               = "Age",
  "percep_less_lonely" = "Perceived Less Loneliness",
  "percep_happy"      = "Perceived Happiness",
  "percep_surface"    = "Perceived Superficiality",
  "percep_reject"     = "Perceived Rejection",
  "rel_sat"           = "Relationship Satisfaction"
)

# tidy 후 정리
model4_summary <- tidy(model4) %>%
  mutate(
    Term = name_map4[term],
    Coef_SE = paste0(sprintf("%.3f", estimate), add_stars(p.value),
                     " (", sprintf("%.3f", std.error), ")"),
    `p-value` = sprintf("%.3f", p.value)
  ) %>%
  select(Term, `Coefficient (SE)` = Coef_SE, `p-value`)


library(gt)
library(webshot2)

# 📌 Model 1–3 결과 저장 (final_model_123을 gt 테이블로 변환)
gt_model_123 <- final_model_123 %>%
  gt() %>%
  sub_missing(columns = everything(), missing_text = "") %>% 
  tab_header(
    title = md("**Logistic Regression Results: Predicting Dating App Use (Models 1–3)**")
  ) %>%
  cols_label(
    Variable = "Variable"
  ) %>%
  tab_options(
    table.font.size = px(14),
    data_row.padding = px(4)
  )

# PNG로 저장
gtsave(gt_model_123, "model1_3_results.png")



# 📌 Model 1–3 결과 저장 (final_model_123을 gt 테이블로 변환)
gt_model_4 <- model4_summary %>%
  gt() %>%
  sub_missing(columns = everything(), missing_text = "") %>% 
  tab_header(
    title = md("**Linear Regression Results: Predicting Loneliness Among Dating App Users (Model 4)**")
  ) %>%
  cols_label(
    Term = "Variable"
  ) %>%
  tab_options(
    table.font.size = px(14),
    data_row.padding = px(4)
  )

# PNG로 저장
gtsave(gt_model_4, "model4results.png")















