# Packages

library(arrow); library(tidyverse); library(lubridate) ; library(dplyr) ; library(easystats)
# set the default plot style
theme_set(theme_minimal())

# Load data
smarvus_sussex <- readRDS(gzcon(url("https://www.maths.ed.ac.uk/gkinnear/datasets/smarvus_sussex.Rds")))


# Task 1a
#CHECKS
smarvus_am <- smarvus_sussex |>
  filter(!is.na(attention_amnesty))

smarvus_star <- smarvus_sussex |>
  filter(!is.na(Q7.1_24))

smarvus_m <- smarvus_sussex |>
  filter(!is.na(Q8.1_21))

sum(smarvus_am$attention_amnesty == "Yes")
sum(smarvus_star$Q7.1_24 == "1")
sum(smarvus_m$Q8.1_21 == "5")

# Task 1b
smarvus_sussex_filtered <- smarvus_sussex |>
  filter(attention_amnesty == "Yes", Q7.1_24 == "1", Q8.1_21 == "5")

nrow(smarvus_sussex_filtered)

# Task 2a
smarvus_sussex_scored <- smarvus_sussex_filtered

#add a column that sums up the entries in each row from columns Q7.1_1 to Q7.1_23
df_stars <- smarvus_sussex_scored %>%
  select(contains("Q7.1_")) %>%
  select(-contains("Q7.1_24"))

smarvus_sussex_scored <- smarvus_sussex_scored %>%
  mutate(stars_score =  rowSums(df_stars))

# Task 2b
cat("Summary stats")
summary(smarvus_sussex_scored$stars_score)
dist_stats_anxiety <- hist(smarvus_sussex_scored$stars_score,
     col=(c("lightblue2")),
     main="Distribution of Statistical Anxiety", xlab="Range of Anxiety")


# Task 3a
smarvus_sussex_scored <- smarvus_sussex_scored |>
  filter(!is.na(grade)) |>
  mutate(grade = as.numeric(sub("%", "", grade))) 
 
#visualise distribution
hist(smarvus_sussex_scored$grade,
     col=(c("indianred1")),
     main="Distribution of grades", xlab="Grades Achieved")

#Task 3b
# summary statistics: mean and std
mean_grade <- mean(smarvus_sussex_scored$grade)
std_grade <- sd(smarvus_sussex_scored$grade)

print(mean_grade)
print(std_grade)


# 4(a) scatter plot STARS_score vs grades
scatter.smooth(smarvus_sussex_scored$stars_score, smarvus_sussex_scored$grade,
               col=(c("indianred1")),
               main="Grades Achieved by Statistical Anxiety", xlab="Statistical Anxiety Measured", ylab="Grades Achieved")

# (b) correlation
smarvus_sussex_scored <- smarvus_sussex_scored|>
  filter(!is.na(stars_score))

cor_result <- cor.test(smarvus_sussex_scored$stars_score, smarvus_sussex_scored$grade)
cor_result

# (c) correlation to d 
effectsize::r_to_d(r=cor_result$estimate)


#Task 5
#summary of the values of 'math_edu_level'
ed_level <- table(smarvus_sussex_scored$math_edu_level)
ed_level

smarvus_sussex_scored_exams <- smarvus_sussex_scored %>% filter (math_edu_level == "GCSE" | math_edu_level == "A Level")

boxplot(grade~math_edu_level, data=smarvus_sussex_scored_exams, 
        col=(c("indianred1","lightblue2")),
        main="Exam performance by education level", xlab="Education level")


#correlation and cohens d

smarvus_sussex_scored_exams$math_edu_level <- factor(smarvus_sussex_scored_exams$math_edu_level, levels =c("GCSE", "A Level"))

cor_result <- cor.test(
  as.numeric(smarvus_sussex_scored_exams$math_edu_level), 
  as.numeric(smarvus_sussex_scored_exams$grade)
)
cor_result

effectsize::r_to_d(r=cor_result$estimate)

# Task 6
# R_MARS
df_rmars <- smarvus_sussex_scored %>%
  select(Q8.2_1:Q8.2_20)

smarvus_sussex_scored <- smarvus_sussex_scored %>%
  mutate(rmars_score =  rowSums(df_rmars))



# STARS-M
df_stars_m <- smarvus_sussex_scored %>%
  select(Q7.1_9, Q7.1_12, Q7.1_17, Q8.1_1:Q8.1_20)

smarvus_sussex_scored <- smarvus_sussex_scored %>%
  mutate(stars_m_score =  rowSums(df_stars_m))



# R-MARS-S
df_rmars_s <- smarvus_sussex_scored %>%
  select(Q7.2_1:Q7.2_19, Q8.2_11)

smarvus_sussex_scored <- smarvus_sussex_scored %>%
  mutate(rmars_s_score =  rowSums(df_rmars_s))



# correlation R-MARS and R-MARS-S
rmars_v_rmars_s <- cor.test(smarvus_sussex_scored$rmars_score, smarvus_sussex_scored$rmars_s_score)
print(rmars_v_rmars_s)

# correlation STAR and STAR-M
stars_v_star_m <- cor.test(smarvus_sussex_scored$stars_score, smarvus_sussex_scored$stars_m_score)
print(stars_v_star_m)

# Scatter plot R-MARS and R-MARS-S
scatter.smooth(smarvus_sussex_scored$rmars_score, smarvus_sussex_scored$rmars_s_score)
# Scatter plot STAR and STAR-M
scatter.smooth(smarvus_sussex_scored$stars_score, smarvus_sussex_scored$stars_m_score)
