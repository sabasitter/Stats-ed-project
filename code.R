## 1. Packages and setup --------------------------------------------------

# Packages

knitr::opts_chunk$set(echo = TRUE)

library(arrow); library(tidyverse); library(lubridate) ; library(dplyr) ; library(easystats); library(gt); library(DescTools)
# set the default plot style
theme_set(theme_minimal())


## 2. Loading and cleaning data ------------------------------------------

# Load data
#Downloading dataset with relevant columns only
smarvus_raw <- read.csv("smarvus_correct_cols.csv")

# Remove participants with no reported grade data or where grade has been reported as 0
smarvus_raw <- smarvus_raw |>
  filter(!grade %in% c(
    "Not Available", 
    NA, 
    "Non complete - LoA", 
    "No stats module on record", 
    "No stats modules", 
    "No access to transcript",
    "-",
    "0",
    "0.00",
    "0%",
    "0.00%"
  ))

# Remove participants who did not answer the anxiety questions 
anxiety_cols <- c(
  paste0("Q7.1_", 1:23),
  paste0("Q7.2_", 1:19),
  paste0("Q8.1_", 1:20),
  paste0("Q8.2_", 1:20)
)

smarvus_filtered <- smarvus_raw |>
  filter(if_all(all_of(anxiety_cols), ~ !is.na(.)))

# Grouping England and Northern Ireland into one country
smarvus_filtered <- smarvus_filtered |>
  mutate(country = if_else(
    country %in% c("England", "Northern Ireland"),
    "United Kingdom",
    country
  ))


## 3. Attention checks ---------------------------------------------------

before <- nrow(smarvus_filtered)

smarvus_filtered <- smarvus_filtered |>
  filter(
    attention_amnesty == "Yes",
    Q7.1_24 == "1",
    Q8.1_21 == "5",
    Q9.1_22 == "1",
    Q11.1_9 == "3",
    Q13.1_17 == "2",
    Q15.1_9 == "4"
  )

# Number of respondents removed
before - nrow(smarvus_filtered)

# Number of respondents remaining
nrow(smarvus_filtered)


## 4. Geographical distribution of respondents ---------------------------

#Finding the number of responses by uni, with the language and creating a table on it 
country_table <- smarvus_filtered |>
  count(country, language, sort = TRUE) |>
  mutate(proportion = n / sum(n)) |>
  gt() |>
  cols_label(
    country = "Country",
    language = "Survey Language",
    n = "Students",
    proportion = "Percentage"
  ) |>
  fmt_percent(columns = proportion, decimals = 1)

country_table

gtsave(country_table, "country_table.png")

#===============================================================================================


## 5. Identifying university grading systems -----------------------------

#Finding the grading convention by uni and creating a table

smarvus_filtered <- smarvus_filtered |>
  mutate(grade_type = case_when(
    grepl("^[0-9]", grade) ~ "Numerical",
    grepl("^[A-F]", grade) ~ "Letter (A-F)",
    TRUE ~ "Other"
  ))

grade_table <- smarvus_filtered |>
  select(university, grade_type, grade_low, grade_high, grade_category_notes) |>
  distinct() |>
  arrange(university) |>
  gt() |>
  cols_label(
    university = "University",
    grade_type = "Grade Type",
    grade_low = "Lowest Grade",
    grade_high = "Highest Grade",
    grade_category_notes = "Grading System"
  )

grade_table

gtsave(grade_table, "grade_table.png")


## 6. Summary of the filtered dataset ------------------------------------

smarvus_filtered |>
  summarise(
    students = n(),
    universities = n_distinct(university, na.rm = TRUE),
    countries = n_distinct(country, na.rm = TRUE),
    languages = n_distinct(language, na.rm = TRUE)
  )


## 7. Calculating anxiety scores -----------------------------------------

# STARS
stars_cols <- paste0("Q7.1_", 1:23)

# R-MARS
rmars_cols <- paste0("Q8.2_", 1:20)

# STARS-M
stars_m_cols <- c(
  paste0("Q8.1_", 1:20),
  "Q7.1_9", "Q7.1_12", "Q7.1_17"
)

# R-MARS-S
rmars_s_cols <- c(
  paste0("Q7.2_", 1:19),
  "Q8.2_11"
)

# Calculate scores
smarvus_filtered <- smarvus_filtered |>
  mutate(
    stars_score = rowSums(pick(all_of(stars_cols))),
    rmars_score = rowSums(pick(all_of(rmars_cols))),
    stars_m_score = rowSums(pick(all_of(stars_m_cols))),
    rmars_s_score = rowSums(pick(all_of(rmars_s_cols)))
  )


## 8.1 Preliminary analysis: University of Sussex ------------------------

# Extract Sussex responses
smarvus_sussex <- smarvus_filtered |>
  filter(university == "University of Sussex")

# Finding number of responses
nrow(smarvus_sussex)

# Making the grade interpretable as a numeric value
smarvus_sussex <- smarvus_sussex |>
  mutate(grade = as.numeric(sub("%", "", grade)))


#Calculating summary statistics
sussex_summary <- smarvus_sussex |>
  summarise(
    across(
      c(stars_score, grade),
      list(
        mean = mean,
        sd = sd,
        median = median,
        min = min,
        max = max
      )
    )
  )

# Visualising distributions
par(mfrow = c(1, 2))

hist(smarvus_sussex$stars_score,
     col = "lightblue2",
     main = "Statistics Anxiety",
     xlab = "STARS Score",
     ylab = "Frequency")

hist(smarvus_sussex$grade,
     col = "indianred1",
     main = "Academic Attainment",
     xlab = "Grade (%)",
     ylab = "Frequency")

par(mfrow = c(1, 1))

# Viewing the descriptive variables
smarvus_sussex |> count(gender)

smarvus_sussex |> count(degree_year)

smarvus_sussex |> count(degree_level_maths)

summary(smarvus_sussex$age)

# Seeing relationship between anxiety and attainment
ggplot(smarvus_sussex,
       aes(x = stars_score, y = grade)) +
  geom_point(alpha = 0.8, col = 'lightblue2') +
  geom_smooth(method = "lm", col = 'indianred1') +
  labs(
    x = "STARS Score",
    y = "Grade (%)",
    title = "Statistics Anxiety and Attainment at Sussex"
  ) +
  theme_minimal()

# Correlation and effect size
cor_result <- cor.test(
  smarvus_sussex$stars_score,
  smarvus_sussex$grade
)

cor_result

effectsize::r_to_d(r = cor_result$estimate)

## 8.2 Brawijaya University --------------------------------------------

# Extract Brawijaya responses
smarvus_brawijaya <- smarvus_filtered |>
  filter(university == "Brawijaya University")

# Finding number of responses
nrow(smarvus_brawijaya)

#Changing grades to numbers 
# Define the order from lowest to highest
grade_levels <- c("E", "D", "D+", "C", "C+", "B", "B+", "A")

smarvus_brawijaya <- smarvus_brawijaya |>
  mutate(grade_numeric = as.numeric(
    factor(grade, levels = grade_levels, ordered = TRUE)
  ))

#Calculating summary statistics
brawijaya_summary <- smarvus_brawijaya |>
  summarise(
    across(
      c(stars_score, grade),
      list(
        mean = mean,
        sd = sd,
        median = median,
        min = min,
        max = max
      )
    ))

# Visualising distributions
par(mfrow = c(1, 2))

hist(smarvus_brawijaya$stars_score,
     col = "steelblue",
     main = 'Statistics Anxiety',
     xlab = "STARS Score",
     ylab = "Frequency")


grade_factor <- factor(
  smarvus_brawijaya$grade,
  levels = c("E", "D", "D+", "C", "C+", "B", "B+", "A")
)

counts <- table(grade_factor)

barplot(
  counts,
  main = "Academic Attainment",
  xlab = "Grade",
  ylab = "Frequency",
  col = "indianred1", 
  cex.names = 0.8
)

par(mfrow = c(1, 1))

# Seeing relationship between anxiety and attainment
ggplot(smarvus_brawijaya,
       aes(x = stars_score, y = grade)) +
  geom_boxplot( col = 'steelblue') +
  labs(
    x = "STARS Score",
    y = "Grade (%)",
    title = "Statistics Anxiety and Attainment at Brawijaya"
  ) +
  theme_minimal()
# Viewing the descriptive variables
smarvus_brawijaya |> count(gender)

smarvus_brawijaya |> count(degree_year)

smarvus_brawijaya |> count(degree_level_maths)

summary(smarvus_brawijaya$age)

#correlation spearmans coefficient

cor_result <- SpearmanRho(
  smarvus_brawijaya$stars_score,
  smarvus_brawijaya$grade,
  conf.level = 0.95
)

cor_result

#turning into cohens d
cor_result <- cor.test(
  smarvus_brawijaya$stars_score,
  smarvus_brawijaya$grade, method = 'spearman'
)

cor_result

effectsize::r_to_d(r = cor_result$estimate)

## 9. Extending the analysis across universities -------------------------



## 10. Meta-analysis ----------------------------------------------------