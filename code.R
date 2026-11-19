## Packages and setup --------------------------------------------------

# Packages

knitr::opts_chunk$set(echo = TRUE)

library(arrow); library(tidyverse); library(lubridate) ; library(dplyr) ; library(easystats); library(gt); library(DescTools);library(correlation);library(effectsize);library(metafor)
# set the default plot style
theme_set(theme_minimal())


## Loading and cleaning data ------------------------------------------

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


# Remove respondents with 2 grades listed
smarvus_filtered <- smarvus_filtered |>
  filter(!(survey_id == "york_can" & grepl(",", grade)))


## Attention checks ---------------------------------------------------

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

# Number of respondents removed due to checks
num_removed <- before - nrow(smarvus_filtered)

# Number of respondents remaining
num_remaining <- nrow(smarvus_filtered)


##  Geographical distribution of respondents ---------------------------

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

gtsave(country_table, "country_table.png")

## Identifying university grading systems -----------------------------

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


gtsave(grade_table, "grade_table.png")


## Summary of the filtered dataset ------------------------------------

smarvus_filtered |>
  summarise(
    students = n(),
    universities = n_distinct(university, na.rm = TRUE),
    countries = n_distinct(country, na.rm = TRUE),
    languages = n_distinct(language, na.rm = TRUE)
  )


## Creating a function to convert numeric grades to a readable format ------------------------------

### Defining the conversion systems
brawijaya_conversion <- c("E", "D", "D+", "C", "C+", "B", "B+", "A")
limerick_conversion <- c("F", "D2", "D1", "C3", "C2", "C1", "B3", "B2", "B1","A2", "A1")
mef_conversion <- c("F", "D", "D+", "C-", "C", "C+",  "B-", "B", "B+","A-", "A")
york_can_conversion <- c("F", "E", "D", "D+", "C", "C+", "B", "B+","A", "A+")
hungary_conversion <- c("1", "2", "3", "4", "5")

numeric_grade <- function(df) {df %>%
    mutate(
      grade = as.character(grade),
      grade_std = case_when(
        # anglia_ruskin and sussex
        survey_id %in% c("anglia_ruskin", "sussex") ~suppressWarnings(as.numeric(sub("%", "", grade))),
        
        # brawijaya
        tolower(survey_id) == "brawijaya" ~
          match(trimws(grade), brawijaya_conversion),
        
        # limerick
        tolower(survey_id) == "limerick" ~
          match(trimws(grade), limerick_conversion),
        
        # mef
        survey_id %in% "mef" ~
          match(grade, brawijaya_conversion),
        
        # york_can
        survey_id %in% "york_can" ~
          match(grade, brawijaya_conversion),
        
        # All other universities stay the same
        survey_id %in% c("bournemouth", "bradford", 
                         "hasan_kalyoncu", "hungary",
                         "la_laguna", "lincoln_1", 
                         "lincoln_2", "macquarie",
                         "queens", "rotterdam", 
                         "southampton", "teesside",
                         "uc_louvain", "vienna", 
                         "western_aus_a", "western_aus_b") ~
          suppressWarnings(as.numeric(grade)),
        
      )
    )
}

###Applying this function to the dataset to get grade column

smarvus_filtered <- numeric_grade(smarvus_filtered)

## Calculating anxiety scores -----------------------------------------

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



#preliminary analysis: University of Sussex ------------------------

# Extract Sussex responses
smarvus_sussex <- smarvus_filtered |>
  filter(university == "University of Sussex")

# Finding number of responses
num_sussex <- nrow(smarvus_sussex)


#Calculating summary statistics
sussex_summary <- smarvus_sussex |>
  summarise(
    across(
      c(stars_score, grade_std),
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
sussex_histograms <- function() {
  
  par(mfrow = c(1, 2))
  
  hist(smarvus_sussex$stars_score,
       col = "steelblue",
       main = "Statistics Anxiety",
       xlab = "STARS Score",
       ylab = "Frequency")
  
  hist(smarvus_sussex$grade_std,
       col = "indianred1",
       main = "Academic Attainment",
       xlab = "Grade (%)",
       ylab = "Frequency")
  
  par(mfrow = c(1, 1))
}

# Viewing the descriptive variables
sussex_demographics <- list(
  Gender = smarvus_sussex |> count(gender),
  Degree_year = smarvus_sussex |> count(degree_year),
  Maths_education = smarvus_sussex |> count(degree_level_maths),
  Age = summary(smarvus_sussex$age)
)

# Seeing relationship between anxiety and attainment
sussex_scatterplot <- ggplot(smarvus_sussex,
         aes(x = stars_score, y = grade_std)) +
  geom_point(alpha = 0.5, col = 'steelblue') +
  geom_smooth(method = "lm", col = 'indianred1') +
  labs(
    x = "STARS Score",
    y = "Grade (%)",
    title = "Statistics Anxiety and Attainment at Sussex"
  ) +
  theme_minimal()


# Correlation and effect size
sussex_ci <- DescTools::SpearmanRho(
  smarvus_sussex$stars_score,
  smarvus_sussex$grade_std,
  conf.level = 0.95
)

sussex_ci

#Converting to Cohen's d
sussex_cor <- cor.test(
  smarvus_sussex$stars_score,
  smarvus_sussex$grade_std, 
  method = 'spearman',
  exact = FALSE
)

effectsize::r_to_d(r = sussex_cor$estimate)

## Brawijaya University --------------------------------------------

# Extract Brawijaya responses
smarvus_brawijaya <- smarvus_filtered |>
  filter(university == "Brawijaya University")

# Finding number of responses
num_brawijaya <- nrow(smarvus_brawijaya)

#Changing grades to numbers 
# Define the order from lowest to highest
smarvus_brawijaya <- smarvus_brawijaya |>
  mutate(grade_std = as.numeric(
    factor(grade, levels = brawijaya_conversion, ordered = TRUE)
  ))

#Calculating summary statistics
brawijaya_summary <- smarvus_brawijaya |>
  summarise(
    across(
      c(stars_score, grade_std),
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
brawijaya_histograms <- function() {par(mfrow = c(1, 2))

hist(smarvus_brawijaya$stars_score,
     col = "steelblue",
     main = 'Statistics Anxiety',
     xlab = "STARS Score",
     ylab = "Frequency")


grade_factor <- factor(
  smarvus_brawijaya$grade,
  levels = brawijaya_conversion
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

par(mfrow = c(1, 1))}

# Seeing relationship between anxiety and attainment
brawijaya_relationship <- ggplot(smarvus_brawijaya,
                                 aes(y = factor(grade, levels = brawijaya_conversion),
                                     x = stars_score)) +
  geom_boxplot(col = "steelblue") +
  labs(
    y = "Grade",
    x = "STARS Score",
    title = "Statistics Anxiety and Attainment at Brawijaya"
  ) +
  theme_minimal()

# Viewing the descriptive variables
brawijaya_demographics <- list(
  Gender = smarvus_brawijaya |> count(gender),
  Degree_year = smarvus_brawijaya |> count(degree_year),
  Maths_education = smarvus_brawijaya |> count(degree_level_maths),
  Age = summary(smarvus_brawijaya$age)
)


#correlation spearmans coefficient
brawijaya_ci <- DescTools::SpearmanRho(
  smarvus_brawijaya$stars_score,
  smarvus_brawijaya$grade_std,
  conf.level = 0.95
)

brawijaya_ci

#Converting to Cohen's d
brawijaya_cor <- cor.test(
  smarvus_brawijaya$stars_score,
  smarvus_brawijaya$grade_std, 
  method = 'spearman',
  exact = FALSE
)

effectsize::r_to_d(r = brawijaya_cor$estimate)


##  Meta-analysis ----------------------------------------------------

### Forest plots
smarvus_cont <- smarvus_filtered
smarvus_cont |>
  filter(survey_id %in% c("brawijaya", "limerick")) |>
  distinct(survey_id, grade, grade_std)
cor_func <- function(uni) {
  d <- smarvus_cont %>% filter(survey_id == uni)
  test <- correlation::cor_test(d, "grade", "stars_score", method = "spearman") #get spearman CIs
  tibble(survey_id = uni,
         n    = test$n_Obs,
         d    = r_to_d(test$rho),
         low  = r_to_d(test$CI_low),
         high = r_to_d(test$CI_high))
}

meta_data <- map_dfr(unique(smarvus_cont$survey_id), cor_func) %>%
  mutate(se = (high - low) / 3.92)

meta_result <- metafor::rma(d, sei = se, slab = survey_id, data = meta_data)
meta_forest <- function() {forest(meta_result)}

## Create table of coefficients for the report
cor_func <- function(uni) {
  d <- smarvus_cont |> filter(survey_id == uni)
  
  test <- correlation::cor_test(
    d, "grade_std", "stars_score",
    method = "spearman"
  )
  
  tibble(
    survey_id = uni,
    n = test$n_Obs,
    rho = test$rho,
    d = r_to_d(test$rho),
    low = r_to_d(test$CI_low),
    high = r_to_d(test$CI_high)
  )
}

meta_data <- map_dfr(unique(smarvus_cont$survey_id), cor_func) |>
  mutate(se = (high - low) / 3.92)
effect_size_table <- meta_data |>
  select(survey_id, n, rho, d, low, high) |>
  arrange(d) |>
  gt() |>
  cols_label(
    survey_id = "Survey ID",
    n = "Sample Size",
    rho = "Spearman's Rho",
    d = "Cohen's d",
    low = "Lower CI",
    high = "Upper CI"
  ) |>
  tab_spanner(
    label = "95% Confidence Interval",
    columns = c(low, high)
  ) |>
  fmt_number(
    columns = c(rho, d, low, high),
    decimals = 3
  )

effect_size_table

gtsave(effect_size_table, "effect_size_table.png")
