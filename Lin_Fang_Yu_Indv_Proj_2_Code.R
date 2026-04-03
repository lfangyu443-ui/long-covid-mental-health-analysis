# ============================================================
# SPH5103 Individual Project 2
# Full Analysis Script: C1 to C4
# Topic: Long COVID and Mental Health (Depression & Anxiety)
# Data: 2023 National Health Interview Survey (NHIS)
# Author: Lin Fang Yu
# ============================================================
# INSTRUCTIONS:
# 1. Set your working directory to the folder containing adult23.csv
# 2. Run the entire script from top to bottom
# 3. All outputs (tables, figures) will be saved in your working directory
# ============================================================

# ---- Install packages if needed ----
# install.packages(c("tidyverse", "gridExtra"))

library(tidyverse)
library(gridExtra)
library(grid)


# ============================================================
# C1: CLEANING AND RECODING DATASET
# ============================================================

# ---- Load the raw NHIS dataset ----
# IMPORTANT: stringsAsFactors=FALSE prevents truncation of 2-digit codes
# This is recommended by the NHIS readme file included with the dataset
nhis_raw <- read.csv("adult23.csv", stringsAsFactors = FALSE)

# Convert all columns to numeric
# Note: NAs introduced by coercion is expected — blank fields correctly become NA
suppressWarnings(
  nhis_raw <- nhis_raw %>% mutate(across(everything(), as.numeric))
)

# Check initial sample size
cat("Step 1 - Variables selected. Sample size:", nrow(nhis_raw), "\n")
# Expected: 29,522


# ---- STEP 1: Keep only variables needed for analysis ----
nhis <- nhis_raw %>%
  select(
    EVERCOVD_A,   # Ever had COVID-19 - used to filter sample
    LONGCOVD1_A,  # Long COVID (symptoms 3+ months) - EXPOSURE
    DEPEV_A,      # Ever told had depression - OUTCOME 1
    ANXEV_A,      # Ever told had anxiety disorder - OUTCOME 2
    AGEP_A,       # Age (top-coded at 85) - CONFOUNDER
    SEX_A,        # Sex - CONFOUNDER
    EDUCP_A,      # Education level - CONFOUNDER
    RATCAT_A,     # Income-to-poverty ratio - CONFOUNDER
    NOTCOV_A      # Health insurance status - CONFOUNDER
  )


# ---- STEP 2: Filter to adults who ever had COVID-19 ----
# LONGCOVD1_A only asked of those with COVID (EVERCOVD_A = 1)
# EVERCOVD_A: 1 = Yes, 2 = No, 7/8/9 = missing
nhis <- nhis %>% filter(EVERCOVD_A == 1)
cat("Step 2 - Filtered to adults who ever had COVID. Sample size:", nrow(nhis), "\n")
# Expected: 15,354


# ---- STEP 3: Recode EXPOSURE (Long COVID) ----
# LONGCOVD1_A: 1 = Yes, 2 = No, 9 = Don't know
nhis <- nhis %>%
  mutate(
    longcovid = case_when(
      LONGCOVD1_A == 1 ~ 1,
      LONGCOVD1_A == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    longcovid = factor(longcovid, levels = c(0, 1),
                       labels = c("No Long COVID", "Long COVID"))
  )
cat("Step 3 - Long COVID recoded.\n")
table(nhis$longcovid, useNA = "always")


# ---- STEP 4: Recode OUTCOMES ----

# Depression (DEPEV_A): 1 = Yes, 2 = No, 7/9 = missing
nhis <- nhis %>%
  mutate(
    depression = case_when(
      DEPEV_A == 1 ~ 1,
      DEPEV_A == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    depression = factor(depression, levels = c(0, 1),
                        labels = c("No Depression", "Depression"))
  )
cat("Step 4a - Depression recoded.\n")
table(nhis$depression, useNA = "always")

# Anxiety (ANXEV_A): 1 = Yes, 2 = No, 7/9 = missing
nhis <- nhis %>%
  mutate(
    anxiety = case_when(
      ANXEV_A == 1 ~ 1,
      ANXEV_A == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    anxiety = factor(anxiety, levels = c(0, 1),
                     labels = c("No Anxiety", "Anxiety"))
  )
cat("Step 4b - Anxiety recoded.\n")
table(nhis$anxiety, useNA = "always")


# ---- STEP 5: Recode CONFOUNDERS ----

# Age: recoded into 4 groups consistent with prior NHIS analyses
nhis <- nhis %>%
  mutate(
    age_group = case_when(
      AGEP_A >= 18 & AGEP_A <= 34 ~ "18-34",
      AGEP_A >= 35 & AGEP_A <= 49 ~ "35-49",
      AGEP_A >= 50 & AGEP_A <= 64 ~ "50-64",
      AGEP_A >= 65               ~ "65+",
      TRUE ~ NA_character_
    ),
    age_group = factor(age_group, levels = c("18-34", "35-49", "50-64", "65+"))
  )
cat("Step 5a - Age groups recoded.\n")

# Sex: 1 = Male, 2 = Female
nhis <- nhis %>%
  mutate(
    sex = case_when(
      SEX_A == 1 ~ "Male",
      SEX_A == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    sex = factor(sex, levels = c("Male", "Female"))
  )
cat("Step 5b - Sex recoded.\n")

# Education: collapsed from 10 codes into 4 categories
# 1-3 = Less than HS | 4 = HS/GED | 5-6 = Some college | 7-10 = Bachelor's+
# 97/99 = Refused/Don't know -> missing
nhis <- nhis %>%
  mutate(
    education = case_when(
      EDUCP_A %in% c(1, 2, 3)     ~ "Less than high school",
      EDUCP_A == 4                 ~ "High school/GED",
      EDUCP_A %in% c(5, 6)        ~ "Some college/Associate",
      EDUCP_A %in% c(7, 8, 9, 10) ~ "Bachelor's or higher",
      TRUE ~ NA_character_
    ),
    education = factor(education,
                       levels = c("Less than high school", "High school/GED",
                                  "Some college/Associate", "Bachelor's or higher"))
  )
cat("Step 5c - Education recoded.\n")

# Income: recoded into 5 FPL categories
nhis <- nhis %>%
  mutate(
    income_cat = case_when(
      RATCAT_A == 1                    ~ "< 100% FPL",
      RATCAT_A %in% c(2, 3, 4)        ~ "100-199% FPL",
      RATCAT_A %in% c(5, 6, 7)        ~ "200-299% FPL",
      RATCAT_A %in% c(8, 9, 10)       ~ "300-399% FPL",
      RATCAT_A %in% c(11, 12, 13, 14) ~ "400%+ FPL",
      TRUE ~ NA_character_
    ),
    income_cat = factor(income_cat,
                        levels = c("< 100% FPL", "100-199% FPL",
                                   "200-299% FPL", "300-399% FPL",
                                   "400%+ FPL"))
  )
cat("Step 5d - Income recoded.\n")

# Insurance: 1 = Uninsured, 2 = Insured
nhis <- nhis %>%
  mutate(
    insurance = case_when(
      NOTCOV_A == 1 ~ "Uninsured",
      NOTCOV_A == 2 ~ "Insured",
      TRUE ~ NA_character_
    ),
    insurance = factor(insurance, levels = c("Insured", "Uninsured"))
  )
cat("Step 5e - Insurance recoded.\n")


# ---- STEP 6: Keep recoded variables and drop missing ----
nhis_clean <- nhis %>%
  select(longcovid, depression, anxiety,
         age_group, sex, education, income_cat, insurance)

cat("Before dropping missing values. Sample size:", nrow(nhis_clean), "\n")
nhis_clean <- nhis_clean %>% drop_na()
cat("After dropping missing values. Final analytic sample size:", nrow(nhis_clean), "\n")
# Expected: 15,158


# ---- STEP 7: Final check ----
glimpse(nhis_clean)
summary(nhis_clean)

# ---- STEP 8: Save clean dataset ----
write.csv(nhis_clean, "nhis_clean.csv", row.names = FALSE)
cat("Clean dataset saved as nhis_clean.csv\n\n")


# ============================================================
# C2: UNIVARIABLE ANALYSIS
# ============================================================

cat("============================================================\n")
cat("C2: UNIVARIABLE ANALYSIS\n")
cat("============================================================\n\n")

n_all <- nrow(nhis_clean)
n_no  <- sum(nhis_clean$longcovid == "No Long COVID")
n_yes <- sum(nhis_clean$longcovid == "Long COVID")

# Prevalence of key variables
cat("--- Exposure: Long COVID ---\n")
nhis_clean %>% count(longcovid) %>%
  mutate(percent = round(n/sum(n)*100, 1)) %>% print()

cat("\n--- Outcome 1: Depression ---\n")
nhis_clean %>% count(depression) %>%
  mutate(percent = round(n/sum(n)*100, 1)) %>% print()

cat("\n--- Outcome 2: Anxiety ---\n")
nhis_clean %>% count(anxiety) %>%
  mutate(percent = round(n/sum(n)*100, 1)) %>% print()

cat("\n--- Age group ---\n")
nhis_clean %>% count(age_group) %>%
  mutate(percent = round(n/sum(n)*100, 1)) %>% print()

cat("\n--- Sex ---\n")
nhis_clean %>% count(sex) %>%
  mutate(percent = round(n/sum(n)*100, 1)) %>% print()

cat("\n--- Education ---\n")
nhis_clean %>% count(education) %>%
  mutate(percent = round(n/sum(n)*100, 1)) %>% print()

cat("\n--- Income ---\n")
nhis_clean %>% count(income_cat) %>%
  mutate(percent = round(n/sum(n)*100, 1)) %>% print()

cat("\n--- Insurance ---\n")
nhis_clean %>% count(insurance) %>%
  mutate(percent = round(n/sum(n)*100, 1)) %>% print()

# Figure 1: Prevalence bar chart
prevalence_data <- tibble(
  Variable   = c("Long COVID", "Depression\n(ever diagnosed)",
                 "Anxiety\n(ever diagnosed)"),
  Prevalence = c(
    round(mean(nhis_clean$longcovid  == "Long COVID")  * 100, 1),
    round(mean(nhis_clean$depression == "Depression")  * 100, 1),
    round(mean(nhis_clean$anxiety    == "Anxiety")     * 100, 1)
  )
)

cat("\nFigure 1 prevalence data:\n")
print(prevalence_data)

figure1 <- ggplot(prevalence_data, aes(x = Variable, y = Prevalence, fill = Variable)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = paste0(Prevalence, "%")),
            vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = c(
    "Long COVID"                    = "#2196F3",
    "Depression\n(ever diagnosed)"  = "#E53935",
    "Anxiety\n(ever diagnosed)"     = "#FB8C00"
  )) +
  scale_y_continuous(limits = c(0, 30),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Figure 1. Prevalence of Long COVID, depression, and anxiety",
    subtitle = "Among U.S. adults who ever had COVID-19, 2023 NHIS (n = 15,158)",
    x = NULL, y = "Prevalence (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 11, color = "grey40"),
        panel.grid.major.x = element_blank())

print(figure1)
ggsave("Figure1_prevalence.png", figure1, width = 7, height = 5,
       dpi = 300, bg = "white")
cat("Figure 1 saved as Figure1_prevalence.png\n\n")


# ============================================================
# C3: BIVARIABLE ANALYSIS
# ============================================================

cat("============================================================\n")
cat("C3: BIVARIABLE ANALYSIS\n")
cat("============================================================\n\n")

# Helper function: print table to console and save as PNG
print_and_save <- function(df, title, footnote, filename) {
  cat("\n", strrep("=", 90), "\n", sep = "")
  cat(title, "\n")
  cat(strrep("-", 90), "\n")
  print(df, row.names = FALSE)
  cat(strrep("-", 90), "\n")
  cat("Note:", footnote, "\n\n")

  p <- ggplot() +
    theme_void() +
    annotation_custom(
      tableGrob(df, rows = NULL,
                theme = ttheme_minimal(
                  base_size = 11,
                  core    = list(fg_params = list(hjust = 0, x = 0.05),
                                 bg_params = list(fill = c("white", "#F5F5F5"),
                                                  col  = "grey80")),
                  colhead = list(fg_params = list(fontface = "bold",
                                                  hjust = 0, x = 0.05,
                                                  col = "white"),
                                 bg_params = list(fill = "#1565C0",
                                                  col  = "grey80"))
                ))
    ) +
    labs(title = title, caption = paste0("Note: ", footnote)) +
    theme(plot.title   = element_text(face = "bold", size = 12,
                                      margin = margin(b = 10)),
          plot.caption = element_text(size = 9, color = "grey40",
                                      hjust = 0, margin = margin(t = 8)))

  ggsave(filename, p, width = 10, height = nrow(df) * 0.45 + 2,
         dpi = 300, bg = "white")
  cat("PNG saved as:", filename, "\n\n")
}

# --- TABLE 2: Long COVID x Depression ---
dep_no_yes  <- sum(nhis_clean$longcovid == "No Long COVID" & nhis_clean$depression == "Depression")
dep_no_no   <- sum(nhis_clean$longcovid == "No Long COVID" & nhis_clean$depression == "No Depression")
dep_yes_yes <- sum(nhis_clean$longcovid == "Long COVID"    & nhis_clean$depression == "Depression")
dep_yes_no  <- sum(nhis_clean$longcovid == "Long COVID"    & nhis_clean$depression == "No Depression")
dep_all_yes <- dep_no_yes + dep_yes_yes
dep_all_no  <- dep_no_no  + dep_yes_no
chisq_dep   <- chisq.test(table(nhis_clean$longcovid, nhis_clean$depression))

table2_df <- data.frame(
  "Depression Status"   = c("Depression", "No Depression"),
  "Overall n (%)"       = c(paste0(dep_all_yes, " (", round(dep_all_yes/n_all*100,1), "%)"),
                             paste0(dep_all_no,  " (", round(dep_all_no/n_all*100,1),  "%)")),
  "No Long COVID n (%)" = c(paste0(dep_no_yes,  " (", round(dep_no_yes/n_no*100,1),   "%)"),
                             paste0(dep_no_no,   " (", round(dep_no_no/n_no*100,1),    "%)")),
  "Long COVID n (%)"    = c(paste0(dep_yes_yes, " (", round(dep_yes_yes/n_yes*100,1), "%)"),
                             paste0(dep_yes_no,  " (", round(dep_yes_no/n_yes*100,1),  "%)")),
  "p-value"             = c("<0.001", ""),
  check.names = FALSE, stringsAsFactors = FALSE
)
print_and_save(table2_df,
  "Table 2. Association between Long COVID and depression, 2023 NHIS (n = 15,158)",
  paste0("Values are n (%). p-value from Pearson's chi-squared test. X2 = ",
         round(chisq_dep$statistic, 2), ", df = 1."),
  "Table2_depression.png")

# --- TABLE 3: Long COVID x Anxiety ---
anx_no_yes  <- sum(nhis_clean$longcovid == "No Long COVID" & nhis_clean$anxiety == "Anxiety")
anx_no_no   <- sum(nhis_clean$longcovid == "No Long COVID" & nhis_clean$anxiety == "No Anxiety")
anx_yes_yes <- sum(nhis_clean$longcovid == "Long COVID"    & nhis_clean$anxiety == "Anxiety")
anx_yes_no  <- sum(nhis_clean$longcovid == "Long COVID"    & nhis_clean$anxiety == "No Anxiety")
anx_all_yes <- anx_no_yes + anx_yes_yes
anx_all_no  <- anx_no_no  + anx_yes_no
chisq_anx   <- chisq.test(table(nhis_clean$longcovid, nhis_clean$anxiety))

table3_df <- data.frame(
  "Anxiety Status"      = c("Anxiety", "No Anxiety"),
  "Overall n (%)"       = c(paste0(anx_all_yes, " (", round(anx_all_yes/n_all*100,1), "%)"),
                             paste0(anx_all_no,  " (", round(anx_all_no/n_all*100,1),  "%)")),
  "No Long COVID n (%)" = c(paste0(anx_no_yes,  " (", round(anx_no_yes/n_no*100,1),   "%)"),
                             paste0(anx_no_no,   " (", round(anx_no_no/n_no*100,1),    "%)")),
  "Long COVID n (%)"    = c(paste0(anx_yes_yes, " (", round(anx_yes_yes/n_yes*100,1), "%)"),
                             paste0(anx_yes_no,  " (", round(anx_yes_no/n_yes*100,1),  "%)")),
  "p-value"             = c("<0.001", ""),
  check.names = FALSE, stringsAsFactors = FALSE
)
print_and_save(table3_df,
  "Table 3. Association between Long COVID and anxiety disorder, 2023 NHIS (n = 15,158)",
  paste0("Values are n (%). p-value from Pearson's chi-squared test. X2 = ",
         round(chisq_anx$statistic, 2), ", df = 1."),
  "Table3_anxiety.png")

# --- TABLE 1: Participant Characteristics ---
vars <- list(
  list(var = "age_group",  label = "Age group"),
  list(var = "sex",        label = "Sex"),
  list(var = "education",  label = "Education level"),
  list(var = "income_cat", label = "Income-to-poverty ratio"),
  list(var = "insurance",  label = "Health insurance status"),
  list(var = "depression", label = "Depression (ever diagnosed)"),
  list(var = "anxiety",    label = "Anxiety (ever diagnosed)")
)
rows <- list()
for (v in vars) {
  chi  <- chisq.test(table(nhis_clean[[v$var]], nhis_clean$longcovid))
  pval <- ifelse(chi$p.value < 0.001, "<0.001", as.character(round(chi$p.value, 3)))
  rows[[length(rows)+1]] <- data.frame(
    "Variable" = v$label, "Overall n (%)" = "",
    "No Long COVID n (%)" = "", "Long COVID n (%)" = "",
    "p-value" = pval, check.names = FALSE, stringsAsFactors = FALSE)
  for (lev in levels(nhis_clean[[v$var]])) {
    n_a <- sum(nhis_clean[[v$var]] == lev)
    n_n <- sum(nhis_clean[[v$var]] == lev & nhis_clean$longcovid == "No Long COVID")
    n_y <- sum(nhis_clean[[v$var]] == lev & nhis_clean$longcovid == "Long COVID")
    rows[[length(rows)+1]] <- data.frame(
      "Variable" = paste0("  ", lev),
      "Overall n (%)"       = paste0(n_a, " (", round(n_a/n_all*100,1), "%)"),
      "No Long COVID n (%)" = paste0(n_n, " (", round(n_n/n_no*100,1),  "%)"),
      "Long COVID n (%)"    = paste0(n_y, " (", round(n_y/n_yes*100,1), "%)"),
      "p-value" = "", check.names = FALSE, stringsAsFactors = FALSE)
  }
}
table1_df <- bind_rows(rows)
print_and_save(table1_df,
  "Table 1. Participant characteristics by Long COVID status, 2023 NHIS (n = 15,158)",
  "Values are n (%). p-value from Pearson's chi-squared test.",
  "Table1_characteristics.png")

# Figure 2: Grouped bar chart
figure2_data <- tibble(
  outcome = c("Depression", "Depression", "Anxiety disorder", "Anxiety disorder"),
  group   = c("No Long COVID", "Long COVID", "No Long COVID", "Long COVID"),
  percent = c(round(dep_no_yes/n_no*100,1), round(dep_yes_yes/n_yes*100,1),
              round(anx_no_yes/n_no*100,1), round(anx_yes_yes/n_yes*100,1))
)
figure2 <- ggplot(figure2_data, aes(x = outcome, y = percent, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5) +
  geom_text(aes(label = paste0(percent, "%")),
            position = position_dodge(width = 0.6),
            vjust = -0.5, size = 4.2, fontface = "bold") +
  scale_fill_manual(values = c("No Long COVID" = "#90CAF9", "Long COVID" = "#1565C0"),
                    name = "Long COVID status") +
  scale_y_continuous(limits = c(0, 55), labels = function(x) paste0(x, "%")) +
  labs(title    = "Figure 2. Prevalence of depression and anxiety by Long COVID status",
       subtitle = "Among U.S. adults who ever had COVID-19, 2023 NHIS (n = 15,158)",
       x = "Mental health outcome", y = "Prevalence (%)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 11, color = "grey40"),
        legend.position = "top", panel.grid.major.x = element_blank())
print(figure2)
ggsave("Figure2_bivariable.png", figure2, width = 8, height = 5.5,
       dpi = 300, bg = "white")
cat("Figure 2 saved as Figure2_bivariable.png\n\n")


# ============================================================
# C4: MULTIVARIABLE ANALYSIS
# ============================================================

cat("============================================================\n")
cat("C4: MULTIVARIABLE ANALYSIS\n")
cat("============================================================\n\n")

# MODEL 1: Depression
cat("--- Model 1: Long COVID and Depression ---\n")
model1 <- glm(
  depression ~ longcovid + age_group + sex + education + income_cat + insurance,
  data = nhis_clean, family = binomial(link = "logit")
)
summary(model1)

# MODEL 2: Anxiety
cat("\n--- Model 2: Long COVID and Anxiety ---\n")
model2 <- glm(
  anxiety ~ longcovid + age_group + sex + education + income_cat + insurance,
  data = nhis_clean, family = binomial(link = "logit")
)
summary(model2)

# Extract ORs, CIs, p-values
extract_results <- function(model) {
  coefs <- coef(model)
  cis   <- confint.default(model)
  pvals <- summary(model)$coefficients[, 4]
  OR    <- round(exp(coefs), 2)
  CI_lo <- round(exp(cis[, 1]), 2)
  CI_hi <- round(exp(cis[, 2]), 2)
  pval_fmt <- ifelse(pvals < 0.001, "<0.001", round(pvals, 3))
  result <- data.frame(
    Variable = names(coefs), OR = OR,
    CI_95    = paste0("(", CI_lo, ", ", CI_hi, ")"),
    P_value  = pval_fmt,
    stringsAsFactors = FALSE, row.names = NULL
  )
  result <- result[result$Variable != "(Intercept)", ]
  result$Variable <- result$Variable %>%
    str_replace("longcovidLong COVID",             "Long COVID (vs No Long COVID)") %>%
    str_replace("age_group35-49",                  "Age 35-49 (vs 18-34)") %>%
    str_replace("age_group50-64",                  "Age 50-64 (vs 18-34)") %>%
    str_replace("age_group65\\+",                  "Age 65+ (vs 18-34)") %>%
    str_replace("sexFemale",                       "Female (vs Male)") %>%
    str_replace("educationHigh school/GED",        "High school/GED (vs <HS)") %>%
    str_replace("educationSome college/Associate", "Some college/Associate (vs <HS)") %>%
    str_replace("educationBachelor's or higher",   "Bachelor's or higher (vs <HS)") %>%
    str_replace("income_cat100-199% FPL",          "100-199% FPL (vs <100%)") %>%
    str_replace("income_cat200-299% FPL",          "200-299% FPL (vs <100%)") %>%
    str_replace("income_cat300-399% FPL",          "300-399% FPL (vs <100%)") %>%
    str_replace("income_cat400\\+% FPL",           "400%+ FPL (vs <100%)") %>%
    str_replace("income_cat400\\+ FPL",            "400%+ FPL (vs <100%)") %>%
    str_replace("insuranceUninsured",              "Uninsured (vs Insured)")
  return(result)
}

# Helper: print and save regression table as PNG
print_and_save_reg <- function(df, title, footnote, filename) {
  names(df) <- c("Variable", "Odds Ratio", "95% CI", "p-value")
  cat("\n", strrep("=", 80), "\n", sep = "")
  cat(title, "\n")
  cat(strrep("-", 80), "\n")
  print(df, row.names = FALSE)
  cat(strrep("-", 80), "\n")
  cat("Note:", footnote, "\n\n")
  p <- ggplot() +
    theme_void() +
    annotation_custom(
      tableGrob(df, rows = NULL,
                theme = ttheme_minimal(
                  base_size = 11,
                  core    = list(fg_params = list(hjust = 0, x = 0.05),
                                 bg_params = list(fill = c("white", "#F5F5F5"),
                                                  col  = "grey80")),
                  colhead = list(fg_params = list(fontface = "bold",
                                                  hjust = 0, x = 0.05,
                                                  col = "white"),
                                 bg_params = list(fill = "#1565C0",
                                                  col  = "grey80"))
                ))
    ) +
    labs(title = title, caption = paste0("Note: ", footnote)) +
    theme(plot.title   = element_text(face = "bold", size = 12, margin = margin(b = 10)),
          plot.caption = element_text(size = 9, color = "grey40",
                                      hjust = 0, margin = margin(t = 8)))
  ggsave(filename, p, width = 10, height = nrow(df) * 0.45 + 2,
         dpi = 300, bg = "white")
  cat("PNG saved as:", filename, "\n\n")
}

results_dep <- extract_results(model1)
results_anx <- extract_results(model2)

print_and_save_reg(results_dep,
  "Table 4. Multivariable logistic regression: Long COVID and depression, 2023 NHIS (n = 15,158)",
  "OR = odds ratio. 95% CI = 95% confidence interval. Reference categories in parentheses. Adjusted for age, sex, education, income, and insurance status.",
  "Table4_regression_depression.png")

print_and_save_reg(results_anx,
  "Table 5. Multivariable logistic regression: Long COVID and anxiety disorder, 2023 NHIS (n = 15,158)",
  "OR = odds ratio. 95% CI = 95% confidence interval. Reference categories in parentheses. Adjusted for age, sex, education, income, and insurance status.",
  "Table5_regression_anxiety.png")

# Figure 3: Forest plot
or_dep <- exp(coef(model1)["longcovidLong COVID"])
ci_dep <- exp(confint.default(model1)["longcovidLong COVID", ])
or_anx <- exp(coef(model2)["longcovidLong COVID"])
ci_anx <- exp(confint.default(model2)["longcovidLong COVID", ])

forest_data <- tibble(
  outcome = c("Depression", "Anxiety disorder"),
  OR      = c(round(or_dep, 2), round(or_anx, 2)),
  CI_lo   = c(round(ci_dep[1], 2), round(ci_anx[1], 2)),
  CI_hi   = c(round(ci_dep[2], 2), round(ci_anx[2], 2))
)

figure3 <- ggplot(forest_data, aes(x = OR, y = outcome)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.8) +
  geom_errorbar(aes(xmin = CI_lo, xmax = CI_hi),
                width = 0.15, color = "#1565C0", linewidth = 1, orientation = "y") +
  geom_point(size = 4, color = "#1565C0") +
  geom_text(aes(label = paste0("OR = ", OR, " (95% CI: ", CI_lo, "-", CI_hi, ")")),
            vjust = -1, size = 3.8) +
  scale_x_continuous(limits = c(0.5, 4),
                     breaks = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4)) +
  labs(
    title    = "Figure 3. Adjusted odds ratios for the association between Long COVID\nand mental health outcomes",
    subtitle = "Among U.S. adults who ever had COVID-19, 2023 NHIS (n = 15,158)",
    x = "Odds Ratio (95% CI)", y = "Outcome"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 10, color = "grey40"),
        axis.text.y = element_text(size = 12),
        panel.grid.minor = element_blank())

print(figure3)
ggsave("Figure3_forest_plot.png", figure3, width = 9, height = 4.5,
       dpi = 300, bg = "white")
cat("Figure 3 saved as Figure3_forest_plot.png\n")

cat("\n============================================================\n")
cat("ALL ANALYSES COMPLETE\n")
cat("Files saved in your working directory:\n")
cat("  - nhis_clean.csv\n")
cat("  - Table1_characteristics.png\n")
cat("  - Table2_depression.png\n")
cat("  - Table3_anxiety.png\n")
cat("  - Table4_regression_depression.png\n")
cat("  - Table5_regression_anxiety.png\n")
cat("  - Figure1_prevalence.png\n")
cat("  - Figure2_bivariable.png\n")
cat("  - Figure3_forest_plot.png\n")
cat("============================================================\n")
