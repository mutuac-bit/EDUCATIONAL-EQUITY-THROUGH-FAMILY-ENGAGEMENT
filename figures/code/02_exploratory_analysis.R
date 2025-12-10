### EXPLORATORY DATA ANALYSIS
```{r}

library(tidyverse)
library(patchwork)  # For combining plots
library(scales)     # For nice axis labels

# Custom theme for all plots
theme_project <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

project_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442")
```

#### 1. THE ACHIEVEMENT GAP - BY INCOME
```{r viz-income-gap}
#| label: viz-income-gap
#| fig-width: 10
#| fig-height: 7

# This visualization establishes the fundamental problem: income predicts grades

gap_by_income <- pfi_analysis %>%
  count(income_3cat, grades_cat) %>%
  group_by(income_3cat) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

p1 <- ggplot(gap_by_income, aes(x = income_3cat, y = pct, fill = grades_cat)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(
    values = project_colors,
    name = "Academic Performance"
  ) +
  labs(
    title = "The Achievement Gap is Real",
    subtitle = "High-income students dramatically outperform low-income peers",
    x = "Household Income Level",
    y = "Percentage of Students"
  ) +
  theme_project +
  geom_text(
    aes(label = sprintf("%.0f%%", pct)),
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3
  )

print(p1)
```

##### INTERPRETATION:
The 25.8 percentage point gap between Low (36.5%) and High (62.3%) income students achieving "Mostly A's" represents a 1.7x disparity.
This is the gap our engagement interventions aim to reduce.


#### 2. THE ACHIEVEMENT GAP - BY PARENT EDUCATION
```{r viz-education-gap}
#| label: viz-education-gap
#| fig-width: 10
#| fig-height: 7

# Parent education often shows even stronger associations with student outcomes than income because it captures cultural capital and educational expectations.

gap_by_ed <- pfi_analysis %>%
  count(parent_ed_3cat, grades_cat) %>%
  group_by(parent_ed_3cat) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

p2 <- ggplot(gap_by_ed, aes(x = parent_ed_3cat, y = pct, fill = grades_cat)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(
    values = project_colors,
    name = "Academic Performance"
  ) +
  labs(
    title = "Parent Education Strongly Predicts Student Success",
    subtitle = "Children of college graduates are 2x more likely to be high achievers",
    x = "Parent Education Level",
    y = "Percentage of Students"
  ) +
  theme_project +
  scale_x_discrete(labels = c("HS or less", "Some college", "College grad"))

print(p2)
```

##### INTERPRETATION:
The parent education gap is even steeper than the income gap. This suggests educational expectations and academic support behaviors   (which engagement programs can target) play a major role.

#### 3. ENGAGEMENT PATTERNS BY SOCIOECONOMIC STATUS

```{r viz-engagement-by-income}
#| label: viz-engagement-by-income
#| fig-width: 10
#| fig-height: 7

# Do families of different income levels engage differently with schools? If so, this suggests barriers that interventions should address.
engagement_by_income <- pfi_analysis %>%
  group_by(income_3cat) %>%
  summarise(
    mean_engagement = mean(school_engagement, na.rm = TRUE),
    se = sd(school_engagement, na.rm = TRUE) / sqrt(n())
  )

p3 <- ggplot(engagement_by_income, 
             aes(x = income_3cat, y = mean_engagement, fill = income_3cat)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = mean_engagement - se, ymax = mean_engagement + se),
                width = 0.2) +
  scale_fill_manual(values = c("#D55E00", "#E69F00", "#009E73"), guide = "none") +
  labs(
    title = "Higher-Income Families Engage More with Schools",
    subtitle = "Average number of school activities participated in",
    x = "Household Income Level",
    y = "Mean School Engagement Score (0-8)"
  ) +
  theme_project +
  geom_text(aes(label = sprintf("%.2f", mean_engagement)), 
            vjust = -1, fontface = "bold")

print(p3)
```

##### INTERPRETATION:
The 0.88 activity gap (4.54 - 3.66 = 0.88) between high and low-income families has an effect size of d = 0.44 (medium effect).
This gap likely reflects barriers: time constraints from multiple jobs,transportation challenges, or less welcoming school environments.
Key insight: Low-income families CAN engage more if barriers are removed.

#### 4. THE COMPENSATORY EFFECT - PREVIEW

```{r viz-compensatory}
#| label: viz-compensatory
#| fig-width: 12
#| fig-height: 8

# THE COMPENSATORY HYPOTHESIS: DOES ENGAGEMENT HELP THE DISADVANTAGED MORE?
# This is our CORE research question. We're testing whether the "return on investment" from family engagement is higher for low-income students.
# Create engagement tertiles for visualization
pfi_analysis <- pfi_analysis %>%
  mutate(
    engagement_level = case_when(
      school_engagement <= 2 ~ "Low (0-2)",
      school_engagement <= 5 ~ "Medium (3-5)",
      school_engagement >= 6 ~ "High (6-8)"
    ),
    engagement_level = factor(engagement_level, 
                              levels = c("Low (0-2)", "Medium (3-5)", "High (6-8)"))
  )

# Success rate by income and engagement
success_rates <- pfi_analysis %>%
  mutate(high_performer = if_else(grades_cat == "High_Achievers", 1, 0)) %>%
  group_by(income_3cat, engagement_level) %>%
  summarise(
    success_rate = mean(high_performer, na.rm = TRUE) * 100,
    n = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(engagement_level))

p4 <- ggplot(success_rates, 
             aes(x = engagement_level, y = success_rate, 
                 color = income_3cat, group = income_3cat)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("#D55E00", "#E69F00", "#009E73"),
    name = "Income Level"
  ) +
  labs(
    title = "Engagement Helps Everyone, But Matters MORE for Low-Income Students",
    subtitle = "% of students achieving mostly A's by engagement level and income",
    x = "Family Engagement Level",
    y = "% High Achievers (Mostly A's)"
  ) +
  theme_project +
  geom_text(aes(label = sprintf("%.0f%%", success_rate)), 
            vjust = -1, size = 3, show.legend = FALSE)

print(p4)
```
#### 7. KEY STATISTICS FOR REPORT

```{r key-statistics}

cat("\n KEY STATISTICS FOR REPORT \n\n")

# Overall success rates by income
cat("1. HIGH ACHIEVER RATES BY INCOME:\n")
pfi_analysis %>%
  mutate(high_performer = grades_cat == "High_Achievers") %>%
  group_by(income_3cat) %>%
  summarise(pct_high = mean(high_performer) * 100) %>%
  print()

# Engagement differences
cat("\n2. ENGAGEMENT SCORE DIFFERENCES:\n")
pfi_analysis %>%
  group_by(income_3cat) %>%
  summarise(
    mean_engagement = mean(school_engagement),
    sd_engagement = sd(school_engagement)
  ) %>%
  print()

# Save updated data with engagement levels
saveRDS(pfi_analysis, "pfi_analysis_data.rds")
cat("\n Data updated with engagement levels\n")
```
"High-income students are 1.7x more likely to be high achievers (62.3% vs 36.5%), representing a 25.8 percentage point gap."
"High-income families engage in 4.54 school activities on average,compared to 3.66 for low-income families (d = 0.44, p < 0.001)."


```{r prepare_ggpairs}
# Prepare data for ggpairs
pfi_explore <- pfi_analysis %>%
  mutate(
    # Convert to numeric for correlation
    grades_numeric = as.numeric(grades_cat),
    # Make race unknown explicit
    race_ethnicity = fct_explicit_na(race_ethnicity, na_level = "Unknown")
  ) %>%
  # Handle school choice missing
  mutate(
    has_school_choice = replace_na(has_school_choice, 0)
  ) %>%
  filter(!is.na(at_risk))  # Remove NA outcomes

cat("Data prepared for exploratory analysis:", nrow(pfi_explore), "observations\n")
```
####Ggpairs Analysis: Core relationships

##### GGPAIRS PLOT 1: CORE RELATIONSHIPS
This plot examines correlations between outcomes, engagement measures, and SES.
We look for: (1) significant correlations, (2) multicollinearity concerns

```{r ggpairs-core}
#| label: ggpairs-core
#| fig-width: 14
#| fig-height: 12

library(GGally)
library(tidyverse)

# Prepare data for ggpairs
pfi_explore <- pfi_analysis %>%
  mutate(
    # Convert to numeric for correlation
    grades_numeric = as.numeric(grades_cat),
    # Make race unknown explicit
    race_ethnicity = fct_explicit_na(race_ethnicity, na_level = "Unknown")
  ) %>%
  # Handle school choice missing
  mutate(
    has_school_choice = replace_na(has_school_choice, 0)
  ) %>%
  filter(!is.na(at_risk))  # Remove NA outcomes


cat("\n CORE RELATIONSHIPS PLOT \n")
cat("This will take a minute to render...\n")

core_vars <- pfi_explore %>%
  select(
    # Outcome
    grades_numeric,
    
    # Engagement measures
    school_engagement,
    homework_involvement,
    cultural_enrichment,
    
    # SES measures
    income_3cat,
    parent_ed_3cat,
    two_parent
  )

p1 <- ggpairs(
  core_vars,
  title = "Core Relationships: Outcomes, Engagement & SES",
  lower = list(continuous = "smooth", combo = "box"),
  upper = list(continuous = wrap("cor", size = 3)),
  diag = list(continuous = "barDiag"),
  columnLabels = c("Grades", "School\nEngage", "Homework\nInvolve", 
                   "Cultural\nEnrich", "Income", "Parent\nEd", "Two\nParent")
) +
  theme_minimal()

print(p1)
ggsave("ggpairs_core_relationships.png", p1, width = 14, height = 12, dpi = 300)
```
##### INTERPRETATION:
All three engagement measures show NEGATIVE correlations with grades_numeric(r = -0.13 to -0.14, all p < 0.001). NEGATIVE because lower codes = better grades.
This means MORE engagement → BETTER grades (lower code numbers).
MULTICOLLINEARITY CHECK: Engagement measures have low-moderate correlations(r = 0.09 to 0.33), so including all three is justified - they capture DIFFERENT aspects of family involvement.


#### GGPAIRS PLOT 2: INDIVIDUAL ENGAGEMENT ACTIVITIES
Which specific activities matter most for grades? See which specific activities matter most.

```{r ggpairs-components}
#| label: ggpairs-components
#| fig-width: 14
#| fig-height: 12


cat("\n ENGAGEMENT COMPONENTS PLOT \n")

engagement_vars <- pfi_explore %>%
  select(
    grades_numeric,
    attend_event,
    volunteer,
    general_meeting,
    pta_meeting,
    parent_teacher_conf,
    fundraising,
    committee,
    counselor
  )

p2 <- ggpairs(
  engagement_vars,
  title = "Individual Engagement Activities vs Grades",
  lower = list(continuous = "smooth"),
  upper = list(continuous = wrap("cor", size = 3)),
  columnLabels = c("Grades", "Event", "Vol", "Meeting", "PTA", 
                   "P-T Conf", "Fundraise", "Committee", "Counselor")
) +
  theme_minimal()

print(p2)
ggsave("ggpairs_engagement_components.png", p2, width = 14, height = 12, dpi = 300)

cat("\n Engagement components plot saved!\n")
```
##### INTERPRETATION:
All activities show small negative correlations (engagement → better grades).
Parent-teacher conferences show one of the stronger effects.
This justifies using the COMPOSITE score rather than individual activities.


#### GGPAIRS 3: CONTROL VARIABLES
Check relationships between outcomes and potential confounders.

```{r ggpairs-control}
#| label: ggpairs-controls
#| fig-width: 12
#| fig-height: 10

control_vars <- pfi_explore %>%
  select(
    grades_numeric,
    grade_level,
    male,
    has_disability,
    num_siblings,
    public_school,
    year
  )

p3 <- ggpairs(
  control_vars,
  title = "Control Variables vs Grades",
  lower = list(continuous = "smooth", combo = "box"),
  upper = list(continuous = wrap("cor", size = 3)),
  columnLabels = c("Grades", "Grade\nLevel", "Male", "Disability", 
                   "Siblings", "Public\nSchool", "Year")
) +
  theme_minimal()

print(p3)
ggsave("ggpairs_control_variables.png", p3, width = 12, height = 10, dpi = 300)
```

##### INTERPRETATION:
has_disability shows the STRONGEST correlation (r = 0.207) - students with disabilities have lower grades, highlighting need for specialized support.
  male: r = 0.143 - boys have slightly lower grades than girls.
  grade_level: r = 0.095 - grades decline slightly as students get older.
  These MUST be controlled in our models to isolate engagement effects.


#### GGPAIRS 4: POTENTIAL INTERACTIONS
Test if relationships differ by income
Does the relationship between engagement and grades DIFFER by income level?
Different colored lines/slopes would suggest interaction effects.

```{r ggpairs-interactions}
#| label: ggpairs-interactions
#| fig-width: 14
#| fig-height: 12

interaction_data <- pfi_explore %>%
  select(grades_numeric, school_engagement, homework_involvement, 
         cultural_enrichment, income_3cat) %>%
  filter(!is.na(income_3cat))

p4 <- ggpairs(
  interaction_data,
  columns = 1:4,
  mapping = aes(color = income_3cat, alpha = 0.5),
  title = "Engagement Relationships by Income Level (Check for Interactions)",
  lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.5)),
  upper = list(continuous = wrap("cor", size = 2.5))
) +
  theme_minimal() +
  scale_color_manual(values = c("#D55E00", "#E69F00", "#009E73")) +
  scale_fill_manual(values = c("#D55E00", "#E69F00", "#009E73"))

print(p4)
ggsave("ggpairs_income_interactions.png", p4, width = 14, height = 12, dpi = 300)
```
##### INTERPRETATION:
If the regression lines (smoothers) have DIFFERENT SLOPES by income group,this visually suggests an interaction effect that we should test formally.The compensatory hypothesis predicts steeper slopes for low-income students.

#### CORRELATION MATRIX and STATISTICAL TESTS
```{r correlation-tests}
#| label: correlation-tests

cor_data <- pfi_explore %>%
  select(
    grades_numeric, at_risk, days_absent,
    school_engagement, homework_involvement, cultural_enrichment,
    grade_level, male, has_disability, num_siblings, 
    two_parent, public_school
  ) %>%
  # Convert factors to numeric for correlation
  mutate(across(where(is.factor), as.numeric))

cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")

cat("\nCorrelations with GRADES (grades_numeric):\n")
cor_with_grades <- sort(cor_matrix[,"grades_numeric"], decreasing = TRUE)
print(round(cor_with_grades, 3))

cat("\n\nCorrelations with AT-RISK:\n")
cor_with_risk <- sort(cor_matrix[,"at_risk"], decreasing = TRUE)
print(round(cor_with_risk, 3))

cat("\n\nCorrelations with DAYS ABSENT:\n")
cor_with_absent <- sort(cor_matrix[,"days_absent"], decreasing = TRUE)
print(round(cor_with_absent, 3))
```
##### INTERPRETATION:
Positive correlations mean higher grades_numeric (worse grades)
Negative correlations mean lower grades_numeric (better grades)
STRONGEST PREDICTORS:
 1. has_disability: r = 0.207 (disability → worse grades)
 2. homework_involvement: r = -0.139 (more involvement → better grades)
 3. school_engagement: r = -0.136 (more engagement → better grades)
 4. two_parent: r = -0.136 (two-parent household → better grades)
 
 
```{r}
# Check multicollinearity among predictors
cat("\n\n MULTICOLLINEARITY CHECK \n")
cat("High correlations (>0.7) between predictors:\n")

predictor_cors <- cor_matrix[
  c("school_engagement", "homework_involvement", "cultural_enrichment"),
  c("school_engagement", "homework_involvement", "cultural_enrichment")
]

print(round(predictor_cors, 3))
```
##### INTERPRETATION:
All correlations between engagement measures are < 0.35. NO multicollinearity concerns - safe to include all three in models.


#### STATISTICAL TESTS FOR KEY RELATIONSHIPS
##### STATISTICAL SIGNIFICANCE TESTS
```{r}
# 1. School engagement vs grades
cat("\n1. School Engagement vs Grades:\n")
engagement_test <- cor.test(pfi_explore$school_engagement, 
                            pfi_explore$grades_numeric)
cat(sprintf("   Correlation: %.3f, p-value: %.2e\n", 
           engagement_test$estimate, engagement_test$p.value))

# 2. Homework involvement vs grades
cat("\n2. Homework Involvement vs Grades:\n")
homework_test <- cor.test(pfi_explore$homework_involvement, 
                         pfi_explore$grades_numeric)
cat(sprintf("   Correlation: %.3f, p-value: %.2e\n", 
           homework_test$estimate, homework_test$p.value))

# 3. Cultural enrichment vs grades
cat("\n3. Cultural Enrichment vs Grades:\n")
cultural_test <- cor.test(pfi_explore$cultural_enrichment, 
                         pfi_explore$grades_numeric)
cat(sprintf("   Correlation: %.3f, p-value: %.2e\n", 
           cultural_test$estimate, cultural_test$p.value))

# 4. Test if engagement matters MORE for low-income
cat("\n4. Interaction Test: Does engagement matter more for low-income?\n")
cat("   (Comparing correlations across income groups)\n")

by_income <- pfi_explore %>%
  group_by(income_3cat) %>%
  summarise(
    cor_engagement_grades = cor(school_engagement, grades_numeric, 
                               use = "complete.obs"),
    n = n()
  )
print(by_income)
```
##### INTERPRETATION:
The correlations are similar across groups in this simple analysis.We need formal interaction testing in regression to properly assess
the compensatory hypothesis (which controls for other variables)
