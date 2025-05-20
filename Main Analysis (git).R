## Data Clean 

rm(list = ls())

library(tidyverse)
library(lme4)
library(lmerTest)
library(nationalparkcolors)
library(MuMIn)
library(gt)

dat <- ... %>% 
  janitor::clean_names() %>% 
  select(!contains("date")) %>% 
  mutate(
    sex = str_to_title(sex),
    oa_grade = case_when(
      most_severe_grade == "2" ~ "Grade 2",
      most_severe_grade == "3" ~ "Grade 3",
      most_severe_grade == "4" ~ "Grade 4"),
    prior_injection = case_when(
      !is.na(prior_injection) ~ "Prior Injection",
      is.na(prior_injection) ~ "No Prior Injection"
    ),
    prior_surgery = case_when(
      !is.na(prior_surgery) ~ "Prior Surgery",
      is.na(prior_surgery) ~ "No Prior Surgery"
    ),
    additional_injection = case_when(
      !is.na(additional_injection) ~ "Needed Additional Product",
      is.na(additional_injection) ~ "No Additional Product Needed"
    )
    )


## Summary Table Functions 
{
  
  library(gtsummary)
  library(nlme)
  
  reset_gtsummary_theme() 
  
  summary_render <- list(
    all_continuous() ~ c(
      "{mean} ({sd})", 
      "{median} ({p25}, {p75})",
      "{min}, {max}"),      
    all_categorical() ~ "{n} ({p}%)"
  )
  
  lme_stat_function <- function(data, by, variable, group, ...) {
    nlme::lme(
      data = data, 
      fixed = reformulate(response = variable, termlabels = by),
      random = reformulate(glue::glue("1 | {group}")),
      na.action = na.omit
    ) %>% 
      broom.mixed::tidy() %>%
      dplyr::filter(startsWith(term, by)) 
  }
  
  chisq.lmm <- function(data, variable, by, group, ...) {   
    group <- group 
    chisq.test(x = data[[variable]], y = as.factor(data[[by]]), correct = FALSE, simulate.p.value = TRUE) %>%    
      broom::tidy() 
  }
  
  anova_stat_render <- list(
    all_continuous() ~ "aov", 
    all_categorical() ~ "chisq.test")
  
  
  ttest_stat_render <- list(
    all_continuous() ~ "t.test", 
    all_categorical() ~ "chisq.test")
  
  lme_stat_render <- list(
    all_continuous() ~ lme_stat_function,
    all_categorical() ~ chisq.lmm
  )
  
  }

## Demographic Comparisons 
{
dem_tab <- dat %>% 
  select(
    patient_id, sex, age, height, weight, bmi,
    oa_grade
  ) %>% 
  distinct(patient_id, .keep_all = TRUE) %>% 
  tbl_summary(
    by = sex,
    include = -patient_id,
    type = all_continuous() ~ "continuous2",
    statistic = summary_render,
    digits = list(
      all_continuous() ~ 2
    ),
    missing_text = "(Missing)",
    label = list(

    )
  ) %>%
  bold_labels() %>%
  italicize_levels() %>%
  add_p(
    test = ttest_stat_render
  )

dem_tab

}

## Knee Characteristics
{
knee_char_tab <- dat %>% 
  select(
    patient_id, sex, 
    oa_grade, 
    laterality, prior_injection, prior_surgery, most_severe_grade, 
  ) %>% 
  tbl_strata(
    strata = oa_grade, 
    ~ .x %>% 
      tbl_summary(
        by = sex,
        include = -patient_id, 
        type = all_continuous() ~ "continuous2",
        statistic = summary_render,
        digits = list(
          all_continuous() ~ 2
        ),
        missing_text = "(Missing)",
        label = list(
          
        )
      ) %>%
      bold_labels() %>%
      italicize_levels() %>%
      add_p(
        test = lme_stat_render,
        group = patient_id
      )
  )

  knee_char_tab
}

## Injection Characteristics (using PATIENT as random effect) 
{
inj_char_tab <- dat %>% 
  select(
    patient_id, sex, oa_grade,
    ultrasound_guidance, time_blood_draw_to_delivery_min,  
    injection_volume_cc, additional_injection
  ) %>% 
  tbl_strata(
    strata = oa_grade, 
    ~ .x %>% 
    tbl_summary(
      by = sex,
      include = -patient_id, 
      type = list(
        where(is.numeric) ~ "continuous2",
        ultrasound_guidance ~ "categorical"),
      statistic = summary_render,
      digits = list(
        all_continuous() ~ 2
      ),
      missing_text = "(Missing)",
      label = list()
    ) %>%
    bold_labels() %>%
    italicize_levels() %>% 
    add_p(
      test = lme_stat_render,
      group = patient_id
      )
  )

}

## PRO Analysis; sex, time, OA Grade (using PATIENT as random effect)
{
### Pain 
dat_pain <- dat %>% 
  select(patient_id, injection_id, age, sex, oa_grade, contains("pain")) %>% 
  pivot_longer(
    baseline_pain:x1yr_pain,
    names_to = "time_c",
    values_to = "score"
  ) %>% 
  mutate(
    time_m = case_when(
      time_c == "baseline_pain" ~ 0,
      time_c == "x1yr_pain" ~ 12, 
      time_c == "x2wk_pain" ~ 0.5, 
      time_c == "x3m_pain" ~ 3, 
      time_c == "x6m_pain" ~ 6
    )
  )

dat_pain_bl <- dat %>% 
  select(patient_id, injection_id, age, sex, oa_grade, contains("pain")) %>% 
  pivot_longer(
    x2wk_pain:x1yr_pain,
    names_to = "time_c",
    values_to = "score"
  ) %>% 
  mutate(
    time_m = case_when(
      time_c == "x1yr_pain" ~ 12, 
      time_c == "x2wk_pain" ~ 0.5, 
      time_c == "x3m_pain" ~ 3, 
      time_c == "x6m_pain" ~ 6
    )
  )

pain_p1 <- ggplot(
  data = dat_pain,
  aes(
    x = as.factor(time_m),
    y = score,
    fill = sex
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  geom_vline(
    xintercept = 1.5,
    linetype = "dashed"
  ) + 
  scale_fill_manual(
    values = park_palette("Hawaii")
    ) + 
  theme_bw(
    
  ) + 
  labs(
    x = "Months Post-Op",
    y = "PROMIS Pain Score",
    fill = NULL
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  facet_wrap(
    ~ oa_grade
  )

pain_p1

pain_p1.1 <- ggplot(
  data = dat_pain,
  aes(
    x = time_m,
    y = score,
    fill = sex
  )
) + 
  geom_point(
    aes(
      x = time_m,
      y = score,
      group = as.factor(patient_id),
      color = sex
    ),
    alpha = 0.5, 
    position = position_jitter(width = 0.1, height = 0.1)
  ) +
  stat_smooth(
    aes(
      x = time_m,
      y = score,
      color = sex,
      group = as.factor(patient_id)
    ),
    geom = "line", 
    method = "lm",
    se = FALSE, 
    alpha = 0.5, 
    linetype = "dashed"
  ) +
  stat_summary(
    aes(
      x = time_m,
      y = score,
      group = as.factor(sex),
      color = as.factor(sex)
    ),
    geom = "line",
    fun = "mean",
    size = 1.5,
    position = position_jitter(width = 0.1, seed = 12)
  ) +
  stat_summary(
    aes(
      x = time_m,
      y = score,
      fill = as.factor(sex),
      group = as.factor(sex)
    ),
    geom = "point",
    fun = "mean",
    shape = 21,
    size = 4,
    color = "black",
    position = position_jitter(width = 0.1, seed = 12)
  ) +
  theme_bw() + 
  labs(
    x = "Months Post-Op",
    y = "PROMIS Pain Score",
    fill = NULL,
    color = NULL
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  scale_fill_manual(
    values = park_palette("Hawaii")
  )  + 
  scale_color_manual(
    values = park_palette("Hawaii")
  )  + 
  facet_wrap(
    ~ oa_grade
  )

pain_p1.1


pain_p2 <- ggplot(
  data = dat_pain %>% 
    filter(time_m %in% c(0, 12)),
  aes(
    x = as.factor(time_m),
    y = score,
    fill = sex
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  geom_vline(
    xintercept = 1.5,
    linetype = "dashed"
  ) + 
  scale_fill_manual(
    values = park_palette("Hawaii")
  ) + 
  theme_bw(
    
  ) + 
  labs(
    x = "Months Post-Op",
    y = "PROMIS Pain Score",
    fill = NULL
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  facet_wrap(
    ~ oa_grade
  )

pain_p2

pain_p2.1 <- ggplot(
  data = dat_pain %>% 
    filter(time_m %in% c(0, 12)),
  aes(
    x = time_m,
    y = score,
    fill = sex
  )
) + 
  geom_point(
    aes(
      x = time_m,
      y = score,
      group = as.factor(patient_id),
      color = sex
    ),
    alpha = 0.5, 
    position = position_jitter(width = 0.1, height = 0.1)
  ) +
  stat_smooth(
    aes(
      x = time_m,
      y = score,
      color = sex,
      group = as.factor(patient_id)
    ),
    geom = "line", 
    method = "lm",
    se = FALSE, 
    alpha = 0.5, 
    linetype = "dashed"
  ) +
  stat_summary(
    aes(
      x = time_m,
      y = score,
      group = as.factor(sex),
      color = as.factor(sex)
    ),
    geom = "line",
    fun = "mean",
    size = 1.5,
    position = position_jitter(width = 0.1, seed = 12)
  ) +
  stat_summary(
    aes(
      x = time_m,
      y = score,
      fill = as.factor(sex),
      group = as.factor(sex)
    ),
    geom = "point",
    fun = "mean",
    shape = 21,
    size = 4,
    color = "black",
    position = position_jitter(width = 0.1, seed = 12)
  ) +
  theme_bw() + 
  labs(
    x = "Months Post-Op",
    y = "PROMIS Pain Score",
    fill = NULL,
    color = NULL
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  scale_fill_manual(
    values = park_palette("Hawaii")
  )  + 
  scale_color_manual(
    values = park_palette("Hawaii")
  ) + 
  facet_wrap(
    ~ oa_grade
  )

pain_p2.1

pain_p3 <- ggplot(
  data = dat_pain_bl,
  aes(
    x = baseline_pain, 
    y = score
  )
) + 
  geom_point(
    aes(
      color = as.factor(time_m)
    )
  ) + 
  geom_smooth(
    method = "lm",
    se = F,
    linetype = "dashed",
    color = "black"
  ) + 
  theme_bw(
    
  ) + 
  labs(
    x = "Baseline Pain",
    y = "Post-Op Pain Score",
    color = "Months Post-Op"
  ) + 
  theme(
    legend.position = "bottom"
  )

pain_p3

### Model 

pain_mod <- lmer(
  score ~ time_m*sex + age + oa_grade + baseline_pain + (1|patient_id),
  data = dat_pain_bl
) 

pain_mod_anova <- as.data.frame(car::Anova(pain_mod, type = "II")) %>% 
  rownames_to_column() %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  rename_with(~c("Variable", "Chi-Squared", "df", "p-value")) %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "< 0.001",
      TRUE ~ as.character(`p-value`)
    ),
    Variable = case_when(
      Variable == "time_m" ~ "Months Post-Op",
      Variable == "sex" ~ "Sex",
      Variable == "age" ~ "Age",
      Variable == "oa_grade" ~ "OA Grade",
      Variable == "baseline_pain" ~ "Baseline Pain",
      Variable == "time_m:sex" ~ "Months Post-Op x Sex (Interaction)",
      
    )
  ) %>% 
  gt()
  
pain_mod_anova

pain_mod_sum <- pain_mod %>% 
  tbl_regression(
    label = list(
      time_m ~ "Measurement Time",
      sex ~ "Sex",
      oa_grade ~ "OA Grade",
      age ~ "Age"
    )
  )

pain_mod_sum

pain_mod2 <- lmer(
  baseline_pain ~ oa_grade + (1|patient_id), data = dat
)

pain_mod2_anova <- as.data.frame(car::Anova(pain_mod2, type = "II")) %>% 
  rownames_to_column() %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  rename_with(~c("Variable", "Chi-Squared", "df", "p-value")) %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "< 0.001",
      TRUE ~ as.character(`p-value`)
    ),
    Variable = case_when(
      Variable == "oa_grade" ~ "OA Grade"
    )
  ) %>% 
  gt()

pain_mod2_anova

pain_mod2_sum <- pain_mod2 %>% 
  tbl_regression(
    label = list(
      oa_grade ~ "OA Grade"
    )
  )

pain_mod2_sum

### Physical Function 
dat_function <- dat %>% 
  select(patient_id, injection_id, age, sex, oa_grade, contains("function")) %>% 
  pivot_longer(
    baseline_function:x1yr_function,
    names_to = "time_c",
    values_to = "score"
  ) %>% 
  mutate(
    time_m = case_when(
      time_c == "baseline_function" ~ 0,
      time_c == "x1yr_function" ~ 12, 
      time_c == "x2wk_function" ~ 0.5, 
      time_c == "x3m_function" ~ 3, 
      time_c == "x6m_function" ~ 6
    )
  )

dat_function_bl <- dat %>% 
  select(patient_id, injection_id, age, sex, oa_grade, contains("function")) %>% 
  pivot_longer(
    x3m_function:x1yr_function,
    names_to = "time_c",
    values_to = "score"
  ) %>% 
  mutate(
    time_m = case_when(
      time_c == "x1yr_function" ~ 12, 
      time_c == "x3m_function" ~ 3, 
      time_c == "x6m_function" ~ 6
    )
  )

function_p1 <- ggplot(
  data = dat_function,
  aes(
    x = as.factor(time_m),
    y = score,
    fill = sex
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  geom_vline(
    xintercept = 1.5,
    linetype = "dashed"
  ) + 
  scale_fill_manual(
    values = park_palette("Hawaii")
  ) + 
  theme_bw(
    
  ) + 
  labs(
    x = "Months Post-Op",
    y = "PROMIS Physical Function Score",
    fill = NULL
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  facet_wrap(
    ~ oa_grade
  )

function_p1

function_p1.1 <- ggplot(
  data = dat_function,
  aes(
    x = time_m,
    y = score,
    fill = sex
  )
) + 
  geom_point(
    aes(
      x = time_m,
      y = score,
      group = as.factor(patient_id),
      color = sex
    ),
    alpha = 0.5, 
    position = position_jitter(width = 0.1, height = 0.1)
  ) +
  stat_smooth(
    aes(
      x = time_m,
      y = score,
      color = sex,
      group = as.factor(patient_id)
    ),
    geom = "line", 
    method = "lm",
    se = FALSE, 
    alpha = 0.5, 
    linetype = "dashed"
  ) +
  stat_summary(
    aes(
      x = time_m,
      y = score,
      group = as.factor(sex),
      color = as.factor(sex)
    ),
    geom = "line",
    fun = "mean",
    size = 1.5,
    position = position_jitter(width = 0.1, seed = 12)
  ) +
  stat_summary(
    aes(
      x = time_m,
      y = score,
      fill = as.factor(sex),
      group = as.factor(sex)
    ),
    geom = "point",
    fun = "mean",
    shape = 21,
    size = 4,
    color = "black",
    position = position_jitter(width = 0.1, seed = 12)
  ) +
  theme_bw() + 
  labs(
    x = "Months Post-Op",
    y = "PROMIS Physical Function Score",
    fill = NULL,
    color = NULL
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  scale_fill_manual(
    values = park_palette("Hawaii")
  )  + 
  scale_color_manual(
    values = park_palette("Hawaii")
  )  + 
  facet_wrap(
    ~ oa_grade
  )

function_p1.1

function_p2 <- ggplot(
  data = dat_function %>% 
    filter(time_m %in% c(0, 12)),
  aes(
    x = as.factor(time_m),
    y = score,
    fill = sex
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  geom_vline(
    xintercept = 1.5,
    linetype = "dashed"
  ) + 
  scale_fill_manual(
    values = park_palette("Hawaii")
  ) + 
  theme_bw(
    
  ) + 
  labs(
    x = "Months Post-Op",
    y = "PROMIS Physical Function Score",
    fill = NULL
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  facet_wrap(
    ~ oa_grade
  )

function_p2

function_p2.1 <- ggplot(
  data = dat_function %>% 
    filter(time_m %in% c(0, 12)),
  aes(
    x = time_m,
    y = score,
    fill = sex
  )
) + 
  geom_point(
    aes(
      x = time_m,
      y = score,
      group = as.factor(patient_id),
      color = sex
    ),
    alpha = 0.5, 
    position = position_jitter(width = 0.1, height = 0.1)
  ) +
  stat_smooth(
    aes(
      x = time_m,
      y = score,
      color = sex,
      group = as.factor(patient_id)
    ),
    geom = "line", 
    method = "lm",
    se = FALSE, 
    alpha = 0.5, 
    linetype = "dashed"
  ) +
  stat_summary(
    aes(
      x = time_m,
      y = score,
      group = as.factor(sex),
      color = as.factor(sex)
    ),
    geom = "line",
    fun = "mean",
    size = 1.5,
    position = position_jitter(width = 0.1, seed = 12)
  ) +
  stat_summary(
    aes(
      x = time_m,
      y = score,
      fill = as.factor(sex),
      group = as.factor(sex)
    ),
    geom = "point",
    fun = "mean",
    shape = 21,
    size = 4,
    color = "black",
    position = position_jitter(width = 0.1, seed = 12)
  ) +
  theme_bw() + 
  labs(
    x = "Months Post-Op",
    y = "PROMIS Physical Function Score",
    fill = NULL,
    color = NULL
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  scale_fill_manual(
    values = park_palette("Hawaii")
  )  + 
  scale_color_manual(
    values = park_palette("Hawaii")
  ) + 
  facet_wrap(
    ~ oa_grade
  )

function_p2.1

function_p3 <- ggplot(
  data = dat_function_bl %>% drop_na(score),
  aes(
    x = baseline_function, 
    y = score
  )
) + 
  geom_point(
    aes(
      color = as.factor(time_m)
    )
  ) + 
  geom_smooth(
    method = "lm",
    se = F,
    linetype = "dashed",
    color = "black"
  ) + 
  theme_bw(
    
  ) + 
  labs(
    x = "Baseline Physical Function",
    y = "Post-Op Physical Function Score",
    color = "Months Post-Op"
  ) + 
  theme(
    legend.position = "bottom"
  )

function_p3

function_p4 <- ggplot(
  data = dat_function_bl %>% drop_na(score),
  aes(
    x = age, 
    y = score
  )
) + 
  geom_point(
    aes(
      color = as.factor(time_m)
    )
  ) + 
  geom_smooth(
    method = "lm",
    se = F,
    linetype = "dashed",
    color = "black"
  ) + 
  theme_bw(
    
  ) + 
  labs(
    x = "Age (yrs)",
    y = "Post-Op Physical Function Score",
    color = "Months Post-Op"
  ) + 
  theme(
    legend.position = "bottom"
  )

function_p4

### Model 

function_mod <- lmer(
  score ~ time_m*sex + age + oa_grade + baseline_function + (1|patient_id),
  data = dat_function_bl
) 

function_mod_anova <- as.data.frame(car::Anova(function_mod, type = "II")) %>% 
  rownames_to_column() %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  rename_with(~c("Variable", "Chi-Squared", "df", "p-value")) %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "< 0.001",
      TRUE ~ as.character(`p-value`)
    ),
    Variable = case_when(
      Variable == "time_m" ~ "Months Post-Op",
      Variable == "sex" ~ "Sex",
      Variable == "age" ~ "Age",
      Variable == "oa_grade" ~ "OA Grade",
      Variable == "baseline_function" ~ "Baseline Pain",
      Variable == "time_m:sex" ~ "Months Post-Op x Sex (Interaction)",
      
    )
  ) %>% 
  gt()

function_mod_anova

function_mod_sum <- function_mod %>% 
  tbl_regression(
    label = list(
      time_m ~ "Measurement Time",
      sex ~ "Sex",
      oa_grade ~ "OA Grade",
      age ~ "Age",
      baseline_function ~ "Baseline Function"
    )
  )

function_mod_sum

function_mod2 <- lmer(
  baseline_function ~ oa_grade + (1|patient_id), data = dat
)

function_mod2_anova <- as.data.frame(car::Anova(function_mod2, type = "II")) %>% 
  rownames_to_column() %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  rename_with(~c("Variable", "Chi-Squared", "df", "p-value")) %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "< 0.001",
      TRUE ~ as.character(`p-value`)
    ),
    Variable = case_when(
      Variable == "oa_grade" ~ "OA Grade"
    )
  ) %>% 
  gt()

function_mod2_anova

function_mod2_sum <- function_mod2 %>% 
  tbl_regression(
    label = list(
      oa_grade ~ "OA Grade"
    )
  )

function_mod2_sum


### KOOS 
dat_koos <- dat %>% 
  select(patient_id, injection_id, age, sex, oa_grade, contains("koos")) %>% 
  pivot_longer(
    baseline_koos:x1yr_koos,
    names_to = "time_c",
    values_to = "score"
  ) %>% 
  mutate(
    time_m = case_when(
      time_c == "baseline_koos" ~ 0,
      time_c == "x1yr_koos" ~ 12, 
      time_c == "x2wk_koos" ~ 0.5, 
      time_c == "x3m_koos" ~ 3, 
      time_c == "x6m_koos" ~ 6
    )
  )

dat_koos_bl <- dat %>% 
  select(patient_id, injection_id, age, sex, oa_grade, contains("koos")) %>% 
  pivot_longer(
    x3m_koos:x1yr_koos,
    names_to = "time_c",
    values_to = "score"
  ) %>% 
  mutate(
    time_m = case_when(
      time_c == "x1yr_koos" ~ 12, 
      time_c == "x3m_koos" ~ 3, 
      time_c == "x6m_koos" ~ 6
    )
  )

function_p1 <- ggplot(
  data = dat_koos,
  aes(
    x = as.factor(time_m),
    y = score,
    fill = sex
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  geom_vline(
    xintercept = 1.5,
    linetype = "dashed"
  ) + 
  scale_fill_manual(
    values = park_palette("Hawaii")
  ) + 
  theme_bw(
    
  ) + 
  labs(
    x = "Months Post-Op",
    y = "KOOS Score",
    fill = NULL
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  facet_wrap(
    ~ oa_grade
  )

function_p1

function_p1.1 <- ggplot(
  data = dat_koos,
  aes(
    x = time_m,
    y = score,
    fill = sex
  )
) + 
  geom_point(
    aes(
      x = time_m,
      y = score,
      group = as.factor(patient_id),
      color = sex
    ),
    alpha = 0.5, 
    position = position_jitter(width = 0.1, height = 0.1)
  ) +
  stat_smooth(
    aes(
      x = time_m,
      y = score,
      color = sex,
      group = as.factor(patient_id)
    ),
    geom = "line", 
    method = "lm",
    se = FALSE, 
    alpha = 0.5, 
    linetype = "dashed"
  ) +
  stat_summary(
    aes(
      x = time_m,
      y = score,
      group = as.factor(sex),
      color = as.factor(sex)
    ),
    geom = "line",
    fun = "mean",
    size = 1.5,
    position = position_jitter(width = 0.1, seed = 12)
  ) +
  stat_summary(
    aes(
      x = time_m,
      y = score,
      fill = as.factor(sex),
      group = as.factor(sex)
    ),
    geom = "point",
    fun = "mean",
    shape = 21,
    size = 4,
    color = "black",
    position = position_jitter(width = 0.1, seed = 12)
  ) +
  theme_bw() + 
  labs(
    x = "Months Post-Op",
    y = "KOOS Score",
    fill = NULL,
    color = NULL
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  scale_fill_manual(
    values = park_palette("Hawaii")
  )  + 
  scale_color_manual(
    values = park_palette("Hawaii")
  )  + 
  facet_wrap(
    ~ oa_grade
  )

function_p1.1

function_p2 <- ggplot(
  data = dat_koos %>% 
    filter(time_m %in% c(0, 12)),
  aes(
    x = as.factor(time_m),
    y = score,
    fill = sex
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  geom_vline(
    xintercept = 1.5,
    linetype = "dashed"
  ) + 
  scale_fill_manual(
    values = park_palette("Hawaii")
  ) + 
  theme_bw(
    
  ) + 
  labs(
    x = "Months Post-Op",
    y = "KOOS Score",
    fill = NULL
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  facet_wrap(
    ~ oa_grade
  )

function_p2

function_p2.1 <- ggplot(
  data = dat_koos %>% 
    filter(time_m %in% c(0, 12)),
  aes(
    x = time_m,
    y = score,
    fill = sex
  )
) + 
  geom_point(
    aes(
      x = time_m,
      y = score,
      group = as.factor(patient_id),
      color = sex
    ),
    alpha = 0.5, 
    position = position_jitter(width = 0.1, height = 0.1)
  ) +
  stat_smooth(
    aes(
      x = time_m,
      y = score,
      color = sex,
      group = as.factor(patient_id)
    ),
    geom = "line", 
    method = "lm",
    se = FALSE, 
    alpha = 0.5, 
    linetype = "dashed"
  ) +
  stat_summary(
    aes(
      x = time_m,
      y = score,
      group = as.factor(sex),
      color = as.factor(sex)
    ),
    geom = "line",
    fun = "mean",
    size = 1.5,
    position = position_jitter(width = 0.1, seed = 12)
  ) +
  stat_summary(
    aes(
      x = time_m,
      y = score,
      fill = as.factor(sex),
      group = as.factor(sex)
    ),
    geom = "point",
    fun = "mean",
    shape = 21,
    size = 4,
    color = "black",
    position = position_jitter(width = 0.1, seed = 12)
  ) +
  theme_bw() + 
  labs(
    x = "Months Post-Op",
    y = "KOOS Score",
    fill = NULL,
    color = NULL
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  scale_fill_manual(
    values = park_palette("Hawaii")
  )  + 
  scale_color_manual(
    values = park_palette("Hawaii")
  ) + 
  facet_wrap(
    ~ oa_grade
  )

function_p2.1

function_p3 <- ggplot(
  data = dat_koos_bl %>% drop_na(score),
  aes(
    x = baseline_koos, 
    y = score
  )
) + 
  geom_point(
    aes(
      color = as.factor(time_m)
    )
  ) + 
  geom_smooth(
    method = "lm",
    se = F,
    linetype = "dashed",
    color = "black"
  ) + 
  theme_bw(
    
  ) + 
  labs(
    x = "Baseline Physical Function",
    y = "Post-Op Physical Function Score",
    color = "Months Post-Op"
  ) + 
  theme(
    legend.position = "bottom"
  )

function_p3

function_p4 <- ggplot(
  data = dat_koos_bl %>% drop_na(score),
  aes(
    x = age, 
    y = score
  )
) + 
  geom_point(
    aes(
      color = as.factor(time_m)
    )
  ) + 
  geom_smooth(
    method = "lm",
    se = F,
    linetype = "dashed",
    color = "black"
  ) + 
  theme_bw(
    
  ) + 
  labs(
    x = "Age (yrs)",
    y = "Post-Op Physical Function Score",
    color = "Months Post-Op"
  ) + 
  theme(
    legend.position = "bottom"
  )

function_p4

### Model 

function_mod <- lmer(
  score ~ time_m*sex + age + oa_grade + baseline_koos + (1|patient_id),
  data = dat_koos_bl
) 

function_mod_anova <- as.data.frame(car::Anova(function_mod, type = "II")) %>% 
  rownames_to_column() %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  rename_with(~c("Variable", "Chi-Squared", "df", "p-value")) %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "< 0.001",
      TRUE ~ as.character(`p-value`)
    ),
    Variable = case_when(
      Variable == "time_m" ~ "Months Post-Op",
      Variable == "sex" ~ "Sex",
      Variable == "age" ~ "Age",
      Variable == "oa_grade" ~ "OA Grade",
      Variable == "baseline_koos" ~ "Baseline Pain",
      Variable == "time_m:sex" ~ "Months Post-Op x Sex (Interaction)",
      
    )
  ) %>% 
  gt()

function_mod_anova

function_mod_sum <- function_mod %>% 
  tbl_regression(
    label = list(
      time_m ~ "Measurement Time",
      sex ~ "Sex",
      oa_grade ~ "OA Grade",
      age ~ "Age",
      baseline_koos ~ "Baseline Function"
    )
  )

function_mod_sum

function_mod2 <- lmer(
  baseline_koos ~ oa_grade + (1|patient_id), data = dat
)

function_mod2_anova <- as.data.frame(car::Anova(function_mod2, type = "II")) %>% 
  rownames_to_column() %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  rename_with(~c("Variable", "Chi-Squared", "df", "p-value")) %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "< 0.001",
      TRUE ~ as.character(`p-value`)
    ),
    Variable = case_when(
      Variable == "oa_grade" ~ "OA Grade"
    )
  ) %>% 
  gt()

function_mod2_anova

function_mod2_sum <- function_mod2 %>% 
  tbl_regression(
    label = list(
      oa_grade ~ "OA Grade"
    )
  )

function_mod2_sum

}


## Binary PRO Analysis
{
### Meds 
dat_meds <- dat %>% 
  select(
    patient_id, injection_id, sex, age, oa_grade, contains("meds")
  ) %>% 
  pivot_longer(
    x3mo_meds:x1year_meds,
    names_to = "time_c", 
    values_to = "score"
  ) %>% 
  mutate(
    time_m = case_when(
      time_c == "x3mo_meds" ~ 3, 
      time_c == "x6mo_meds" ~ 6, 
      time_c == "x1year_meds" ~ 12
    ),
    score = ifelse(score == "Yes", 1, 0)
  )

meds_mod <- glmer(score ~ time_m*sex + age + oa_grade + (1|patient_id), family = binomial, data = dat_meds) %>% 
  tbl_regression(
    label = list(
      time_m ~ "Months Post-Op",
      oa_grade ~ "OA Grade",
      sex ~ "Sex",
      age ~ "Age"
    )
  )

meds_mod

### Return to Activity
dat_activity <- dat %>% 
  select(
    patient_id, injection_id, sex, age, oa_grade, contains("activity")
  ) %>% 
  pivot_longer(
    x3mo_activity:x6mo_activity,
    names_to = "time_c", 
    values_to = "score"
  ) %>% 
  mutate(
    time_m = case_when(
      time_c == "x3mo_activity" ~ 3, 
      time_c == "x6mo_activity" ~ 6
    ),
    score = ifelse(score == "Yes", 1, 0)
  )

activity_mod <- glmer(score ~ time_m*sex + age + oa_grade + (1|patient_id), family = binomial, data = dat_activity) %>% 
  tbl_regression(
    label = list(
      time_m ~ "Months Post-Op",
      oa_grade ~ "OA Grade",
      sex ~ "Sex",
      age ~ "Age"
    )
  )

activity_mod
}


## Conversion to Injection 
dat_inj <- dat %>% 
  select(patient_id, injection_id, sex, oa_grade, conversion_to_injection_1yr) %>% 
  mutate(
    conv_to_inj = ifelse(conversion_to_injection_1yr == "Yes", 1, 0)
  )

dat_post_athro <- dat %>% 
  select(patient_id, injection_id, sex, oa_grade, post_arthroscopy) %>% 
  mutate(
    post_arthroscopy = case_when(post_arthroscopy == "Yes" ~ 1, TRUE ~ 0)
  )

dat_tka <- dat %>% 
  select(patient_id, injection_id, sex, oa_grade, post_tka) %>% 
  mutate(
    post_tka = case_when(post_tka == "Yes" ~ 1, TRUE ~ 0)
  )

### Logistic Regression (Technically by injection ID, but no need for random effect since nothing is repeated)

inj_mod1 <- glm(conv_to_inj ~ sex + oa_grade, family = binomial, data = dat_inj) %>% 
  tbl_regression(
    label = list(
      oa_grade ~ "OA Grade",
      sex ~ "Sex"
    ),
    exponentiate = T
  )

inj_mod1

post_athr_mod1 <- glm(post_arthroscopy ~ sex + oa_grade, family = binomial, data = dat_post_athro) %>% 
  tbl_regression(
    label = list(
      oa_grade ~ "OA Grade",
      sex ~ "Sex"
    ),
    exponentiate = TRUE
  )

post_athr_mod1  # Interpret with care! Mostly 0s in OA 3 & 4

post_tka_mod1 <- glm(post_tka ~ sex + oa_grade, family = binomial, data = dat_tka) %>% 
  tbl_regression(
    label = list(
      oa_grade ~ "OA Grade",
      sex ~ "Sex"
    ),
    exponentiate = TRUE
  )

post_tka_mod1


### Generalized Linear Mixed Model (Random Effect of Patient ID)
inj_mod2 <- glmer(conv_to_inj ~ sex + oa_grade + (1|patient_id), family = binomial, data = dat_inj) %>% 
  tbl_regression(
    label = list(
      oa_grade ~ "OA Grade",
      sex ~ "Sex"
    )
  )

inj_mod2

post_athr_mod2 <- glmer(post_arthroscopy ~ sex + oa_grade + (1|patient_id), family = binomial, data = dat_post_athro) %>% 
  tbl_regression(
    label = list(
      oa_grade ~ "OA Grade",
      sex ~ "Sex"
    ),
    exponentiate = TRUE
  )

post_athr_mod2  # Random effects are small! Interpret with care

post_tka_mod2 <- glmer(post_tka ~ sex + oa_grade + (1|patient_id), family = binomial, data = dat_tka) %>% 
  tbl_regression(
    label = list(
      oa_grade ~ "OA Grade",
      sex ~ "Sex"
    ),
    exponentiate = TRUE
  )

post_tka_mod2


## Huge Summary Table 

dat_sum <- dat %>% 
  mutate_if(~ is.numeric(.) && all(unique(.) %in% c(0, 1, NA)), factor) %>% 
  mutate_if(is.character, as.factor)

dat_sum %>% 
  tbl_summary(
    by = sex,
    include = -c(patient_id, injection_id),
    type = list(
      all_continuous() ~ "continuous2",
      conversion_to_injection_1yr ~ "categorical",
      convert_to_sx_1yr ~ "categorical",
      baseline_meds:x6mo_activity ~ "categorical"),
    statistic = summary_render,
    digits = list(
      all_continuous() ~ 2
    ),
    missing_text = "(Missing)",
    label = list(
      
    )
  ) %>%
  bold_labels() %>%
  italicize_levels() %>%
  add_p(
    test = lme_stat_render,
    group = patient_id
  )
