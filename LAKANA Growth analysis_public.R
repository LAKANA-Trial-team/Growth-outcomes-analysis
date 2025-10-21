#############################################################################################################################################
# Project   : LAKANA Trial clinicaltrials.gov Identifier: NCT04424511 
# Outcomes  : Secondary outcomes -  Growth Outcomes Analysis
# Script    : LAKANA Growth analysis_public.R
# 
#
# Author    : Laura ADUBRA
# Affiliation: Center for Child, Adolescent and Maternal Health Research, Faculty of Medicine and Health Technology, Tampere University. 
# Contact   : laura.adubra@tuni.fi
#
# Date      : 2025-09-25
# Version   : 1.0
#
#############################################################################################################################################






library(readxl)
library(dplyr)
library(writexl)
require(tidyverse)    
require(reshape2)    
require(lme4)       
library(emmeans)
library(tidyr)
library(ggplot2)


growth<- read.csv("LAKANA Growth outcomes data_de-ident_HIPAA_20251003.csv", na.strings = "NA")
str(growth)

growth <- growth %>%
  mutate(
    # IDs as character
    anon_id     = as.character(anon_id),
    anon_villid = as.character(anon_villid),
    
    # categorical variables as factors
    HhCompChildSex     = as.factor(HhCompChildSex),
    VillageClass        = as.factor(VillageClass),
    intv               = as.factor(intv),
    visit              = as.factor(visit),
    assetindex_cat     = as.factor(assetindex_cat),
    WASHindex_cat      = as.factor(WASHindex_cat),
    season             = as.factor(season),
    dist5km            = as.factor(dist5km),
    Strat              = as.factor(Strat),
    agegroup           = as.factor(agegroup),
    underweight        = as.factor(underweight),
    severe_underweight = as.factor(severe_underweight),
    stunting           = as.factor(stunting),
    severe_stunting    = as.factor(severe_stunting),
    wasting            = as.factor(wasting),
    severe_wasting     = as.factor(severe_wasting),
    smc                = as.factor(smc),
    waz_group_first_MDA= as.factor(waz_group_first_MDA),
    age_group_first_MDA= as.factor(age_group_first_MDA),
    
    # continuous variables as numeric
    age        = as.numeric(age),
    meanWeight = as.numeric(meanWeight),
    meanLength = as.numeric(meanLength),
    meanMUAC   = as.numeric(meanMUAC),
    WAZ        = as.numeric(WAZ),
    LAZ        = as.numeric(LAZ),
    WLZ        = as.numeric(WLZ),
    MUACZ      = as.numeric(MUACZ),
    WASHindex  = as.numeric(WASHindex),
    assetindex = as.numeric(assetindex),
  )


summary(growth)


#Table 2: Anthropometric outcomes by treatment group among children aged 6–8 and 12–14 months

#WAZ
# Null and full model

null_mod_WAZ <- lmer(WAZ ~ agegroup + VillageClass + (1 | anon_villid) + (1 | anon_id), data = growth, REML = FALSE)

mod_WAZ <- lmer(
  WAZ ~ factor(intv) + agegroup + VillageClass + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Summary and model comparison
summary(mod_WAZ)
anova(mod_WAZ,null_mod_WAZ)




#LAZ
# Null and full model

null_mod_LAZ <- lmer(LAZ ~ agegroup + VillageClass +(1 | anon_villid) + (1 | anon_id), data = growth, REML = FALSE)

mod_LAZ <- lmer(
  LAZ ~ factor(intv) + agegroup + VillageClass + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Summary and model comparison
summary(mod_LAZ)
anova(mod_LAZ, null_mod_LAZ)


#WLZ

null_mod_WLZ <- lmer(WLZ ~ agegroup + VillageClass +(1 | anon_villid) + (1 | anon_id), data = growth, REML = FALSE)

mod_WLZ <- lmer(
  WLZ ~ factor(intv) + agegroup + VillageClass +(1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Summary and model comparison
summary(mod_WLZ)
anova(mod_WLZ, null_mod_WLZ)

#muacz

null_mod_MUACZ <- lmer(MUACZ ~ agegroup + VillageClass +(1 | anon_villid) + (1 | anon_id), data = growth, REML = FALSE)

mod_MUACZ <- lmer(
  MUACZ ~ factor(intv) +agegroup + VillageClass + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
# Summary and model comparison
summary(mod_MUACZ)
anova(mod_MUACZ, null_mod_MUACZ)




# Weight in kg model

# Null model 
null_mod_weight <- lmer(meanWeight ~ agegroup + VillageClass + (1 | anon_villid) + (1 | anon_id), data = growth, REML = FALSE)

# Full model 
mod_weight <- lmer(
  meanWeight ~ factor(intv) +agegroup + VillageClass + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


summary(mod_weight)
anova(mod_weight, null_mod_weight)

# 
# Length model

# Null model
null_mod_length <- lmer(meanLength ~ agegroup + VillageClass +(1 | anon_villid) + (1 | anon_id), data = growth, REML = FALSE)

# Full model
mod_length <- lmer(
  meanLength ~ factor(intv) + agegroup + VillageClass + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(mod_length)
anova(mod_length, null_mod_length)



#Mean weight per group

growth %>%
  group_by(intv) %>%
  summarise(
    Mean = round(mean(meanWeight, na.rm = TRUE), 2),
    SD = round(sd(meanWeight, na.rm = TRUE), 2)
  )

#Mean length per group
growth %>%
  group_by(intv) %>%
  summarise(
    Mean = round(mean(meanLength, na.rm = TRUE), 2),
    SD = round(sd(meanLength, na.rm = TRUE), 2)
  )



########## Table 3: Prevalence of underweight, stunting, and wasting by treatment group and age group



data_sub <- growth %>% filter(agegroup == 1)

#data_sub <- growth %>% filter(agegroup == 2) to rerun the same models for the 12-14 mo. age group

#Prevalence among infants aged  6-8 months per group

data_sub %>%
  filter(agegroup == 1) %>%
  group_by(intv) %>%
  summarise(
    n_total = n_distinct(anon_id),
    n_stunted = sum(stunting == 1, na.rm = TRUE),
    percent_stunted = round(100 * n_stunted / n_total, 1),
    .groups = "drop"
  )


data_sub %>%
  filter(agegroup == 1) %>%
  group_by(intv) %>%
  summarise(
    n_total = n_distinct(anon_id),
    n_wasted = sum(wasting == 1, na.rm = TRUE),
    percent_wasted = round(100 * n_wasted / n_total, 1),
    .groups = "drop"
  )

data_sub %>%
  filter(agegroup == 1) %>%
  group_by(intv) %>%
  summarise(
    n_total = n_distinct(anon_id),
    n_underweight = sum(underweight == 1, na.rm = TRUE),
    percent_underweight = round(100 * n_underweight / n_total, 1),
    .groups = "drop"
  )


data_sub %>%
  filter(agegroup == 1) %>%
  group_by(intv) %>%
  summarise(
    n_total = n_distinct(anon_id),
    n_severe_stunting = sum(severe_stunting == 1, na.rm = TRUE),
    percent_severe_stunting = round(100 * n_severe_stunting / n_total, 1),
    .groups = "drop"
  )


data_sub %>%
  filter(agegroup == 1) %>%
  group_by(intv) %>%
  summarise(
    n_total = n_distinct(anon_id),
    n_severe_wasted = sum(severe_wasting == 1, na.rm = TRUE),
    percent_severe_wasted = round(100 * n_severe_wasted / n_total, 1),
    .groups = "drop"
  )


data_sub %>%
  filter(agegroup == 1) %>%
  group_by(intv) %>%
  summarise(
    n_total = n_distinct(anon_id),
    n_severe_underweight = sum(severe_underweight == 1, na.rm = TRUE),
    percent_severe_underweight = round(100 * n_severe_underweight / n_total, 1),
    .groups = "drop"
  )




data_sub <- growth %>% filter(agegroup == 2)
# Prevalence among infants aged  12-14 months per group

data_sub %>%
  filter(agegroup == 2) %>%
  group_by(intv) %>%
  summarise(
    n_total = n_distinct(anon_id),
    n_underweight = sum(underweight == 1, na.rm = TRUE),
    percent_underweight = round(100 * n_underweight / n_total, 1),
    .groups = "drop"
  )


data_sub %>%
  filter(agegroup == 2) %>%
  group_by(intv) %>%
  summarise(
    n_total = n_distinct(anon_id),
    n_severe_underweight = sum(severe_underweight == 1, na.rm = TRUE),
    percent_severe_underweight = round(100 * n_severe_underweight / n_total, 1),
    .groups = "drop"
  )


data_sub %>%
  filter(agegroup == 2) %>%
  group_by(intv) %>%
  summarise(
    n_total = n_distinct(anon_id),
    n_stunted = sum(stunting == 1, na.rm = TRUE),
    percent_stunted = round(100 * n_stunted / n_total, 1),
    .groups = "drop"
  )


data_sub %>%
  filter(agegroup == 2) %>%
  group_by(intv) %>%
  summarise(
    n_total = n_distinct(anon_id),
    n_severe_stunting = sum(severe_stunting == 1, na.rm = TRUE),
    percent_severe_stunting = round(100 * n_severe_stunting / n_total, 1),
    .groups = "drop"
  )


data_sub %>%
  filter(agegroup == 2) %>%
  group_by(intv) %>%
  summarise(
    n_total = n_distinct(anon_id),
    n_severe_stunting = sum(severe_stunting == 1, na.rm = TRUE),
    percent_severe_stunting = round(100 * n_severe_stunting / n_total, 1),
    .groups = "drop"
  )


data_sub %>%
  filter(agegroup == 2) %>%
  group_by(intv) %>%
  summarise(
    n_total = n_distinct(anon_id),
    n_wasted = sum(wasting == 1, na.rm = TRUE),
    percent_wasted = round(100 * n_wasted / n_total, 1),
    .groups = "drop"
  )



data_sub %>%
  filter(agegroup == 2) %>%
  group_by(intv) %>%
  summarise(
    n_total = n_distinct(anon_id),
    n_severe_wasted = sum(severe_wasting == 1, na.rm = TRUE),
    percent_severe_wasted = round(100 * n_severe_wasted / n_total, 1),
    .groups = "drop"
  )


#Models

# Null and full models 
null_mod <- glmer(
  stunting ~  VillageClass + (1 | anon_villid) + (1 | anon_id),
  data = data_sub,
  family = binomial(link = "logit"),
  nAGQ=0,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


mod_stunting <- glmer(
  stunting ~ VillageClass +factor(intv) + (1 | anon_villid) + (1 | anon_id) ,
  data = data_sub,
  family = binomial(link = "logit"),
  nAGQ=0,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Likelihood Ratio Test (overall p-value for intv effect)
anova(null_mod, mod_stunting, test = "Chisq")




# Null and full models
null_mod <- glmer(
  severe_stunting ~ VillageClass +(1 | anon_villid) + (1 | anon_id),
  data = data_sub,
  family = binomial(link = "logit"),
  nAGQ=0,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


mod_sevstunting <- glmer(
  severe_stunting ~ VillageClass +factor(intv) + (1 | anon_villid) + (1 | anon_id),
  data = data_sub,
  family = binomial(link = "logit"),
  nAGQ=0,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Likelihood Ratio Test (overall p-value for intv effect)
anova(null_mod, mod_sevstunting, test = "Chisq")




#underweight
names(data_sub)
# Null and full models 
null_mod <- glmer(
  underweight ~ VillageClass +(1 | anon_villid) + (1 | anon_id),
  data = data_sub,
  family = binomial(link = "logit"),
  nAGQ=0,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


mod_underweight <- glmer(
  underweight ~ VillageClass +factor(intv) + (1 | anon_villid) + (1 | anon_id) ,
  data = data_sub,
  family = binomial(link = "logit"),
  nAGQ=0,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Likelihood Ratio Test (overall p-value for intv effect)
anova(null_mod, mod_underweight, test = "Chisq")




#severe underweight
names(data_sub)
# Null and full models 
null_mod <- glmer(
  severe_underweight ~ VillageClass +(1 | anon_villid) + (1 | anon_id),
  data = data_sub,
  family = binomial(link = "logit"),
  nAGQ=0,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


mod_severe_underweight <- glmer(
  severe_underweight ~ VillageClass +factor(intv) + (1 | anon_villid) + (1 | anon_id),
  data = data_sub,
  family = binomial(link = "logit"),
  nAGQ=0,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Likelihood Ratio Test (overall p-value for intv effect)
anova(null_mod, mod_severe_underweight, test = "Chisq")




#wasting

# Null and full models 
null_mod <- glmer(
  wasting ~ VillageClass +(1 | anon_villid) + (1 | anon_id),
  data = data_sub,
  family = binomial(link = "logit"),
  nAGQ=0,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


mod_wasting <- glmer(
  wasting ~ VillageClass +factor(intv) + (1 | anon_villid) + (1 | anon_id),
  data = data_sub,
  family = binomial(link = "logit"),
  nAGQ=0,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Likelihood Ratio Test (overall p-value for intv effect)
anova(null_mod, mod_wasting, test = "Chisq")


str(growth)
#severe wasting
names(data_sub)
# Null and full models 
null_mod <- glmer(
  severe_wasting ~ VillageClass +(1 | anon_villid) + (1 | anon_id),
  data = data_sub,
  family = binomial(link = "logit"),
  nAGQ=0,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


mod_severe_wasting <- glmer(
  severe_wasting ~ VillageClass +factor(intv) + (1 | anon_villid) + (1 | anon_id),
  data = data_sub,
  family = binomial(link = "logit"),
  nAGQ=0,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Likelihood Ratio Test (overall p-value for intv effect)
anova(null_mod, mod_severe_wasting, test = "Chisq")






####### eTables 3  and 4: Weight and length gains by treatment group among children aged 6–8 and 12–14 months with repeated measurements
### Preparing the dataset
dataset_with_measurements <- growth %>%
  filter(!is.na(meanWeight) & !is.na(meanLength) & !is.na(age))

# Identify children with more than 1 measurement
children_with_repeats <- dataset_with_measurements %>%
  group_by(anon_id) %>%
  summarise(n_visits = n(), .groups = "drop") %>%
  filter(n_visits > 1)

dataset_with_repeats <- dataset_with_measurements %>%
  semi_join(children_with_repeats, by = "anon_id")

dataset_with_repeats %>%
  distinct(anon_id, .keep_all = TRUE) %>%
  count(intv) 

# Subset to only children with multiple measurements
growth_repeat <- dataset_with_measurements %>%
  semi_join(children_with_repeats, by = "anon_id") %>%
  arrange(anon_id, visit)


# Calculate weight gain (g/day) and length gain (mm/day)
growth_repeat <- growth_repeat %>%
  arrange(anon_id, age) %>%   # ensure chronological order
  group_by(anon_id) %>%
  mutate(
    prev_weight = lag(meanWeight),
    prev_length = lag(meanLength),
    prev_age = lag(age)
  ) %>%
  ungroup() %>%
  mutate(
    weight_gain_g_per_day = ifelse(
      !is.na(prev_weight) & !is.na(prev_age),
      (meanWeight - prev_weight) * 1000 / (age - prev_age),
      NA_real_
    ),
    length_gain_mm_per_day = ifelse(
      !is.na(prev_length) & !is.na(prev_age),
      (meanLength - prev_length) * 10 / (age - prev_age),
      NA_real_
    )
  )


growth_repeat %>%
  filter(!is.na(weight_gain_g_per_day) | !is.na(length_gain_mm_per_day)) %>%
  count(anon_id) %>%
  count(n, name = "n_children") %>%
  arrange(n)




#Mean weight per group

growth_repeat %>%
  group_by(intv) %>%
  summarise(
    Mean = round(mean(weight_gain_g_per_day, na.rm = TRUE), 2),
    SD = round(sd(weight_gain_g_per_day, na.rm = TRUE), 2)
  )

#Mean length per group
growth_repeat %>%
  group_by(intv) %>%
  summarise(
    Mean = round(mean(length_gain_mm_per_day, na.rm = TRUE), 2),
    SD = round(sd(length_gain_mm_per_day, na.rm = TRUE), 2)
  )


##### Models

#Null model 
null_mod_weight_gain <- lmer(
  weight_gain_g_per_day ~  agegroup + VillageClass +(1 | anon_villid) + (1 | anon_id),
  data = growth_repeat,
  REML = FALSE
)

null_mod_length_gain <- lmer(
  length_gain_mm_per_day ~ agegroup + VillageClass+ (1 | anon_villid) + (1 | anon_id),
  data = growth_repeat,
  REML = FALSE
)

# Full models 
mod_weight_gain <- lmer(
  weight_gain_g_per_day ~ agegroup + VillageClass + factor(intv) + (1 | anon_villid) + (1 | anon_id),
  data = growth_repeat,
  REML = FALSE
)

mod_length_gain <- lmer(
  length_gain_mm_per_day ~ agegroup + VillageClass +  factor(intv) + (1 | anon_villid) + (1 | anon_id),
  data = growth_repeat,
  REML = FALSE
)

anova(mod_weight_gain, null_mod_weight_gain)
anova(mod_length_gain, null_mod_length_gain)
summary(mod_length_gain)
summary(mod_weight_gain)

#Etable 4 samemodels without adjusting on agegroup and VillageClass

#Null model 
null_mod_weight_gain <- lmer(
  weight_gain_g_per_day ~  (1 | anon_villid) + (1 | anon_id),
  data = growth_repeat,
  REML = FALSE
)

null_mod_length_gain <- lmer(
  length_gain_mm_per_day ~  (1 | anon_villid) + (1 | anon_id),
  data = growth_repeat,
  REML = FALSE
)

# Full models 
mod_weight_gain <- lmer(
  weight_gain_g_per_day ~  factor(intv) + (1 | anon_villid) + (1 | anon_id),
  data = growth_repeat,
  REML = FALSE
)

mod_length_gain <- lmer(
  length_gain_mm_per_day ~  factor(intv) + (1 | anon_villid) + (1 | anon_id),
  data = growth_repeat,
  REML = FALSE
)

anova(mod_weight_gain, null_mod_weight_gain)
anova(mod_length_gain, null_mod_length_gain)








################################## Effect Modification = Supplementary Figure 1 abcd and 2abcd
# Model with interaction
mod_WAZ_sex <- lmer(
  WAZ ~ factor(intv) * factor(HhCompChildSex)  + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

# estimated marginal means
emm_sex <- emmeans(mod_WAZ_sex, ~ intv | HhCompChildSex)
raw_summary <- growth %>%
  group_by(intv, HhCompChildSex) %>%
  summarise(
    mean_WAZ = mean(WAZ, na.rm = TRUE),
    sd_WAZ   = sd(WAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

print(raw_summary)

# mean differences for each subgroup
contrast_sex <- contrast(emm_sex, method = "revpairwise", by = "HhCompChildSex", adjust = "none")
summary(contrast_sex,infer = c(TRUE, TRUE))



#p of intreaction
# model without interaction 
mod_WAZ_no_int <- lmer(
  WAZ ~ factor(intv) + factor(HhCompChildSex) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WAZ_no_int, mod_WAZ_sex)




# model with interaction for age_group_first_MDA
mod_WAZ_agegroup <- lmer(
  WAZ ~ factor(intv) * factor(age_group_first_MDA) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

# estimated marginal means
emm_agegroup <- emmeans(mod_WAZ_agegroup, ~ intv | age_group_first_MDA)
raw_summary <- growth %>%
  filter(!is.na(age_group_first_MDA)) %>%
  group_by(intv, age_group_first_MDA) %>%
  summarise(
    mean_WAZ = mean(WAZ, na.rm = TRUE),
    sd_WAZ   = sd(WAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

# contrasts (mean differences)
contrast_agegroup <- contrast(emm_agegroup, method = "revpairwise")
summary(contrast_agegroup,infer = c(TRUE, TRUE))

#p of intreaction
# model without interaction 
mod_WAZ_no_int <- lmer(
  WAZ ~ factor(intv) + factor(age_group_first_MDA) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WAZ_no_int, mod_WAZ_agegroup)




#### Effect Modifer =  Age group at anthropometric measurement #####


mod_WAZ_meas_agegroup <- lmer(
  WAZ ~ intv * agegroup + VillageClass + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


# estimated marginal means
emm_meas_agegroup <- emmeans(mod_WAZ_meas_agegroup, ~ intv | agegroup)

# contrasts (mean differences)
contrast_meas_agegroup <- contrast(emm_meas_agegroup, method = "revpairwise")
summary(contrast_meas_agegroup,infer = c(TRUE, TRUE))





raw_summary <- growth %>%
  group_by(intv, agegroup) %>%
  summarise(
    mean_WAZ = mean(WAZ, na.rm = TRUE),
    sd_WAZ   = sd(WAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )
# model without interaction 
mod_WAZ_no_int <- lmer(
  WAZ ~ factor(intv) + factor(agegroup) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WAZ_no_int, mod_WAZ_meas_agegroup)



#### Effect Modifer =   WAZ group (<-2SD and >= -2 SD) at first MDA exposure #####



mod_WAZ_wazgroup <- lmer(
  WAZ ~ factor(intv) * factor(waz_group_first_MDA) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)



emm_wazgroup <- emmeans(mod_WAZ_wazgroup, ~ intv | waz_group_first_MDA)

# contrasts (mean differences)
contrast_emm_wazgroup <- contrast(emm_wazgroup, method = "revpairwise")
summary(contrast_emm_wazgroup,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, waz_group_first_MDA) %>%
  summarise(
    mean_WAZ = mean(WAZ, na.rm = TRUE),
    sd_WAZ   = sd(WAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

# model without interaction 
mod_WAZ_no_int <- lmer(
  WAZ ~ factor(intv) + factor(waz_group_first_MDA) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WAZ_no_int, mod_WAZ_wazgroup)


#### Effect Modifer =   SMC #####


mod_WAZ_smc <- lmer(
  WAZ ~ intv * smc + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

emm_WAZ_smc <- emmeans(mod_WAZ_smc, ~ intv | smc)

# contrasts (mean differences)
contrast_emm_WAZ_smc <- contrast(emm_WAZ_smc, method = "revpairwise")
summary(contrast_emm_WAZ_smc,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, smc) %>%
  summarise(
    mean_WAZ = mean(WAZ, na.rm = TRUE),
    sd_WAZ   = sd(WAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

mod_WAZ_no_int <- lmer(
  WAZ ~ factor(intv) + factor(smc) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WAZ_no_int, mod_WAZ_smc)


#### Effect Modifer = assetindex_cat #####


mod_WAZ_asset <- lmer(
  WAZ ~ intv * factor(assetindex_cat) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

emm_WAZ_asset <- emmeans(mod_WAZ_asset, ~ intv | assetindex_cat)

# contrasts (mean differences)
contrast_emm_WAZ_asset <- contrast(emm_WAZ_asset, method = "revpairwise")
summary(contrast_emm_WAZ_asset,infer = c(TRUE, TRUE))



raw_summary <- growth %>%
  group_by(intv, assetindex_cat) %>%
  summarise(
    mean_WAZ = mean(WAZ, na.rm = TRUE),
    sd_WAZ   = sd(WAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

mod_WAZ_no_int <- lmer(
  WAZ ~ factor(intv) + factor(assetindex_cat) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WAZ_no_int, mod_WAZ_asset)



#### Effect Modifer = WASHindex_cat #####

mod_WAZ_wash <- lmer(
  WAZ ~ intv * factor(WASHindex_cat) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

emm_WAZ_wash <- emmeans(mod_WAZ_wash, ~ intv | WASHindex_cat)

# contrasts (mean differences)
contrast_emm_WAZ_wash <- contrast(emm_WAZ_wash, method = "revpairwise")
summary(contrast_emm_WAZ_wash,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, WASHindex_cat) %>%
  summarise(
    mean_WAZ = mean(WAZ, na.rm = TRUE),
    sd_WAZ   = sd(WAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

mod_WAZ_no_int <- lmer(
  WAZ ~ factor(intv) + factor(WASHindex_cat) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WAZ_no_int, mod_WAZ_wash)

#### Effect Modifer = season #####

mod_WAZ_season <- lmer(
  WAZ ~ intv * factor(season) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


emm_season <- emmeans(mod_WAZ_season, ~ intv | season)


# contrasts (mean differences)
contrast_emm_season <- contrast(emm_season, method = "revpairwise")
summary(contrast_emm_season,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, season) %>%
  summarise(
    mean_WAZ = mean(WAZ, na.rm = TRUE),
    sd_WAZ   = sd(WAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

mod_WAZ_no_int <- lmer(
  WAZ ~ factor(intv) + factor(season) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WAZ_no_int, mod_WAZ_season)

#### Effect Modifer = Distance to nearest health center #####

mod_WAZ_dist <- lmer(
  WAZ ~ intv * factor(dist5km) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


emm_dist <- emmeans(mod_WAZ_dist, ~ intv | factor(dist5km))

# contrasts (mean differences)
contrast_emm_dist <- contrast(emm_dist, method = "revpairwise")
summary(contrast_emm_dist,infer = c(TRUE, TRUE))



raw_summary <- growth %>%
  group_by(intv, factor(dist5km)) %>%
  summarise(
    mean_WAZ = mean(WAZ, na.rm = TRUE),
    sd_WAZ   = sd(WAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

mod_WAZ_no_int <- lmer(
  WAZ ~ factor(intv) + factor(dist5km) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WAZ_no_int, mod_WAZ_dist)

#### Effect Modifer = strategieAvance #####

mod_WAZ_strat <- lmer(
  WAZ ~ intv * factor(Strat) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(mod_WAZ_strat)

emm_strat <- emmeans(mod_WAZ_strat, ~ intv | factor(Strat))

# contrasts (mean differences)
contrast_emm_strat <- contrast(emm_strat, method = "revpairwise")
summary(contrast_emm_strat,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, factor(Strat)) %>%
  summarise(
    mean_WAZ = mean(WAZ, na.rm = TRUE),
    sd_WAZ   = sd(WAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

mod_WAZ_no_int <- lmer(
  WAZ ~ factor(intv) + factor(Strat) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WAZ_no_int, mod_WAZ_strat)


#### Effect Modifer =  order of MDA 6 7 8  #####

mod_WAZ_visit <- lmer(
  WAZ ~ intv * factor(visit) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)



emm_visit <- emmeans(mod_WAZ_visit, ~ intv | factor(visit))

# contrasts (mean differences)
contrast_emm_visit <- contrast(emm_visit, method = "revpairwise")
summary(contrast_emm_visit,infer = c(TRUE, TRUE))



raw_summary <- growth %>%
  group_by(intv, factor(visit)) %>%
  summarise(
    mean_WAZ = mean(WAZ, na.rm = TRUE),
    sd_WAZ   = sd(WAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

mod_WAZ_no_int <- lmer(
  WAZ ~ factor(intv) + factor(visit) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WAZ_no_int, mod_WAZ_visit)



#### LAZ

#### Effect Modifer = Child Sex #####

# Model with interaction
mod_LAZ_sex <- lmer(
  LAZ ~ factor(intv) * factor(HhCompChildSex)  + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

# estimated marginal means
emm_sex <- emmeans(mod_LAZ_sex, ~ intv | HhCompChildSex)
raw_summary <- growth %>%
  group_by(intv, HhCompChildSex) %>%
  summarise(
    mean_LAZ = mean(LAZ, na.rm = TRUE),
    sd_LAZ   = sd(LAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

print(raw_summary)

# mean differences for each subgroup
contrast_sex <- contrast(emm_sex, method = "revpairwise", by = "HhCompChildSex", adjust = "none")
summary(contrast_sex,infer = c(TRUE, TRUE))

#p of intreaction
# model without interaction 
mod_LAZ_no_int <- lmer(
  LAZ ~ factor(intv) + factor(HhCompChildSex) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_LAZ_no_int, mod_LAZ_sex)



#### Effect Modifer = Age group at first exposure #####

mod_LAZ_agegroup <- lmer(
  LAZ ~ factor(intv) * factor(age_group_first_MDA) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

# estimated marginal means
emm_agegroup <- emmeans(mod_LAZ_agegroup, ~ intv | age_group_first_MDA)
raw_summary <- growth %>%
  group_by(intv, age_group_first_MDA) %>%
  summarise(
    mean_LAZ = mean(LAZ, na.rm = TRUE),
    sd_LAZ   = sd(LAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

# contrasts (mean differences)
contrast_agegroup <- contrast(emm_agegroup, method = "revpairwise")
summary(contrast_agegroup,infer = c(TRUE, TRUE))

#p of intreaction
# model without interaction 
mod_LAZ_no_int <- lmer(
  LAZ ~ factor(intv) + factor(age_group_first_MDA) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_LAZ_no_int, mod_LAZ_agegroup)





#### Effect Modifer =  Age group at anthropometric measurement #####


mod_LAZ_meas_agegroup <- lmer(
  LAZ ~ intv * agegroup + VillageClass + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


# estimated marginal means
emm_meas_agegroup <- emmeans(mod_LAZ_meas_agegroup, ~ intv | agegroup)

# contrasts (mean differences)
contrast_meas_agegroup <- contrast(emm_meas_agegroup, method = "revpairwise")
summary(emm_meas_agegroup,infer = c(TRUE, TRUE))
summary(contrast_meas_agegroup,infer = c(TRUE, TRUE))




raw_summary <- growth %>%
  group_by(intv, agegroup) %>%
  summarise(
    mean_LAZ = mean(LAZ, na.rm = TRUE),
    sd_LAZ   = sd(LAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

#p of intreaction
# model without interaction 
mod_LAZ_no_int <- lmer(
  LAZ ~ factor(intv) + factor(agegroup) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_LAZ_no_int, mod_LAZ_meas_agegroup)


#### Effect Modifer =   w AZ group (<-2SD and >= -2 SD) at first MDA exposure #####

mod_LAZ_LAZgroup <- lmer(
  LAZ ~ intv * waz_group_first_MDA + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)



emm_LAZgroup <- emmeans(mod_LAZ_LAZgroup, ~ intv | waz_group_first_MDA)

# contrasts (mean differences)
contrast_emm_LAZgroup <- contrast(emm_LAZgroup, method = "revpairwise")
summary(contrast_emm_LAZgroup,infer = c(TRUE, TRUE))



raw_summary <- growth %>%
  group_by(intv, waz_group_first_MDA) %>%
  summarise(
    mean_LAZ = mean(LAZ, na.rm = TRUE),
    sd_LAZ   = sd(LAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )
#p of intreaction
# model without interaction 
mod_LAZ_no_int <- lmer(
  LAZ ~ factor(intv) + factor(waz_group_first_MDA) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_LAZ_no_int, mod_LAZ_LAZgroup)



#### Effect Modifer =   SMC #####


mod_LAZ_smc <- lmer(
  LAZ ~ intv * smc + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

emm_LAZ_smc <- emmeans(mod_LAZ_smc, ~ intv | smc)

# contrasts (mean differences)
contrast_emm_LAZ_smc <- contrast(emm_LAZ_smc, method = "revpairwise")
summary(contrast_emm_LAZ_smc,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, smc) %>%
  summarise(
    mean_LAZ = mean(LAZ, na.rm = TRUE),
    sd_LAZ   = sd(LAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

#p of intreaction
# model without interaction 
mod_LAZ_no_int <- lmer(
  LAZ ~ factor(intv) + factor(smc) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_LAZ_no_int, mod_LAZ_smc)

#### Effect Modifer = assetindex_cat #####


mod_LAZ_asset <- lmer(
  LAZ ~ intv * factor(assetindex_cat) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

emm_LAZ_asset <- emmeans(mod_LAZ_asset, ~ intv | assetindex_cat)

# contrasts (mean differences)
contrast_emm_LAZ_asset <- contrast(emm_LAZ_asset, method = "revpairwise")
summary(contrast_emm_LAZ_asset,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, assetindex_cat) %>%
  summarise(
    mean_LAZ = mean(LAZ, na.rm = TRUE),
    sd_LAZ   = sd(LAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )
#p of intreaction
# model without interaction 
mod_LAZ_no_int <- lmer(
  LAZ ~ factor(intv) + factor(assetindex_cat) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_LAZ_no_int, mod_LAZ_asset)

#### Effect Modifer = WASHindex_cat #####


mod_LAZ_wash <- lmer(
  LAZ ~ intv * factor(WASHindex_cat) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

emm_LAZ_wash <- emmeans(mod_LAZ_wash, ~ intv | WASHindex_cat)

# contrasts (mean differences)
contrast_emm_LAZ_wash <- contrast(emm_LAZ_wash, method = "revpairwise")
summary(contrast_emm_LAZ_wash,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, WASHindex_cat) %>%
  summarise(
    mean_LAZ = mean(LAZ, na.rm = TRUE),
    sd_LAZ   = sd(LAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

#p of intreaction
# model without interaction 
mod_LAZ_no_int <- lmer(
  LAZ ~ factor(intv) + factor(WASHindex_cat) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_LAZ_no_int, mod_LAZ_wash)



#### Effect Modifer = season #####

mod_LAZ_season <- lmer(
  LAZ ~ intv * factor(season) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


emm_season <- emmeans(mod_LAZ_season, ~ intv | factor(season))


# contrasts (mean differences)
contrast_emm_season <- contrast(emm_season, method = "revpairwise")
summary(contrast_emm_season,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, factor(season)) %>%
  summarise(
    mean_LAZ = mean(LAZ, na.rm = TRUE),
    sd_LAZ   = sd(LAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )
#p of intreaction
# model without interaction 
mod_LAZ_no_int <- lmer(
  LAZ ~ factor(intv) + factor(season) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_LAZ_no_int, mod_LAZ_season )


#### Effect Modifer = Distance to nearest health center #####

# model with interaction
mod_LAZ_dist <- lmer(
  LAZ ~ intv * factor(dist5km) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


emm_dist <- emmeans(mod_LAZ_dist, ~ intv | dist5km)

# contrasts (mean differences)
contrast_emm_dist <- contrast(emm_dist, method = "revpairwise")
summary(contrast_emm_dist,infer = c(TRUE, TRUE))



raw_summary <- growth %>%
  group_by(intv, factor(dist5km)) %>%
  summarise(
    mean_LAZ = mean(LAZ, na.rm = TRUE),
    sd_LAZ   = sd(LAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )
#p of intreaction
# model without interaction 
mod_LAZ_no_int <- lmer(
  LAZ ~ factor(intv) + factor(dist5km) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_LAZ_no_int, mod_LAZ_dist )




#### Effect Modifer = strategieAvance #####


mod_LAZ_strat <- lmer(
  LAZ ~ intv * factor(Strat) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(mod_LAZ_strat)

emm_strat <- emmeans(mod_LAZ_strat, ~ intv | factor(Strat))

# contrasts (mean differences)
contrast_emm_strat <- contrast(emm_strat, method = "revpairwise")
summary(contrast_emm_strat,infer = c(TRUE, TRUE))



raw_summary <- growth %>%
  group_by(intv, factor(Strat)) %>%
  summarise(
    mean_LAZ = mean(LAZ, na.rm = TRUE),
    sd_LAZ   = sd(LAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

#p of intreaction
# model without interaction 
mod_LAZ_no_int <- lmer(
  LAZ ~ factor(intv) + factor(Strat) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_LAZ_no_int, mod_LAZ_strat )



#### Effect Modifer =  order of MDA/visit 6 7 8 9  #####


mod_LAZ_visit <- lmer(
  LAZ ~ intv * factor(visit) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)



emm_visit <- emmeans(mod_LAZ_visit, ~ intv | factor(visit))

# contrasts (mean differences)
contrast_emm_visit <- contrast(emm_visit, method = "revpairwise")
summary(contrast_emm_visit,infer = c(TRUE, TRUE))



raw_summary <- growth %>%
  group_by(intv, factor(visit)) %>%
  summarise(
    mean_LAZ = mean(LAZ, na.rm = TRUE),
    sd_LAZ   = sd(LAZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

#p of intreaction
# model without interaction 
mod_LAZ_no_int <- lmer(
  LAZ ~ factor(intv) + factor(visit) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_LAZ_no_int, mod_LAZ_visit )


#### WLZ

#### Effect Modifer = Child Sex #####

mod_WLZ_sex <- lmer(
  WLZ ~ factor(intv) * factor(HhCompChildSex)  + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

# estimated marginal means
emm_sex <- emmeans(mod_WLZ_sex, ~ intv | HhCompChildSex)
raw_summary <- growth %>%
  group_by(intv, HhCompChildSex) %>%
  summarise(
    mean_WLZ = mean(WLZ, na.rm = TRUE),
    sd_WLZ   = sd(WLZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

print(raw_summary)

# mean differences for each subgroup
contrast_sex <- contrast(emm_sex, method = "revpairwise", by = "HhCompChildSex", adjust = "none")
summary(contrast_sex,infer = c(TRUE, TRUE))

#p of intreaction
# model without interaction 
mod_WLZ_no_int <- lmer(
  WLZ ~ factor(intv) + factor(HhCompChildSex) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WLZ_no_int, mod_WLZ_sex )



#### Effect Modifer = Age group at first exposure #####

mod_WLZ_agegroup <- lmer(
  WLZ ~ factor(intv) * factor(age_group_first_MDA) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

# estimated marginal means
emm_agegroup <- emmeans(mod_WLZ_agegroup, ~ intv | age_group_first_MDA)
raw_summary <- growth %>%
  group_by(intv, age_group_first_MDA) %>%
  summarise(
    mean_WLZ = mean(WLZ, na.rm = TRUE),
    sd_WLZ   = sd(WLZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

# contrasts (mean differences)
contrast_agegroup <- contrast(emm_agegroup, method = "revpairwise")
summary(contrast_agegroup,infer = c(TRUE, TRUE))

#p of intreaction
# model without interaction 
mod_WLZ_no_int <- lmer(
  WLZ ~ factor(intv) + factor(age_group_first_MDA) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WLZ_no_int, mod_WLZ_agegroup )




#### Effect Modifer =  Age group at anthropometric measurement #####

mod_WLZ_meas_agegroup <- lmer(
  WLZ ~ intv * agegroup + VillageClass + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


# estimated marginal means
emm_meas_agegroup <- emmeans(mod_WLZ_meas_agegroup, ~ intv | agegroup)

# contrasts (mean differences)
contrast_meas_agegroup <- contrast(emm_meas_agegroup, method = "revpairwise")
summary(emm_meas_agegroup,infer = c(TRUE, TRUE))
summary(contrast_meas_agegroup,infer = c(TRUE, TRUE))



raw_summary <- growth %>%
  group_by(intv, agegroup) %>%
  summarise(
    mean_WLZ = mean(WLZ, na.rm = TRUE),
    sd_WLZ   = sd(WLZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

#p of intreaction
# model without interaction 
mod_WLZ_no_int <- lmer(
  WLZ ~ factor(intv) + factor(agegroup) + VillageClass +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WLZ_no_int, mod_WLZ_meas_agegroup )


#### Effect Modifer =   wAZ group (<-2SD and >= -2 SD) at first MDA exposure #####

mod_WLZ_WLZgroup <- lmer(
  WLZ ~ intv * waz_group_first_MDA + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)



emm_WLZgroup <- emmeans(mod_WLZ_WLZgroup, ~ intv | waz_group_first_MDA)

# contrasts (mean differences)
contrast_emm_WLZgroup <- contrast(emm_WLZgroup, method = "revpairwise")
summary(contrast_emm_WLZgroup,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, waz_group_first_MDA) %>%
  summarise(
    mean_WLZ = mean(WLZ, na.rm = TRUE),
    sd_WLZ   = sd(WLZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

#p of intreaction
# model without interaction 
mod_WLZ_no_int <- lmer(
  WLZ ~ factor(intv) + factor(waz_group_first_MDA) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WLZ_no_int, mod_WLZ_WLZgroup )


#### Effect Modifer =   SMC #####

# model with interaction
mod_WLZ_smc <- lmer(
  WLZ ~ intv * smc + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

emm_WLZ_smc <- emmeans(mod_WLZ_smc, ~ intv | smc)

# contrasts (mean differences)
contrast_emm_WLZ_smc <- contrast(emm_WLZ_smc, method = "revpairwise")
summary(contrast_emm_WLZ_smc,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, smc) %>%
  summarise(
    mean_WLZ = mean(WLZ, na.rm = TRUE),
    sd_WLZ   = sd(WLZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )
#p of intreaction
# model without interaction 
mod_WLZ_no_int <- lmer(
  WLZ ~ factor(intv) + factor(smc) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WLZ_no_int, mod_WLZ_smc )


#### Effect Modifer = assetindex_cat #####


mod_WLZ_asset <- lmer(
  WLZ ~ intv * factor(assetindex_cat) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

emm_WLZ_asset <- emmeans(mod_WLZ_asset, ~ intv | assetindex_cat)

# contrasts (mean differences)
contrast_emm_WLZ_asset <- contrast(emm_WLZ_asset, method = "revpairwise")
summary(contrast_emm_WLZ_asset,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, assetindex_cat) %>%
  summarise(
    mean_WLZ = mean(WLZ, na.rm = TRUE),
    sd_WLZ   = sd(WLZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

#p of intreaction
# model without interaction 
mod_WLZ_no_int <- lmer(
  WLZ ~ factor(intv) + factor(assetindex_cat) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WLZ_no_int, mod_WLZ_asset )
#### Effect Modifer = WASHindex_cat #####


mod_WLZ_wash <- lmer(
  WLZ ~ intv * factor(WASHindex_cat) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

emm_WLZ_wash <- emmeans(mod_WLZ_wash, ~ intv | WASHindex_cat)

# contrasts (mean differences)
contrast_emm_WLZ_wash <- contrast(emm_WLZ_wash, method = "revpairwise")
summary(contrast_emm_WLZ_wash,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, WASHindex_cat) %>%
  summarise(
    mean_WLZ = mean(WLZ, na.rm = TRUE),
    sd_WLZ   = sd(WLZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

#p of intreaction
# model without interaction 
mod_WLZ_no_int <- lmer(
  WLZ ~ factor(intv) + factor(WASHindex_cat) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WLZ_no_int, mod_WLZ_wash )





#### Effect Modifer = season #####

mod_WLZ_season <- lmer(
  WLZ ~ intv * factor(season) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


emm_season <- emmeans(mod_WLZ_season, ~ intv | factor(season))


# contrasts (mean differences)
contrast_emm_season <- contrast(emm_season, method = "revpairwise")
summary(contrast_emm_season,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, factor(season)) %>%
  summarise(
    mean_WLZ = mean(WLZ, na.rm = TRUE),
    sd_WLZ   = sd(WLZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )
#p of intreaction
# model without interaction 
mod_WLZ_no_int <- lmer(
  WLZ ~ factor(intv) + factor(season) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WLZ_no_int, mod_WLZ_season )



#### Effect Modifer = Distance to nearest health center #####


mod_WLZ_dist <- lmer(
  WLZ ~ intv * factor(dist5km) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


emm_dist <- emmeans(mod_WLZ_dist, ~ intv |factor(dist5km))

# contrasts (mean differences)
contrast_emm_dist <- contrast(emm_dist, method = "revpairwise")
summary(contrast_emm_dist,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, factor(dist5km)) %>%
  summarise(
    mean_WLZ = mean(WLZ, na.rm = TRUE),
    sd_WLZ   = sd(WLZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

#p of intreaction
# model without interaction 
mod_WLZ_no_int <- lmer(
  WLZ ~ factor(intv) + factor(dist5km) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WLZ_no_int, mod_WLZ_dist )


#### Effect Modifer = strategieAvance #####

mod_WLZ_strat <- lmer(
  WLZ ~ intv * factor(Strat) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(mod_WLZ_strat)

emm_strat <- emmeans(mod_WLZ_strat, ~ intv | factor(Strat))

# contrasts (mean differences)
contrast_emm_strat <- contrast(emm_strat, method = "revpairwise")
summary(contrast_emm_strat,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, factor(Strat)) %>%
  summarise(
    mean_WLZ = mean(WLZ, na.rm = TRUE),
    sd_WLZ   = sd(WLZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

#p of intreaction
# model without interaction 
mod_WLZ_no_int <- lmer(
  WLZ ~ factor(intv) + factor(Strat) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WLZ_no_int, mod_WLZ_strat )



#### Effect Modifer =  order of MDA/visit 6 7 8 9  #####

mod_WLZ_visit <- lmer(
  WLZ ~ intv * factor(visit) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


emm_visit <- emmeans(mod_WLZ_visit, ~ intv | factor(visit))

# contrasts (mean differences)
contrast_emm_visit <- contrast(emm_visit, method = "revpairwise")
summary(contrast_emm_visit,infer = c(TRUE, TRUE))



raw_summary <- growth %>%
  group_by(intv, factor(visit)) %>%
  summarise(
    mean_WLZ = mean(WLZ, na.rm = TRUE),
    sd_WLZ   = sd(WLZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

#p of intreaction
# model without interaction 
mod_WLZ_no_int <- lmer(
  WLZ ~ factor(intv) + factor(visit) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_WLZ_no_int, mod_WLZ_visit )



#### MUACZ

#### Effect Modifer = Child Sex #####


mod_MUACZ_sex <- lmer(
  MUACZ ~ factor(intv) * factor(HhCompChildSex)  + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

# estimated marginal means
emm_sex <- emmeans(mod_MUACZ_sex, ~ intv | HhCompChildSex)
raw_summary <- growth %>%
  group_by(intv, HhCompChildSex) %>%
  summarise(
    mean_MUACZ = mean(MUACZ, na.rm = TRUE),
    sd_MUACZ   = sd(MUACZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

print(raw_summary)

# mean differences for each subgroup
contrast_sex <- contrast(emm_sex, method = "revpairwise", by = "HhCompChildSex", adjust = "none")
summary(contrast_sex,infer = c(TRUE, TRUE))

#p of intreaction
# model without interaction 
mod_MUACZ_no_int <- lmer(
  MUACZ ~ factor(intv) + factor(HhCompChildSex) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_MUACZ_no_int, mod_MUACZ_sex )


#### Effect Modifer = Age group at first exposure #####

mod_MUACZ_agegroup <- lmer(
  MUACZ ~ factor(intv) * factor(age_group_first_MDA) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

# estimated marginal means
emm_agegroup <- emmeans(mod_MUACZ_agegroup, ~ intv | age_group_first_MDA)
raw_summary <- growth %>%
  group_by(intv, age_group_first_MDA) %>%
  summarise(
    mean_MUACZ = mean(MUACZ, na.rm = TRUE),
    sd_MUACZ   = sd(MUACZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

# contrasts (mean differences)
contrast_agegroup <- contrast(emm_agegroup, method = "revpairwise")
summary(contrast_agegroup,infer = c(TRUE, TRUE))

#p of intreaction
# model without interaction 
mod_MUACZ_no_int <- lmer(
  MUACZ ~ factor(intv) + factor(age_group_first_MDA) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_MUACZ_no_int, mod_MUACZ_agegroup )


#### Effect Modifer =  Age group at anthropometric measurement #####


mod_MUACZ_meas_agegroup <- lmer(
  MUACZ ~ intv * agegroup + VillageClass + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


# estimated marginal means
emm_meas_agegroup <- emmeans(mod_MUACZ_meas_agegroup, ~ intv | agegroup)

# contrasts (mean differences)
contrast_meas_agegroup <- contrast(emm_meas_agegroup, method = "revpairwise")
summary(emm_meas_agegroup,infer = c(TRUE, TRUE))
summary(contrast_meas_agegroup,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, agegroup) %>%
  summarise(
    mean_MUACZ = mean(MUACZ, na.rm = TRUE),
    sd_MUACZ   = sd(MUACZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

#p of intreaction
# model without interaction 
mod_MUACZ_no_int <- lmer(
  MUACZ ~ factor(intv) + factor(agegroup) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_MUACZ_no_int, mod_MUACZ_meas_agegroup )


#### Effect Modifer =   wAZ group (<-2SD and >= -2 SD) at first MDA exposure #####

mod_MUACZ_MUACZgroup <- lmer(
  MUACZ ~ intv * factor(waz_group_first_MDA) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


emm_MUACZgroup <- emmeans(mod_MUACZ_MUACZgroup, ~ intv | factor(waz_group_first_MDA))

# contrasts (mean differences)
contrast_emm_MUACZgroup <- contrast(emm_MUACZgroup, method = "revpairwise")
summary(contrast_emm_MUACZgroup,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, waz_group_first_MDA) %>%
  summarise(
    mean_MUACZ = mean(MUACZ, na.rm = TRUE),
    sd_MUACZ   = sd(MUACZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )
#p of intreaction
# model without interaction 
mod_MUACZ_no_int <- lmer(
  MUACZ ~ factor(intv) + factor(waz_group_first_MDA) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_MUACZ_no_int, mod_MUACZ_MUACZgroup )



#### Effect Modifer =   SMC #####

mod_MUACZ_smc <- lmer(
  MUACZ ~ intv * smc + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

emm_MUACZ_smc <- emmeans(mod_MUACZ_smc, ~ intv | smc)

# contrasts (mean differences)
contrast_emm_MUACZ_smc <- contrast(emm_MUACZ_smc, method = "revpairwise")
summary(contrast_emm_MUACZ_smc,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, smc) %>%
  summarise(
    mean_MUACZ = mean(MUACZ, na.rm = TRUE),
    sd_MUACZ   = sd(MUACZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )
#p of intreaction
# model without interaction 
mod_MUACZ_no_int <- lmer(
  MUACZ ~ factor(intv) + factor(smc) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_MUACZ_no_int, mod_MUACZ_smc )



#### Effect Modifer = assetindex_cat #####

mod_MUACZ_asset <- lmer(
  MUACZ ~ intv * factor(assetindex_cat) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

emm_MUACZ_asset <- emmeans(mod_MUACZ_asset, ~ intv | assetindex_cat)

# contrasts (mean differences)
contrast_emm_MUACZ_asset <- contrast(emm_MUACZ_asset, method = "revpairwise")
summary(contrast_emm_MUACZ_asset,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, assetindex_cat) %>%
  summarise(
    mean_MUACZ = mean(MUACZ, na.rm = TRUE),
    sd_MUACZ   = sd(MUACZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )
#p of intreaction
# model without interaction 
mod_MUACZ_no_int <- lmer(
  MUACZ ~ factor(intv) + factor(assetindex_cat) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_MUACZ_no_int, mod_MUACZ_asset )

#### Effect Modifer = WASHindex_cat #####

mod_MUACZ_wash <- lmer(
  MUACZ ~ intv * factor(WASHindex_cat) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

emm_MUACZ_wash <- emmeans(mod_MUACZ_wash, ~ intv | WASHindex_cat)

# contrasts (mean differences)
contrast_emm_MUACZ_wash <- contrast(emm_MUACZ_wash, method = "revpairwise")
summary(contrast_emm_MUACZ_wash,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, WASHindex_cat) %>%
  summarise(
    mean_MUACZ = mean(MUACZ, na.rm = TRUE),
    sd_MUACZ   = sd(MUACZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )
#p of intreaction
# model without interaction 
mod_MUACZ_no_int <- lmer(
  MUACZ ~ factor(intv) + factor(WASHindex_cat) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_MUACZ_no_int, mod_MUACZ_wash )

#### Effect Modifer = season #####

mod_MUACZ_season <- lmer(
  MUACZ ~ intv * factor(season) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


emm_season <- emmeans(mod_MUACZ_season, ~ intv | factor(season))


# contrasts (mean differences)
contrast_emm_season <- contrast(emm_season, method = "revpairwise")
summary(contrast_emm_season,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, factor(season)) %>%
  summarise(
    mean_MUACZ = mean(MUACZ, na.rm = TRUE),
    sd_MUACZ   = sd(MUACZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

#p of intreaction
# model without interaction 
mod_MUACZ_no_int <- lmer(
  MUACZ ~ factor(intv) + factor(season) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_MUACZ_no_int, mod_MUACZ_season )

#### Effect Modifer = Distance to nearest health center #####

mod_MUACZ_dist <- lmer(
  MUACZ ~ intv * factor(dist5km) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


emm_dist <- emmeans(mod_MUACZ_dist, ~ intv |factor(dist5km))

# contrasts (mean differences)
contrast_emm_dist <- contrast(emm_dist, method = "revpairwise")
summary(contrast_emm_dist,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, factor(dist5km)) %>%
  summarise(
    mean_MUACZ = mean(MUACZ, na.rm = TRUE),
    sd_MUACZ   = sd(MUACZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )
#p of intreaction
# model without interaction 
mod_MUACZ_no_int <- lmer(
  MUACZ ~ factor(intv) + factor(dist5km) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_MUACZ_no_int, mod_MUACZ_dist )

#### Effect Modifer = strategieAvance #####

mod_MUACZ_strat <- lmer(
  MUACZ ~ intv * factor(Strat) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(mod_MUACZ_strat)

emm_strat <- emmeans(mod_MUACZ_strat, ~ intv | factor(Strat))

# contrasts (mean differences)
contrast_emm_strat <- contrast(emm_strat, method = "revpairwise")
summary(contrast_emm_strat,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, factor(Strat)) %>%
  summarise(
    mean_MUACZ = mean(MUACZ, na.rm = TRUE),
    sd_MUACZ   = sd(MUACZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )

#p of intreaction
# model without interaction 
mod_MUACZ_no_int <- lmer(
  MUACZ ~ factor(intv) + factor(Strat) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_MUACZ_no_int, mod_MUACZ_strat )



#### Effect Modifer =  order of MDA/visit 6 7 8 9  #####

mod_MUACZ_visit <- lmer(
  MUACZ ~ intv * factor(visit) + VillageClass + agegroup + (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

emm_visit <- emmeans(mod_MUACZ_visit, ~ intv | factor(visit))

# contrasts (mean differences)
contrast_emm_visit <- contrast(emm_visit, method = "revpairwise")
summary(contrast_emm_visit,infer = c(TRUE, TRUE))


raw_summary <- growth %>%
  group_by(intv, factor(visit)) %>%
  summarise(
    mean_MUACZ = mean(MUACZ, na.rm = TRUE),
    sd_MUACZ   = sd(MUACZ, na.rm = TRUE),
    n        = n(),
    .groups = "drop"
  )


#p of intreaction
# model without interaction 
mod_MUACZ_no_int <- lmer(
  MUACZ ~ factor(intv) + factor(visit) + VillageClass + agegroup +
    (1 | anon_villid) + (1 | anon_id),
  data = growth,
  REML = FALSE
)

anova(mod_MUACZ_no_int, mod_MUACZ_visit )


