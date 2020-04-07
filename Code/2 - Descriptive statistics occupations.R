library(tidyverse)
library(haven)

library(readxl)
library(stringr)

library(googlesheets4)

theme_set(theme_bw())

# Import data ----

# Import questionnaire data assembled in previous script
quest <- read_rds("./Data/questionnaire.rds")

# Import table from Google sheets, comparing teleworking questions
# Will require authentication to access Google sheets the first time
telework_table <- read_sheet("https://docs.google.com/spreadsheets/d/1XF7tZrjl9eTILeq7M4mLeuPoqX4zlK4GflUfsbT2sgU") 

# Import conversion table between Istat CP2011 (5-digit) and ISCO08 (3-digit) occupational codes
conversion_cp_isco <- read_csv("./Metadata/Conversion CP-ISCO.csv", col_types = "cccc") %>% 
  select(
    cp2011 = codice_CP2011,
    isco08 = codice_ISCO08,
    occupation_title = nome_ISCO08
    )

# Select telework varables in questionnaire ---

telework_occupation <- telework_table %>% 
  select(
    question = `Qu. N.`,
    telework = `Telework Matteo`,
    category = Category
  ) %>% 
  filter(!is.na(category)) %>% 
  inner_join(quest, by = c("question" = "var")) %>% 
  select(cp2011 = pro,  question, question_label_en, val, telework, category) 

telework_occupation_isco <- telework_occupation %>% 
  left_join(conversion_cp_isco, by = "cp2011") %>%
  select(cp2011, isco08, occupation_title, everything()) %>% 
  # FIXME: conversion 5-3 digit not weighed by occup volume!
  group_by(isco08, question) %>%
  mutate(
    val_avg = mean(val, na.rm = T),
    val_sd = sd(val, na.rm = T)
    ) %>% 
  distinct(isco08, .keep_all = T) %>% 
  select(-cp2011)

# Expand values of telework questions by ISCO 
telework_occupation_isco %>%
  arrange(category, question) %>% 
  select(isco08, occupation_title, question_label_en, val) %>% 
  pivot_wider(id_cols = c("isco08", "occupation_title"), names_from = question_label_en, values_from = val) %>% 
  arrange(isco08) %>% 
  write_csv("isco_questions.csv")

# Aggregate indices of teleworkability
telework_occupation_isco %>% 
  group_by(isco08, occupation_title, category) %>% 
  summarise(index = mean(val)) %>% 
  pivot_wider(id_cols = c("isco08", "occupation_title"), names_from = category, values_from = index) %>% 
  write_csv("isco_telework.csv")
  
  

# Teleworkability questions population ----

# Select a few questions as examples

quest %>%
  filter(var %in% c("G19A", "H1", "H3", "H4", "H6", "H7", "H8", "H9", "H18", "H19", "H20", "H21", "H29")) %>%
  mutate(question_label = str_wrap(question_label_en, 30)) %>% 
  ggplot(aes(x = val)) +
  geom_histogram() +
  facet_wrap(~ question_label, ncol = 4, scales = "free_y")

# Import occupational structure of Italy, from previous work
# Occupation 
pro_set <- read_rds("./Data/pro_set.rds") %>%
  mutate(n_occup = round(n_occup) %>% as.integer())

# Count people employed by 4-digit occupations (from Labour FOrce Survey) 
# in 2016 (most recent data I have)
employed_4d <- pro_set %>% 
  filter(anno == 2016) %>% 
  group_by(pro) %>% 
  summarise(n_people = sum(n_occup))

# Weight of occupations from 5 digits (precision of ICP) to 4 digits (precision of LFS)
pesi_cp <- read_excel("./Metadata/pesi_cp.xlsx")

# Add 4-digit weights and occupation volumes
quest %>% 
  mutate(cp_4 = str_extract(pro, "^\\d\\.\\d\\.\\d\\.\\d")) %>%
  left_join(pesi_cp, by = c("pro" = "cp_5")) %>% 
  left_join(employed_4d, by = c("cp_4" = "pro")) %>% 
  mutate(n_people_5 = n_people * peso_cp5) %>% 
  filter(var %in% c("G19A", "H1", "H3", "H4", "H6", "H7", "H8", "H9", "H18", "H19", "H20", "H21", "H29")) %>%
  mutate(question_label = str_wrap(question_label_en, 30)) %>% 
  ggplot(aes(x = val, y = stat(density))) +
  geom_histogram(aes(weight = n_people_5)) +
  facet_wrap(~ question_label, ncol = 4, scales = "free_y") +
  labs(
    title = "Distribution of teleworking-relevant activities in Itay",
    subtitle = "Importance or frequency of selected activites (2012 ICP/ONET), weighted across working population in 2016",
    x = "Importance or frequency",
    y = "Frequency across working population"
  )

