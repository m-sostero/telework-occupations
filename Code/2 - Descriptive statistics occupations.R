library(tidyverse)
library(haven)
library(readxl)
library(stringr)

theme_set(theme_bw())

quest <- read_rds("./Data/questionnaire.rds")

# Select teleworkability questions ----

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

