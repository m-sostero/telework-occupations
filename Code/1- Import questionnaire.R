library(tidyverse)
library(haven)
library(readxl)
library(stringr)

theme_set(theme_bw())

# Inputs and outputs:
# < ./Data/ICP/9_sez_g.xls : answers to section G of ICP questionnaire
# < ./Data/ICP/10_sez_h.xls : answers to section H of ICP questionnaire
# < ./Metadata/questionnaire ONET-ICP.xlsx : 


# Import and combine questionnaire and legend ----

# Import data from sections G and H of ICP questionnaire
# Corresponding to G: "Generalized Work Activities" and H: "Work Context"
quest_G_activities <- read_excel("./Data/ICP/9_sez_g.xls") 
quest_H_context <- read_excel("./Data/ICP/10_sez_h.xls") %>% select(-Descrizione) # Exclude occupation label descriptor

# Join (stack horizontally) questionnaires by occupation ("up_new" in the original file)
quest_wide <- quest_G_activities %>%
  full_join(quest_H_context, by = "up_new") %>%
  rename(pro = up_new, pro_desc = Descrizione) %>%
  rename_at(3:ncol(.), toupper)

# Import question variable labels (text of questions, in Italian and English)
# This is the table shared on Google Sheets, will add ways of importing it from there
question_labels <- read_excel("./Metadata/questionnaire ONET-ICP.xlsx", range = "D1:F99") %>% 
  rename(question = 1, question_label_it = 2, question_label_en = 3)

# Reshape data in long form: occupation-description:question-value
quest <- quest_wide %>%
  gather(var, val, -pro, -pro_desc) %>%
  left_join(question_labels, c("var" = "question"))

write_rds(quest, "./Data/questionnaire.rds")
