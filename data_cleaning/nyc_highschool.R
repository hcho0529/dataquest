library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(ggplot2)

combined <- read_csv("combined.csv")
general <- read_tsv("masterfile11_gened_final.txt")
d75 <- read_tsv("masterfile11_d75_final.txt")

# selecting only the relevant columns and filtering only highschool data since that is what we are interested in.
general_select <- general %>%
  select(dbn:aca_tot_11) %>%
  filter(schooltype == "High School")

d75_select <- d75 %>%
  select(dbn:aca_tot_11)

# combine selected general and d75 data frames in rows and rename dbn to upper case so that I can later join with the combined data
total_select <- general_select %>%
  bind_rows(d75_select) %>%
  rename(DBN = dbn)

# combine with the "combined" data using left_join as we are interested in the survey data that are part of the data in the combined data
combined_total <- combined %>%
  left_join(total_select, by = "DBN")

# I am going to check correlation between the variables
cor_mat <- combined_total %>%
  select(avg_sat_score, saf_p_11:aca_tot_11) %>%
  cor(use = "pairwise.complete.obs")
cor_mat

cor_tib <- cor_mat %>%
  as_tibble(rownames = "variable")
cor_mat

# as the matrix is big, I will find the ones that have strong correlation (>|0.25|)
cor_strong <- cor_tib %>%
  select(variable, avg_sat_score) %>%
  filter(avg_sat_score > abs(0.25))

cor_strong

# reshape the data so that I can check the difference between student, parent, and the teacher

combined_gather <- combined_total %>%
  gather(key = "survey", value = score, saf_p_11:aca_tot_11)

# make the score more readable (response_type, question)\
combined_gather <- combined_gather %>%
  mutate(response_type = str_sub(survey, 5, 6)) %>%
  mutate(question = str_sub(survey, 1, 3))

combined_gather <- combined_gather %>%
  mutate(response_type = if_else(response_type == "p_", "parent", 
                                 if_else(response_type == "t_", "teacher", 
                                         if_else(response_type == "s_", "student",
                                                 if_else(response_type == "to", "total", "NA")))))

# plot boxplot for visualization
combined_gather %>%
  filter(response_type != "total") %>%
  ggplot() +
    aes(x = question, y = score, fill = response_type) +
    geom_boxplot()