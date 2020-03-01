library(dplyr)
library(ggplot2)
library(purrr)
library(gridExtra)

df <- read.csv("forestfires.csv")
head(df)
View(df)

df %>%
  group_by(month) %>%
  summarise(n())
df %>%
  group_by(day) %>%
  summarise(n())

ggplot(df) +
  aes(x = month) +
  geom_bar()

ggplot(df) +
  aes(x = day) +
  geom_bar

df <- df %>%
  mutate(month = factor(month, level = c("jan", "feb", "mar", "apr", "may", "jun", 
                                         "jul", "aug", "sep", "oct", "nov", "dec"))) %>%
  mutate(day = factor(day, level = c("mon", "tue", "wed", "thu", "fri", "sat", "sun")))


ggplot(df) +
  aes(x = month) +
  geom_bar()

ggplot(df) +
  aes(x = day) +
  geom_bar()

create_box <- function (x, y) {
  ggplot(data = df) +
    aes_string(x = x, y = y) +
    geom_boxplot() +
    theme(panel.background = element_rect(fill = "white"))
}

x_var <- names(df)[3]
x_var2 <- names(df)[4]
y_var <- names(df)[5:12]

month_box <- map2(x_var, y_var, create_box)
month_box[[2]]
day_box <- map2(x_var2, y_var, create_box)
day_box

create_scatter <- function (x, y) {
  ggplot(data = df) +
    aes_string(x = x, y = y) +
    geom_point(alpha = 0.3) +
    theme(panel.background = element_rect(fill = "white"))
}

x_var_scat <- names(df)[5:12]
y_var_scat <- names(df)[13]

scatters <- map2(x_var_scat, y_var_scat, create_scatter)
gridExtra::grid.arrange((grobs = scatters))
