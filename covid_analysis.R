# ---- Load libraries ----
library(tidyverse)
library(zoo)
library(ggplot2)

library(readr)

# ---- Load the dataset from local CSV ----
covid <- read_csv("owid-covid-data.csv", show_col_types = FALSE)

# ---- Quick check ----
head(covid)


# ---- Filter for India ----
df <- covid %>%
  filter(location == "India") %>%
  select(date, total_cases, new_cases, total_deaths, new_deaths, population) %>%
  arrange(date)

# ---- Quick summary ----
summary(select(df, -date))

# ---- Plot: Daily new cases ----
ggplot(df, aes(date, new_cases)) +
  geom_line(color="blue") +
  labs(title="Daily New COVID-19 Cases (India)", x="Date", y="New Cases")

#-----Total Cases Over Time -----
ggplot(df, aes(date, total_cases)) +
  geom_line(color="darkgreen") +
  labs(title="Total COVID-19 Cases (India)", x="Date", y="Total Cases")

#------7-Day Rolling Average (to smooth the noisy daily data)---
df <- df %>%
  mutate(new_cases_7day = rollmean(new_cases, 7, fill = NA, align = "right"))
ggplot(df, aes(date, new_cases_7day)) +
  geom_line(color="purple") +
  labs(title="7-Day Rolling Average of New Cases (India)", x="Date", y="New Cases (7-day avg)")


#-----cases vs deaths:------
ggplot(df, aes(date)) +
  geom_line(aes(y = new_cases, color="Cases")) +
  geom_line(aes(y = new_deaths, color="Deaths")) +
  labs(title="New Cases vs New Deaths (India)", x="Date", y="Count") +
  scale_color_manual("", values=c("Cases"="blue", "Deaths"="red"))

