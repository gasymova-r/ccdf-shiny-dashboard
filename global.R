library(tidyverse)
library(shiny)
library(rsconnect)
library(shinydashboard)
library(shinydashboardPlus)

# working directory
setwd("set_your_working_dir")

# data_2014 = as_tibble(read.csv("2014.tsv", sep = "\t")) %>% mutate(YEAR = "2014")
# data_2015 = as_tibble(read.csv("2015.tsv", sep = "\t")) %>% mutate(YEAR = "2015")
# data_2016 = as_tibble(read.csv("2016.tsv", sep = "\t")) %>% mutate(YEAR = "2016")
# data_2017 = as_tibble(read.csv("2017.tsv", sep = "\t")) %>% mutate(YEAR = "2017")
# data_2018 = as_tibble(read.csv("2018.tsv", sep = "\t")) %>% mutate(YEAR = "2018")
# data_2019 = as_tibble(read.csv("2019.tsv", sep = "\t")) %>% mutate(YEAR = "2019")
# 
# data = bind_rows(data_2014, data_2015, data_2016, data_2017, data_2018, data_2019)
# 
# data_2014 = as_tibble(read.csv("2014_p.tsv", sep = "\t")) %>% mutate(YEAR = "2014")
# data_2015 = as_tibble(read.csv("2015_p.tsv", sep = "\t")) %>% mutate(YEAR = "2015")
# data_2016 = as_tibble(read.csv("2016_p.tsv", sep = "\t")) %>% mutate(YEAR = "2016")
# data_2017 = as_tibble(read.csv("2017_p.tsv", sep = "\t")) %>% mutate(YEAR = "2017")
# data_2018 = as_tibble(read.csv("2018_p.tsv", sep = "\t")) %>% mutate(YEAR = "2018")
# data_2019 = as_tibble(read.csv("2019_p.tsv", sep = "\t")) %>% mutate(YEAR = "2019")
# 
# data2 = bind_rows(data_2014, data_2015, data_2016, data_2017, data_2018, data_2019)
# write.csv(data2, "payments.csv")

data = read_csv("family.csv")
data2 = read_csv("payments.csv")
data2 = data2 %>% drop_na()

# NAs

# The dataset contains NAs in almost 100,000 rows.
# Main column with NAs is **INCHOUS** -
# Family income source: housing voucher or cash assistance (70,000+ NAs are in this column).
# As we currently expect, however, the column will not play a significant role in our analysis.
data[!complete.cases(data), ]


# some preprocessing to make sure values make sense for a user

data = 
  data %>% 
  drop_na() %>% 
  mutate(SINGPAR = ifelse(SINGPAR == 1, "Yes", "No"), 
         REASON = case_when(
           REASON == 1 ~ "Employment", 
           REASON == 2 ~ "Education/Training", 
           REASON == 3 ~ "Employment and training/education", 
           REASON == 4 ~ "Protective Services", 
           REASON == 6 ~ "Federally-declared emergency - employment", 
           REASON == 7 ~ "Federally-declared emergency - training/education", 
           REASON == 8 ~ "Federally-declared emergency - both employment and training/education", 
           REASON == 9 ~ "Federally-declared emergency and protective services"
         ), 
         INCEMP = ifelse(INCEMP == 1, "Employed, including self-employment", "Not employed"),
         INCFDST = ifelse(INCFDST == 1, "Yes", "No"))

# getting unique values for selection
STATE_vars = unique(data$STATE)
SINGPAR_vars = unique(data$SINGPAR)
INCOME_vars = unique(data$INCOME) 
INCEMP_vars = unique(data$INCEMP) # EMPLOYED OR NO
REASON_vars = unique(data$REASON)
INCFDST_vars = unique(data$INCFDST) # RECEIVE SUPPLEMENTAL NUTRITION ASSISTANCE
FAMILYSZ_vars = unique(data$FAMILYSZ) # Number of family members upon which eligibility is based

STATE_vars2 = unique(data2$STATE)


