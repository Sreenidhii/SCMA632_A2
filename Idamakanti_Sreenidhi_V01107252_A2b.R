# Set the working directory and verify it
setwd('D:\\CHRIST\\Boot camp\\DATA')
getwd()

# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA", "glue")
lapply(libraries, install_and_load)

# Reading the file into R
df_ipl <- read.csv("IPL_ball_by_ball_updated till 2024.csv")
salary <- read_xlsx("IPL SALARIES 2024.xlsx")

# Load required libraries
library(dplyr)
library(readr)
library(stringr)
library(readxl)
library(ggplot2)

# Load datasets
ipl_bbb <- read_csv('IPL_ball_by_ball_updated till 2024.csv')
ipl_salary <- read_excel('IPL SALARIES 2024.xlsx')

# Libraries and Read Data
library(dplyr)
library(readr)
library(readxl)
library(fuzzyjoin)
library(caret)
library(lmtest)
library(stringdist)
library(stats)

# View columns
colnames(df_ipl)


# Group Data
  grouped_data <- df_ipl %>%
    group_by(Season, `Innings.No`, Striker, Bowler) %>%
    summarise(runs_scored = sum(runs_scored, na.rm = TRUE),
              wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE))
  

# Aggregate Data
total_runs_each_year <- grouped_data %>%
  group_by(Season, Striker) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE)) %>%
  ungroup()

total_wicket_each_year <- grouped_data %>%
  group_by(Season, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE)) %>%
  ungroup()

# Fuzzy Matching Function
match_names <- function(name, names_list) {
  matches <- stringdist::stringdist(name, names_list, method = "jw")
  if(min(matches) < 0.2) return(names_list[which.min(matches)]) else return(NA)
}

# Fuzzy Matching and Merging Data
df_salary <- salary
df_runs <- total_runs_each_year

df_salary$Matched_Player <- sapply(df_salary$Player, match_names, df_runs$Striker)
df_merged <- dplyr::left_join(df_salary, df_runs, by = c("Matched_Player" = "Striker"))

# View Unique Seasons and Data
unique(df_merged$Season)
head(df_merged)

# Linear Regression with caret
X <- df_merged %>% select(runs_scored)
y <- df_merged$Rs

set.seed(42)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_index, ]
y_train <- y[train_index]
X_test <- X[-train_index, ]
y_test <- y[-train_index]

model <- train(X_train, y_train, method = "lm")
y_pred <- predict(model, X_test)
mse <- mean((y_test - y_pred)^2)


# Linear Regression with stats
X_train_sm <- cbind(1, as.matrix(X_train))
model_sm <- lm(y_train ~ X_train_sm)

# Print summary of the linear regression model
summary(model_sm)

# Repeat for Wickets
df_runs <- total_wicket_each_year

df_salary$Matched_Player <- sapply(df_salary$Player, match_names, df_runs$Bowler)
df_merged <- dplyr::left_join(df_salary, df_runs, by = c("Matched_Player" = "Bowler"))
df_merged[df_merged$wicket_confirmation > 10, ]

# Subset Data for a Specific Season
df_merged <- df_merged %>% filter(Season %in% c('2022'))

# Linear Regression on Wickets with stats
X <- df_merged %>% select(wicket_confirmation)
y <- df_merged$Rs

set.seed(42)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_index, ]
y_train <- y[train_index]
X_test <- X[-train_index, ]
y_test <- y[-train_index]

X_train_sm <- cbind(1, as.matrix(X_train))
model_sm <- lm(y_train ~ X_train_sm - 1)

# Print summary of the linear regression model
summary(model_sm)

