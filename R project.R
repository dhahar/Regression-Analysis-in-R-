
# Load libraries
library(tidyverse)   
library(corrplot) 
library(ggplot2)
# Load dataset
df= read.csv("C:\\R\\Student_Performance.csv")
df

head(df)        # preview first rows
str(df)         # check data types
summary(df)     # summary statistics
review(df)

colSums(is.na(df)) #counting missing values per column

# Encode Extracurricular.Activities: Yes = 1, No = 0
df$Extracurricular.Activities <- ifelse(df$Extracurricular.Activities == "Yes", 1, 0)

# Check the first 5 rows
head(df$Extracurricular.Activities, 5)



#Explore distributions using HIstogram

df %>%
  gather(key = "Variable", value = "Value", -Performance.Index) %>%
  ggplot(aes(x = Value)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  facet_wrap(~Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms of Numeric Predictors including Extracurricular Activities",
       x = "Value",
       y = "Count")




# Correlation analysi
numeric_df <- df %>% select_if(is.numeric)
cor_matrix <- cor(numeric_df, use = "complete.obs")

print(cor_matrix)


install.packages("corrplot")   # only run once
library(corrplot) 

corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black")



#  70/30 split
sample_index <- sample(1:nrow(df), 0.7 * nrow(df))
train_data <- df[sample_index, ]
test_data  <- df[-sample_index, ]

mlr_model <- lm(Performance.Index ~ Hours.Studied + Previous.Scores + 
                  Extracurricular.Activities + Sleep.Hours + 
                  Sample.Question.Papers.Practiced, data = train_data)

summary(mlr_model)




