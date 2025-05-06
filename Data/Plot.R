load("recents.RData")
load("Top90s.RData")
load("Top80s.RData")
load("Top70s.RData")
load("Top60s.RData")
load("Top00s.RData")
load("Love_90s.RData")
load("Love_80s.RData")
load("Love_70s.RData")
load("Love_60s.RData")
load("Love_00s.RData")
load("Love_recents.RData")
load("Dance1_90s.RData")
load("Dance2_90s.RData")
load("Dance_00s.RData")
load("Dance_recents.RData")
load("Happy_Songs.RData")
load("Sad_Songs.RData")
load("MASTER_DATA.RData")

library(tibble)
library(dplyr)
library(ggplot2)
library(fmsb)

All00s = bind_rows(Top00s, Love_00s, Dance_00s) %>% distinct()
Allrecents = bind_rows(Recents, Love_recents, Dance_recents) %>% distinct()
All90s = bind_rows(Top90s, Love_90s, Dance1_90s, Dance2_90s) %>% distinct()
All80s = bind_rows(Top80s, Love_80s) %>% distinct()
All70s = bind_rows(Top70s, Love_70s) %>% distinct()
All60s = bind_rows(Top60s, Love_60s) %>% distinct()

plot00s = ggplot(All00s, aes(x = Popularity, y = acousticness, color = loudness)) + 
  geom_point(size = 5*All00s$energy, alpha = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.5) +
  scale_color_viridis_c(option = "plasma") +
  scale_size_continuous(range = c(1, 6)) +
  labs(
    title = "Factors affecting popularity in 00s",
    subtitle = "Popularity has the highest correlation with acousticness and loudness, the size denotes energy",
    x = "Popularity",
    y = "Acousticness",
    color = "Loudness",
    size = "Energy"
  ) 

ggsave("plot.png", plot = plot00s, width = 10, height = 8, dpi = 300)

corr_matrix <- function(df) {
  n = ncol(df)
  cor_mat = matrix(0, n-2, n-2)
  
  for (i in 3:n) {
    x = df[[i]]  
    for (j in 3:n) {
      y = df[[j]]  
      cor_mat[i-2, j-2] = cor(x, y)  
    }
  }
  colnames(cor_mat) = names(df)[3:n]
  rownames(cor_mat) = names(df)[3:n]
  
  return(cor_mat)
}

create_corrmat = function(df){
  
  cor_mat = corr_matrix(df)
  
  correlation = expand.grid(
    rows = rownames(cor_mat),
    columns = colnames(cor_mat)
  )
  correlation$value <- as.vector(cor_mat)
  
  # Create the heatmap
  ggplot(correlation, aes(x = rows, y = columns, fill = value)) +
    geom_tile() +
    geom_text(aes(label = round(value, 2)),
              color = ifelse(abs(correlation$value) > 0.5, "white", "black"),
              size = 3) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = c(-1, 1)
    )
}

plotrecents = ggplot(Allrecents, aes(x = Popularity, y = acousticness, color = loudness)) + 
  geom_point(size = 5*Allrecents$energy, alpha = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.5) +
  scale_color_viridis_c(option = "plasma") +
  scale_size_continuous(range = c(1, 6)) +
  labs(
    title = "Factors affecting popularity in 00s",
    subtitle = "Popularity has the highest correlation with acousticness and loudness, the size denotes energy",
    x = "Popularity",
    y = "Acousticness",
    color = "Loudness",
    size = "Energy"
  ) 

ggsave("plotrecents.png", plot = plotrecents, width = 10, height = 8, dpi = 300)

All = bind_rows(Allrecents, All00s, All90s, All80s, All70s, All60s) %>% distinct()
save(All, file = "MASTER_DATA.RData")

violin = function(df){
  
  df$decade <- floor(df$Released / 10) * 10
  
  # Create violin plot
  ggplot(df, aes(x = acousticness, y = Popularity, fill = as.factor(decade))) +
    geom_violin() +
    facet_wrap(~decade, scales = "free_x") +
    labs(x = "Acousticness", y = "Popularity", fill = "Decade")
}

violin_plot = violin(All)
ggsave("Violin_plot.png", plot = violin_plot, width = 10, height = 8, dpi = 300)

density_plot = ggplot(All, aes(x = acousticness, y = Popularity)) +  
  geom_density_2d_filled(alpha = 0.6) +
  geom_point(aes(size = energy), alpha = 0.4) +  
  scale_fill_viridis_d(name = "Density Level") +
  scale_size_continuous(name = "Energy",
                        range = c(0,3),
                        breaks = seq(0, 1, by = 0.2)) +
  labs(title = "2D Density Distribution",
       subtitle = "Darker colors = Higher density of points",
       caption = "Point size represents energy level")

ggsave("density_plot.png", plot = density_plot, width = 10, height = 8, dpi = 300)

boxplot(x = Happy$danceability)

Happy$Mood = "Happy"
Sad$Mood = "Sad"
Mood_Test = bind_rows(Happy, Sad) %>% distinct()

library(tidyverse)
library(caret)

# 1. First, split your data into training and testing sets
# Assuming your dataframe is called 'df' and target variable is 'mood'
df = Mood_Test
df$mood <- ifelse(df$Mood == "Sad", 1, 0)
set.seed(123)  # for reproducibility
train_index <- createDataPartition(df$mood, p = 0.8, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

formula <- as.formula(paste("mood ~", paste(names(df)[c(5,6,10,11)], collapse = " + ")))
# 2. Fit the logistic regression model
model <- glm(formula, data = train_data, family = "binomial")

# 3. Look at the model summary
summary(model)

# 4. Make predictions on test set
predictions_prob <- predict(model, newdata = test_data, type = "response")
predictions <- ifelse(predictions_prob > 0.5, "Sad", "Happy")

# 5. Evaluate the model
# Create confusion matrix
conf_matrix <- table(Predicted = predictions, Actual = test_data$mood)
print("Confusion Matrix:")
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", round(accuracy, 3)))


# Detailed model evaluation
library(broom)

# Get model coefficients in tidy format
tidy_model <- tidy(model)
print(tidy_model)

# Plot coefficients
ggplot(tidy_model, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = estimate - std.error, 
                    ymax = estimate + std.error), 
                width = 0.2) +
  coord_flip() +
  labs(title = "Feature Coefficients",
       x = "Features",
       y = "Coefficient Estimate")

# Cross-validation
library(boot)
cv_model <- cv.glm(data = train_data, 
                   glmfit = model,
                   K = 10)  # 10-fold cross-validation
print(paste("CV Error:", mean(cv_model$delta)))

# Plot predicted probabilities
test_data$predicted_prob <- predictions_prob
ggplot(test_data, aes(x = predicted_prob, fill = mood)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Predicted Probabilities by Actual Class",
       x = "Predicted Probability",
       y = "Density")