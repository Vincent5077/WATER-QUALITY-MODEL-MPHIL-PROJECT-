# Load libraries
library(caret)
library(tidyverse)
library(corrplot)

# Read data
data <- read.csv("C:/Users/PC/Downloads/real_data2.csv")

# Parameters to model
water_params <- c("pH", "TDS", "TSS", "EC", "DO", "Turbidity")

# ✅ Log-transform water parameters
data[water_params] <- log(data[water_params])

# Cross-validation control
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

# Storage lists
results_list <- list()
residuals_list <- list()
metrics_list <- list()

# Loop over each water parameter
for (param in water_params) {
  
  # Formula: param ~ bands
  formula <- as.formula(paste(param, "~ Blue + Green + Red + NIR + SWIR1 + SWIR2"))
  
  # Train kNN model
  model_knn <- train(formula, 
                     data = data, 
                     method = "knn",
                     trControl = ctrl,
                     preProcess = c("center", "scale"),
                     tuneLength = 10)  # tries several k values
  
  # Predictions (log scale)
  preds_log <- predict(model_knn, newdata = data)
  obs_log <- data[[param]]
  
  # ✅ Back-transform to original scale
  preds <- exp(preds_log)
  obs <- exp(obs_log)
  
  # Residuals in original scale
  residuals <- obs - preds
  
  # Compute accuracy metrics
  valid_idx <- which(!is.na(obs) & !is.na(preds))
  obs_v <- obs[valid_idx]
  pred_v <- preds[valid_idx]
  
  if (length(obs_v) > 0) {
    ss_res <- sum((obs_v - pred_v)^2)
    ss_tot <- sum((obs_v - mean(obs_v))^2)
    r2 <- ifelse(ss_tot == 0, NA, 1 - ss_res / ss_tot)
    rmse <- sqrt(mean((obs_v - pred_v)^2))
    mae  <- mean(abs(obs_v - pred_v))
  } else {
    r2 <- NA; rmse <- NA; mae <- NA
  }
  
  # Store metrics
  metrics_list[[param]] <- data.frame(
    Parameter = param,
    R2 = r2,
    RMSE = rmse,
    MAE = mae
  )
  
  # Store predictions for plotting
  results_list[[param]] <- data.frame(
    Parameter = param,
    Observed = obs,
    Predicted = preds
  )
  
  # Store residuals
  residuals_list[[param]] <- data.frame(
    Parameter = param,
    Observed = obs,
    Predicted = preds,
    Residuals = residuals
  )
}

# Combine all data frames
all_results <- bind_rows(results_list)
all_residuals <- bind_rows(residuals_list)
all_metrics <- bind_rows(metrics_list)

#Summary of results
library(dplyr)

summary_by_param <- all_results %>%
  group_by(Parameter) %>%
  summarise(
    n = n(),                                # number of samples
    mean_pred = mean(Predicted, na.rm = TRUE),
    sd_pred   = sd(Predicted, na.rm = TRUE),
    min_obs = min(Predicted, na.rm = TRUE),
    max_obs = max(Predicted, na.rm = TRUE)
  )

write.csv(summary_by_param, 'kNN_Summary.csv')

# Print metrics
print(all_metrics)

# Save metrics to CSV
write.csv(all_metrics, "D:/PROJECTS/Ankobra Raster/Random Forest/metrics_kNN.csv", row.names = FALSE)

# Merge R² values into results for plotting
plot_results <- all_results %>%
  left_join(all_metrics %>% select(Parameter, R2), by = "Parameter")

# 1️⃣ Facet-wrapped Observed vs Predicted
ggplot(plot_results, aes(x = Observed, y = Predicted)) +
  geom_point(color = "red", alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  facet_wrap(~ Parameter, scales = "free") +
  geom_text(
    data = plot_results %>%
      group_by(Parameter) %>%
      summarise(
        R2 = unique(R2), 
        x = min(Observed, na.rm = TRUE),
        y = max(Predicted, na.rm = TRUE)
      ),
    aes(x = x, y = y, label = paste0("R² = ", round(R2, 2))),
    inherit.aes = FALSE,
    hjust = -0.1,
    vjust = 1.1,
    size = 4,
    fontface = "bold"
  ) +
  labs(
    title = "k-Nearest Neighbour",
    y = "Predicted"
  )

# 2️⃣ Facet-wrapped residuals plot
ggplot(all_residuals, aes(x = Observed, y = Residuals)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ Parameter, scales = "free") +
  labs(
    title = ,
    x = "Observed", y = "Residuals"
  )
