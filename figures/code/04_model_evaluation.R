#### MODEL 1: MULTINOMIAL LOGISTIC REGRESSION

```{r model-multinomial}
#| label: model-multinomial

cat("   MULTINOMIAL LOGISTIC REGRESSION  ")
cat("Predicting: grades_cat (4 categories)\n")
cat("Purpose: Primary model for grade prediction\n\n")

# Model specification
multinom_spec <- multinom_reg(penalty = 0) %>%
  set_engine("nnet", MaxNWts = 5000) %>%  # Increase weight limit
  set_mode("classification")

# Combine recipe and model into workflow
multinom_wf <- workflow() %>%
  add_recipe(multinom_recipe) %>%
  add_model(multinom_spec)

# Cross-validation
cat("Running 10-fold cross-validation...\n")
multinom_cv <- multinom_wf %>%
  fit_resamples(
    resamples = cv_folds,
    metrics = metric_set(accuracy, roc_auc, mn_log_loss),
    control = control_resamples(save_pred = TRUE, verbose = FALSE)
  )

# Display CV Results

cat("\nCross-validation results:\n")
multinom_cv_metrics <- collect_metrics(multinom_cv)
print(multinom_cv_metrics)
```
##### INTERPRETATION:
 accuracy: 62.9% - Percentage of correct predictions
 roc_auc: 0.669 - Discriminative ability (0.5 = random, 1.0 = perfect)
 mn_log_loss: 0.794 - Lower is better; measures calibration
 
```{r}
# Fit final model on full training data
cat("\nFitting final model on training data...\n")
multinom_fit <- multinom_wf %>%
  fit(train_data)

# Test set performance
multinom_test <- multinom_fit %>%
  predict(test_data) %>%
  bind_cols(test_data %>% dplyr::select(grades_cat)) %>%  
  bind_cols(predict(multinom_fit, test_data, type = "prob"))

test_accuracy <- accuracy(multinom_test, truth = grades_cat, estimate = .pred_class)
cat("\nTest set accuracy:", round(test_accuracy$.estimate, 4), "\n")
```
##### INTERPRETATION:
Test accuracy (62.2%) is very close to CV accuracy (62.9%). This stability indicates NO OVERFITTING - the model generalizes well.
62% exceeds baseline accuracy of 54% (always predicting majority class).


#### MODEL 2: BINARY LOGISTIC REGRESSION
Purpose: Early warning system to identify at-risk students
NOTE: Severe class imbalance (94% not at-risk, 6% at-risk) will affect this model.

```{r model-logistic}
#| label: model-logistic

# Model specification
logistic_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

# Workflow
logistic_wf <- workflow() %>%
  add_recipe(logistic_recipe) %>%
  add_model(logistic_spec)

# Cross-validation
cat("Running 10-fold cross-validation...\n")
logistic_cv <- logistic_wf %>%
  fit_resamples(
    resamples = cv_folds,
    # Use yardstick:: explicitly to avoid conflict with caret
    metrics = metric_set(accuracy, roc_auc, 
                         yardstick::sensitivity, 
                         yardstick::specificity),
    control = control_resamples(save_pred = TRUE, verbose = FALSE)
  )

# Results
cat("\nCross-validation results:\n")
logistic_cv_metrics <- collect_metrics(logistic_cv)
print(logistic_cv_metrics)

# Fit final model
cat("\nFitting final model on training data...\n")
logistic_fit <- logistic_wf %>%
  fit(train_data)

# Test set performance
logistic_test <- logistic_fit %>%
  predict(test_data) %>%
  bind_cols(test_data %>% dplyr::select(at_risk)) %>%  
  bind_cols(predict(logistic_fit, test_data, type = "prob"))

test_roc_auc <- roc_auc(logistic_test, truth = at_risk, .pred_1)
cat("\nTest set ROC-AUC:", round(test_roc_auc$.estimate, 4), "\n")
```
##### INTERPRETATION:
accuracy: 94.1% - Misleadingly high due to class imbalance!
roc_auc: 0.802 - Good discrimination in CV
sensitivity: 99.8% - Almost all non-at-risk correctly identified
specificity: 1.3% - Almost NONE of the at-risk students identified!
The model essentially predicts everyone as "not at-risk" because thatminimizes overall error with 94% of data in that class.


#### MODEL 3: POISSON REGRESSION
Purpose: Model days absent (count data). Poisson is appropriate for count outcomes with no upper bound.
```{r model-poisson}
#| label: model-poisson

cat("Predicting: days_absent (count data)\n")
cat("Purpose: Understanding factors affecting attendance\n\n")

# Model specification
poisson_spec <- poisson_reg() %>%
  set_engine("glm") %>%
  set_mode("regression")

# Workflow
poisson_wf <- workflow() %>%
  add_recipe(poisson_recipe) %>%
  add_model(poisson_spec)

# Cross-validation
cat("Running 10-fold cross-validation...\n")
poisson_cv <- poisson_wf %>%
  fit_resamples(
    resamples = cv_folds,
    metrics = metric_set(rmse, mae, rsq),
    control = control_resamples(save_pred = TRUE, verbose = FALSE)
  )

# Results
cat("\nCross-validation results:\n")
poisson_cv_metrics <- collect_metrics(poisson_cv)
print(poisson_cv_metrics)

# Fit final model
cat("\nFitting final model on training data...\n")
poisson_fit <- poisson_wf %>%
  fit(train_data)

# Test set performance
poisson_test <- poisson_fit %>%
  predict(test_data) %>%
  bind_cols(test_data %>% dplyr::select(days_absent))  

test_rmse <- rmse(poisson_test, truth = days_absent, estimate = .pred)
cat("\nTest set RMSE:", round(test_rmse$.estimate, 4), "\n")
```
#####INTERPRETATION:
rmse: 4.55 days - Root mean squared error
mae: 1.68 days - Average prediction error
rsq: 0.146 - Model explains 14.6% of variance in absences
While R² seems low, attendance is influenced by many unmeasured factors (illness, family emergencies, etc.). The model still identifies
significant predictors.


#### MODEL 4: LINEAR DISCRIMINANT ANALYSIS
Purpose: Validate multinomial findings using different statistical assumptions
LDA assumes multivariate normality and equal covariance matrices.

```{r model-lda}
#| label: model-lda

cat("Predicting: grades_cat (4 categories)\n")
cat("Purpose: Alternative classification; compare with multinomial\n\n")

# Model specification
lda_spec <- discrim_linear() %>%
  set_engine("MASS") %>%
  set_mode("classification")

# Workflow
lda_wf <- workflow() %>%
  add_recipe(multinom_recipe) %>%  # Same recipe as multinomial
  add_model(lda_spec)

# Cross-validation
cat("Running 10-fold cross-validation...\n")
lda_cv <- lda_wf %>%
  fit_resamples(
    resamples = cv_folds,
    metrics = metric_set(accuracy, roc_auc),
    control = control_resamples(save_pred = TRUE, verbose = FALSE)
  )

# Results
cat("\nCross-validation results:\n")
lda_cv_metrics <- collect_metrics(lda_cv)
print(lda_cv_metrics)
```
#####INTERPRETATION:
accuracy: 62.6% - Nearly identical to multinomial (62.9%)!
roc_auc: 0.671 - Nearly identical to multinomial (0.669)!
This CONVERGENCE across different methods provides strong evidence that our findings are ROBUST to statistical assumptions.

```{r}
# Fit final model
cat("\nFitting final model on training data...\n")
lda_fit <- lda_wf %>%
  fit(train_data)

# Test set performance - FIX: Use dplyr::select explicitly
lda_test <- lda_fit %>%
  predict(test_data) %>%
  bind_cols(test_data %>% dplyr::select(grades_cat))  

lda_test_accuracy <- accuracy(lda_test, truth = grades_cat, estimate = .pred_class)
cat("\nTest set accuracy:", round(lda_test_accuracy$.estimate, 4), "\n")
```


```{r save results}
model_results <- list(
  # Models
  multinom_fit = multinom_fit,
  logistic_fit = logistic_fit,
  poisson_fit = poisson_fit,
  lda_fit = lda_fit,
  
  # Cross-validation results
  multinom_cv = multinom_cv,
  logistic_cv = logistic_cv,
  poisson_cv = poisson_cv,
  lda_cv = lda_cv,
  
  # CV metrics
  multinom_cv_metrics = multinom_cv_metrics,
  logistic_cv_metrics = logistic_cv_metrics,
  poisson_cv_metrics = poisson_cv_metrics,
  lda_cv_metrics = lda_cv_metrics,
  
  # Test predictions
  multinom_test = multinom_test,
  logistic_test = logistic_test,
  poisson_test = poisson_test,
  lda_test = lda_test,
  
  # Data splits
  train_data = train_data,
  test_data = test_data,
  cv_folds = cv_folds
)
print(model_results)

saveRDS(model_results, "model_results.rds")
cat(" All models and results saved to 'model_results.rds'\n\n")
```

#### Model Comparison

```{r model-comparison}
cat(" MODEL COMPARISON SUMMARY\n\n")

# Check if objects exist before using them
if(exists("multinom_cv_metrics") && exists("test_accuracy")) {
  cat("MULTINOMIAL LOGISTIC (grades_cat):\n")
  cat(sprintf("  CV Accuracy: %.4f\n", 
             multinom_cv_metrics %>% filter(.metric == "accuracy") %>% pull(mean)))
  cat(sprintf("  Test Accuracy: %.4f\n\n", test_accuracy$.estimate))
} else {
  cat(" Multinomial model results not available\n\n")
}

if(exists("logistic_cv_metrics") && exists("test_roc_auc")) {
  cat("BINARY LOGISTIC (at_risk):\n")
  cat(sprintf("  CV ROC-AUC: %.4f\n", 
             logistic_cv_metrics %>% filter(.metric == "roc_auc") %>% pull(mean)))
  cat(sprintf("  Test ROC-AUC: %.4f\n\n", test_roc_auc$.estimate))
} else {
  cat(" Binary logistic model results not available\n\n")
}

if(exists("poisson_cv_metrics") && exists("test_rmse")) {
  cat("POISSON REGRESSION (days_absent):\n")
  cat(sprintf("  CV RMSE: %.4f\n", 
             poisson_cv_metrics %>% filter(.metric == "rmse") %>% pull(mean)))
  cat(sprintf("  Test RMSE: %.4f\n\n", test_rmse$.estimate))
} else {
  cat(" Poisson model results not available\n\n")
}

if(exists("lda_cv_metrics") && exists("lda_test_accuracy")) {
  cat("LINEAR DISCRIMINANT ANALYSIS (grades_cat):\n")
  cat(sprintf("  CV Accuracy: %.4f\n", 
             lda_cv_metrics %>% filter(.metric == "accuracy") %>% pull(mean)))
  cat(sprintf("  Test Accuracy: %.4f\n\n", lda_test_accuracy$.estimate))
} else {
  cat(" LDA model results not available\n\n")
}
```
