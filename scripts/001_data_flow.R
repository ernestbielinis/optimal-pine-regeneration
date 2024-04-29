# script for initial loading and preparing data
# clean environment
rm(list = ls())

# load tools and paths
source("scripts/000_tools.R")

# load data with target but also including some factors and potential predictors
data_regeneration <- import("data/data_regeneration.xlsx")

# clean columns names
data_flow <- data_regeneration %>% clean_names()

head(data_flow)
str(data_flow)

# show names
data_flow %>% names()

# change names into English
data_flow <- data_flow %>% 
  rename(
    "year" = "rok_pomiaru",
    #"cutting_year_1" = "sposob_odnowienia_i_rok_wykonania_ciec", # remove text in levels
    "cutting_year_2" = "rok_wykonania_ciec", # ask: what is a difference between "cutting_year_1"?
    "forest_district" = "n_ctwo",
    "stand_id" = "oddzial", # check: English name
    "row" = "rzad", # check: how trait this in a model? Mix-modeling/aggregation/coding one-hot/binary features like far, close to the edge, etc.)
    "soil_preparation" = "sposob_przygotowania_gleby",
    "distance_from_edge" = "odleglosc_od_sciany_lasu_m",
    "one_year_old_ridge" = "x1_latki_bruzda",
    "two_year_old_ridge" = "x2_latki_bruzda",
    "three_year_old_ridge" = "x3_latki_bruzda",
    "four_year_old_ridge" = "x4_latki_bruzda",
    "one_year_old_cut" = "x1_latki_skiba", 
    "two_year_old_cut" = "x2_latki_skiba",
    "three_year_old_cut" = "x3_latki_skiba",
    "four_year_old_cut" = "x4_latki_skiba",
    "ridge" = "bruzda",
    "cut" = "skiba",
    "total_n" = "n_so"
    )

data_flow$soil_preparation %>% unique()

# soil preparation as a factor
data_flow <- data_flow %>%
  mutate(soil_preparation = as.factor(soil_preparation)) %>%
  mutate(soil_preparation = fct_recode(soil_preparation,
                                       boar_tilling = "Dziki",
                                       chip_mulching = "Zrębkowanie",
                                       forest_plow = "LPZ",
                                       no_preparation = "Brak",
                                       retention_group = "Biogrupa"
                                       ))

# # set 'cutting_year_1' as a number (year)
# data_flow <- data_flow %>%
#   mutate(cutting_year_1 = parse_number(cutting_year_1))

# set 'stand' as a factor
data_flow <- data_flow %>%
  mutate(stand = as.factor(stand_id))

# forest district as a factor
data_flow <- data_flow %>%
  mutate(forest_district = as.factor(forest_district)) %>%
  mutate(forest_district = fct_recode(forest_district,
                                       administrative_pression_district = "Kudypy",
                                       high_pine_volume_district = "Miłomłyn",
                                       highly_experienced_district = "Olsztynek"
                                      ))

data_flow %>% saveRDS("data_flow_v1.RDS")

data_flow %>% export("data_flow_v1.xlsx")
import("data_flow_v1.xlsx")

library(lme4)

# Model z forest_district i stand jako efektami losowymi

  control <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
  
  model <- lmer(total_n ~ year + cutting_year_1 + cutting_year_2 +
                  soil_preparation + distance_from_edge + 
                  (1 | forest_district) + (1 | stand) +  # Nadleśnictwo i stand jako zagnieżdżone efekty losowe
                  (1 | row),  # Rząd jako efekt losowy
                data = data_flow, control = control)

summary(model)

r_squared <- performance::r2(model)
print(r_squared)


# random forest model
set.seed(123)  # Dla powtarzalności wyników
rf_model <- randomForest(total_n ~ cutting_year_2 + forest_district
                           soil_preparation + distance_from_edge +
                           stand_id + row,
                         data = data_flow, importance = TRUE, ntree = 500)


print(rf_model)

# Sprawdzenie ważności zmiennych
importance(rf_model)
par(mar = c(1, 1, 1,1)) 
varImpPlot(rf_model)


# random forest with caret
set.seed(123)  # Dla powtarzalności wyników

# Konfiguracja kontroli treningu dla walidacji krzyżowej
train_control <- trainControl(method = "cv", number = 10, savePredictions = "final")

# Trenowanie modelu Random Forest za pomocą caret
model_caret <- train(total_n ~ year + cutting_year_1 + cutting_year_2 +
                       soil_preparation + distance_from_edge +
                       forest_district + stand + row,
                     data = data_flow, 
                     method = "rf",  # Ustawienie metody na Random Forest
                     trControl = train_control,  # Kontrola treningu
                     ntree = 500,  # Liczba drzew w lesie
                     importance = TRUE,  # Obliczenie ważności zmiennych
                     metric = "Rsquared")  # Metryka do optymalizacji

# results
print(model_caret)

# Zapisanie modelu
saveRDS(model_caret, "model_rf_caret.rds")

# Wczytanie modelu
loaded_model <- readRDS("model_rf_caret.rds")

# Zapisanie podsumowania modelu do pliku CSV
summary_df <- summary(model_caret)
write.csv(summary_df, "model_summary.csv")

# Użycie modelu do przewidywania
predictions <- predict(loaded_model, newdata = data_flow)

# Show training R2
# Przewidywanie na całym zbiorze treningowym
train_predictions <- predict(model_caret, data_flow)

# Obliczanie R^2
train_r_squared <- with(data_flow, cor(total_n, train_predictions)^2)


#### selected model ###
# training control
set.seed(123)
control <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",
  summaryFunction = defaultSummary  # Domyślna funkcja podsumowująca RMSE i Rsquared
)

rf_model <- train(
  total_n ~ year + cutting_year_1 + cutting_year_2 +
    soil_preparation + distance_from_edge +
    forest_district + stand + row,
  data = data_flow,
  method = "rf",
  trControl = control,
  metric = "RMSE",
  tuneLength = 1,  # Tutaj mtry zostaje ustawione na optymalne 16
  tuneGrid = data.frame(mtry = 16)
)

MAPE <- function(data, lev = NULL, model = NULL) {
  mape <- mean(abs(data$obs - data$pred) / data$obs) * 100
  names(mape) <- "MAPE"
  return(mape)
}

control$summaryFunction <- function(data, lev = NULL, model = NULL) {
  res <- defaultSummary(data, lev, model)
  mape <- MAPE(data, lev, model)
  c(res, mape)
}

# Wyświetlanie wyników
print(rf_model)

# Zapisywanie wyników do CSV
results <- rf_model$results
write.csv(results, "model_performance.csv")

predictions <- rf_model$pred
write.csv(predictions, "model_predictions_each_fold.csv")

# Przygotowanie listy z modelem i wynikami
model_data <- list(
  model = rf_model,
  predictions = rf_model$pred,
  performance = rf_model$results
)

# Zapisanie listy do pliku .rds
saveRDS(model_data, "model_with_results.rds")

# Wczytanie modelu i wyników z pliku RDS
loaded_model_data <- readRDS("model_with_results.rds")

# Dostęp do poszczególnych elementów
model <- loaded_model_data$model
predictions <- loaded_model_data$predictions
performance <- loaded_model_data$performance

# Możesz teraz używać tych danych, np. wyświetlić podsumowanie modelu
print(summary(model))

# Wczytanie modelu i wyników z pliku RDS
loaded_model_data <- readRDS("model_with_results.rds")

# Dostęp do poszczególnych elementów
model <- loaded_model_data$model
predictions <- loaded_model_data$predictions
performance <- loaded_model_data$performance

# Możesz teraz używać tych danych, np. wyświetlić podsumowanie modelu
print(summary(model))

#### R2 trening and MAPE ####
library(caret)

# Używanie modelu do przewidywania na danych treningowych
train_predictions <- predict(rf_model, newdata = data_flow)

# Obliczanie R^2
actuals <- data_flow$total_n  # rzeczywiste wartości zmiennej zależnej
r_squared <- cor(actuals, train_predictions)^2

# Obliczanie MAPE
MAPE <- function(actuals, predicted) {
  mean(abs((actuals - predicted) / actuals)) * 100
}

mape_value <- MAPE(actuals, train_predictions)

# Wyświetlenie wyników
cat(sprintf("R-squared: %.4f\n", r_squared))
cat(sprintf("MAPE: %.4f%%\n", mape_value))

# Opcjonalnie: Zapisywanie wyników do pliku
results_df <- data.frame(R_squared = r_squared, MAPE = mape_value)
write.csv(results_df, "training_metrics.csv")

MAPE <- function(actuals, predicted) {
  mean(abs((actuals - predicted) / (actuals + 1e-10))) * 100
}

mape_value <- MAPE(actuals, train_predictions)
cat(sprintf("MAPE: %.4f%%\n", mape_value))

MAPE <- function(actuals, predicted) {
  epsilon <- 1e-10  # mała wartość dodawana do mianownika
  adjusted_actuals <- ifelse(abs(actuals) < epsilon, actuals + epsilon, actuals)
  mape <- mean(abs((adjusted_actuals - predicted) / adjusted_actuals)) * 100
  return(mape)
}

mape_value <- MAPE(actuals, train_predictions)
cat(sprintf("MAPE: %.4f%%\n", mape_value))


MAE <- function(actuals, predicted) {
  mean(abs(actuals - predicted))
}

RMSE <- function(actuals, predicted) {
  sqrt(mean((actuals - predicted)^2))
}

mae_value <- MAE(actuals, train_predictions)
rmse_value <- RMSE(actuals, train_predictions)

cat(sprintf("MAE: %.4f\n", mae_value))
cat(sprintf("RMSE: %.4f\n", rmse_value))









