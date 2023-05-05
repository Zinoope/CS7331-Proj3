# PART I: cleaning and Visualization -------------------------------------------

## Step I-01: Loading the Dataset ----------------------------------------------------

#load the csv file/ check how many missing values do we have
library("tidyverse")
library(FSelector)
library(seriation)
library(caret)

cases <- read_csv("../data/Covid 04-02 + Census 2020-5yrs + Geo Boundaries.csv")
cases <- cases %>% mutate_if(is.character, factor)
dim(cases)
# check for NA values
is.na(cases) %>% sum()

## Step I-02: Aggregation, Normalization and Selection -------------------------

cases_filtered <- cases %>% mutate(
  female_under_40_ratio= (female_under_5 +
                            female_5_to_9 +
                            female_10_to_14 +
                            female_15_to_17 +
                            female_18_to_19 +
                            female_20 +
                            female_21 +
                            female_22_to_24 +
                            female_25_to_29 +
                            female_30_to_34 +
                            female_35_to_39) / total_pop,
  
  female_pop_P100=female_pop/total_pop*100,
  
  male_under_40_ratio= (male_under_5 +
                          male_5_to_9 +
                          male_10_to_14 +
                          male_15_to_17 +
                          male_18_to_19 +
                          male_20 +
                          male_21 +
                          male_22_to_24 +
                          male_25_to_29 +
                          male_30_to_34 +
                          male_35_to_39) / total_pop,
  
  male_pop_P100=male_pop/total_pop*100,
  
  asian_pop_P1000=asian_pop/total_pop*1000,
  black_pop_P1000=black_pop/total_pop*1000,
  hispanic_pop_P1000=hispanic_pop/total_pop*1000,
  amerindian_pop_P1000 = amerindian_pop / total_pop*1000,
  
  deaths_P1000 = deaths/total_pop*1000,
  confirmed_cases_P1000= confirmed_cases/total_pop*1000,
  
  walked_to_work_P1000 = walked_to_work/total_pop*1000,
  commuters_by_public_transportation_P1000 = commuters_by_public_transportation/total_pop*1000,
  commuters_by_carpool_P1000 = commuters_by_carpool/total_pop*1000,
  commuters_drove_alone_P1000 = commuters_drove_alone/total_pop*1000,
  
  pop_density_Pkm= total_pop * 10^6 /area_land_meters,
  
  # Added:
  nonfamily_households_P1000= nonfamily_households/total_pop*1000,
  housing_units_P1000 = housing_units/total_pop*1000,
  employed_pop_P1000 = employed_pop /total_pop*1000, 
  unemployed_pop_P1000 = unemployed_pop /total_pop*1000,
  #percent_income_spent_on_rent: this one is already normalized 
  deaths_per_confirmed = deaths/confirmed_cases
  
)

cases_cleaned <- cases_filtered %>% select(# identification variables [2]
  county_name,
  state,
  # class continuous variables [2]
  confirmed_cases_P1000,
  deaths_P1000,
  # decision variables [21]
  female_pop_P100,
  male_pop_P100,
  female_under_40_ratio,
  male_under_40_ratio,
  walked_to_work_P1000,
  commuters_by_public_transportation_P1000,
  commuters_by_carpool_P1000,
  commuters_drove_alone_P1000,
  income_per_capita,
  asian_pop_P1000,
  black_pop_P1000,
  hispanic_pop_P1000,
  amerindian_pop_P1000,
  median_age,
  pop_density_Pkm,
  median_income,
  nonfamily_households_P1000,
  housing_units_P1000,
  employed_pop_P1000,
  unemployed_pop_P1000,
  percent_income_spent_on_rent,
  deaths_per_confirmed
  
) 

# check for NA values
is.na(cases_cleaned) %>% sum()
cases_cleaned
rm(cases, cases_filtered)


## Step I-03: visualization and class identification ---------------------------

# confirmed cases removed outliers
cc_rm_outlier <- cases_cleaned %>% filter(confirmed_cases_P1000 <= 600)
# deaths removed outliers
d_rm_outlier <- cases_cleaned %>% filter(deaths_P1000 <= 10)

ggplot(cc_rm_outlier, mapping = aes(confirmed_cases_P1000)) + geom_histogram(bins = 1000)+labs(x= "Confirmed cases per 1000")
ggplot(d_rm_outlier, mapping = aes(deaths_P1000)) + geom_histogram(bins = 100)+labs(x= "Deaths per 1000")

summary(cc_rm_outlier)
summary(d_rm_outlier)

#Confirmed Cases:

#Lower 25% - Low x < 245
#Middle 25-50% - Medium Low 245 < x < 286
#Middle 50-75% - Medium High 286 < x < 330
#Upper 25% - High x > 330

#Deaths:
#Lower 25% - Low x < 2.8
#Middle 25-50% - Medium Low 2.8 < x < 4.0
#Middle 50-75% - Medium High 4.0 < x < 5.0
#Upper 25% - High x > 5.0

#Classify Risk based on confirmed cases
cc_classed <- cc_rm_outlier
cc_classed$confirmed_risk <- cut(cc_rm_outlier$confirmed_cases_P1000,
                                 breaks=c(-1,245.7,290.1,330.4,4000),
                                 labels=c('Low', 'Medium Low', 'Medium High', 'High'))

#Classify Risk based on deaths
d_classed <- d_rm_outlier
d_classed$death_risk <- cut(d_rm_outlier$deaths_P1000,
                            breaks=c(-1,2.8,3.974,5.048,10000),
                            labels=c('Low', 'Medium Low', 'Medium High', 'High'))

summary(cc_classed)
summary(d_classed)
rm(cc_rm_outlier, d_rm_outlier)


# States that are similar in terms of population to Ohio (+- 2 million people):
#Pennsylvania - PA
#Illinois - IL
#Georgia - GA
#North Carolina - NC
#Michigan - MI
#New Jersey - NJ
#Virginia - VA

# Visualize the map of states and their counties in terms of risk
test_copy <- cc_classed #%>% filter((state %in% c("PA", "IL", "OH", "GA", "NC","MI","NJ","VA")))

counties <- as_tibble(map_data("county"))
counties <- counties %>% 
  rename(c(county = subregion, state = region)) %>%
  mutate(state = state.abb[match(state, tolower(state.name))]) %>%
  select(state, county, long, lat, group)
#counties 

counties_all <- counties %>% left_join(test_copy %>% 
                                         mutate(county = county_name %>% str_to_lower() %>% 
                                                  str_replace('\\s+county\\s*$', '') %>%
                                                str_replace('\\s+parish\\s*$', '')))

ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = confirmed_risk))+
  scale_fill_manual(values = c("#4ceb34", "#e7ed26", "#ed8d26", "#ed3726"))

rm(counties, counties_all, test_copy)

# PART II: classification ------------------------------------------------------

## Step II-01: choosing classification variables -------------------------------

# Heat map for possible correlations between the data
cm <- cor(cc_classed %>% select_if(is.numeric) %>% na.omit)
hmap(cm, margins = c(14,14))
rm(cm)

# remove the classes building variables
cc_classed <- cc_classed %>% select(-c(confirmed_cases_P1000, deaths_P1000))
d_classed <- d_classed %>% select(-c(confirmed_cases_P1000, deaths_P1000))

# extract the train set
cases_train <- cc_classed %>% filter(state %in% c("PA", "IL", "GA", "NC", "MI", "NJ", "VA"))
# check for class balance for the filtered dataset
cases_train %>% pull(confirmed_risk) %>% table()

# extract the test set
cases_test <-  cc_classed %>% filter(!(state %in% c("PA", "IL", "GA", "NC", "MI", "NJ", "VA"))) 
# check for class balance for the filtered dataset
cases_test %>% pull(confirmed_risk) %>% table()

# see attribute importance
cases_train %>%  chi.squared(confirmed_risk ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head(n=15)

# make a tree based classification
fit <- cases_train %>%
  train(confirmed_risk ~ . - county_name - state,
        data = . ,
        #method = "rpart",
        method = "rf",
        #method = "nb",
        trControl = trainControl(method = "cv", number = 10)
  )
fit
varImp(fit)

#rm(fit)

X_train <- cases_train %>% select(c(county_name,
                                   state,
                                   black_pop_P1000,
                                   employed_pop_P1000,
                                   median_income,
                                   income_per_capita,
                                   commuters_drove_alone_P1000,
                                   pop_density_Pkm,
                                   walked_to_work_P1000,
                                   amerindian_pop_P1000,
                                   median_age,
                                   nonfamily_households_P1000,
                                   commuters_by_public_transportation_P1000,
                                   housing_units_P1000,
                                   male_under_40_ratio,
                                   confirmed_risk))


## Step II-02: Selecting the Models to Train/ Confirmed Cases ------------------

# K-Folds Validation: 
train_index <- createFolds(X_train$confirmed_risk, k = 10)

# Hyperparameter tuning for cTree
mincriterion <- seq(0.001, 0.01, by=0.0001)
cTree_tuneGrid = as.data.frame(mincriterion)

# Model 1: Decision Tree
ctreeFit <- X_train %>% train(confirmed_risk ~ .- county_name - state,
                                method = "ctree",
                                data = .,
                                tuneLength = 5,
                                tuneGrid = cTree_tuneGrid,
                                trControl = trainControl(method = "cv", indexOut = train_index))
ctreeFit

# Hyperparameter tuning for SVM
C <- seq(0.01, 0.1, by=0.01)
svm_tuneGrid = as.data.frame(C)

# Model 2: Support Vector Machine
svmFit <- X_train %>% train(confirmed_risk ~.- county_name - state,
                              method = "svmLinear",
                              data = .,
                              tuneLength = 5,
                              tuneGrid = svm_tuneGrid,
                              trControl = trainControl(method = "cv", indexOut = train_index))
svmFit

# Hyperparameter tuning for Random Forest
mtry <- seq(0.01, 0.1, by=0.01)
rf_tuneGrid = as.data.frame(mtry)

# Model 3: Random Forest
randomForestFit <- X_train %>% train(confirmed_risk ~ .- county_name - state,
                                       method = "rf",
                                       data = .,
                                       tuneLength = 5,
                                       tuneGrid = rf_tuneGrid,
                                       trControl = trainControl(method = "cv", indexOut = train_index))
randomForestFit

# Hyperparameter tuning for Neural Network
grid <- expand.grid(
  size = as.numeric(c(5, 10, 20, 30)),
  decay = as.numeric(c(0.1, 0.01, 0.001))
)
colnames(grid) <- c("size", "decay", "maxit")
# Model 4: Neural Network
nnetFit <- X_train %>% train(confirmed_risk ~ .- county_name - state,
                               method = "nnet",
                               data = .,
                               tuneLength = 5,
                               tuneGrid = grid,
                               trControl = trainControl(method = "cv", indexOut = train_index),
                               maxit = 500,
                               trace = FALSE)
nnetFit


## Step II-03: Compare the models ----------------------------------------------

resamps <- resamples(list(
  ctree = ctreeFit,
  SVM = svmFit,
  randomForest = randomForestFit,
  NeuralNet = nnetFit
))
resamps
summary(resamps)

library(lattice)
bwplot(resamps, layout = c(3, 1))

difs <- diff(resamps)
difs
summary(difs)

## Step II-04: Testing each model to predict Ohio ------------------------------

### added visualization part 
counties <- as_tibble(map_data("county"))
counties_OH <- counties %>% dplyr::filter(region == "ohio") %>% 
  rename(c(county = subregion))

rm(counties)

#We could just use cases_test here, just make sure you isolate "OH"
# get ohio counties aside
OH_test <- cc_classed %>% filter((state %in% c("OH")))

# before doing any prediction create the ground truth cluster visualization 
truth_OH <- OH_test %>% mutate(county = county_name %>% 
                                          str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
truth_clust <- counties_OH %>% left_join(truth_OH)
truth_map <- ggplot(truth_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = confirmed_risk)) +
  coord_quickmap() +
  scale_fill_manual(values = c("#4ceb34", "#e7ed26", "#ed8d26", "#ed3726")) +
  labs(title = "Truth", fill = "Confirmed risk")

# now create the prediction = remove the class variable
OH_predict <- subset(OH_test, select = -c(confirmed_risk) )

# Decision Tree:
pr <- predict(ctreeFit, OH_predict)
confusionMatrix(pr, reference = OH_test$confirmed_risk)

## visualize:
prediction_OH <- truth_OH %>% mutate(confirmed_risk = pr)
prediction_clust <- counties_OH %>% left_join(prediction_OH)
decisionTree_map <- ggplot(prediction_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = confirmed_risk)) +
  coord_quickmap() +
  scale_fill_manual(values = c("#4ceb34", "#e7ed26", "#ed8d26", "#ed3726")) +
  labs(title = "Decision Tree", fill = "Confirmed risk")


# Support Vector Machine
pr <- predict(svmFit, OH_predict)
confusionMatrix(pr, reference = OH_test$confirmed_risk)

## visualize:
prediction_OH <- truth_OH %>% mutate(confirmed_risk = pr)
prediction_clust <- counties_OH %>% left_join(prediction_OH)
svm_map <- ggplot(prediction_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = confirmed_risk)) +
  coord_quickmap() +
  scale_fill_manual(values = c("#4ceb34", "#e7ed26", "#ed8d26", "#ed3726")) +
  labs(title = "SVM", fill = "Confirmed risk")


# Random Forest
pr <- predict(randomForestFit, OH_predict)
confusionMatrix(pr, reference = OH_test$confirmed_risk)

## visualize:
prediction_OH <- truth_OH %>% mutate(confirmed_risk = pr)
prediction_clust <- counties_OH %>% left_join(prediction_OH)
RandomForest_map <- ggplot(prediction_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = confirmed_risk)) +
  coord_quickmap() +
  scale_fill_manual(values = c("#4ceb34", "#e7ed26", "#ed8d26", "#ed3726")) +
  labs(title = "Random Forest", fill = "Confirmed risk")


# Neural Network 
pr <- predict(nnetFit, OH_predict)
confusionMatrix(pr, reference = OH_test$confirmed_risk)

## visualize:
prediction_OH <- truth_OH %>% mutate(confirmed_risk = pr)
prediction_clust <- counties_OH %>% left_join(prediction_OH)
neuralNetwork_map <- ggplot(prediction_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = confirmed_risk)) +
  coord_quickmap() +
  scale_fill_manual(values = c("#4ceb34", "#e7ed26", "#ed8d26", "#ed3726")) +
  labs(title = "Neural Network", fill = "Confirmed risk")

truth_map
cowplot::plot_grid(decisionTree_map, svm_map, RandomForest_map, neuralNetwork_map, nrow = 2, ncol = 2)

## Step II-05: Compare the decision boundaries ---------------------------------

library(scales)


decisionplot <- function(model, data, class_var, 
                         predict_type = c("class", "prob"), resolution = 3 * 72) {
  # resolution is set to 72 dpi if the image is rendered  3 inches wide. 
  
  y <- data %>% pull(class_var)
  x <- data %>% dplyr::select(-all_of(class_var))
  
  # resubstitution accuracy
  prediction <- predict(model, x, type = predict_type[1])
  # LDA returns a list
  if(is.list(prediction)) prediction <- prediction$class
  prediction <- factor(prediction, levels = levels(y))
  
  cm <- confusionMatrix(data = prediction, reference = y)
  acc <- cm$overall["Accuracy"]
  
  # evaluate model on a grid
  r <- sapply(x[, 1:2], range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each = resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as_tibble(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  cl <- predict(model, g, type = predict_type[1])
  
  # LDA returns a list
  prob <- NULL
  if(is.list(cl)) { 
    prob <- cl$posterior
    cl <- cl$class
  } else
    if(!is.na(predict_type[2]))
      try(prob <- predict(model, g, type = predict_type[2]))
  
  # we visualize the difference in probability/score between the 
  # winning class and the second best class.
  # don't use probability if predict for the classifier does not support it.
  max_prob <- 1
  if(!is.null(prob))
    try({
      max_prob <- t(apply(prob, MARGIN = 1, sort, decreasing = TRUE))
      max_prob <- max_prob[,1] - max_prob[,2]
    }, silent = TRUE) 
  
  cl <- factor(cl, levels = levels(y))
  
  g <- g %>% add_column(prediction = cl, probability = max_prob)
  
  ggplot(g, mapping = aes(
    x = .data[[colnames(g)[1]]], y = .data[[colnames(g)[2]]])) +
    geom_raster(mapping = aes(fill = prediction, alpha = probability)) +
    geom_contour(mapping = aes(z = as.numeric(prediction)), 
                 bins = length(levels(cl)), size = .5, color = "black") +
    geom_point(data = data, mapping =  aes(
      x = .data[[colnames(data)[1]]], 
      y = .data[[colnames(data)[2]]],
      shape = .data[[class_var]]), alpha = .7) + 
    scale_alpha_continuous(range = c(0,1), limits = c(0,1), guide = "none") +  
    labs(subtitle = paste("Training accuracy:", round(acc, 2)))
}


# PART III: Exceptional Work 'deaths' -----------------------------------------
# THIS PART AHS THE SAME ANALYSIS LIKE PART II ONLY THE CLASS VARIBAE IS CHANGED
# TO: "death_risk"

## Step III-01: choosing classification variables ------------------------------


# extract the train set
cases_train <- d_classed %>% filter(state %in% c("PA", "IL", "GA", "NC", "MI", "NJ", "VA"))
# check for class balance for the filtered dataset
cases_train %>% pull(death_risk) %>% table()

# extract the test set
cases_test <-  d_classed %>% filter(!(state %in% c("PA", "IL", "GA", "NC", "MI", "NJ", "VA")))
# check for class balance for the filtered dataset
cases_test %>% pull(death_risk) %>% table()

# see attribute importance
cases_train %>%  chi.squared(death_risk ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head(n=15)

# make a tree based classification
fit <- cases_train %>%
  train(death_risk ~ . - county_name - state,
        data = . ,
        #method = "rpart",
        method = "rf",
        #method = "nb",
        trControl = trainControl(method = "cv", number = 10)
  )
fit
varImp(fit)

#rm(fit)

X_train <- cases_train %>% select(c(county_name,
                                    state,
                                    median_income,
                                    nonfamily_households_P1000,
                                    hispanic_pop_P1000,
                                    median_age,
                                    walked_to_work_P1000,
                                    percent_income_spent_on_rent,
                                    black_pop_P1000,
                                    male_under_40_ratio,
                                    commuters_by_carpool_P1000,
                                    female_under_40_ratio,
                                    asian_pop_P1000,
                                    housing_units_P1000,
                                    income_per_capita,
                                    employed_pop_P1000,
                                    unemployed_pop_P1000,
                                    pop_density_Pkm,
                                    commuters_drove_alone_P1000,
                                    amerindian_pop_P1000,
                                    commuters_by_public_transportation_P1000,
                                    death_risk))


## Step III-02: Selecting the Models to Train/ deaths --------------------------

# K-Folds Validation: 
train_index <- createFolds(X_train$death_risk, k = 10)

# Hyperparameter tuning for cTree
mincriterion <- seq(0.001, 0.01, by=0.0001)
cTree_tuneGrid = as.data.frame(mincriterion)

# Model 1: Decision Tree
ctreeFit_d <- X_train %>% train(death_risk ~ .- county_name - state,
                              method = "ctree",
                              data = .,
                              tuneLength = 5,
                              tuneGrid = cTree_tuneGrid,
                              trControl = trainControl(method = "cv", indexOut = train_index))
ctreeFit_d

# Hyperparameter tuning for SVM
C <- seq(0.01, 0.1, by=0.01)
svm_tuneGrid = as.data.frame(C)

# Model 2: Support Vector Machine
svmFit_d <- X_train %>% train(death_risk ~.- county_name - state,
                            method = "svmLinear",
                            data = .,
                            tuneLength = 5,
                            tuneGrid = svm_tuneGrid,
                            trControl = trainControl(method = "cv", indexOut = train_index))
svmFit_d

# Hyperparameter tuning for Random Forest
mtry <- seq(0.01, 0.1, by=0.01)
rf_tuneGrid = as.data.frame(mtry)

# Model 3: Random Forest
randomForestFit_d <- X_train %>% train(death_risk ~ .- county_name - state,
                                     method = "rf",
                                     data = .,
                                     tuneLength = 5,
                                     tuneGrid = rf_tuneGrid,
                                     trControl = trainControl(method = "cv", indexOut = train_index))
randomForestFit_d

# Hyperparameter tuning for Neural Network
grid <- expand.grid(
  size = as.numeric(c(5, 10, 20, 30)),
  decay = as.numeric(c(0.1, 0.01, 0.001))
)

# Model 4: Neural Network
nnetFit_d <- X_train %>% train(death_risk ~ .- county_name - state,
                             method = "nnet",
                             data = .,
                             tuneLength = 5,
                             tuneGrid = grid,
                             trControl = trainControl(method = "cv", indexOut = train_index),
                             maxit = 500,
                             trace = FALSE)
nnetFit_d


## Step III-03: Compare the models ---------------------------------------------

resamps <- resamples(list(
  ctree_d = ctreeFit_d,
  SVM_d = svmFit_d,
  randomForest_d = randomForestFit_d,
  NeuralNet_d = nnetFit_d
))
resamps
summary(resamps)

library(lattice)
bwplot(resamps, layout = c(3, 1))

difs <- diff(resamps)
difs
summary(difs)

## Step III-04: Testing each model to predict Ohio -----------------------------

### added visualization part 
counties <- as_tibble(map_data("county"))
counties_OH <- counties %>% dplyr::filter(region == "ohio") %>% 
  rename(c(county = subregion))

rm(counties)

#We could just use cases_test here, just make sure you isolate "OH"
# get ohio counties aside
OH_test <- d_classed %>% filter((state %in% c("OH")))

# before doing any prediction create the ground truth cluster visualization 
truth_OH <- OH_test %>% mutate(county = county_name %>% 
                                 str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
truth_clust <- counties_OH %>% left_join(truth_OH)
truth_d_map <- ggplot(truth_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = death_risk)) +
  coord_quickmap() +
  scale_fill_manual(values = c("#4ceb34", "#e7ed26", "#ed8d26", "#ed3726")) +
  labs(title = "Truth", fill = "death risk")

# now create the prediction = remove the class variable
OH_predict <- subset(OH_test, select = -c(death_risk) )

# Decision Tree:
pr <- predict(ctreeFit_d, OH_predict)
confusionMatrix(pr, reference = OH_test$death_risk)

## visualize:
prediction_OH <- truth_OH %>% mutate(death_risk = pr)
prediction_clust <- counties_OH %>% left_join(prediction_OH)
decisionTree_d_map <- ggplot(prediction_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = death_risk)) +
  coord_quickmap() +
  scale_fill_manual(values = c("#4ceb34", "#e7ed26", "#ed8d26", "#ed3726")) +
  labs(title = "Decision Tree", fill = "death risk")


# Support Vector Machine
pr <- predict(svmFit_d, OH_predict)
confusionMatrix(pr, reference = OH_test$death_risk)

## visualize:
prediction_OH <- truth_OH %>% mutate(death_risk = pr)
prediction_clust <- counties_OH %>% left_join(prediction_OH)
svm_d_map <- ggplot(prediction_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = death_risk)) +
  coord_quickmap() +
  scale_fill_manual(values = c("#4ceb34", "#e7ed26", "#ed8d26", "#ed3726")) +
  labs(title = "SVM", fill = "death risk")


# Random Forest
pr <- predict(randomForestFit_d, OH_predict)
confusionMatrix(pr, reference = OH_test$death_risk)

## visualize:
prediction_OH <- truth_OH %>% mutate(death_risk = pr)
prediction_clust <- counties_OH %>% left_join(prediction_OH)
RandomForest_d_map <- ggplot(prediction_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = death_risk)) +
  coord_quickmap() +
  scale_fill_manual(values = c("#4ceb34", "#e7ed26", "#ed8d26", "#ed3726")) +
  labs(title = "Random Forest", fill = "death risk")


# Neural Network 
pr <- predict(nnetFit_d, OH_predict)
confusionMatrix(pr, reference = OH_test$death_risk)

## visualize:
prediction_OH <- truth_OH %>% mutate(death_risk = pr)
prediction_clust <- counties_OH %>% left_join(prediction_OH)
neuralNetwork_d_map <- ggplot(prediction_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = death_risk)) +
  coord_quickmap() +
  scale_fill_manual(values = c("#4ceb34", "#e7ed26", "#ed8d26", "#ed3726")) +
  labs(title = "Neural Network", fill = "death risk")

truth_d_map
cowplot::plot_grid(decisionTree_d_map, svm_d_map, RandomForest_d_map, neuralNetwork_d_map, nrow = 2, ncol = 2)
