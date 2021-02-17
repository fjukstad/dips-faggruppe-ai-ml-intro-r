library(tidyverse)

data = read_csv("data/diabetes.csv")

summary(data)

dim(data)


data = data %>% mutate(Outcome = as.factor(Outcome)) 

ggplot(data, aes(x = SkinThickness, fill=Outcome)) + 
  geom_bar() + 
  facet_grid(Outcome ~ .)

data = data %>% 
  mutate(SkinThickness = case_when(SkinThickness == 0 ~ NA_real_,
                   TRUE ~ SkinThickness))

data = data %>% drop_na()


ggplot(data, aes(x = SkinThickness, fill=Outcome)) + 
  geom_bar() + 
  facet_grid(Outcome ~ .)

View(cor(data %>% select(-Outcome)))

library(tidymodels)

data_split = initial_split(data, strata = "Outcome")

testing = testing(data_split)
training = training(data_split)

log_reg = logistic_reg() %>% 
          set_engine("glm") %>% 
          set_mode("classification") %>% 
          fit(Outcome ~ Glucose + BMI + DiabetesPedigreeFunction, data=training)

predictions = log_reg %>% 
              predict(testing, type="class") %>% 
              bind_cols(testing)

predictions %>% 
  metrics(truth = Outcome, estimate = .pred_class)

conf_mat(predictions, truth = Outcome, estimate = .pred_class)

our_metrics = metric_set(accuracy, spec, sens)

predictions %>% 
  our_metrics(truth = Outcome, estimate = .pred_class)


