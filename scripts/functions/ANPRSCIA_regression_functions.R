#Insert models here 
#model 1: PC1-10-Treatment unit 
#model 2: PC1-10 + treatment unit + age
#model 3: PC1-10 + treatment unit + age + sex 
#model 4: PC1-10 + treatment unit + age + sex + follow up time 
#model 5: PC1-10 + treatment unit + age + sex + follow up time + BMI 
#model 6: PC1-10 + treatment unit + age + sex + follow up time + age at first ed symptom


model1.estimates <- function(outcome, explanatory_variables, data_set, population) {
  # define the variable name of the dependent variable
  dependent.variable <- outcome
  # explanatory variable
  explanatory.variable <- explanatory_variables
  # covariates
  covariates <- c(
    "PC1",
    "PC2",
    "PC3",
    "PC4",
    "PC5",
    "PC6",
    "PC7",
    "PC8",
    "PC9",
    "PC10",
    "treatment.unit.t1"
  )
  # define the independent variables
  independent.variables <- c(
    explanatory.variable,
    covariates
  )
  # create the formula
  glm.formula <- as.formula(paste(dependent.variable, paste(independent.variables, collapse=" + "), sep=" ~ "))
  glm.formula
  # run the glm
  assign(paste0(outcome,".model1"),
         lm( formula = glm.formula,
             data = get(data_set)
         )
  )
  assign(paste0(outcome,"model1_estimates"),
         tidy(get(paste0(outcome,".model1")), conf.int = TRUE) %>%
           add_column(Model = "Model1", .before = "term") %>%
           add_column(Sample = population, .before = "Model") %>%
           mutate(
             "Indepent variable" = recode_factor(
               term,
               "(Intercept)" = "Intercept",
               "treatment.unit.t1Östergötland" = "Östergötland",
               "treatment.unit.t1Norrland" = "Norrland",
               "treatment.unit.t1Västra Götaland" = "Västra Götaland",
               "treatment.unit.t1Stockholm" = "Stockholm"
             ),
             "Estimate" = format(
               round(estimate,
                     digits = 2),
               nsmall = 2),
             "95% CI low" = format(
               round(conf.low,
                     digits = 2),
               nsmall = 2),
             "95% CI up" = format(
               round(conf.high,
                     digits = 2),
               nsmall = 2),
             "SE" = format(
               round(std.error,
                     digits = 2),
               nsmall = 2),
             "z score" = format(
               round(statistic,
                     digits = 2),
               nsmall = 2),
             "p value" = case_when(
               p.value < 0.001 ~ formatC(p.value, format = "e", digits = 2),
               p.value >= 0.001 & p.value < 0.01 ~ formatC(p.value, format = "f", digits = 3),
               p.value >= 0.01 ~ formatC(p.value, format = "f", digits = 2)
             )
           ) %>%
           select(
             "Sample",
             "Indepent variable",
             "Model",
             "Estimate",
             "95% CI low",
             "95% CI up",
             "SE",
             #    "z score",
             "p value"
           )
  )
  return(get(paste0(outcome,"model1_estimates")))
}


model1.reg.stats <- function(outcome, explanatory_variables, data_set, population) {
  # define the variable name of the dependent variable
  dependent.variable <- outcome
  # explanatory variable
  explanatory.variable <- explanatory_variables
  # covariates
  covariates <- c(
    "PC1",
    "PC2",
    "PC3",
    "PC4",
    "PC5",
    "PC6",
    "PC7",
    "PC8",
    "PC9",
    "PC10",
    "treatment.unit.t1"
  )
  # define the independent variables
  independent.variables <- c(
    explanatory.variable,
    covariates
  )
  # create the formula
  glm.formula <- as.formula(paste(dependent.variable, paste(independent.variables, collapse=" + "), sep=" ~ "))
  glm.formula
  # run the glm
  assign(paste0(outcome,".model1"),
         lm( formula = glm.formula,
             data = get(data_set)
         )
  )
  assign(paste0(outcome,"model1_reg_stats"),
         glance(get(paste0(outcome,".model1"))) %>%
           add_column(Model = "Model1", .before = "nobs") %>%
           add_column(Sample = population, .before = "Model") %>%
           mutate(
             "n" = format(
               round(nobs,
                     digits = 0),
               nsmall = 0),
             "R2" = format(
               round(r.squared,
                     digits = 3),
               nsmall = 2),
             "adjust. R2" = format(
               round(adj.r.squared,
                     digits = 3),
               nsmall = 2),
             "F" = format(
               round(statistic,
                     digits = 2),
               nsmall = 2),
             "p value" = case_when(
               p.value < 0.001 ~ formatC(p.value, format = "e", digits = 2),
               p.value >= 0.001 & p.value < 0.01 ~ formatC(p.value, format = "f", digits = 3),
               p.value >= 0.01 ~ formatC(p.value, format = "f", digits = 2)
             )
           ) %>%
           select(
             "Sample",
             "Model",
             "n",
             "R2",
             "adjust. R2",
             "F",
             "p value",
             "df"
           )
  )
  return(get(paste0(outcome,"model1_reg_stats")))
}



################# Model 2
#model 2: PC1-10 + treatment unit + age

model2.estimates <- function(outcome, explanatory_variables, data_set, population) {
  # define the variable name of the dependent variable
  dependent.variable <- outcome
  # explanatory variable
  explanatory.variable <- explanatory_variables
  # covariates
  covariates <- c(
    "PC1",
    "PC2",
    "PC3",
    "PC4",
    "PC5",
    "PC6",
    "PC7",
    "PC8",
    "PC9",
    "PC10",
    "treatment.unit.t1",
    "age.t1"
  )
  # define the independent variables
  independent.variables <- c(
    explanatory.variable,
    covariates
  )
  # create the formula
  glm.formula <- as.formula(paste(dependent.variable, paste(independent.variables, collapse=" + "), sep=" ~ "))
  glm.formula
  # run the glm
  assign(paste0(outcome,".model2"),
         lm( formula = glm.formula,
             data = get(data_set)
         )
  )
  assign(paste0(outcome,"model2_estimates"),
         tidy(get(paste0(outcome,".model2")), conf.int = TRUE) %>%
           add_column(Model = "Model2", .before = "term") %>%
           add_column(Sample = population, .before = "Model") %>%
           mutate(
             "Indepent variable" = recode_factor(
               term,
               "(Intercept)" = "Intercept",
               "treatment.unit.t1Östergötland" = "Östergötland",
               "treatment.unit.t1Norrland" = "Norrland",
               "treatment.unit.t1Västra Götaland" = "Västra Götaland",
               "treatment.unit.t1Stockholm" = "Stockholm"
             ),
             "Estimate" = format(
               round(estimate,
                     digits = 2),
               nsmall = 2),
             "95% CI low" = format(
               round(conf.low,
                     digits = 2),
               nsmall = 2),
             "95% CI up" = format(
               round(conf.high,
                     digits = 2),
               nsmall = 2),
             "SE" = format(
               round(std.error,
                     digits = 2),
               nsmall = 2),
             "z score" = format(
               round(statistic,
                     digits = 2),
               nsmall = 2),
             "p value" = case_when(
               p.value < 0.001 ~ formatC(p.value, format = "e", digits = 2),
               p.value >= 0.001 & p.value < 0.01 ~ formatC(p.value, format = "f", digits = 3),
               p.value >= 0.01 ~ formatC(p.value, format = "f", digits = 2)
             )
           ) %>%
           select(
             "Sample",
             "Indepent variable",
             "Model",
             "Estimate",
             "95% CI low",
             "95% CI up",
             "SE",
             #    "z score",
             "p value"
           )
  )
  return(get(paste0(outcome,"model2_estimates")))
}
model2.reg.stats <- function(outcome, explanatory_variables, data_set, population) {
  # define the variable name of the dependent variable
  dependent.variable <- outcome
  # explanatory variable
  explanatory.variable <- explanatory_variables
  # covariates
  covariates <- c(
    "PC1",
    "PC2",
    "PC3",
    "PC4",
    "PC5",
    "PC6",
    "PC7",
    "PC8",
    "PC9",
    "PC10",
    "treatment.unit.t1",
    "age.t1"
  )
  # define the independent variables
  independent.variables <- c(
    explanatory.variable,
    covariates
  )
  # create the formula
  glm.formula <- as.formula(paste(dependent.variable, paste(independent.variables, collapse=" + "), sep=" ~ "))
  glm.formula
  # run the glm
  assign(paste0(outcome,".model2"),
         lm( formula = glm.formula,
             data = get(data_set)
         )
  )
  assign(paste0(outcome,"model2_reg_stats"),
         glance(get(paste0(outcome,".model2"))) %>%
           add_column(Model = "Model2", .before = "nobs") %>%
           add_column(Sample = population, .before = "Model") %>%
           mutate(
             "n" = format(
               round(nobs,
                     digits = 0),
               nsmall = 0),
             "R2" = format(
               round(r.squared,
                     digits = 3),
               nsmall = 2),
             "adjust. R2" = format(
               round(adj.r.squared,
                     digits = 3),
               nsmall = 2),
             "F" = format(
               round(statistic,
                     digits = 2),
               nsmall = 2),
             "p value" = case_when(
               p.value < 0.001 ~ formatC(p.value, format = "e", digits = 2),
               p.value >= 0.001 & p.value < 0.01 ~ formatC(p.value, format = "f", digits = 3),
               p.value >= 0.01 ~ formatC(p.value, format = "f", digits = 2)
             )
           ) %>%
           select(
             "Sample",
             "Model",
             "n",
             "R2",
             "adjust. R2",
             "F",
             "p value",
             "df"
           )
  )
  return(get(paste0(outcome,"model2_reg_stats")))
}





##################### Model 3
#model 3: PC1-10 + treatment unit + age + sex 

model3.estimates <- function(outcome, explanatory_variables, data_set, population) {
  # define the variable name of the dependent variable
  dependent.variable <- outcome
  # explanatory variable
  explanatory.variable <- explanatory_variables
  # covariates
  covariates <- c(
    "PC1",
    "PC2",
    "PC3",
    "PC4",
    "PC5",
    "PC6",
    "PC7",
    "PC8",
    "PC9",
    "PC10",
    "treatment.unit.t1",
    "age.t1",
    "sex"
  )
  # define the independent variables
  independent.variables <- c(
    explanatory.variable,
    covariates
  )
  # create the formula
  glm.formula <- as.formula(paste(dependent.variable, paste(independent.variables, collapse=" + "), sep=" ~ "))
  glm.formula
  # run the glm
  assign(paste0(outcome,".model3"),
         lm( formula = glm.formula,
             data = get(data_set)
         )
  )
  assign(paste0(outcome,"model3_estimates"),
         tidy(get(paste0(outcome,".model3")), conf.int = TRUE) %>%
           add_column(Model = "Model3", .before = "term") %>%
           add_column(Sample = population, .before = "Model") %>%
           mutate(
             "Indepent variable" = recode_factor(
               term,
               "(Intercept)" = "Intercept",
               "treatment.unit.t1Östergötland" = "Östergötland",
               "treatment.unit.t1Norrland" = "Norrland",
               "treatment.unit.t1Västra Götaland" = "Västra Götaland",
               "treatment.unit.t1Stockholm" = "Stockholm"
             ),
             "Estimate" = format(
               round(estimate,
                     digits = 2),
               nsmall = 2),
             "95% CI low" = format(
               round(conf.low,
                     digits = 2),
               nsmall = 2),
             "95% CI up" = format(
               round(conf.high,
                     digits = 2),
               nsmall = 2),
             "SE" = format(
               round(std.error,
                     digits = 2),
               nsmall = 2),
             "z score" = format(
               round(statistic,
                     digits = 2),
               nsmall = 2),
             "p value" = case_when(
               p.value < 0.001 ~ formatC(p.value, format = "e", digits = 2),
               p.value >= 0.001 & p.value < 0.01 ~ formatC(p.value, format = "f", digits = 3),
               p.value >= 0.01 ~ formatC(p.value, format = "f", digits = 2)
             )
           ) %>%
           select(
             "Sample",
             "Indepent variable",
             "Model",
             "Estimate",
             "95% CI low",
             "95% CI up",
             "SE",
             #    "z score",
             "p value"
           )
  )
  return(get(paste0(outcome,"model3_estimates")))
}
model3.reg.stats <- function(outcome, explanatory_variables, data_set, population) {
  # define the variable name of the dependent variable
  dependent.variable <- outcome
  # explanatory variable
  explanatory.variable <- explanatory_variables
  # covariates
  covariates <- c(
    "PC1",
    "PC2",
    "PC3",
    "PC4",
    "PC5",
    "PC6",
    "PC7",
    "PC8",
    "PC9",
    "PC10",
    "treatment.unit.t1",
    "age.t1",
    "sex"
  )
  # define the independent variables
  independent.variables <- c(
    explanatory.variable,
    covariates
  )
  # create the formula
  glm.formula <- as.formula(paste(dependent.variable, paste(independent.variables, collapse=" + "), sep=" ~ "))
  glm.formula
  # run the glm
  assign(paste0(outcome,".model3"),
         lm( formula = glm.formula,
             data = get(data_set)
         )
  )
  assign(paste0(outcome,"model3_reg_stats"),
         glance(get(paste0(outcome,".model3"))) %>%
           add_column(Model = "Model3", .before = "nobs") %>%
           add_column(Sample = population, .before = "Model") %>%
           mutate(
             "n" = format(
               round(nobs,
                     digits = 0),
               nsmall = 0),
             "R2" = format(
               round(r.squared,
                     digits = 3),
               nsmall = 2),
             "adjust. R2" = format(
               round(adj.r.squared,
                     digits = 3),
               nsmall = 2),
             "F" = format(
               round(statistic,
                     digits = 2),
               nsmall = 2),
             "p value" = case_when(
               p.value < 0.001 ~ formatC(p.value, format = "e", digits = 2),
               p.value >= 0.001 & p.value < 0.01 ~ formatC(p.value, format = "f", digits = 3),
               p.value >= 0.01 ~ formatC(p.value, format = "f", digits = 2)
             )
           ) %>%
           select(
             "Sample",
             "Model",
             "n",
             "R2",
             "adjust. R2",
             "F",
             "p value",
             "df"
           )
  )
  return(get(paste0(outcome,"model3_reg_stats")))
}


###################### Model4
#model 4: PC1-10 + treatment unit + age + sex + follow up time 


model4.estimates <- function(outcome, explanatory_variables, data_set, population) {
  # define the variable name of the dependent variable
  dependent.variable <- outcome
  # explanatory variable
  explanatory.variable <- explanatory_variables
  # covariates
  covariates <- c(
    "PC1",
    "PC2",
    "PC3",
    "PC4",
    "PC5",
    "PC6",
    "PC7",
    "PC8",
    "PC9",
    "PC10",
    "treatment.unit.t1",
    "age.t1",
    "sex",
    "duration.1st.reg.until.ANGI"
  )
  # define the independent variables
  independent.variables <- c(
    explanatory.variable,
    covariates
  )
  # create the formula
  glm.formula <- as.formula(paste(dependent.variable, paste(independent.variables, collapse=" + "), sep=" ~ "))
  glm.formula
  # run the glm
  assign(paste0(outcome,".model4"),
         lm( formula = glm.formula,
             data = get(data_set)
         )
  )
  assign(paste0(outcome,"model4_estimates"),
         tidy(get(paste0(outcome,".model4")), conf.int = TRUE) %>%
           add_column(Model = "Model4", .before = "term") %>%
           add_column(Sample = population, .before = "Model") %>%
           mutate(
             "Indepent variable" = recode_factor(
               term,
               "(Intercept)" = "Intercept",
               "treatment.unit.t1Östergötland" = "Östergötland",
               "treatment.unit.t1Norrland" = "Norrland",
               "treatment.unit.t1Västra Götaland" = "Västra Götaland",
               "treatment.unit.t1Stockholm" = "Stockholm"
             ),
             "Estimate" = format(
               round(estimate,
                     digits = 2),
               nsmall = 2),
             "95% CI low" = format(
               round(conf.low,
                     digits = 2),
               nsmall = 2),
             "95% CI up" = format(
               round(conf.high,
                     digits = 2),
               nsmall = 2),
             "SE" = format(
               round(std.error,
                     digits = 2),
               nsmall = 2),
             "z score" = format(
               round(statistic,
                     digits = 2),
               nsmall = 2),
             "p value" = case_when(
               p.value < 0.001 ~ formatC(p.value, format = "e", digits = 2),
               p.value >= 0.001 & p.value < 0.01 ~ formatC(p.value, format = "f", digits = 3),
               p.value >= 0.01 ~ formatC(p.value, format = "f", digits = 2)
             )
           ) %>%
           select(
             "Sample",
             "Indepent variable",
             "Model",
             "Estimate",
             "95% CI low",
             "95% CI up",
             "SE",
             #    "z score",
             "p value"
           )
  )
  return(get(paste0(outcome,"model4_estimates")))
}
model4.reg.stats <- function(outcome, explanatory_variables, data_set, population) {
  # define the variable name of the dependent variable
  dependent.variable <- outcome
  # explanatory variable
  explanatory.variable <- explanatory_variables
  # covariates
  covariates <- c(
    "PC1",
    "PC2",
    "PC3",
    "PC4",
    "PC5",
    "PC6",
    "PC7",
    "PC8",
    "PC9",
    "PC10",
    "treatment.unit.t1",
    "age.t1",
    "sex",
    "duration.1st.reg.until.ANGI"
  )
  # define the independent variables
  independent.variables <- c(
    explanatory.variable,
    covariates
  )
  # create the formula
  glm.formula <- as.formula(paste(dependent.variable, paste(independent.variables, collapse=" + "), sep=" ~ "))
  glm.formula
  # run the glm
  assign(paste0(outcome,".model4"),
         lm( formula = glm.formula,
             data = get(data_set)
         )
  )
  assign(paste0(outcome,"model4_reg_stats"),
         glance(get(paste0(outcome,".model4"))) %>%
           add_column(Model = "Model4", .before = "nobs") %>%
           add_column(Sample = population, .before = "Model") %>%
           mutate(
             "n" = format(
               round(nobs,
                     digits = 0),
               nsmall = 0),
             "R2" = format(
               round(r.squared,
                     digits = 3),
               nsmall = 2),
             "adjust. R2" = format(
               round(adj.r.squared,
                     digits = 3),
               nsmall = 2),
             "F" = format(
               round(statistic,
                     digits = 2),
               nsmall = 2),
             "p value" = case_when(
               p.value < 0.001 ~ formatC(p.value, format = "e", digits = 2),
               p.value >= 0.001 & p.value < 0.01 ~ formatC(p.value, format = "f", digits = 3),
               p.value >= 0.01 ~ formatC(p.value, format = "f", digits = 2)
             )
           ) %>%
           select(
             "Sample",
             "Model",
             "n",
             "R2",
             "adjust. R2",
             "F",
             "p value",
             "df"
           )
  )
  return(get(paste0(outcome,"model4_reg_stats")))
}


###### Model 5
#model 5: PC1-10 + treatment unit + age + sex + follow up time + BMI 


model5.estimates <- function(outcome, explanatory_variables, data_set, population) {
  # define the variable name of the dependent variable
  dependent.variable <- outcome
  # explanatory variable
  explanatory.variable <- explanatory_variables
  # covariates
  covariates <- c(
    "PC1",
    "PC2",
    "PC3",
    "PC4",
    "PC5",
    "PC6",
    "PC7",
    "PC8",
    "PC9",
    "PC10",
    "treatment.unit.t1",
    "age.t1",
    "sex",
    "duration.1st.reg.until.ANGI",
    "bmi.provided.t1"
  )
  # define the independent variables
  independent.variables <- c(
    explanatory.variable,
    covariates
  )
  # create the formula
  glm.formula <- as.formula(paste(dependent.variable, paste(independent.variables, collapse=" + "), sep=" ~ "))
  glm.formula
  # run the glm
  assign(paste0(outcome,".model5"),
         lm( formula = glm.formula,
             data = get(data_set)
         )
  )
  assign(paste0(outcome,"model5_estimates"),
         tidy(get(paste0(outcome,".model5")), conf.int = TRUE) %>%
           add_column(Model = "Model5", .before = "term") %>%
           add_column(Sample = population, .before = "Model") %>%
           mutate(
             "Indepent variable" = recode_factor(
               term,
               "(Intercept)" = "Intercept",
               "treatment.unit.t1Östergötland" = "Östergötland",
               "treatment.unit.t1Norrland" = "Norrland",
               "treatment.unit.t1Västra Götaland" = "Västra Götaland",
               "treatment.unit.t1Stockholm" = "Stockholm"
             ),
             "Estimate" = format(
               round(estimate,
                     digits = 2),
               nsmall = 2),
             "95% CI low" = format(
               round(conf.low,
                     digits = 2),
               nsmall = 2),
             "95% CI up" = format(
               round(conf.high,
                     digits = 2),
               nsmall = 2),
             "SE" = format(
               round(std.error,
                     digits = 2),
               nsmall = 2),
             "z score" = format(
               round(statistic,
                     digits = 2),
               nsmall = 2),
             "p value" = case_when(
               p.value < 0.001 ~ formatC(p.value, format = "e", digits = 2),
               p.value >= 0.001 & p.value < 0.01 ~ formatC(p.value, format = "f", digits = 3),
               p.value >= 0.01 ~ formatC(p.value, format = "f", digits = 2)
             )
           ) %>%
           select(
             "Sample",
             "Indepent variable",
             "Model",
             "Estimate",
             "95% CI low",
             "95% CI up",
             "SE",
             #    "z score",
             "p value"
           )
  )
  return(get(paste0(outcome,"model5_estimates")))
}
model5.reg.stats <- function(outcome, explanatory_variables, data_set, population) {
  # define the variable name of the dependent variable
  dependent.variable <- outcome
  # explanatory variable
  explanatory.variable <- explanatory_variables
  # covariates
  covariates <- c(
    "PC1",
    "PC2",
    "PC3",
    "PC4",
    "PC5",
    "PC6",
    "PC7",
    "PC8",
    "PC9",
    "PC10",
    "treatment.unit.t1",
    "age.t1",
    "sex",
    "duration.1st.reg.until.ANGI",
    "bmi.provided.t1"
  )
  # define the independent variables
  independent.variables <- c(
    explanatory.variable,
    covariates
  )
  # create the formula
  glm.formula <- as.formula(paste(dependent.variable, paste(independent.variables, collapse=" + "), sep=" ~ "))
  glm.formula
  # run the glm
  assign(paste0(outcome,".model5"),
         lm( formula = glm.formula,
             data = get(data_set)
         )
  )
  assign(paste0(outcome,"model5_reg_stats"),
         glance(get(paste0(outcome,".model5"))) %>%
           add_column(Model = "Model5", .before = "nobs") %>%
           add_column(Sample = population, .before = "Model") %>%
           mutate(
             "n" = format(
               round(nobs,
                     digits = 0),
               nsmall = 0),
             "R2" = format(
               round(r.squared,
                     digits = 3),
               nsmall = 2),
             "adjust. R2" = format(
               round(adj.r.squared,
                     digits = 3),
               nsmall = 2),
             "F" = format(
               round(statistic,
                     digits = 2),
               nsmall = 2),
             "p value" = case_when(
               p.value < 0.001 ~ formatC(p.value, format = "e", digits = 2),
               p.value >= 0.001 & p.value < 0.01 ~ formatC(p.value, format = "f", digits = 3),
               p.value >= 0.01 ~ formatC(p.value, format = "f", digits = 2)
             )
           ) %>%
           select(
             "Sample",
             "Model",
             "n",
             "R2",
             "adjust. R2",
             "F",
             "p value",
             "df"
           )
  )
  return(get(paste0(outcome,"model5_reg_stats")))
}


############ Model 6
#model 6: PC1-10 + treatment unit + age + sex + follow up time + age at first ed symptom

model6.estimates <- function(outcome, explanatory_variables, data_set, population) {
  # define the variable name of the dependent variable
  dependent.variable <- outcome
  # explanatory variable
  explanatory.variable <- explanatory_variables
  # covariates
  covariates <- c(
    "PC1",
    "PC2",
    "PC3",
    "PC4",
    "PC5",
    "PC6",
    "PC7",
    "PC8",
    "PC9",
    "PC10",
    "treatment.unit.t1",
    "age.t1",
    "sex",
    "duration.1st.reg.until.ANGI",
    "bmi.provided.t1",
    "age.first.ED.symptom.t1"
  )
  # define the independent variables
  independent.variables <- c(
    explanatory.variable,
    covariates
  )
  # create the formula
  glm.formula <- as.formula(paste(dependent.variable, paste(independent.variables, collapse=" + "), sep=" ~ "))
  glm.formula
  # run the glm
  assign(paste0(outcome,".model6"),
         lm( formula = glm.formula,
             data = get(data_set)
         )
  )
  assign(paste0(outcome,"model6_estimates"),
         tidy(get(paste0(outcome,".model6")), conf.int = TRUE) %>%
           add_column(Model = "Model6", .before = "term") %>%
           add_column(Sample = population, .before = "Model") %>%
           mutate(
             "Indepent variable" = recode_factor(
               term,
               "(Intercept)" = "Intercept",
               "treatment.unit.t1Östergötland" = "Östergötland",
               "treatment.unit.t1Norrland" = "Norrland",
               "treatment.unit.t1Västra Götaland" = "Västra Götaland",
               "treatment.unit.t1Stockholm" = "Stockholm"
             ),
             "Estimate" = format(
               round(estimate,
                     digits = 2),
               nsmall = 2),
             "95% CI low" = format(
               round(conf.low,
                     digits = 2),
               nsmall = 2),
             "95% CI up" = format(
               round(conf.high,
                     digits = 2),
               nsmall = 2),
             "SE" = format(
               round(std.error,
                     digits = 2),
               nsmall = 2),
             "z score" = format(
               round(statistic,
                     digits = 2),
               nsmall = 2),
             "p value" = case_when(
               p.value < 0.001 ~ formatC(p.value, format = "e", digits = 2),
               p.value >= 0.001 & p.value < 0.01 ~ formatC(p.value, format = "f", digits = 3),
               p.value >= 0.01 ~ formatC(p.value, format = "f", digits = 2)
             )
           ) %>%
           select(
             "Sample",
             "Indepent variable",
             "Model",
             "Estimate",
             "95% CI low",
             "95% CI up",
             "SE",
             #    "z score",
             "p value"
           )
  )
  return(get(paste0(outcome,"model6_estimates")))
}
model6.reg.stats <- function(outcome, explanatory_variables, data_set, population) {
  # define the variable name of the dependent variable
  dependent.variable <- outcome
  # explanatory variable
  explanatory.variable <- explanatory_variables
  # covariates
  covariates <- c(
    "PC1",
    "PC2",
    "PC3",
    "PC4",
    "PC5",
    "PC6",
    "PC7",
    "PC8",
    "PC9",
    "PC10",
    "treatment.unit.t1",
    "age.t1",
    "sex",
    "duration.1st.reg.until.ANGI",
    "bmi.provided.t1",
    "age.first.ED.symptom.t1"
  )
  # define the independent variables
  independent.variables <- c(
    explanatory.variable,
    covariates
  )
  # create the formula
  glm.formula <- as.formula(paste(dependent.variable, paste(independent.variables, collapse=" + "), sep=" ~ "))
  glm.formula
  # run the glm
  assign(paste0(outcome,".model6"),
         lm( formula = glm.formula,
             data = get(data_set)
         )
  )
  assign(paste0(outcome,"model6_reg_stats"),
         glance(get(paste0(outcome,".model6"))) %>%
           add_column(Model = "Model5", .before = "nobs") %>%
           add_column(Sample = population, .before = "Model") %>%
           mutate(
             "n" = format(
               round(nobs,
                     digits = 0),
               nsmall = 0),
             "R2" = format(
               round(r.squared,
                     digits = 3),
               nsmall = 2),
             "adjust. R2" = format(
               round(adj.r.squared,
                     digits = 3),
               nsmall = 2),
             "F" = format(
               round(statistic,
                     digits = 2),
               nsmall = 2),
             "p value" = case_when(
               p.value < 0.001 ~ formatC(p.value, format = "e", digits = 2),
               p.value >= 0.001 & p.value < 0.01 ~ formatC(p.value, format = "f", digits = 3),
               p.value >= 0.01 ~ formatC(p.value, format = "f", digits = 2)
             )
           ) %>%
           select(
             "Sample",
             "Model",
             "n",
             "R2",
             "adjust. R2",
             "F",
             "p value",
             "df"
           )
  )
  return(get(paste0(outcome,"model6_reg_stats")))
}




