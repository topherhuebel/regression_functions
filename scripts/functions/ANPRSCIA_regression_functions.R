#Insert models here 
#model 1: PC1-10-Treatment unit 
#model 2: PC1-10 + treatment unit + age
#model 3: PC1-10 + treatment unit + age + sex 
#model 4: PC1-10 + treatment unit + age + sex + follow up time 
#model 5: PC1-10 + treatment unit + age + sex + follow up time + BMI 
#model 6: PC1-10 + treatment unit + age + sex + follow up time + age at first ed symptom


# outcome: Your outcome variable as continuous variable
# explanatory_variables: A vector with your explanatory variables
# adjustment: A vector with your variables that you want to adjust for
# Population: A character string that names your full sample or a subsample
# name: Name of the model (e.g., Model1, FullModel)

# Appropriate R2 for differen regression models
#https://easystats.github.io/performance/reference/r2.html


#########
# Variables for testing
 outcome = "dcq.sum_score"
 explanatory_variables = "gender"
 adjustment = c()
 data_set = "dat"
 population = "Full sample"
 name = "Model1" # should not have white space
 time_point = "t1"
 duration = "cross-sectional"


linear.estimates <- function(
    outcome,
    explanatory_variables,
    data_set,
    population,
    regression = "Linear",
    name = "Model1",
    time_point = "t1",
    duration = "cross-sectional"
    )
  {
  # define the variable name of the dependent variable
  dependent.variable <- outcome
  
  # explanatory variable
  explanatory.variable <- explanatory_variables
  
  # covariates
  covariates <- adjustment
  
  # define the independent variables
  independent.variables <- c(
    explanatory.variable,
    covariates
  )
  
  # create the formula
  glm.formula <- as.formula(
    paste(
      dependent.variable,
      paste(independent.variables, collapse=" + "),
      sep=" ~ ")
    )
  
  glm.formula
  
  # run the glm
  assign(
    x = paste0(outcome, ".", name),
    value = lm(
      formula = glm.formula,
      data = get(data_set)
      )
    )
  
  # tidy output
  assign(
    x = paste0(outcome, name, "_estimates"),
    value =
      tidy(
        x = get(paste0(outcome, "." , name)),
        conf.int = TRUE
        ) %>%
      add_column("Model" = name, .before = "term") %>%
      add_column("Sample" = population, .before = "Model") %>%
      add_column("Regression" = regression, .before = "Model") %>%
      add_column("Time point" = time_point, .before = "Model") %>%
      add_column("Duration" = duration, .before = "Model") %>%
      add_column("Dependent variable" = dependent.variable, .before = "Model") %>%
      mutate(
        "Indepent variable" = recode_factor(
          term,
          "(Intercept)" = "Intercept",
          ),
        "Estimate" = 
          as.numeric(
            format(
              round(estimate,
                    digits = 2),
              nsmall = 2)
            ),
        "95% CI low" =
          as.numeric(
            format(
              round(conf.low,
                    digits = 2),
              nsmall = 2)
            ),
        "95% CI up" =
          as.numeric(
            format(
              round(conf.high,
                    digits = 2),
              nsmall = 2)
            ),
        "SE" =
          as.numeric(
            format(
              round(std.error,
                    digits = 2),
              nsmall = 2)
            ),
        "z score" =
          as.numeric(
            format(
              round(statistic,
                    digits = 2),
              nsmall = 2)
            ),
        "p value" = case_when(
          p.value < 0.001 ~ formatC(p.value, format = "e", digits = 2),
          p.value >= 0.001 & p.value < 0.01 ~ formatC(p.value, format = "f", digits = 3),
          p.value >= 0.01 ~ formatC(p.value, format = "f", digits = 2)
          )
        ) %>%
      select(
        "Sample",
        "Time point",
        "Duration",
        "Model",
        "Regression",
        "Dependent variable",
        "Indepent variable",
        "Estimate",
        "95% CI low",
        "95% CI up",
        "SE",
        "z score",
        "p value"
        )
    )
  
  return(get(paste0(outcome, name, "_estimates")))
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




