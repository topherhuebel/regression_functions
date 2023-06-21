# outcome: Your outcome variable as binary factor (unordered)
# explanatory_variables: A vector with your explanatory variables
# adjustment: A vector with your variables that you want to adjust for
# Population: A character string that names your full sample or a subsample
# name: Name of the model (e.g., Model1, FullModel)

# Appropriate R2 for different regression models
#h ttps://easystats.github.io/performance/reference/r2.html


#########
# Variables for testing
# outcome = "suicide.attempts.last.12.months.t1.recoded.collapsed"
# explanatory_variables = "year.inclusion"
# adjustment = c()
# data_set = "dat"
# population = "Full sample"
# name = "Model1"
# time_point = "t1"
# duration = "cross-sectional"


logistic.regression.statistics <- function(
  outcome,
  explanatory_variables,
  adjustment = c(),
  data_set,
  population = "Full sample",
  name = "Model1",
  regression = "Logistic",
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
  logistic.formula <- as.formula(
    paste(
      dependent.variable,
      paste(independent.variables, collapse=" + "),
      sep=" ~ ")
  )
  
  logistic.formula
  
  # run logistic regression
  assign(
    x = paste0(outcome,".",name),
    value = glm(
      formula = logistic.formula,
      data = get(data_set),
      family = "binomial"
    )
  )
  
  # calcualte Nagelkerke's R2: run and save in object
  R2_object <-
    performance::r2_nagelkerke(
      get(paste0(outcome, ".", name))
    )
  
  R2_object
  
  # tidy output
  assign(
    x = paste0(outcome, name, "_statistics"),
    value = 
      glance(get(paste0(outcome, ".", name))) %>%
           add_column(
             "Sample" = population,
             "Model" = name,
             "Regression" = regression,
             "Duration" = duration,
             "Dependent variable" = dependent.variable,
             "Independent variable" = paste(explanatory_variables, collapse = ", "),
             "Time point" = time_point,
             "R2" = round(R2_object[[1]], digits = 3),
             .before = "nobs") %>%
           mutate(
             "n" = round(nobs,
                     digits = 0)
           ) %>%
           select(
             "Sample",
             "Time point",
             "Duration",
             "Model",
             "Regression",
             "Dependent variable",
             "Independent variable",
             "n",
             "R2",
             "Null deviance" = null.deviance,
             "Deviance" = deviance,
             "logLikelihood" = logLik,
             "AIC",
             "BIC",
             "df Residual" = df.residual
           )
  )
  
  return(get(paste0(outcome, name, "_statistics")))
}

