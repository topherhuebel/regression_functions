# outcome: Your outcome variable as continuous variable
# explanatory_variables: A vector with your explanatory variables
# adjustment: A vector with your variables that you want to adjust for
# Population: A character string that names your full sample or a subsample
# name: Name of the model (e.g., Model1, FullModel)

# Appropriate R2 for differen regression models
#https://easystats.github.io/performance/reference/r2.html


#########
# Variables for testing
# outcome = "dcq.sum_score"
# explanatory_variables = "gender"
# adjustment = c()
# data_set = "dat"
# population = "Full sample"
# name = "Model1" # should not have white space
# time_point = "t1"
# duration = "cross-sectional"



linear.regression.statistics <- function(
    outcome,
    explanatory_variables,
    adjustment = c(),
    data_set,
    population = "Full sample",
    name = "Model1", # no white space
    regression = "Linear",
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
    x = paste0(outcome, name, "_statistics"),
    value = 
      glance(get(paste0(outcome, ".", name))) %>%
      add_column(
        "Sample" = population,
        "Model" = name,
        "Regression" = regression,
        "Duration" = duration,
        "Dependent variable" = dependent.variable,
        "Independent variable" = explanatory_variables,
        "Time point" = time_point,
    #    "R2" = round(R2_object[[1]], digits = 3),
        .before = "nobs") %>%
      mutate(
        "n" = 
          as.numeric(
            format(
              round(nobs,
                    digits = 0),
              nsmall = 0)
            ),
        "R2" = as.numeric(
          format(
            round(r.squared,
                  digits = 3),
            nsmall = 2)
          ),
        "adjust. R2" =
          as.numeric(
            format(
              round(adj.r.squared,
                    digits = 3),
              nsmall = 2)
            ),
        "F" = as.numeric(
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
        "Independent variable",
        "n",
        "R2",
        "adjust. R2", 
        "sigma",
        "F",
        "p value",
        "df",
        "Deviance" = deviance,
        "logLikelihood" = logLik,
        "AIC",
        "BIC",
        "df Residual" = df.residual
        )
    )
  
  return(get(paste0(outcome, name, "_statistics")))
  }


