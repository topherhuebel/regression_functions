# outcome: Your outcome variable as binary factor (unordered)
# explanatory_variables: A vector with your explanatory variables
# adjustment: A vector with your variables that you want to adjust for
# Population: A character string that names your full sample or a subsample
# name: Name of the model (e.g., Model1, FullModel)

# Appropriate R2 for differen regression models
#https://easystats.github.io/performance/reference/r2.html


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


logistic.estimates <- function(
    outcome,
    explanatory_variables,
    adjustment = c(),
    data_set,
    population = "Full sample",
    regression = "Logistic",
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
  
  # tidy output
  assign(
    x = paste0(outcome, name, "_estimates"),
    value = 
      tidy(
        x = get(paste0(outcome, ".", name)),
        exponentiate = TRUE,
        conf.int = TRUE
        ) %>%
      add_column("Model" = name, .before = "term") %>%
      add_column("Sample" = population, .before = "Model") %>%
      add_column("Regression" = regression, .before = "Model") %>%
      add_column("Time point" = time_point, .before = "Model") %>%
      add_column("Duration" = duration, .before = "Model") %>%
      add_column("Dependent variable" = dependent.variable, .before = "Model") %>%
      mutate(
        "Independent variable" = recode_factor(
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
        "Independent variable",
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