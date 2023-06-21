# outcome: Your outcome variable as categorical factor (unordered)
# explanatory_variables: A vector with your explanatory variables
# adjustment: A vector with your variables that you want to adjust for
# Population: A character string that names your full sample or a subsample
# name: Name of the model (e.g., Model1, FullModel)

# Appropriate R2 for different regression models
#https://easystats.github.io/performance/reference/r2.html


#########
# Variables for testing
# outcome = "ed.diagnosis.dsm5.t1"
# explanatory_variables = "year.inclusion"
# adjustment = c()
# data_set = "dat"
# population = "Full sample"
# name = "Model1"
# time_point = "t1"


multinom.reg.statistics <- function(
  outcome,
  explanatory_variables,
  adjustment = c(),
  data_set,
  population = "Full sample",
  name = "Model1",
  regression = "Multinomial",
  time_point = "t1",
  duration = "cross-sectional"
  ) {
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
  #

  # create the formula
  multinom.formula <- as.formula(paste(dependent.variable, paste(independent.variables, collapse=" + "), sep=" ~ "))
  multinom.formula
  
  # data frame
  multin_mcfadden_df <<- get(data_set)
  
  # run the multinom
  assign(paste0(outcome, ".", name),
         multinom(formula = multinom.formula,
             data = multin_mcfadden_df
         )
  )
  
  # run and save in object
  R2_object <-
    performance::r2_mcfadden(
      get(paste0(outcome, ".", name))
    )
  
  assign(paste0(outcome, name, "_statistics"),
         glance(get(paste0(outcome, ".", name))) %>%
           add_column(
             "Sample" = population,
             "Model" = name,
             "Regression" = regression,
             "Duration" = duration,
             "Dependent variable" = dependent.variable,
             "Independent variable" = paste(explanatory_variables, collapse = ", "),
             "Time point" = time_point,
             "R2" = round(R2_object[["R2"]], digits = 3),
             "adjust. R2" = round(R2_object[["R2_adjusted"]], digits = 3),
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
             "adjust. R2",
             "edf",
             "Deviance" = "deviance",
             "AIC"
           )
  )
  
  return(get(paste0(outcome, name, "_statistics")))
}


