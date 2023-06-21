#########
# Variables for testing
# outcome = "EDEQ22.vomiting.freq.capped.t1"
# explanatory_variables = "year.inclusion"
# adjustment = c()
# data_set = "dat"
# population = "Full sample"
# name = "Model1"
# time_point = "t1"


neg.binomial.reg.stats <- function(
  outcome,
  explanatory_variables,
  adjustment = c(),
  data_set,
  population = "Full sample",
  name = "Model1",
  regression = "Negative binomial",
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
  # create the formula
  negbi.formula <- as.formula(paste(dependent.variable, paste(independent.variables, collapse=" + "), sep=" ~ "))
  negbi.formula
  # run the multinom
  assign(paste0(outcome, ".", name),
         MASS::glm.nb(formula = negbi.formula,
                      data = get(data_set)
         )
  )
  
  # run and save in object
  R2_object <-
    get(paste0(outcome, ".", name)) %>%
    performance::r2()

  # run the multinom
  
  assign(paste0(outcome, name, "_reg_stats"),
         glance(get(paste0(outcome, ".", name))) %>%
           add_column(
             "Model" = name,
             "Regression" = regression,
             "Sample" = population,
             "Time point" = time_point,
             "Duration" = duration,
             "Dependent variable" = outcome,
             "Independent variable" = paste(explanatory_variables, collapse = ", "),
             "Nagelkerke's R2" = round(R2_object[["R2_Nagelkerke"]], digits = 3),
             .before = "nobs") %>%
           mutate(
             "n" = round(
                 nobs,
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
             "Nagelkerke's R2",
             "Null deviance" = null.deviance,
             "Deviance" = deviance,
             "df null" = df.null,
             "df residual" = df.residual,
             "AIC",
             "BIC"
           )
  )
  
  return(get(paste0(outcome, name, "_reg_stats")))
  
}

