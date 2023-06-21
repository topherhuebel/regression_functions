# outcome: Your outcome variable as categorical factor (unordered)
# explanatory_variables: A vector with your explanatory variables
# adjustment: A vector with your variables that you want to adjust for
# Population: A character string that names your full sample or a subsample
# name: Name of the model (e.g., Model1, FullModel)

# Appropriate R2 for differen regression models
#https://easystats.github.io/performance/reference/r2.html



#########
# Variables for testing
# outcome = "EDEQ22.vomiting.freq.capped.t1"
# explanatory_variables = "year.inclusion"
# adjustment = c()
# data_set = "dat"
# population = "Full sample"
# name = "Model1"
# time_point = "t1"



zeroinf.neg.binomial.reg.estimates <- function(
  outcome,
  explanatory_variables,
  adjustment = c(),
  data_set,
  population = "Full sample",
  name = "Model1",
  regression = "Zero-inflated negative binomial",
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
         pscl::zeroinfl(formula = negbi.formula,
             data = get(data_set),
             dist = "negbin",
             link = "logit"
         )
  )
  
  assign(paste0(outcome, name, "_estimates"),
         performance::performance(
           get(paste0(outcome, ".", name))
           ) %>%
           add_column(
             "Sample" = population,
             "Time point" = time_point,
             "Duration" = duration,
             "Model" = name,
             "Regression" = regression,
             "Dependent variable" = outcome,
             "Independent variable" = paste(explanatory_variables, collapse = ", "),
             "n" = get(paste0(outcome, ".", name))$`n`,
             "df Residual" = get(paste0(outcome, ".", name))$`SE.logtheta`, # adapt name
             "logLikelihood" = get(paste0(outcome, ".", name))$`loglik`, # adapt name
             "theta" = get(paste0(outcome, ".", name))$`theta`, # adapt name
             "SE log(theta)" = get(paste0(outcome, ".", name))$`SE.logtheta`, # adapt name
             .before = "AIC") %>%
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
             "adjust. R2" = R2_adjusted,
             "df Residual",
             "AIC",
             "BIC",
             "RMSE",
             "Sigma",
             "Score_log",
             "Score_spherical",
             "theta",
             "SE log(theta)"
           ) %>%
           as_tibble()
  )
  return(get(paste0(outcome, name, "_estimates")))
}




