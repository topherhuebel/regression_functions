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



zeroinf.neg.binomial.estimates <- function(
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
         parameters::model_parameters(
           get(paste0(outcome, ".", name)),
           ci = .95,
           iterations = 20,
           exponentiate = TRUE,
           verbose = TRUE
           ) %>%
           add_column(
             "Sample" = population,
             "Time point" = time_point,
             "Duration" = duration,
             "Model" = name,
             "Regression" = regression,
             "Dependent variable" = outcome,
             .before = "Parameter") %>%
           separate(
             col = Parameter, 
             into = c(
               "Part",
               "Parameter_intm"
               ), 
             sep = "_", 
             extra = "merge"
           ) %>%
           mutate(
             "Independent variable" = recode_factor(
               Parameter_intm,
               "(Intercept)" = "Intercept"
             ),
             "Estimate" =
               as.numeric(
                 format(
                   round(Coefficient,
                         digits = 2),
                   nsmall = 2)
               ),
             "95% CI low" = 
               as.numeric(
                 format(
                   round(CI_low,
                         digits = 2),
                   nsmall = 2)
               ),
             "95% CI up" = 
               as.numeric(
                 format(
                   round(CI_high,
                         digits = 2),
                   nsmall = 2)
               ),
             "p value" = case_when(
               p < 0.001 ~ formatC(p, format = "e", digits = 2),
               p >= 0.001 & p < 0.01 ~ formatC(p, format = "f", digits = 3),
               p >= 0.01 ~ formatC(p, format = "f", digits = 2)
             )
           ) %>%
           select(
             "Sample",
             "Time point",
             "Duration",
             "Model",
             "Regression",
             "Dependent variable",
             "Part",
             "Independent variable",
             "Estimate",
             "95% CI low",
             "95% CI up",
             "SE",
             "z score" = z,
             "p value"
           ) %>%
           as_tibble()
  )
  return(get(paste0(outcome, name, "_estimates")))
}




