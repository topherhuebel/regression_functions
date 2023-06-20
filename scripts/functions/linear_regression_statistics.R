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


