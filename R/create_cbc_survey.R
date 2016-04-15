#' Creating a Choice Based Conjoint Survey
#'
#' Takes in a vector of levels and variable names, then creates a survey
#' @param level_vec A vector of variable levels
#' @param var_names A vector of variable names
#' @return sorted_survey A sorted CBC survey
#' @export

create_cbc_survey = function(level_vec, var_names){
require(AlgDesign)
# Create full factorial design
ffd = gen.factorial(level_vec, varNames=var_names, factors="all")
# Create fractional factorial design
set.seed(54321)
fract_fact_size = 0.5*prod(level_vec)
des = optFederov(~.,ffd,fract_fact_size)
survey=des$design
#Create choice sets using random selection without replacement
survey = transform(survey, r1=runif(fract_fact_size))
#Sort based on random order
sorted_survey = survey[order(survey$r1),]
sorted_survey = sorted_survey[var_names] # remove r1 column
return(sorted_survey)
}
