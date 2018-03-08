#' evaluateCompetitionModel
#' 
#' This simple script just applies a model entry (which matches a particular form) to a data set, using an evaluation measure. All of these can be specified, but have default functions
#' 
#' 
#' @param dataSet A tibble with whatever headers are needed to fit the model, but which has to at least include 
#' @param model A model that takes whatever dataSet is, and creates a new column, by default named "modelProjectionB", 
#' @param fitFunction A function that integrates the outcome of the model application and the dataSet. Defaults to mean squared error

#' 
#' @return A list containing a tibble and a fit outcome.
#' @export
#' 
evaluateCompetitionModel <- function(dataSet=cpcRawData, model, fitFunction=F){
  appliedModel <- model(dataSet)
  if(!fitFunction){
    fitFunction <- function(a, b){sqrt(mean((b-a)^2))} #default to rmse
  }
  fitFunction(dataSet$B, model$modelProjectionB)
}