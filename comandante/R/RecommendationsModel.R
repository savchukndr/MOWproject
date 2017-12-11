#' Recommendation model.
#'
#' @name RecommendationsModel
#' @import Matrix

library(Matrix)

#' @export
RecommendationsModel <- setClass(
        # Name
        "RecommendationsModel",
        representation(mat="Matrix"))

# TODO Add some kind of constructor that takes user-item matrix as one of the parameters

setGeneric(name="Recommend",
                       def=function(model, userList, n)
                       {
                            standardGeneric("Recommend")
                       }
                       )
#' @export
setMethod(f="Recommend",
                      signature="RecommendationsModel",
                      definition=function(model, userList, n)
                      {
                            recommendations <- list()
                            for (i in seq(from=1, to=length(userList)))
                            {
                                # @TODO Add n best recommendations
                                recommendations[[i]] <- vector("list", length=n)
                            }
                            return(recommendations)
                      }
                      )

setGeneric(name="PredictRating",
                       def=function(model, userId, itemId)
                       {
                            standardGeneric("PredictRating")
                       }
                       )
#' @export
setMethod(f="PredictRating",
                      signature="RecommendationsModel",
                      definition=function(model, userId, itemId)
                      {
                          return(10)
                      }
                      )