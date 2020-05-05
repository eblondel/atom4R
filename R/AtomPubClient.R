#' AtomPubClient
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#'
#' @name AtomPubClient
#' @title AtomPubClient class
#' @description This class models an AtomPub service client
#' @keywords Atom Person
#' @return Object of \code{\link{R6Class}} for modelling an AtomPub client
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(url, user, password, token)}}{
#'    This method is to instantiate an AtomPub Client
#'  }
#' }
#'
#' @note Abstract class used internally for AtomPub (Atom Publishing Protocol) clients
#'
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
AtomPubClient <- R6Class("AtomPubClient",
  inherit = atom4RLogger,
  private = list(
    url = NULL,
    token = NULL
  ),
  public = list(

    service = NULL,

    #initialize
    initialize = function(url, token = NULL, logger = NULL){
      super$initialize(logger = logger)
      private$url = url
      private$token = token
    },

    #getServiceDocument
    getServiceDocument = function(){
      stop("'getServiceDocument' not implemented in AtomPub abstract client")
    },

    #listCollections
    listCollections = function(pretty = FALSE){
      if(is.null(self$service)) self$getServiceDocument()
      collections <- self$service$collections
      if(pretty) collections <- do.call("rbind", lapply(collections, as.data.frame))
      return(collections)
    },

    #getCollectionMembers
    getCollectionMembers = function(){
      stop("'getCollectionMembers' not implemented in AtomPub abstract client")
    }
  )
)
