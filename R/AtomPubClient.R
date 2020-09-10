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
#'  \item{\code{new(url, token)}}{
#'    This method is to instantiate an AtomPub Client
#'  }
#'  \item{\code{getToken()}}{
#'    Retrieves user token.
#'  }
#'  \item{\code{getServiceDocument()}}{
#'    Gets service document description. Unimplemented in abstract classes.
#'  }
#'  \item{\code{listCollections(pretty)}}{
#'    Lists the available collections. Use \code{pretty} to return a "data.frame" insteaf
#'    of a list.
#'  }
#'  \item{\code{getCollectionMembers(collectionId)}}{
#'    List members of a collection. Unimplemented in abstract classes.
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
    keyring_service = NULL,
    url = NULL
  ),
  public = list(

    service = NULL,

    #initialize
    initialize = function(url, token = NULL, logger = NULL){
      super$initialize(logger = logger)
      private$url = url
      if((!is.character(token) && is.null(token)) | (is.character(token) && !nzchar(token))){
        errMsg <- "A token is required"
        self$ERROR(errMsg)
        stop(errMsg)
      }
      if(!is.null(token)) if(nzchar(token)){
        private$keyring_service <- paste0("atom4R@", url)
        keyring::key_set_with_value(private$keyring_service, username = "atom4R", password = token)
      }
    },

    #getToken
    getToken = function(){
      token <- NULL
      if(!is.null(private$keyring_service)){
        token <- keyring::key_get(service = private$keyring_service, username = "atom4R")
      }
      return(token)
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
    getCollectionMembers = function(collectionId){
      stop("'getCollectionMembers' not implemented in AtomPub abstract client")
    }
  )
)
