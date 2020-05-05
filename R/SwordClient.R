#' SwordClient
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#'
#' @name SwordClient
#' @title SwordClient class
#' @description This class models an Sword service client
#' @keywords Atom Person
#' @return Object of \code{\link{R6Class}} for modelling an Sword client
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(url, user, password, token)}}{
#'    This method is to instantiate an Sword Client
#'  }
#' }
#'
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
SwordClient <- R6Class("SwordClient",
   inherit = AtomPubClient,
   private = list(
     version = NULL
   ),
   public = list(

     #initialize
     initialize = function(url, version = "2", token = NULL, logger = NULL){
       super$initialize(url = url, token = token, logger = logger)
       if(version != "2") stop("Only SWORD API v2 is currently supported")
       private$version = version
     },

     #getServiceDocument
     getServiceDocument = function(){
       out <- NULL
       if(is.null(self$service) | force){
         path <- file.path(private$url, "service-document")
         self$INFO(sprintf("GET - Sword service document at '%s'", path))
         r <- httr::GET(path, httr::authenticate(private$token, ""))
         xml <- XML::xmlParse(httr::content(r, "text"))
         out <- SwordServiceDocument$new(xml = xml)
         self$service <- out
       }else{
         out <- self$service
       }
       return(out)
     },

     #getCollectionMembers
     getCollectionMembers = function(){
       stop("'getCollectionMembers' not implemented in Sword abstract client")
     }
   )
)

#'SwordDataverseClient
#'@export
SwordDataverseClient <- R6Class("SwordDataverseClient",
  inherit = SwordClient,
  public = list(
    initialize = function(hostname, token = NULL, logger = NULL){
      super$initialize(
        url = file.path(hostname, "dvn/api/data-deposit/v1.1/swordv2"),
        version = "2",
        token = token,
        logger = logger
      )
    },

    #getServiceDocument
    getServiceDocument = function(force = FALSE){
      out <- NULL
      if(is.null(self$service) | force){
        path <- file.path(private$url, "service-document")
        self$INFO(sprintf("GET - Sword Dataverse service document at '%s'", path))
        r <- httr::GET(path, httr::authenticate(private$token, ""))
        xml <- XML::xmlParse(httr::content(r, "text"))
        out <- SwordServiceDocument$new(xml = xml)
        self$service <- out
      }else{
        out <- self$service
      }
      return(out)
    },

    #getCollectionMembers
    getCollectionMembers = function(collectionId){
      path <- file.path(private$url, "collection/dataverse", collectionId)
      self$INFO(sprintf("GET - Sword Dataverse Atom Feed document at '%s'", path))
      r <- httr::GET(path, httr::authenticate(private$token, ""))
      xml <- XML::xmlParse(httr::content(r, "text"))
      out <- AtomFeed$new(xml = xml)
      return(out)
    },

    #getDataverses
    getDataverses = function(pretty = FALSE){
      self$listCollections(pretty = pretty)
    },

    #getDataverse
    getDataverse = function(dataverse){
      self$getCollectionMembers(dataverse)
    },

    #getDataverseEntry
    getDataverseEntry = function(dataverse, identifier){
      path <- file.path(private$url, "edit/study", identifier)
      self$INFO(sprintf("GET - Sword Dataverse Atom Entry document at '%s'", path))
      r <- httr::GET(path, httr::authenticate(private$token, ""))
      xml <- XML::xmlParse(httr::content(r, "text"))
      out <- AtomEntry$new(xml = xml)
      return(out)
    }

  )
)

