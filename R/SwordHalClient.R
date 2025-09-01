#' SwordHalClient
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#'
#' @name SwordHalClient
#' @title SwordHalClient class
#' @description This class models an Sword service client for HAL (Archives Houvertes)
#' @keywords SWORD API Client
#' @return Object of \code{ \link[R6]{R6Class}} for modelling an Sword client
#' @format \code{ \link[R6]{R6Class}} object.
#'
#' @note Experimental
#'
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
SwordHalClient <- R6Class("SwordHalClient",
   inherit = SwordClient,
   private = list(
     version = NULL
   ),
   public = list(

     #'@description This method is to instantiate an Sword HAL (Archive Ouvertes - \url{https://hal.archives-ouvertes.fr/}) Client.
      #'    By default the version is set to "2".
      #'
      #'    The \code{keyring_backend} can be set to use a different backend for storing
      #'    the SWORD API user token with \pkg{keyring} (Default value is 'env').
      #'
      #'    The \code{logger} allows to specify the level of log (default is NULL), either "INFO"
      #'    for \pkg{atom4R} logs or "DEBUG" for verbose HTTP client (curl) logs.
      #'@param url url
      #'@param user user
      #'@param pwd pwd
      #'@param logger logger
      #'@param keyring_backend keyring backend. Default value is 'env'
     initialize = function(url, user = NULL, pwd = NULL, logger = NULL,
                           keyring_backend = 'env'){
       super$initialize(
         url = url,
         version = "2",
         user = user,
         pwd = pwd,
         token = NULL,
         logger = logger,
         keyring_backend = keyring_backend
       )
     },

     #'@description Get service document
     #'@param force force Force getting/refreshing of service document
     #'@return object of class \link{SwordServiceDocument}
     getServiceDocument = function(force = FALSE){
       out <- NULL
       if(is.null(self$service) | force){
         path <- file.path(private$url, "servicedocument")
         self$INFO(sprintf("GET - Sword service document at '%s'", path))
         r <- httr::GET(path, httr::authenticate(self$getUser(), self$getPwd()))
         xml <- XML::xmlParse(httr::content(r, "text"))
         out <- SwordServiceDocument$new(xml = xml)
         self$service <- out
       }else{
         out <- self$service
       }
       return(out)
     },

     #'@description Get collection members
     #'@param collectionId collection ID
     #'@return a list of \link{AtomFeed}
     getCollectionMembers = function(collectionId){
       path <- file.path(private$url, collectionId)
       self$INFO(sprintf("GET - Sword HAL Atom Feed document at '%s'", path))
       r <- httr::GET(path, httr::authenticate(self$getUser(), self$getPwd()))
       httr::stop_for_status(r)
       xml <- XML::xmlParse(httr::content(r, "text"))
       out <- AtomFeed$new(xml = xml)
       return(out)
     }
   )
)
