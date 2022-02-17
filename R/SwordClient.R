#' SwordClient
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#'
#' @name SwordClient
#' @title SwordClient class
#' @description This class models an Sword service client
#' @keywords SWORD API Client
#' @return Object of \code{\link{R6Class}} for modelling an Sword client
#' @format \code{\link{R6Class}} object.
#'
#' @note Abstract class
#'
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
SwordClient <- R6Class("SwordClient",
   inherit = AtomPubClient,
   private = list(
     version = NULL
   ),
   public = list(

     #'@description This method is to instantiate an Sword Client. By default the version is set to "2".
      #'
      #'    The \code{keyring_backend} can be set to use a different backend for storing
      #'    the SWORD API user token with \pkg{keyring} (Default value is 'env').
      #'
      #'    The \code{logger} allows to specify the level of log (default is NULL), either "INFO"
      #'    for \pkg{atom4R} logs or "DEBUG" for verbose HTTP client (curl) logs.
      #'@param url url
      #'@param version version. Default is "2"
      #'@param user user
      #'@param pwd pwd
      #'@param token token
      #'@param logger logger
      #'@param keyring_backend keyring backend. Default is 'env'
     initialize = function(url, version = "2", user = NULL, pwd = NULL, token = NULL, logger = NULL,
                           keyring_backend = 'env'){
       super$initialize(url = url, user = user, pwd = pwd, token = token, logger = logger, keyring_backend = keyring_backend)
       if(version != "2") stop("Only SWORD API v2 is currently supported")
       private$version = version

       serviceDoc <- try(self$getServiceDocument())
       if(is(serviceDoc, "SwordServiceDocument")){
         self$INFO(sprintf("Successfully connected to SWORD API at '%s'", url))
       }else{
         errMsg <- "Error while retrieving SWORD service document"
         self$ERROR(errMsg)
         stop(errMsg)
       }
     },

     #'@description Get service document
     #'@param force force Force getting/refreshing of service document
     #'@return object of class \link{SwordServiceDocument}
     getServiceDocument = function(force = FALSE){
       out <- NULL
       if(is.null(self$service) | force){
         path <- file.path(private$url, "service-document")
         self$INFO(sprintf("GET - Sword service document at '%s'", path))
         token <- self$getToken()
         r <- httr::GET(path, if(!is.null(token)) { httr::authenticate(token, "") } else { httr::authenticate(self$getUser(), self$getPwd()) })
         xml <- XML::xmlParse(httr::content(r, "text"))
         out <- SwordServiceDocument$new(xml = xml)
         self$service <- out
       }else{
         out <- self$service
       }
       return(out)
     },

     #'@description Get collection members. Unimplemented abstract method at \link{SwordClient} level
     getCollectionMembers = function(){
       stop("'getCollectionMembers' not implemented in Sword abstract client")
     }
   )
)
