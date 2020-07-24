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

    #editDataverseEntry
    editDataverseEntry = function(identifier){
      path <- file.path(private$url, "edit/study", identifier)
      self$INFO(sprintf("GET - Sword Dataverse Atom Entry document at '%s'", path))
      if(!is.null(self$loggerType)) if(self$loggerType=="DEBUG"){
        r <- httr::with_verbose(httr::GET(path, httr::authenticate(private$token, "")))
      }else{
        r <- httr::GET(path, httr::authenticate(private$token, ""))
      }
      xml <- XML::xmlParse(httr::content(r, "text"))
      out <- AtomEntry$new(xml = xml)
      return(out)
    },

    #getDataverseEntry
    getDataverseEntry = function(identifier){
      path <- file.path(private$url, "statement/study", identifier)
      self$INFO(sprintf("GET - Sword Dataverse Atom Entry document at '%s'", path))
      if(!is.null(self$loggerType)) if(self$loggerType=="DEBUG"){
        r <- httr::with_verbose(httr::GET(path, httr::authenticate(private$token, "")))
      }else{
        r <- httr::GET(path, httr::authenticate(private$token, ""))
      }
      xml <- XML::xmlParse(httr::content(r, "text"))
      out <- AtomEntry$new(xml = xml)
      return(out)
    },

    #createDataverseEntry
    createDataverseEntry = function(dataverse, entry){
      out <- NULL
      if(!is(entry, "AtomEntry")) stop("The 'entry' should be an object of class 'AtomEntry'")
      ebody <- as(entry$encode(), "character")

      path <- file.path(private$url, "collection/dataverse", dataverse)
      self$INFO(sprintf("POST - Sword Dataverse Atom Entry document creation at '%s'", path))
      r <- NULL
      if(!is.null(self$loggerType)) if(self$loggerType=="DEBUG"){
        r <- httr::with_verbose(httr::POST(path, httr::authenticate(private$token, ""),  httr::add_headers("Content-Type" = "application/atom+xml"), body = ebody))
      }else{
        r <- httr::POST(path, httr::authenticate(private$token, ""),  httr::add_headers("Content-Type" = "application/atom+xml"), body = ebody)
      }
      if(httr::status_code(r) == 201){
        xml <- XML::xmlParse(httr::content(r, "text"))
        out <- AtomEntry$new(xml = xml)
      }
      return(out)

    },

    updateDataverseEntry = function(dataverse, entry, doi){
      out <- NULL
      if(!is(entry, "AtomEntry")) stop("The 'entry' should be an object of class 'AtomEntry'")
      tmpfile = tempfile(fileext = ".xml")
      entry$save(tmpfile)
      ebody <- httr::upload_file(tmpfile)
      path <- file.path(private$url, paste0("edit/study/doi:", doi))
      self$INFO(sprintf("POST - Sword Dataverse Atom Entry document at update '%s'", path))
      r <- NULL
      if(!is.null(self$loggerType)) if(self$loggerType=="DEBUG"){
        r <- httr::with_verbose(httr::PUT(path, httr::authenticate(private$token, ""),  httr::add_headers("Content-Type" = "application/atom+xml"), body = ebody))
      }else{
        r <- httr::PUT(path, httr::authenticate(private$token, ""),  httr::add_headers("Content-Type" = "application/atom+xml"), body = ebody)
      }
      if(httr::status_code(r) == 200){
        xml <- XML::xmlParse(httr::content(r, "text"))
        out <- AtomEntry$new(xml = xml)
      }
      unlink(tmpfile)
      return(out)
    },

    deleteDataverseEntry = function(identifier){
      path <- file.path(private$url, "edit/study", identifier)
      self$INFO(sprintf("DELETE - Sword Dataverse Atom Entry document at '%s'", path))
      if(!is.null(self$loggerType)) if(self$loggerType=="DEBUG"){
        r <- httr::with_verbose(httr::DELETE(path, httr::authenticate(private$token, "")))
      }else{
        r <- httr::DELETE(path, httr::authenticate(private$token, ""))
      }
      xml <- XML::xmlParse(httr::content(r, "text"))
    }

  )
)

