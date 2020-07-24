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
#' @section Methods:
#' \describe{
#'  \item{\code{new(url, version, token, logger)}}{
#'    This method is to instantiate an Sword Client. By default the version is set to "2".
#'    The \code{logger} allows to specify the level of log (default is NULL), either "INFO"
#'    for \pkg{atom4R} logs or "DEBUG" for verbose HTTP client (curl) logs.
#'  }
#'  \item{\code{getServiceDocument()}}{
#'    Gets a representation in R of the SWORD service document (capabilities)
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
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#'
#' @name SwordDataverseClient
#' @title SWORD Dataverse client class
#' @description This class models an Sword service Dataverse-specific  API client
#' @keywords SWORD API Client Dataverse
#' @return Object of \code{\link{R6Class}} for modelling an Sword Dataverse-specific APIclient
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods inherited from \code{SwordClient}:
#' \describe{
#'  \item{\code{getServiceDocument()}}{
#'    Gets a representation in R of the SWORD service document (capabilities)
#'  }
#' }
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(url, token, logger)}}{
#'    This method is to instantiate an Sword API Dataverse-specific Client.
#'    The \code{logger} allows to specify the level of log (default is NULL), either "INFO"
#'    for \pkg{atom4R} logs or "DEBUG" for verbose HTTP client (curl) logs.
#'  }
#'  \item{\code{getCollectionMembers(collectionId)}}{
#'    List the collection members, ie list of entries for a dataverse.
#'  }
#'  \item{\code{getDataverse(pretty)}}{
#'    List the dataverses, equivalent to \code{listCollections()}. The \code{pretty} argument
#'    can be set to \code{TRUE} to retrieve a \code{data.frame}, otherwise a \code{list} is
#'    returned.
#'  }
#'  \item{\code{getDataverse(dataverse)}}{
#'    Get a dataverse by ID
#'  }
#'  \item{\code{editDataverseEntry(identifier)}}{
#'    Edits a dataverse entry by identifier
#'  }
#'  \item{\code{getDataverseEntry(identifier)}}{
#'    Gets a dataverse entry by identifier
#'  }
#'  \item{\code{createDataverseEntry(dataverse, entry)}}{
#'    Creates a dataverse entry in the target \code{dataverse}. The entry should be an object
#'    of class \code{AtomEntry} or \code{DCEntry} (Dublin core entry).
#'  }
#'  \item{\code{updateDataverseEntry(dataverse, entry, doi)}}{
#'    Update a dataverse entry in the target \code{dataverse}. The entry should be an object
#'    of class \code{AtomEntry} or \code{DCEntry} (Dublin core entry). To update the entry it
#'    is necessary to specify the \code{doi} of the entry
#'  }
#'  \item{\code{deleteDataverseEntry(identifier)}}{
#'    Deletes a dataverse entry by identifier
#'  }
#' }
#'
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
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

    #updateDataverseEntry
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

    #deleteDataverseEntry
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

