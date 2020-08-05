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
         token <- keyring::key_get(service = private$keyring_service, username = "atom4R")
         r <- httr::GET(path, httr::authenticate(token, ""))
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
#'  \item{\code{getDataverseRecord(identifier)}}{
#'    Gets a dataverse record by identifier
#'  }
#'  \item{\code{createDataverseRecord(dataverse, entry)}}{
#'    Creates a dataverse record in the target \code{dataverse}. The entry should be an object
#'    of class \code{AtomEntry} or \code{DCEntry} (Dublin core entry).
#'  }
#'  \item{\code{updateDataverseRecord(dataverse, entry, doi)}}{
#'    Update a dataverse entry in the target \code{dataverse}. The entry should be an object
#'    of class \code{AtomEntry} or \code{DCEntry} (Dublin core entry). To update the entry it
#'    is necessary to specify the \code{doi} of the entry
#'  }
#'  \item{\code{deleteDataverseRecord(identifier)}}{
#'    Deletes a dataverse record by identifier
#'  }
#'  \item{\code{publishDataverseRecord(identifier)}}{
#'    Publishes a dataverse record by identifier
#'  }
#'  \item{\code{addFilesToDataverseRecord(identifier, files)}}{
#'    Adds one or more files to a Dataverse record. The \code{files} should be a vector of class
#'    "character" listing the files to be added/uploaded. Return \code{TRUE} if files are
#'    successfully added to the record.
#'  }
#'  \item{\code{deleteFilesFromDataverseRecord(identifier, files)}}{
#'    Deletes one or more files from a Dataverse record. The \code{files} should be a vector of class
#'    "character" listing the files to be added/uploaded. By default this argument is \code{NULL} and all
#'    files will be deleted. Returns a \code{data.frame} specifying for each file \code{TRUE} if it has
#'    been deleted, \code{FALSE} otherwise.
#'  }
#' }
#'
#' @examples
#' \dontrun{
#'    #connect to SWORD Dataverse API
#'    SWORD <- SwordDataverseClient$new(
#'      hostname = "localhost:8085",
#'      token = "<token>",
#'      logger = "DEBUG"
#'    )
#'
#'    #for detailed operations check the wiki at:
#'    #https://github.com/eblondel/atom4R/wiki#atom4R-publish-sword-dataverse
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
        token <- keyring::key_get(service = private$keyring_service, username = "atom4R")
        r <- httr::GET(path, httr::authenticate(token, ""))
        httr::stop_for_status(r)
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
      token <- keyring::key_get(service = private$keyring_service, username = "atom4R")
      r <- httr::GET(path, httr::authenticate(token, ""))
      httr::stop_for_status(r)
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
      token <- keyring::key_get(service = private$keyring_service, username = "atom4R")
      if(!is.null(self$loggerType)) if(self$loggerType=="DEBUG"){
        r <- httr::with_verbose(httr::GET(path, httr::authenticate(token, "")))
      }else{
        r <- httr::GET(path, httr::authenticate(token, ""))
      }
      httr::stop_for_status(r)
      xml <- XML::xmlParse(httr::content(r, "text"))
      out <- AtomEntry$new(xml = xml)
      return(out)
    },

    #getDataverseRecord
    getDataverseRecord = function(identifier){
      path <- file.path(private$url, "statement/study", identifier)
      self$INFO(sprintf("GET - Sword Dataverse Atom Entry document at '%s'", path))
      token <- keyring::key_get(service = private$keyring_service, username = "atom4R")
      if(!is.null(self$loggerType)) if(self$loggerType=="DEBUG"){
        r <- httr::with_verbose(httr::GET(path, httr::authenticate(token, "")))
      }else{
        r <- httr::GET(path, httr::authenticate(token, ""))
      }
      httr::stop_for_status(r)
      xml <- XML::xmlParse(httr::content(r, "text"))
      out <- AtomFeed$new(xml = xml)
      return(out)
    },

    #createDataverseRecord
    createDataverseRecord = function(dataverse, entry){
      out <- NULL
      if(!is(entry, "AtomEntry")) stop("The 'entry' should be an object of class 'AtomEntry'")
      ebody <- as(entry$encode(), "character")

      path <- file.path(private$url, "collection/dataverse", dataverse)
      self$INFO(sprintf("POST - Sword Dataverse Atom Entry document creation at '%s'", path))
      r <- NULL
      token <- keyring::key_get(service = private$keyring_service, username = "atom4R")
      if(!is.null(self$loggerType)) if(self$loggerType=="DEBUG"){
        r <- httr::with_verbose(httr::POST(path, httr::authenticate(ptoken, ""),  httr::add_headers("Content-Type" = "application/atom+xml"), body = ebody))
      }else{
        r <- httr::POST(path, httr::authenticate(token, ""),  httr::add_headers("Content-Type" = "application/atom+xml"), body = ebody)
      }
      httr::stop_for_status(r)
      if(httr::status_code(r) == 201){
        xml <- XML::xmlParse(httr::content(r, "text"))
        out <- AtomEntry$new(xml = xml)
      }
      return(out)

    },

    #updateDataverseRecord
    updateDataverseRecord = function(dataverse, entry, identifier){
      out <- NULL
      if(!is(entry, "AtomEntry")) stop("The 'entry' should be an object of class 'AtomEntry'")
      tmpfile = tempfile(fileext = ".xml")
      entry$save(tmpfile)
      ebody <- httr::upload_file(tmpfile)
      path <- file.path(private$url, "edit/study", identifier)
      self$INFO(sprintf("POST - Sword Dataverse Atom Entry document at update '%s'", path))
      r <- NULL
      token <- keyring::key_get(service = private$keyring_service, username = "atom4R")
      if(!is.null(self$loggerType)) if(self$loggerType=="DEBUG"){
        r <- httr::with_verbose(httr::PUT(path, httr::authenticate(token, ""),  httr::add_headers("Content-Type" = "application/atom+xml"), body = ebody))
      }else{
        r <- httr::PUT(path, httr::authenticate(token, ""),  httr::add_headers("Content-Type" = "application/atom+xml"), body = ebody)
      }
      httr::stop_for_status(r)
      if(httr::status_code(r) == 200){
        xml <- XML::xmlParse(httr::content(r, "text"))
        out <- AtomEntry$new(xml = xml)
      }
      unlink(tmpfile)
      return(out)
    },

    #deleteDataverseRecord
    deleteDataverseRecord = function(identifier){
      path <- file.path(private$url, "edit/study", identifier)
      self$INFO(sprintf("DELETE - Sword Dataverse Atom Entry document at '%s'", path))
      token <- keyring::key_get(service = private$keyring_service, username = "atom4R")
      if(!is.null(self$loggerType)) if(self$loggerType=="DEBUG"){
        r <- httr::with_verbose(httr::DELETE(path, httr::authenticate(token, "")))
      }else{
        r <- httr::DELETE(path, httr::authenticate(token, ""))
      }
      httr::stop_for_status(r)
      return(TRUE)
    },

    #publishDataverseRecord
    publishDataverseRecord = function(identifier){
      out <- NULL
      path <- file.path(private$url, "edit/study", identifier)
      self$INFO(sprintf("POST - Sword Dataverse Atom Entry record publication at '%s'", path))
      r <- NULL
      token <- keyring::key_get(service = private$keyring_service, username = "atom4R")
      if(!is.null(self$loggerType)) if(self$loggerType=="DEBUG"){
        r <- httr::with_verbose(httr::POST(path, httr::authenticate(token, ""),
                                           httr::add_headers("In-Progress" = "false")))
      }else{
        r <- httr::POST(path, httr::authenticate(token, ""),
                        httr::add_headers("In-Progress" = "false"))
      }
      httr::stop_for_status(r)
      if(httr::status_code(r) == 200){
        xml <- XML::xmlParse(httr::content(r, "text"))
        out <- AtomEntry$new(xml = xml)
      }
      return(out)
    },

    #addFilesToDataverseRecord
    addFilesToDataverseRecord = function(identifier, files){

      tmpfile <- tempfile(fileext = ".zip")
      on.exit(unlink(tmpfile))
      zip::zipr(zipfile = tmpfile, files = files)

      out <- NULL
      path <- file.path(private$url, "edit-media/study", identifier)
      self$INFO(sprintf("POST - Sword Dataverse Add files to record '%s'", path))

      h <- httr::add_headers(
        "Content-Disposition" = sprintf("filename=%s", tmpfile),
        "Content-Type" = "application/zip",
        "Packaging" = "http://purl.org/net/sword/package/SimpleZip"
      )
      token <- keyring::key_get(service = private$keyring_service, username = "atom4R")
      if(!is.null(self$loggerType)) if(self$loggerType=="DEBUG"){
        r <- httr::with_verbose(httr::POST(path, httr::authenticate(token, ""), h,
                                           body = httr::upload_file(tmpfile)))
      }else{
        r <- httr::POST(path, httr::authenticate(token, ""), h,
                        body = httr::upload_file(tmpfile))
      }
      httr::stop_for_status(r)
      if(httr::status_code(r) == 201){
        self$INFO(sprintf("Successfully added files to record with identifier '%s'", identifier))
        xml <- XML::xmlParse(httr::content(r, "text"))
        out <- AtomEntry$new(xml = xml)
      }
    },

    #deleteFileFromDataverseRecord
    deleteFilesFromDataverseRecord = function(identifier, files = NULL){

      rec <- self$getDataverseRecord(identifier)
      if(length(rec$entry)==0) {
        self$WARN(sprintf("No existing files associated to record '%s'", identifier))
        return(FALSE)
      }
      remote_files <- sapply(rec$entry, function(x){x$id})

      if(is.null(files)){
        #if no files specified we delete all files
        files <- sapply(remote_files, function(x){parts = unlist(strsplit(x,"/")); return(parts[length(parts)])})
        names(files) <- NULL
      }

      out <- data.frame(
        file = files,
        deleted = sapply(files, function(x){
          del <- FALSE
          print(remote_files)
          if(any(endsWith(remote_files, x))){
            path <- remote_files[endsWith(remote_files, x)][1]
            self$INFO(sprintf("DELETE - Sword Dataverse Remove files from record '%s'", path))
            token <- keyring::key_get(service = private$keyring_service, username = "atom4R")
            if(!is.null(self$loggerType)) if(self$loggerType=="DEBUG"){
              r <- httr::with_verbose(httr::DELETE(path, httr::authenticate(token, "")))
            }else{
              r <- httr::DELETE(path, httr::authenticate(token, ""))
            }
            httr::stop_for_status(r)
            del <- TRUE
          }
          return(del)
        }),
        stringsAsFactors = FALSE
      )
      return(out)
    }

  )
)

