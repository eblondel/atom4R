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
#' @return Object of \code{ \link[R6]{R6Class}} for modelling an Sword Dataverse-specific APIclient
#' @format \code{ \link[R6]{R6Class}} object.
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

    #'@description This method is to instantiate an Sword API Dataverse-specific Client.
    #'
    #'    The \code{keyring_backend} can be set to use a different backend for storing
    #'    the SWORD DataVerse API user token with \pkg{keyring} (Default value is 'env').
    #'
    #'    The \code{logger} allows to specify the level of log (default is NULL), either "INFO"
    #'    for \pkg{atom4R} logs or "DEBUG" for verbose HTTP client (curl) logs.
    #'@param hostname host name
    #'@param token token
    #'@param logger logger
    #'@param keyring_backend keyring backend. Default is 'env'
    initialize = function(hostname, token = NULL, logger = NULL,
                          keyring_backend = 'env'){
      sword_api_url <- file.path(hostname, "dvn/api/data-deposit/v1.1/swordv2")
      if((!is.character(token) && is.null(token)) | (is.character(token) && !nzchar(token))){
        errMsg <- "A token is required"
        self$ERROR(errMsg)
        stop(errMsg)
      }
      super$initialize(
        url = sword_api_url,
        user = "atom4R",
        pwd = "",
        version = "2",
        token = token,
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
        path <- file.path(private$url, "service-document")
        self$INFO(sprintf("GET - Sword Dataverse service document at '%s'", path))
        r <- httr::GET(path, httr::authenticate(self$getToken(), ""))
        httr::stop_for_status(r)
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
      path <- file.path(private$url, "collection/dataverse", collectionId)
      self$INFO(sprintf("GET - Sword Dataverse Atom Feed document at '%s'", path))
      r <- httr::GET(path, httr::authenticate(self$getToken(), ""))
      httr::stop_for_status(r)
      xml <- XML::xmlParse(httr::content(r, "text"))
      out <- AtomFeed$new(xml = xml)
      return(out)
    },

    #'@description Get dataverses. Equivalent to \code{listCollections()} from \link{AtomPubClient}
    #'@param pretty prettify output as \code{data.frame}. Default is \code{FALSE}
    #'@return an object of class \code{data.frame}
    getDataverses = function(pretty = FALSE){
      self$listCollections(pretty = pretty)
    },

    #'@description Get dataverse members by dataverse name. Equivlaent to \code{getCollectionMembers()}
    #'@param dataverse dataverse name
    #'@return a list of \link{AtomFeed}
    getDataverse = function(dataverse){
      self$getCollectionMembers(dataverse)
    },

    #'@description Edits a dataverse entry
    #'@param identifier identifier
    #'@return an object of class \link{AtomEntry}
    editDataverseEntry = function(identifier){
      path <- file.path(private$url, "edit/study", identifier)
      self$INFO(sprintf("GET - Sword Dataverse Atom Entry document at '%s'", path))
      token <- self$getToken()
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

    #'@description Get dataverse record
    #'@param identifier identifier
    #'@return an object of class \link{AtomFeed}
    getDataverseRecord = function(identifier){
      path <- file.path(private$url, "statement/study", identifier)
      self$INFO(sprintf("GET - Sword Dataverse Atom Entry document at '%s'", path))
      token <- self$getToken()
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

    #'@description Creates a dataverse record
    #'@param dataverse dataverse name
    #'@param entry entry
    #'@param the created \link{AtomEntry}
    createDataverseRecord = function(dataverse, entry){
      out <- NULL
      if(!is(entry, "AtomEntry")) stop("The 'entry' should be an object of class 'AtomEntry'")
      ebody <- as(entry$encode(), "character")

      path <- file.path(private$url, "collection/dataverse", dataverse)
      self$INFO(sprintf("POST - Sword Dataverse Atom Entry document creation at '%s'", path))
      r <- NULL
      token <- self$getToken()
      if(!is.null(self$loggerType)) if(self$loggerType=="DEBUG"){
        r <- httr::with_verbose(httr::POST(path, httr::authenticate(token, ""),  httr::add_headers("Content-Type" = "application/atom+xml"), body = ebody))
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

    #'@description Updates a dataverse record
    #'@param dataverse dataverse name
    #'@param entry entry
    #'@param identifier identifier of the entry to update
    #'@param the created \link{AtomEntry}
    updateDataverseRecord = function(dataverse, entry, identifier){
      out <- NULL
      if(!is(entry, "AtomEntry")) stop("The 'entry' should be an object of class 'AtomEntry'")
      tmpfile = tempfile(fileext = ".xml")
      entry$save(tmpfile)
      ebody <- httr::upload_file(tmpfile)
      path <- file.path(private$url, "edit/study", identifier)
      self$INFO(sprintf("POST - Sword Dataverse Atom Entry document at update '%s'", path))
      r <- NULL
      token <- self$getToken()
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

    #'@description Deletes a dataverse record
    #'@param identifier identifier
    #'@return \code{TRUE} if deleted, or returns an error otherwise
    deleteDataverseRecord = function(identifier){
      path <- file.path(private$url, "edit/study", identifier)
      self$INFO(sprintf("DELETE - Sword Dataverse Atom Entry document at '%s'", path))
      token <- self$getToken()
      if(!is.null(self$loggerType)) if(self$loggerType=="DEBUG"){
        r <- httr::with_verbose(httr::DELETE(path, httr::authenticate(token, "")))
      }else{
        r <- httr::DELETE(path, httr::authenticate(token, ""))
      }
      httr::stop_for_status(r)
      return(TRUE)
    },

    #'@description Publishes a dataverse record
    #'@param identifier identifier
    #'@return the published \link{AtomEntry}
    publishDataverseRecord = function(identifier){
      out <- NULL
      path <- file.path(private$url, "edit/study", identifier)
      self$INFO(sprintf("POST - Sword Dataverse Atom Entry record publication at '%s'", path))
      r <- NULL
      token <- self$getToken()
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

    #'@description Add files to a dataverse record
    #'@param identifier identifier
    #'@param files files
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
      token <- self$getToken()
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

    #'@description Deletes files from a Dataverse record
    #'@param identifier identifier
    #'@param files files
    #'@return an object of class \code{data.frame} giving each file and it's deletion status
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
            token <- self$getToken()
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

