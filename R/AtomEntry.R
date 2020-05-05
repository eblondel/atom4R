#' AtomEntry
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#'
#' @name AtomEntry
#' @title Atom Entry class
#' @description This class models an atom Entry
#' @keywords atom Entry
#' @return Object of \code{\link{R6Class}} for modelling an Atom Entry
#' @format \code{\link{R6Class}} object.
#'
#' @field id
#'
#' @examples
#' \dontrun{

#' }
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to create an Atom
#'  }
#' }
#'
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
AtomEntry <- R6Class("AtomEntry",
  inherit = AtomAbstractObject,
  private = list(
    xmlElement = "entry",
    xmlNamespacePrefix = "ATOM",
    document = TRUE
  ),
  public = list(
    id = NULL,
    updated = Sys.time(),
    published = NULL,
    title = NULL,
    summary = NULL,
    rights = NULL,
    source = NULL,
    author = list(),
    contributor = list(),
    category = list(),
    content = NULL,

    initialize = function(xml = NULL){
      super$initialize(xml = xml, wrap = FALSE)
    },

    #setId
    setId = function(id){
      if(!is(id, "character")) stop("Atom feed id should be a 'character' object")
      self$id <- id
    },

    #setUpdated
    setUpdated = function(updated){
      if(!is(updated, "Date") & !is(updated, "POSIXt")){
        stop("Atom feed 'updated' should be a 'Date' or 'POSIXt' object")
      }
      self$updated <- updated
    },

    #setPublished
    setPublished = function(published){
      if(!is(published, "Date") & !is(published, "POSIXt")){
        stop("Atom feed 'published' should be a 'Date' or 'POSIXt' object")
      }
      self$published <- published
    },

    #setTitle
    setTitle = function(title, type = "text"){
      self$title = self$createElement(title, type)
    },

    #setSummary
    setSummary = function(summary, type = "text"){
      self$summary = self$createElement(summary, type)
    },

    #setRights
    setRights = function(rights, type = "text"){
      self$rights <- self$createElement(rights, type)
    },

    #setSource
    setSource = function(source, type = "text"){
      self$source <- self$createElement(source, type)
    },

    #addAuthor
    addAuthor = function(author){
      if(!is(author, "AtomAuthor")){
        stop("Author should be an object of class 'AtomAuthor")
      }
      return(self$addListElement("author", author))
    },

    #delAuthor
    delAuthor = function(author){
      if(!is(author, "AtomAuthor")){
        stop("Author should be an object of class 'AtomAuthor")
      }
      return(self$delListElement("author", author))
    },

    #addContributor
    addContributor = function(contributor){
      if(!is(contributor, "AtomContributor")){
        stop("Contributor should be an object of class 'AtomContributor")
      }
      return(self$addListElement("contributor", contributor))
    },

    #delContributor
    delContributor = function(contributor){
      if(!is(contributor, "AtomContributor")){
        stop("Contributor should be an object of class 'AtomContributor")
      }
      return(self$delListElement("contributor", contributor))
    },

    #addCategory
    addCategory = function(term, scheme = NULL, label = NULL){
      category <- AtomCategory$new(term = term, scheme = scheme, label = label)
      self$category[[length(self$category)+1]] <- category
      return(TRUE)
    },

    #delCategory
    delCategory = function(term, scheme = NULL, label = NULL){
      category <- AtomCategory$new(term = term, scheme = scheme, label = label)
      catLength <- length(self$category)
      if(length(self$category)>0){
        self$category <- self$category[sapply(self$category, function(x){
          x$attrs[["term"]]!=term
        })]
      }
      return(self$category == catLength-1)
    },

    #addLink
    addLink = function(link, rel = "alternate", type = "text/html"){
      if(!rel %in% c("self", "alternate", "related", "enclosure", "via")){
        stop("Link relation 'rel' should be among values ['self', 'alternate', 'related', 'enclosure', 'via']")
      }
      thelink <- AtomLink$new(href = link, rel = rel, type = type)
      self$link[[length(self$link)+1]] <- thelink
    },

    #delLink
    delLink = function(link, rel = "alternate", type = "text/html"){
      if(!rel %in% c("self", "alternate", "related", "enclosure", "via")){
        stop("Link relation 'rel' should be among values ['self', 'alternate', 'related', 'enclosure', 'via']")
      }
      linkLength <- length(self$link)
      if(length(self$link)>0){
        self$link <- self$link[sapply(self$link, function(x){
          x$attrs[["rel"]]!=rel &
            x$attrs[["type"]]!=type &
            x$attrs[["href"]]!=link
        })]
      }
      return(length(self$link) == linkLength-1)
    },

    setContent = function(content){

    }

  )
)
