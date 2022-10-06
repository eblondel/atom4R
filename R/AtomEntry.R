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
#' @examples
#'  \dontrun{
#'   #encoding
#'   atom <- AtomEntry$new()
#'   atom$setId("my-atom-entry")
#'   atom$setTitle("My Atom feed entry")
#'   atom$setSummary("My Atom feed entry very comprehensive abstract")
#'   author1 <- AtomAuthor$new(
#'     name = "John Doe",
#'     uri = "http://www.atomxml.com/johndoe",
#'     email = "johndoe@@atom4R.com"
#'   )
#'   atom$addAuthor(author1)
#'   author2 <- AtomAuthor$new(
#'     name = "John Doe's sister",
#'     uri = "http://www.atomxml.com/johndoesister",
#'     email = "johndoesister@@atom4R.com"
#'   )
#'   atom$addAuthor(author2)
#'   contrib1 <- AtomContributor$new(
#'     name = "Contrib1",
#'     uri = "http://www.atomxml.com/contrib1",
#'     email = "contrib1@@atom4R.com"
#'   )
#'   atom$addContributor(contrib1)
#'   contrib2 <- AtomContributor$new(
#'     name = "Contrib2",
#'     uri = "http://www.atomxml.com/contrib2",
#'     email = "contrib2@@atom4R.com"
#'   )
#'   atom$addContributor(contrib2)
#'   atom$addCategory("draft", "dataset")
#'   atom$addCategory("world", "spatial")
#'   atom$addCategory("fisheries", "domain")
#'
#'   xml <- atom$encode()
#'  }
#'
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
AtomEntry <- R6Class("AtomEntry",
  inherit = AtomAbstractObject,
  lock_class = FALSE,
  lock_objects = FALSE,
  private = list(
    xmlElement = "entry",
    xmlNamespacePrefix = "ATOM",
    document = TRUE
  ),
  public = list(
    #' @field id identifier
    id = NULL,
    #' @field updated Update date/time
    updated = NULL,
    #' @field published Publication date/time
    published = NULL,
    #' @field title Title
    title = NULL,
    #' @field summary Summary
    summary = NULL,
    #' @field rights Rights
    rights = NULL,
    #' @field source Source
    source = NULL,
    #' @field author Author(s)
    author = list(),
    #' @field contributor Contributor(s)
    contributor = list(),
    #' @field category Category
    category = list(),
    #' @field content Content
    content = NULL,

    #'@description Initializes an \link{AtomEntry}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    initialize = function(xml = NULL){
      self$setUpdated(Sys.time())
      super$initialize(xml = xml, wrap = FALSE)
    },

    #'@description Set ID
    #'@param id id
    setId = function(id){
      if(!is(id, "character")) stop("Atom feed id should be a 'character' object")
      self$id <- id
    },

    #'@description Set updated date
    #'@param updated object of class \code{Date} or \code{POSIXt}
    setUpdated = function(updated){
      if(!is(updated, "Date") & !is(updated, "POSIXt")){
        stop("Atom entry 'updated' should be a 'Date' or 'POSIXt' object")
      }
      attr(updated, "tzone") <- NULL
      self$updated <- updated
    },

    #'@description Set published date
    #'@param published object of class \code{Date} or \code{POSIXt}
    setPublished = function(published){
      if(!is(published, "Date") & !is(published, "POSIXt")){
        stop("Atom entry 'published' should be a 'Date' or 'POSIXt' object")
      }
      self$published <- published
    },

    #'@description Set title
    #'@param title title
    #'@param type type. Default is "text"
    setTitle = function(title, type = "text"){
      self$title = self$createElement(title, type)
    },

    #'@description Set summary
    #'@param summary summary
    #'@param type type. Default is "text"
    setSummary = function(summary, type = "text"){
      self$summary = self$createElement(summary, type)
    },

    #'@description Set rights
    #'@param rights rights
    #'@param type type. Default is "text"
    setRights = function(rights, type = "text"){
      self$rights <- self$createElement(rights, type)
    },

    #'@description Set source
    #'@param source source
    #'@param type type. Default is "text"
    setSource = function(source, type = "text"){
      self$source <- self$createElement(source, type)
    },

    #'@description Adds author
    #'@param author object of class \link{AtomAuthor}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addAuthor = function(author){
      if(!is(author, "AtomAuthor")){
        stop("Author should be an object of class 'AtomAuthor")
      }
      return(self$addListElement("author", author))
    },

    #'@description Deletes author
    #'@param author object of class \link{AtomAuthor}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delAuthor = function(author){
      if(!is(author, "AtomAuthor")){
        stop("Author should be an object of class 'AtomAuthor")
      }
      return(self$delListElement("author", author))
    },

    #'@description Adds contributor
    #'@param contributor object of class \link{AtomContributor}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addContributor = function(contributor){
      if(!is(contributor, "AtomContributor")){
        stop("Contributor should be an object of class 'AtomContributor")
      }
      return(self$addListElement("contributor", contributor))
    },

    #'@description Deletes contributor
    #'@param contributor object of class \link{AtomContributor}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delContributor = function(contributor){
      if(!is(contributor, "AtomContributor")){
        stop("Contributor should be an object of class 'AtomContributor")
      }
      return(self$delListElement("contributor", contributor))
    },

    #'@description Adds category
    #'@param value value
    #'@param term term
    #'@param scheme scheme
    #'@param label label
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addCategory = function(value, term, scheme = NULL, label = NULL){
      category <- AtomCategory$new(value = value, term = term, scheme = scheme, label = label)
      self$category[[length(self$category)+1]] <- category
      return(TRUE)
    },

    #'@description Deletes category
    #'@param value value
    #'@param term term
    #'@param scheme scheme
    #'@param label label
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delCategory = function(value, term, scheme = NULL, label = NULL){
      category <- AtomCategory$new(value = value, term = term, scheme = scheme, label = label)
      catLength <- length(self$category)
      if(length(self$category)>0){
        self$category <- self$category[sapply(self$category, function(x){
          x$attrs[["term"]]!=term
        })]
      }
      return(self$category == catLength-1)
    },

    #'@description Adds link
    #'@param link link
    #'@param rel relation. Default is "alternate"
    #'@param type type. Default is "text/html"
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addLink = function(link, rel = "alternate", type = "text/html"){
      if(!rel %in% c("self", "alternate", "related", "enclosure", "via")){
        stop("Link relation 'rel' should be among values ['self', 'alternate', 'related', 'enclosure', 'via']")
      }
      thelink <- AtomLink$new(href = link, rel = rel, type = type)
      self$link[[length(self$link)+1]] <- thelink
    },

    #'@description Deletes link
    #'@param link link
    #'@param rel relation. Default is "alternate"
    #'@param type type. Default is "text/html"
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
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


    #'@description Set content
    #'@param content content
    setContent = function(content){
      stop("'setContent' not yet implemented")
    }

  )
)
