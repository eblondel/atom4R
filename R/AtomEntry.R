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
#' @field id identifier
#' @field updated Update date/time
#' @field published Publication date/time
#' @field title Title
#' @field summary Summary
#' @field rights Rights
#' @field source Source
#' @field author Author(s)
#' @field contributor Contributor(s)
#' @field category Category
#' @field content Content
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to create an Atom
#'  }
#'  \item{\code{setId(id)}}{
#'    Set identifier
#'  }
#'  \item{\code{setUpdated(updated)}}{
#'    Set update date (object of class 'character' or 'POSIX')
#'  }
#'  \item{\code{setPublished(published)}}{
#'    Set publication date (object of class 'character' or 'POSIX')
#'  }
#'  \item{\code{setTitle(title)}}{
#'    Set title
#'  }
#'  \item{\code{setSummary(summary)}}{
#'    Set summary
#'  }
#'  \item{\code{setRights(rights)}}{
#'    Set rights
#'  }
#'  \item{\code{setSource(source)}}{
#'    Set source
#'  }
#'  \item{\code{addAuthor(author)}}{
#'    Adds an author, object of class \code{AtomAuthor}
#'  }
#'  \item{\code{delAuthor(author)}}{
#'    Deletes an author, object of class \code{AtomAuthor}
#'  }
#'  \item{\code{addContributor(contributor)}}{
#'    Adds a contributor, object of class \code{AtomContributor}
#'  }
#'  \item{\code{delContributor(contributor)}}{
#'    Deletes a contributor, object of class \code{AtomContributor}
#'  }
#'  \item{\code{addCategory(value, term, scheme, label)}}{
#'    Adds a category
#'  }
#'  \item{\code{delCategory(value, term, scheme, label)}}{
#'    Deletes a category
#'  }
#'  \item{\code{addLink(link, rel, type)}}{
#'    Adds a link. Default \code{rel} value is set to "alternate". Default
#'    \code{type} value is set to "text/html"
#'  }
#'  \item{\code{delLink(link, rel, type)}}{
#'    Deletes a link
#'  }
#' }
#'
#' @examples
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
#'   atom$addCategory("dataset")
#'   atom$addCategory("spatial")
#'   atom$addCategory("fisheries")
#'
#'   xml <- atom$encode()
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
    addCategory = function(value, term, scheme = NULL, label = NULL){
      category <- AtomCategory$new(value = value, term = term, scheme = scheme, label = label)
      self$category[[length(self$category)+1]] <- category
      return(TRUE)
    },

    #delCategory
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
      stop("'setContent' not yet implemented")
    }

  )
)
