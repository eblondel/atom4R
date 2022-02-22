#' AtomFeed
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#'
#' @name AtomFeed
#' @title Atom feed class
#' @description This class models an atom feed
#' @keywords atom feed
#' @return Object of \code{\link{R6Class}} for modelling an Atom feed
#' @format \code{\link{R6Class}} object.

#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to create an Atom Feed
#'  }
#'  \item{\code{setId(id)}}{
#'    Set identifier
#'  }
#'  \item{\code{setUpdated(updated)}}{
#'    Set update date (object of class 'character' or 'POSIX')
#'  }
#'  \item{\code{addLink(link, rel, type)}}{
#'    Adds a link. Default \code{rel} value is set to "alternate". Default
#'    \code{type} value is set to "text/html"
#'  }
#'  \item{\code{delLink(link, rel, type)}}{
#'    Deletes a link
#'  }
#'  \item{\code{setSelfLink(link)}}{
#'    Sets a self-relation link
#'  }
#'  \item{\code{setAlternateLink(link, type)}}{
#'    Sets an alternate-relation link. Default type is "text/html"
#'  }
#'  \item{\code{setTitle(title)}}{
#'    Set title
#'  }
#'  \item{\code{setSubtitle(subtitle)}}{
#'    Set subtitle
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
#'  \item{\code{setGenerator(generator, type)}}{
#'    Sets generator
#'  }
#'  \item{\code{setIcon(icon)}}{
#'    Sets icon
#'  }
#'  \item{\code{addCategory(term, scheme, label)}}{
#'    Adds a category
#'  }
#'  \item{\code{delCategory(term, scheme, label)}}{
#'    Deletes a category
#'  }
#'  \item{\code{addEntry(entry)}}{
#'    Adds an entry, object of class \code{AtomEntry}
#'  }
#'  \item{\code{delEntry(entry)}}{
#'    Deletes an entry, object of class \code{AtomEntry}
#'  }
#' }
#'
#' @examples
#'   #encoding
#'   atom <- AtomFeed$new()
#'   atom$setId("my-atom-feed")
#'   atom$setTitle("My Atom feed title")
#'   atom$setSubtitle("MyAtom feed subtitle")
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
#'   atom$setIcon("https://via.placeholder.com/300x150.png/03f/fff?text=atom4R")
#'   atom$setSelfLink("http://example.com/atom.feed")
#'   atom$setAlternateLink("http://example.com/my-atom-feed")
#'   atom$addCategory("draft", "dataset")
#'   atom$addCategory("world", "spatial")
#'   atom$addCategory("fisheries", "domain")

#'   #add entry
#'   entry <- AtomEntry$new()
#'   entry$setId("my-atom-entry")
#'   entry$setTitle("My Atom feed entry")
#'   entry$setSummary("My Atom feed entry very comprehensive abstract")
#'   author1 <- AtomAuthor$new(
#'     name = "John Doe",
#'     uri = "http://www.atomxml.com/johndoe",
#'     email = "johndoe@@atom4R.com"
#'   )
#'   entry$addAuthor(author1)
#'   author2 <- AtomAuthor$new(
#'     name = "John Doe's sister",
#'     uri = "http://www.atomxml.com/johndoesister",
#'     email = "johndoesister@@atom4R.com"
#'   )
#'   entry$addAuthor(author2)
#'   contrib1 <- AtomContributor$new(
#'     name = "Contrib1",
#'     uri = "http://www.atomxml.com/contrib1",
#'     email = "contrib1@@atom4R.com"
#'   )
#'   entry$addContributor(contrib1)
#'   contrib2 <- AtomContributor$new(
#'     name = "Contrib2",
#'     uri = "http://www.atomxml.com/contrib2",
#'     email = "contrib2@@atom4R.com"
#'   )
#'   entry$addContributor(contrib2)
#'   entry$addCategory("draft", "dataset")
#'   entry$addCategory("world", "spatial")
#'   entry$addCategory("fisheries", "domain")
#'   atom$addEntry(entry)
#'   xml <- atom$encode()
#'
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
AtomFeed <- R6Class("AtomFeed",
  inherit = AtomAbstractObject,
  lock_class = FALSE,
  lock_objects = FALSE,
  private = list(
    xmlElement = "feed",
    xmlNamespacePrefix = "ATOM",
    document =TRUE
  ),
  public = list(
    #' @field id Identifier
    id = NULL,
    #' @field updated Update date
    updated = Sys.time(),
    #' @field published Publication date
    published = NULL,
    #' @field title Title
    title = NULL,
    #' @field subtitle Subtitle
    subtitle = NULL,
    #' @field rights Rights (license, use, ...)
    rights = NULL,
    #' @field author Author person
    author = list(),
    #' @field contributor Contributor person
    contributor = list(),
    #' @field generator Generator
    generator = {
      gen = "atom4R"
      attr(gen, "uri") = "https://github.com/eblondel/atom4R"
      attr(gen, "version") = as(packageVersion("atom4R"), "character")
      gen
    },
    #' @field icon Icon
    icon = NULL,
    #' @field logo Logo
    logo = NULL,
    #' @field category Category
    category = list(),
    #'@field link links
    link = list(),
    #' @field entry List of entries
    entry = list(),

    #'@description Initializes a \link{AtomFeed}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
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
        stop("Atom feed 'updated' should be a 'Date' or 'POSIXt' object")
      }
      self$updated <- updated
    },

    #'@description Set published date
    #'@param published object of class \code{Date} or \code{POSIXt}
    setPublished = function(published){
      if(!is(published, "Date") & !is(published, "POSIXt")){
        stop("Atom feed 'published' should be a 'Date' or 'POSIXt' object")
      }
      self$published <- published
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

    #'@description Set self link
    #'@param link link
    #'@return \code{TRUE} if set, \code{FALSE} otherwise
    setSelfLink = function(link){
      if(length(self$link)>0) self$link <- self$link[sapply(self$link, function(x){x$attrs[["rel"]]!="self"})]
      self$addLink(link, rel = "self", type = "application/atom+xml")
    },

    #'@description Set alternate link
    #'@param link link
    #'@param type type. Default is "text/html"
    #'@return \code{TRUE} if set, \code{FALSE} otherwise
    setAlternateLink = function(link, type = "text/html"){
      if(length(self$link)>0) self$link <- self$link[sapply(self$link, function(x){x$attrs[["rel"]]!="alternate"})]
      self$addLink(link, rel = "alternate", type = type)
    },

    #'@description Set title
    #'@param title title
    #'@param type type. Default is "text"
    setTitle = function(title, type = "text"){
       self$title = self$createElement(title, type)
    },

    #'@description Set subtitle
    #'@param subtitle subtitle
    #'@param type type. Default is "text"
    setSubtitle = function(subtitle, type = "text"){
      self$subtitle = self$createElement(subtitle, type)
    },

    #'@description Set rights
    #'@param rights rights
    #'@param type type. Default is "text"
    setRights = function(rights, type = "text"){
      self$rights = self$createElement(rights, type)
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

    #'@description Set generator
    #'@param generator generator
    #'@param type type. Default is "text"
    setGenerator = function(generator, type = "text"){
      self$generator = self$createElement(generator, type)
    },

    #'@description Set icon
    #'@param icon icon
    setIcon = function(icon){
      self$icon = icon
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

    #'@description Adds an entry
    #'@param entry object of class \link{AtomEntry}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addEntry = function(entry){
      if(!is(entry, "AtomEntry")){
        stop("The 'entry' argument should be an 'AtomEntry' object")
      }
      return(self$addListElement("entry", entry))
    },

    #'@description Deletes an entry
    #'@param entry object of class \link{AtomEntry}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delEntry = function(entry){
      if(!is(entry, "AtomEntry")){
        stop("The 'entry' argument should be an 'AtomEntry' object")
      }
      return(self$delListElement("entry", entry))
    }

   )
)
