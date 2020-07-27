#' AtomPerson
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#'
#' @name AtomPerson
#' @title Atom Person class
#' @description This class models an Atom Person
#' @keywords Atom Person
#' @return Object of \code{\link{R6Class}} for modelling an Atom Person
#' @format \code{\link{R6Class}} object.
#'
#' @field name Person name
#' @field uri Person uri
#' @field email Person email
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, name, uri, email)}}{
#'    This method is used to create an Atom
#'  }
#'  \item{\code{setName(name)}}{
#'    Set person name
#'  }
#'  \item{\code{setUri(uri)}}{
#'    Set person uri
#'  }
#'  \item{\code{setEmail(email)}}{
#'    Set person email
#'  }
#' }
#'
#' @note Abstract class used internally for person-like classes
#'
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
AtomPerson <- R6Class("AtomPerson",
   inherit = AtomAbstractObject,
   private = list(
     xmlElement = "person",
     xmlNamespacePrefix = "ATOM",
     document = FALSE
   ),
   public = list(
    name = NA,
    uri = NA,
    email = NA,

    #initialize
    initialize = function(xml = NULL, name = NULL, uri = NULL, email = NULL){
      super$initialize(xml = xml, wrap = FALSE)
      if(!is.null(name)) self$setName(name)
      if(!is.null(uri)) self$setUri(uri)
      if(!is.null(email)) self$setEmail(email)
    },

    #setName
    setName = function(name){
      self$name <- name
    },

    #setUri
    setUri = function(uri){
      self$uri <- uri
    },

    #setEmail
    setEmail = function(email){
      self$email <- email
    }
   )
)

#' AtomAuthor
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#'
#' @name AtomAuthor
#' @title Atom Author class
#' @description This class models an Atom Author
#' @keywords Atom Author
#' @return Object of \code{\link{R6Class}} for modelling an Atom Author
#' @format \code{\link{R6Class}} object.
#'
#' @field name Person name
#' @field uri Person uri
#' @field email Person email
#'
#' @examples
#' \dontrun{
#'   author <- AtomAuthor$new(name "John Doe", email = "john.doe@@atom4R.com")
#' }
#'
#' @section Methods inherited from \code{AtomPerson}:
#' \describe{
#'  \item{\code{new(xml, name, uri, email)}}{
#'    This method is used to create an Atom
#'  }
#'  \item{\code{setName(name)}}{
#'    Set person name
#'  }
#'  \item{\code{setUri(uri)}}{
#'    Set person uri
#'  }
#'  \item{\code{setEmail(email)}}{
#'    Set person email
#'  }
#' }
#'
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
AtomAuthor <- R6Class("AtomAuthor",
  inherit = AtomPerson,
  private = list(
    xmlElement = "author",
    xmlNamespacePrefix = "ATOM",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, name = NULL, uri = NULL, email = NULL){
      super$initialize(xml = xml, name = name, uri = uri, email = email)
    }
  )
)

#' AtomContributor
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#'
#' @name AtomContributor
#' @title Atom Contributorr class
#' @description This class models an Atom Contributor
#' @keywords Atom Author
#' @return Object of \code{\link{R6Class}} for modelling an Atom Contributor
#' @format \code{\link{R6Class}} object.
#'
#' @field name Person name
#' @field uri Person uri
#' @field email Person email
#'
#' @examples
#' \dontrun{
#'   contrib <- AtomContributor$new(name "John Doe", email = "john.doe@@atom4R.com")
#' }
#'
#' @section Methods inherited from \code{AtomPerson}:
#' \describe{
#'  \item{\code{new(xml, name, uri, email)}}{
#'    This method is used to create an Atom
#'  }
#'  \item{\code{setName(name)}}{
#'    Set person name
#'  }
#'  \item{\code{setUri(uri)}}{
#'    Set person uri
#'  }
#'  \item{\code{setEmail(email)}}{
#'    Set person email
#'  }
#' }
#'
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
AtomContributor <- R6Class("AtomContributor",
  inherit = AtomPerson,
  private = list(
    xmlElement = "contributor",
    xmlNamespacePrefix = "ATOM",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, name = NULL, uri = NULL, email = NULL){
      super$initialize(xml = xml, name = name, uri = uri, email = email)
    }
  )
)
