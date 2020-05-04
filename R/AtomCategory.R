#' AtomCategory
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#'
#' @name AtomCategory
#' @title Atom Category class
#' @description This class models an atom Category
#' @keywords atom Category
#' @return Object of \code{\Category{R6Class}} for modelling an Atom Category
#' @format \code{\Category{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to create an Atom Category
#'  }
#'  \item{\code{setHref(href)}}{
#'    Sets the href
#'  }
#'  \item{\code{setHreflang(hreflang)}}{
#'    Sets the href lang
#'  }
#'  \item{\code{setRel(rel)}}{
#'    Sets the rel
#'  }
#'  \item{\code{setType(type)}}{
#'    Sets the type
#'  }
#'  \item{\code{setTitle(title)}}{
#'    Sets the title
#'  }
#'  \item{\code{setLength(length)}}{
#'    Sets the length
#'  }
#' }
#'
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
AtomCategory <- R6Class("AtomCategory",
  inherit = AtomAbstractObject,
  private = list(
    xmlElement = "category",
    xmlNamespacePrefix = "ATOM"
  ),
  public = list(
    attrs = list(),
    initialize = function(xml = NULL,
                          term = NULL, scheme = NULL, label = NULL) {
      super$initialize(xml = xml, wrap = FALSE)
      if(is.null(xml)){
        if(!is.null(term)) self$setTerm(term)
        if(!is.null(scheme)) self$setScheme(scheme)
        if(!is.null(label)) self$setLabel(label)
      }
    },

    setTerm = function(term){
      self$attrs[["term"]] <- term
    },

    setScheme = function(scheme){
      self$attrs[["scheme"]] <- scheme
    },

    setLabel = function(label){
      self$attrs[["label"]] <- label
    }
  )
)
