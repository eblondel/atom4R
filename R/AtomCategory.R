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
#' @return Object of \code{R6Class} for modelling an Atom Category
#' @format \code{R6Class} object.
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
    #'@field attrs attrs
    attrs = list(),
    #'@field value value
    value = NULL,

    #'@description Initializes an \link{AtomCategory}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    #'@param term term
    #'@param scheme scheme
    #'@param label label
    initialize = function(xml = NULL, value = NULL,
                          term = NULL, scheme = NULL, label = NULL) {
      super$initialize(xml = xml, wrap = FALSE)
      if(is.null(xml)){
        self$value = value
        if(!is.null(term)) self$setTerm(term)
        if(!is.null(scheme)) self$setScheme(scheme)
        if(!is.null(label)) self$setLabel(label)
      }
    },

    #'@description Set term
    #'@param term term
    setTerm = function(term){
      self$attrs[["term"]] <- term
    },

    #'@description Set scheme
    #'@param scheme scheme
    setScheme = function(scheme){
      self$attrs[["scheme"]] <- scheme
    },

    #'@description Set label
    #'@param label label
    setLabel = function(label){
      self$attrs[["label"]] <- label
    }
  )
)
