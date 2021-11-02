#' AtomLink
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#'
#' @name AtomLink
#' @title Atom Link class
#' @description This class models an atom Link
#' @keywords atom Link
#' @return Object of \code{\link{R6Class}} for modelling an Atom Link
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to create an Atom Link
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
AtomLink <- R6Class("AtomLink",
   inherit = AtomAbstractObject,
   private = list(
     xmlElement = "link",
     xmlNamespacePrefix = "ATOM"
   ),
   public = list(
     attrs = list(),
     initialize = function(xml = NULL,
                           rel = NULL, type = NULL,
                           href = NULL, hreflang = NULL,
                           title = NULL, length = NULL){
       super$initialize(xml = xml, wrap = FALSE)
       if(is.null(xml)){
         if(!is.null(rel)) self$setRel(rel)
         if(!is.null(type)) self$setType(type)
         if(!is.null(href)) self$setHref(href)
         if(!is.null(hreflang)) set$setHreflang(hreflang)
         if(!is.null(title)) self$setTitle(title)
         if(!is.null(length)) self$setLength(length)
       }

     },

     setRel = function(rel){
       self$attrs[["rel"]] <- rel
     },

     setType = function(type){
       self$attrs[["type"]] <- type
     },

     setHref = function(href){
       self$attrs[["href"]] <- href
     },

     setHreflang = function(hreflang){
       self$attrs[["hreflang"]] <- hreflang
     },

     setTitle = function(title){
       self$attrs[["title"]] <- title
     },

     setLength = function(length){
       self$attrs[["length"]] <- length
     }
   )
)
