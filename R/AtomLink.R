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
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
AtomLink <- R6Class("AtomLink",
   inherit = AtomAbstractObject,
   private = list(
     xmlElement = "link",
     xmlNamespacePrefix = "ATOM"
   ),
   public = list(
     #'@field attrs attrs
     attrs = list(),

     #'@description Initializes an \link{AtomLink}
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param rel rel
     #'@param type type
     #'@param href href
     #'@param hreflang hreflang
     #'@param title title
     #'@param length length
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

     #'@description Set relation
     #'@param rel rel
     setRel = function(rel){
       self$attrs[["rel"]] <- rel
     },

     #'@description Set type
     #'@param type type
     setType = function(type){
       self$attrs[["type"]] <- type
     },

     #'@description Set href
     #'@param href href
     setHref = function(href){
       self$attrs[["href"]] <- href
     },

     #'@description Set href lang
     #'@param hreflang hreflang
     setHreflang = function(hreflang){
       self$attrs[["hreflang"]] <- hreflang
     },

     #'@description Set title
     #'@param title title
     setTitle = function(title){
       self$attrs[["title"]] <- title
     },

     #'@description Set length
     #'@param length length
     setLength = function(length){
       self$attrs[["length"]] <- length
     }
   )
)
