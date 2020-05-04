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
    xmlNamespacePrefix = "ATOM"
  ),
  public = list(

    published = NULL,

    initialize = function(xml = NULL){
      super$initialize(xml = xml, wrap = FALSE)
    },

    #setPublished
    setPublished = function(published){
      if(!is(published, "Date") & !is(published, "POSIXt")){
        stop("Atom feed 'published' should be a 'Date' or 'POSIXt' object")
      }
      self$published <- published
    }



  )
)
