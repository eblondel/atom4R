#' SwordServiceDocument
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#'
#' @name SwordServiceDocument
#' @title SwordServiceDocument class
#' @description This class models an Sword service document
#' @keywords Atom Person
#' @return Object of \code{ \link[R6]{R6Class}} for modelling an Sword service document
#' @format \code{ \link[R6]{R6Class}} object.
#'
#' @note class used internally by \pkg{atom4R}
#'
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
SwordServiceDocument <- R6Class("SwordServiceDocument",
   inherit = atom4RLogger,
   public = list(
     #'@field title title
     title = NULL,
     #'@field collections collections
     collections = list(),

     #'@description Initializes a \link{SwordServiceDocument} from XML
     #'@param xml object of class \link[XML]{XMLInternalNode-class} from \pkg{XML}
     #'@param logger logger
     initialize = function(xml, logger = NULL){
       super$initialize(logger = logger)
       print(xml)
       title <- getNodeSet(xml, "//app:workspace/atom:title", c(app = "http://www.w3.org/2007/app", atom = "http://www.w3.org/2005/Atom"))
       if(length(title)>0) self$title <- xmlValue(title[[1]])
       cols <- getNodeSet(xml, "//ns:workspace/ns:collection", c(ns = "http://www.w3.org/2007/app"))
       self$collections <- lapply(cols, function(col){
         col_children <- xmlChildren(col)
         out_col <- list()
         out_col[["title"]] <- xmlValue(col_children$title)
         out_col[["policy"]] <- xmlValue(col_children$collectionPolicy)
         out_col[["href"]] <- xmlGetAttr(col, "href")
         return(out_col)
       })

     },

     #'@description Get title
     #'@return object of class \code{character}
     getTitle = function(){
       return(self$title)
     },

     #'@description Get collections
     #'@return object of class \code{character}
     getCollections = function(){
       return(self$collections)
     }
   )
)
