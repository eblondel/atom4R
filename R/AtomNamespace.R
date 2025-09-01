#' AtomNamespace
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO metadata namespace
#' @return Object of \code{ \link[R6]{R6Class}} for modelling an Atom Namespace
#' @format \code{ \link[R6]{R6Class}} object.
#'
#' @note ISO class used internally by atom4R for specifying XML namespaces
#'
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
AtomNamespace <- R6Class("AtomNamespace",
  public = list(
    #'@field id id
    id = NA,
    #'@field uri uri
    uri = NA,

    #'@description Initializes an \link{AtomNamespace}
    #'@param id id
    #'@param uri uri
    initialize = function(id, uri){
      self$id = id
      self$uri = uri
    },

    #'@description Get definition
    #'@return a named list defining the namespace
    getDefinition = function(){
      ns <- list(self$uri)
      names(ns) <- self$id
      return(ns)
    }
  )
)
AtomNamespace$ATOM = AtomNamespace$new("atom", "http://www.w3.org/2005/Atom")
AtomNamespace$DC = AtomNamespace$new("dc", "http://purl.org/dc/elements/1.1/")
AtomNamespace$DCTERMS = AtomNamespace$new("dcterms", "http://purl.org/dc/terms/")
AtomNamespace$XLINK = AtomNamespace$new("xlink", "http://www.w3.org/1999/xlink")
AtomNamespace$XSI = AtomNamespace$new("xsi", "http://www.w3.org/2001/XMLSchema-instance")

#' setMetadataNamespaces
#' @export
setAtomNamespaces <- function(){
  .atom4R$namespaces <- list(
    AtomNamespace$ATOM,
    AtomNamespace$DC,
    AtomNamespace$DCTERMS,
    AtomNamespace$XLINK,
    AtomNamespace$XSI
  )
}

#' @name getAtomNamespaces
#' @aliases getAtomNamespaces
#' @title getAtomNamespaces
#' @export
#' @description \code{getAtomNamespaces} gets the list of namespaces registered
#'
#' @usage getAtomNamespaces()
#'
#' @examples
#'   getAtomNamespaces()
#'
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
getAtomNamespaces = function(){
  return(.atom4R$namespaces)
}

#' @name getAtomNamespace
#' @aliases getAtomNamespace
#' @title getAtomNamespace
#' @export
#' @description \code{getAtomNamespace} gets a namespace given its id
#'
#' @usage getAtomNamespace(id)
#'
#' @param id namespace prefix
#'
#' @examples
#'   getAtomNamespace("GMD")
#'
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
getAtomNamespace = function(id){
  return(AtomNamespace[[id]])
}

#' @name registerAtomNamespace
#' @aliases registerAtomNamespace
#' @title registerAtomNamespace
#' @export
#' @description \code{registerAtomNamespace} allows to register a new namespace
#' in \pkg{atom4R}
#'
#' @usage registerAtomNamespace(id, uri, force)
#'
#' @param id prefix of the namespace
#' @param uri URI of the namespace
#' @param force logical parameter indicating if registration has be to be forced
#' in case the identified namespace is already registered
#'
#' @examples
#'   registerAtomNamespace(id = "myprefix", uri = "http://someuri")
#'
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
registerAtomNamespace <- function(id, uri, force = FALSE){
  ns <- getAtomNamespace(toupper(id))
  if(!is.null(ns)){
    if(!force) stop(sprintf("Atom Namespace with id '%s' already exists. Use force = TRUE to force registration", id))
    ns <- AtomNamespace$new(id, uri)
    AtomNamespace[[toupper(id)]] <- ns
    .atom4R$namespaces[sapply(.atom4R$namespaces, function(x){x$id == id})][[1]] <- ns
  }else{
    ns <- AtomNamespace$new(id, uri)
    AtomNamespace[[toupper(id)]] <- ns
    .atom4R$namespaces <- c(.atom4R$namespaces, ns)
  }
}
