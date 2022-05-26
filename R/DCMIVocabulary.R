#' DCMIVocabulary
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#'
#' @name DCMIVocabulary
#' @title DCMI Vocabulary class
#' @description This class models an DCMI Vocabulary
#' @keywords Dublin Core element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core element
#' @format \code{\link{R6Class}} object.
#'
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
DCMIVocabulary <- R6Class("DCMIVocabulary",
  public = list(
    #'@field id id
    id = NULL,
    #'@field doc doc
    doc = NULL,
    #'@field representation representation
    representation = NULL,
    #'@field data data
    data = NULL,

    #'@description This method is used to read a DCMI vocabulary RDF doc. The format corresponds to
    #'    the RDF format as used by \pkg{rdflib} \code{rdf_parse} function.
    #'@param id id
    #'@param doc doc
    #'@param format format
    #'@param fetch fetch
    initialize = function(id, doc, format, fetch = TRUE){
      self$id <- id
      self$doc <- doc
      self$representation <- rdflib::rdf_parse(doc, format = format)
      if(fetch) self$fetch()
    },

    #'@description Runs a Sparql query over the RDF vocabulary to fetch the vocabulary content.
    fetch = function(){
      if(is.null(self$data)){
        sparql <-
          'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           PREFIX owl: <http://www.w3.org/2002/07/owl#>
           PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
           PREFIX dcam: <http://purl.org/dc/dcam/>
           PREFIX dcterms: <http://purl.org/dc/terms/>
           PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
           SELECT *
           WHERE {
            ?s dcterms:issued ?issued.
            ?s rdfs:comment ?comment.
            ?s rdfs:isDefinedBy ?isDefinedBy.
            ?s rdfs:label ?label.
            OPTIONAL{ ?s dcam:memberOf ?memberOf. }
            OPTIONAL{ ?s dcterms:description ?description }
           }
        '
        out <- rdflib::rdf_query(self$representation, query = sparql)
        out <- as.data.frame(out)
        self$data <- out
      }
    }
  )
)

#' setDCMIVocabularies
#' @export
setDCMIVocabularies <- function(){
  vocabs <- list(
    DCMIVocabulary$new(
      id = "http://purl.org/dc/terms/",
      doc = system.file("extdata/vocabularies/dc/dublin_core_terms.rdf", package = "atom4R"),
      format = "rdfxml", fetch = TRUE
    ),
    DCMIVocabulary$new(
      id = "http://purl.org/dc/dcmitype/",
      doc = system.file("extdata/vocabularies/dc/dublin_core_type.rdf", package = "atom4R"),
      format = "rdfxml", fetch = TRUE
    )
  )
  names(vocabs) <- sapply(vocabs, function(x){x$id})
  .atom4R$dcmi_vocabularies <- vocabs
}

#' @name getDCMIVocabularies
#' @aliases getDCMIVocabularies
#' @title getDCMIVocabularies
#' @export
#' @description \code{getDCMIVocabularies} allows to get the list of DCMI Vocabularies
#' registered in \pkg{atom4R}
#'
#' @usage getDCMIVocabularies()
#'
#' @examples
#'   getDCMIVocabularies()
#'
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
getDCMIVocabularies <- function(){
  return(.atom4R$dcmi_vocabularies)
}

#' @name getDCMIVocabulary
#' @aliases getDCMIVocabulary
#' @title getDCMIVocabulary
#' @export
#' @description \code{getDCMIVocabulary} allows to get a registered DCMI Vocabulary by id
#' registered in \pkg{atom4R}
#'
#' @usage getDCMIVocabulary(id)
#'
#' @param id identifier of the vocabulary
#'
#' @examples
#'   getDCMIVocabulary(id = "http://purl.org/dc/dcmitype/")
#'
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
getDCMIVocabulary <- function(id){
  vocabulary <- NULL
  invisible(lapply(getDCMIVocabularies(), function(vocab){
    if(vocab$id == id){
      vocabulary <<- vocab
    }
  }))
  return(vocabulary)
}
