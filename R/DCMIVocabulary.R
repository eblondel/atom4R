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
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(doc, format)}}{
#'    This method is used to read a DCMI vocabulary RDF doc. The format corresponds to
#'    the RDF format as used by \pkg{rdflib} \code{rdf_parse} function.
#'  }
#'  \item{\code{get()}}{
#'    Runs a Sparql query over the RDF vocabulary to return the vocabulary content.
#'    Returns an object of class \code{data.frame}
#'  }
#' }
#'
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
DCMIVocabulary <- R6Class("DCMIVocabulary",
  public = list(
    id = NULL,
    doc = NULL,
    representation = NULL,

    #initialize
    initialize = function(id, doc, format){
      self$id <- id
      self$doc <- doc
      self$representation <- rdflib::rdf_parse(doc, format = format)
    },

    #get
    get = function(){
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
      return(out)
    }
  )
)

#' setDCMIVocabularies
#' @export
setDCMIVocabularies <- function(){
  packageStartupMessage("Loading DCMI vocabularies...")
  vocabs <- list(
    DCMIVocabulary$new(
      id = "http://purl.org/dc/terms/",
      doc = system.file("extdata/vocabularies/dc/dublin_core_terms.rdf", package = "atom4R"),
      format = "rdfxml"
    ),
    DCMIVocabulary$new(
      id = "http://purl.org/dc/dcmitype/",
      doc = system.file("extdata/vocabularies/dc/dublin_core_type.rdf", package = "atom4R"),
      format = "rdfxml"
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
