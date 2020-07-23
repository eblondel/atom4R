#' DCEntry
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#'
#' @name DCEntry
#' @title Dublin Core Entry class
#' @description This class models an Dublin Core Entry
#' @keywords Dublin Core dc Entry
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Entry
#' @format \code{\link{R6Class}} object.
#'
#'
#' @examples
#' \dontrun{

#' }
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to create an Dublin Core
#'  }
#' }
#'
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
DCEntry <- R6Class("DCEntry",
   inherit = AtomEntry,
   lock_class = FALSE,
   lock_objects = FALSE,
   private = list(
     xmlElement = "entry",
     xmlNamespacePrefix = "ATOM",
     document = TRUE
   ),
   public = list(

     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },

     #addDCElement
     addDCElement = function(term, value){
       elem <- NULL
       if(is(value,"DCElement")){
         elem <- value
       }else{
         clazz <- DCElement$getDCClassByElement(term)
         elem <- clazz$new(value = value)
       }
       if(is.null(self[[term]])) self[[term]] <- list()
       self[[term]][[length(self[[term]])+1]] <- elem
     },

     #delDCElement
     delDCElement = function(term, value){
       if(is.null(self[[term]])) self[[term]] <- list()
       termLength <- length(self[[term]])
       if(length(self[[term]])>0){
         self[[term]] <- self[[term]][sapply(self[[term]], function(x){
           valueToCompare <- value
           if(is(value, "DCElement")) valueToCompare <- value$value
           x$value != valueToCompare
         })]
       }
       return(length(self[[term]]) == termLength-1)
     },

     #addDCAbstract
     addDCAbstract = function(abstract){
       self$addDCElement("abstract", abstract)
     },

     #delDCAbstract
     delDCAbstract = function(abstract){
       self$delDCElement("abstract", abstract)
     },

     #addDCAccessRights
     addDCAccessRights = function(accessRights){
       self$addDCElement("accessRights", accessRights)
     },

     #delDCAccessRight
     delDCAccessRights = function(accessRights){
       self$delDCElement("accessRights", accessRights)
     },

     #addDCAccrualMethod
     addDCAccrualMethod = function(accrualMethod){
       self$addDCElement("accrualMethod", accrualMethod)
     },

     #delDCAccrualMethod
     delDCAccrualMethod = function(accrualMethod){
       self$delDCElement("accrualMethod", accrualMethod)
     },

     #addDCAccrualPeriodicity
     addDCAccrualPeriodicity = function(accrualPeriodicity){
       self$addDCElement("accrualPeriodicity", accrualPeriodicity)
     },

     #delDCAccrualPeriodicity
     delDCAccrualPeriodicity = function(accrualPeriodicity){
       self$delDCElement("accrualPeriodicity", accrualPeriodicity)
     },

     #addDCAccrualPolicy
     addDCAccrualPolicy = function(accrualPolicy){
       self$addDCElement("accrualPolicy", accrualPolicy)
     },

     #delDCAccrualPolicy
     delDCAccrualPolicy = function(accrualPolicy){
       self$delDCElement("accrualPolicy", accrualPolicy)
     },

     #addDCAlternative
     addDCAlternative = function(alternative){
       self$addDCElement("alternative", alternative)
     },

     #delDCAlternative
     delDCAlternative = function(alternative){
       self$delDCElement("alternative", alternative)
     },

     #addDCAudience
     addDCAudience = function(audience){
       self$addDCElement("audience", audience)
     },

     #delDCAudience
     delDCAudience = function(audience){
       self$delDCElement("audience", audience)
     },

     #addDCAvailable
     addDCAvailable = function(available){
       self$addDCElement("available", available)
     },

     #delDCAvailable
     delDCAvailable = function(available){
       self$delDCElement("available", available)
     },

     #addDCBibliographicCitation
     addDCBibliographicCitation = function(bibliographicCitation){
       self$addDCElement("bibliographicCitation", bibliographicCitation)
     },

     #delDCBibliographicCitation
     delDCBibliographicCitation = function(bibliographicCitation){
       self$delDCElement("bibliographicCitation", bibliographicCitation)
     },

     #addDCConformsTo
     addDCConformsTo = function(conformsTo){
       self$addDCElement("conformsTo", conformsTo)
     },

     #delDCConformsTo
     delDCConformsTo = function(conformsTo){
       self$delDCElement("conformsTo", conformsTo)
     },

     #addDCContributor
     addDCContributor = function(contributor){
       self$addDCElement("contributor", contributor)
     },

     #delDCContributor
     delDCContributor = function(contributor){
       self$delDCElement("contributor", contributor)
     },

     #addDCCoverage
     addDCCoverage = function(coverage){
       self$addDCElement("coverage", coverage)
     },

     #delDCCoverage
     delDCCoverage = function(coverage){
       self$delDCElement("coverage", coverage)
     },

     #addDCCreated
     addDCCreated = function(created){
       self$addDCElement("created", created)
     },

     #delDCCreated
     delDCCreated = function(created){
       self$delDCElement("created", created)
     },

     #addDCCreator
     addDCCreator = function(creator){
       self$addDCElement("creator", creator)
     },

     #delDCCreator
     delDCCreator = function(creator){
       self$delDCElement("creator", creator)
     },

     #addDCDate
     addDCDate = function(date){
       self$addDCElement("date", date)
     },

     #delDCDate
     delDCDate = function(date){
       self$delDCElement("date", date)
     },

     #addDCDateAccepted
     addDCDateAccepted = function(dateAccepted){
       self$addDCElement("dateAccepted",  dateAccepted)
     },

     #delDCDateAccepted
     delDCDateAccepted = function(dateAccepted){
       self$delDCElement("dateAccepted", dateAccepted)
     },

     #addDCDateCopyrighted
     addDCDateCopyrighted = function(dateCopyrighted){
       self$addDCElement("dateCopyrighted", dateCopyrighted)
     },

     #delDCDateCopyrighted
     delDCDateCopyrighted = function(dateCopyrighted){
       self$delDCElement("dateCopyrighted", dateCopyrighted)
     },

     #addDCDateSubmitted
     addDCDateSubmitted = function(dateSubmitted){
       self$addDCElement("dateSubmitted", dateSubmitted)
     },

     #delDCDateSubmitted
     delDCDateSubmitted = function(dateSubmitted){
       self$delDCElement("dateSubmitted", dateSubmitted)
     },

     #addDCDescription
     addDCDescription = function(description){
       self$addDCElement("description", description)
     },

     #delDCDescription
     delDCDescription = function(description){
       self$delDCElement("description", description)
     },

     #addDCEducationalLevel
     addDCEducationalLevel = function(educationalLevel){
       self$addDCElement("educationalLevel", educationalLevel)
     },

     #delDCEducationalLevel
     delDCEducationalLevel = function(educationalLevel){
       self$delDCElement("educationalLevel", educationalLevel)
     },

     #addDCExtent
     addDCExtent = function(extent){
       self$addDCElement("extent", extent)
     },

     #delDCExtent
     delDCExtent = function(extent){
       self$delDCElement("extent", extent)
     },

     #addDCFormat
     addDCFormat = function(format){
       self$addDCElement("format", format)
     },

     #delDCFormat
     delDCFormat = function(format){
       self$delDCElement("format", format)
     },

     #addDCIdentifier
     addDCIdentifier = function(identifier){
       self$addDCElement("identifier", identifier)
     },

     #delDCIdentifier
     delDCIdentifier = function(identifier){
       self$delDCElement("identifier", identifier)
     },

     #addDCInstructionalMethod
     addDCInstructionalMethod = function(instructionalMethod){
       self$addDCElement("instructionalMethod", instructionalMethod)
     },

     #delDCInstructionalMethod
     delDCInstructionalMethod = function(instructionalMethod){
       self$delDCElement("instructionalMethod", instructionalMethod)
     },

     #addDCIssued
     addDCIssued = function(issued){
       self$addDCElement("issued", issued)
     },

     #delDCIssued
     delDCIssued = function(issued){
       self$delDCElement("issued", issued)
     },

     #addDCLanguage
     addDCLanguage = function(language){
       self$addDCElement("language", language)
     },

     #delDCLanguage
     delDCLanguage = function(language){
       self$delDCElement("language", language)
     },

     #addDCLicense
     addDCLicense = function(license){
       self$addDCElement("license", license)
     },

     #delDCLicense
     delDCLicense = function(license){
       self$delDCElement("license", license)
     },

     #addDCMediator
     addDCMediator = function(mediator){
       self$addDCElement("mediator", mediator)
     },

     #delDCMediator
     delDCMediator = function(mediator){
       self$delDCElement("mediator", mediator)
     },

     #addDCMedium
     addDCMedium = function(medium){
       self$addDCElement("medium", medium)
     },

     #delDCMedium
     delDCMedium = function(medium){
       self$delDCElement("medium", medium)
     },

     #addDCModified
     addDCModified = function(modified){
       self$addDCElement("modified", modified)
     },

     #delDCModified
     delDCModified = function(modified){
       self$delDCElement("modified", modified)
     },

     #addDCProvenance
     addDCProvenance = function(provenance){
       self$addDCElement("provenance", provenance)
     },

     #delDCProvenance
     delDCProvenance = function(provenance){
       self$delDCElement("provenance", provenance)
     },

     #addDCPublisher
     addDCPublisher = function(publisher){
       self$addDCElement("publisher", publisher)
     },

     #delDCPublisher
     delDCPublisher = function(publisher){
       self$delDCElement("publisher", publisher)
     },

     #addDCReferences
     addDCReferences = function(references){
       self$addDCElement("references", references)
     },

     #delDCReferences
     delDCReferences = function(references){
       self$delDCElement("references", references)
     },

     #addDCRelation
     addDCRelation = function(relation){
       self$addDCElement("relation", relation)
     },

     #delDCRelation
     delDCRelation = function(relation){
       self$delDCElement("relation", relation)
     },

     #addDCReplaces
     addDCReplaces = function(replaces){
       self$addDCElement("replaces", replaces)
     },

     #delDCReplaces
     delDCReplaces = function(replaces){
       self$delDCElement("replaces", replaces)
     },

     #addDCRequires
     addDCRequires = function(requires){
       self$addDCElement("requires", requires)
     },

     #delDCRequires
     delDCRequires = function(requires){
       self$delDCElement("requires", requires)
     },

     #addDCRights
     addDCRights = function(rights){
       self$addDCElement("rights", rights)
     },

     #delDCRights
     delDCRights = function(rights){
       self$delDCElement("rights", rights)
     },

     #addDCRightsHolder
     addDCRightsHolder = function(rightsHolder){
       self$addDCElement("rightsHolder", rightsHolder)
     },

     #delDCRightsHolder
     delDCRightsHolder = function(rightsHolder){
       self$delDCElement("rightsHolder", rightsHolder)
     },

     #addDCSource
     addDCSource = function(source){
       self$addDCElement("source", source)
     },

     #delDCSource
     delDCSource = function(source){
       self$delDCElement("source", source)
     },

     #addDCSubject
     addDCSubject = function(subject){
       self$addDCElement("subject", subject)
     },

     #delDCSubject
     delDCSubject = function(subject){
       self$delDCElement("subject", subject)
     },

     #addDCTableOfContents
     addDCTableOfContents = function(tableOfContents){
       self$addDCElement("tableOfContents", tableOfContents)
     },

     #delDCTableOfContents
     delDCTableOfContents = function(tableOfContents){
       self$delDCElement("tableOfContents", tableOfContents)
     },

     #addDCTemporal
     addDCTemporal = function(temporal){
       self$addDCElement("temporal", temporal)
     },

     #delDCTemporal
     delDCTemporal = function(temporal){
       self$delDCElement("temporal", temporal)
     },

     #addDCTitle
     addDCTitle = function(title){
       self$addDCElement("title", title)
     },

     #delDCTitle
     delDCTitle = function(title){
       self$delDCElement("title", title)
     },

     #addDCType
     addDCType = function(type){
       self$addDCElement("type", type)
     },

     #delDCType
     delDCType = function(type){
       self$delDCElement("type", type)
     }

   )
)
