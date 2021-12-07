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
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to create an Dublin Core entry
#'  }
#'  \item{\code{addDCElement(term, value)}}{
#'    Adds an element
#'  }
#'  \item{\code{delDCElement(term, value)}}{
#'    Deletes an element
#'  }
#'  \item{\code{addDCAbstract(abstract)}}{
#'    Adds an abstract
#'  }
#'  \item{\code{delDCAbstract(abstract)}}{
#'    Deletes an abstract
#'  }
#'  \item{\code{addDCAccessRights(accessRights)}}{
#'    Adds accessRights
#'  }
#'  \item{\code{delDCAccessRights(accessRights)}}{
#'    Deletes accessRights
#'  }
#'  \item{\code{addDCAccrualMethod(accrualMethod)}}{
#'    Adds accrualMethod
#'  }
#'  \item{\code{delDCAccrualMethod(accrualMethod)}}{
#'    Deletes accrualMethod
#'  }
#'  \item{\code{addDCAccrualPeriodicity(accrualPeriodicity)}}{
#'    Adds accrualPeriodicity
#'  }
#'  \item{\code{delDCAccrualPeriodicity(accrualPeriodicity)}}{
#'    Deletes accrualPeriodicity
#'  }
#'  \item{\code{addDCAccrualPolicy(accrualPolicy)}}{
#'    Adds accrualPolicy
#'  }
#'  \item{\code{delDCAccrualPolicy(accrualPolicy)}}{
#'    Deletes accrualPolicy
#'  }
#'  \item{\code{addDCAlternative(alternative)}}{
#'    Adds alternative title
#'  }
#'  \item{\code{delDCAlternative(alternative)}}{
#'    Deletes alternative title
#'  }
#'  \item{\code{addDCAudience(audience)}}{
#'    Adds an audience
#'  }
#'  \item{\code{delDCAudience(audience)}}{
#'    Deletes an audience
#'  }
#'  \item{\code{addDCAvailable(available)}}{
#'    Adds availability date/time
#'  }
#'  \item{\code{delDCAvailable(available)}}{
#'    Deletes availability date/time
#'  }
#'  \item{\code{addDCBibliographicCitation(bibliographicCitation)}}{
#'    Adds a bibliographic citation
#'  }
#'  \item{\code{delDCBibliographicCitation(bibliographicCitation)}}{
#'    Deletes a bibliographic citation
#'  }
#'  \item{\code{addDCConformsTo(conformsTo)}}{
#'    Adds a 'conformsTo' element
#'  }
#'  \item{\code{delDCConformsTo(conformsTo)}}{
#'    Deletes a 'conformsTo' element
#'  }
#'  \item{\code{addDCContributor(contributor)}}{
#'    Adds a contributor
#'  }
#'  \item{\code{delDCContributor(contributor)}}{
#'    Deletes a contributor
#'  }
#'  \item{\code{addDCCoverage(coverage)}}{
#'    Adds a coverage
#'  }
#'  \item{\code{delDCCoverage(coverage)}}{
#'    Deletes a coverage
#'  }
#'  \item{\code{addDCCreated(created)}}{
#'    Adds a created date/time
#'  }
#'  \item{\code{delDCCreated(created)}}{
#'    Deletes a created date/time
#'  }
#'  \item{\code{addDCCreator(creator)}}{
#'    Adds a creator
#'  }
#'  \item{\code{delDCCreator(creator)}}{
#'    Deletes a creator
#'  }
#'  \item{\code{addDCDate(date)}}{
#'    Adds a date
#'  }
#'  \item{\code{delDCDate(date)}}{
#'    Deletes a date
#'  }
#'  \item{\code{addDCDateAccepted(dateAccepted)}}{
#'    Adds a date (Accepted)
#'  }
#'  \item{\code{delDCDateAccepted(dateAccepted)}}{
#'    Deletes a date (Accepted)
#'  }
#'  \item{\code{addDCDateCopyrighted(dateCopyrighted)}}{
#'    Adds a date (Copyrighted)
#'  }
#'  \item{\code{delDCDateCopyrighted(dateCopyrighted)}}{
#'    Deletes a date (Copyrighted)
#'  }
#'  \item{\code{addDCDateSubmitted(dateSubmitted)}}{
#'    Adds a date (Submitted)
#'  }
#'  \item{\code{delDCDateSubmitted(dateSubmitted)}}{
#'    Deletes a date (Submitted)
#'  }
#'  \item{\code{addDCDescription(description)}}{
#'    Adds a description
#'  }
#'  \item{\code{delDCDescription(description)}}{
#'    Deletes a description
#'  }
#'  \item{\code{addDCEducationalLevel(educationalLevel)}}{
#'    Adds educational level
#'  }
#'  \item{\code{delDCEducationalLevel(educationalLevel)}}{
#'    Deletes educational level
#'  }
#'  \item{\code{addDCExtent(extent)}}{
#'    Adds extent
#'  }
#'  \item{\code{delDCExtent(extent)}}{
#'    Deletes extent
#'  }
#'  \item{\code{addDCFormat(format)}}{
#'    Adds format
#'  }
#'  \item{\code{delDCFormat(format)}}{
#'    Deletes format
#'  }
#'  \item{\code{addDCIdentifier(identifier)}}{
#'    Adds identifier
#'  }
#'  \item{\code{delDCIdentifier(identifier)}}{
#'    Deletes identifier
#'  }
#'  \item{\code{addDCInstructionalMethod(instructionalMethod)}}{
#'    Adds instructional method
#'  }
#'  \item{\code{delDCInstructionalMethod}}{
#'    Deletes instructional method
#'  }
#'  \item{\code{addDCIssued(issued)}}{
#'    Adds issuance date
#'  }
#'  \item{\code{delDCIssued(issued)}}{
#'    Deletes issuance date
#'  }
#'  \item{\code{addDCLanguage(language)}}{
#'    Adds language
#'  }
#'  \item{\code{delDCLanguage(language)}}{
#'    Deletes language
#'  }
#'  \item{\code{addDCLicense(license)}}{
#'    Adds license
#'  }
#'  \item{\code{delDCLicense(license)}}{
#'    Deletes license
#'  }
#'  \item{\code{addDCMediator(mediator)}}{
#'    Adds mediator
#'  }
#'  \item{\code{delDCMediator(mediator)}}{
#'    Deletes mediator
#'  }
#'  \item{\code{addDCMedium(medium)}}{
#'    Adds medium
#'  }
#'  \item{\code{delDCMedium(medium)}}{
#'    Deletes medium
#'  }
#'  \item{\code{addDCModified(modified)}}{
#'    Adds date of modification
#'  }
#'  \item{\code{delDCModified(modified)}}{
#'    Deletes date of modification
#'  }
#'  \item{\code{addDCProvenance(provenance)}}{
#'    Adds provenance
#'  }
#'  \item{\code{delDCProvenance(provenance)}}{
#'    Deletes provenance
#'  }
#'  \item{\code{addPublisher(publisher)}}{
#'    Adds publisher
#'  }
#'  \item{\code{delPublisher(publisher)}}{
#'    Deletes publisher
#'  }
#'  \item{\code{addDCReferences(references)}}{
#'    Adds references
#'  }
#'  \item{\code{delDCReferences(references)}}{
#'    Deletes references
#'  }
#'  \item{\code{addDCRelation(relation)}}{
#'    Adds relation
#'  }
#'  \item{\code{delDCRelation(relation)}}{
#'    Deletes relation
#'  }
#'  \item{\code{addDCReplaces(replaces)}}{
#'    Adds 'replaces' element
#'  }
#'  \item{\code{delDCReplaces(replaces)}}{
#'    Deletes 'replaces' element
#'  }
#'  \item{\code{addDCRequires(requires)}}{
#'    Adds 'requires' element
#'  }
#'  \item{\code{delDCRequires(requires)}}{
#'    Deletes 'requires' element
#'  }
#'  \item{\code{addDCRights(rights)}}{
#'    Adds rights
#'  }
#'  \item{\code{delDCRights(rights)}}{
#'    Deletes rights
#'  }
#'  \item{\code{addDCRightsHolder(rightsHolder)}}{
#'    Adds rights Holder
#'  }
#'  \item{\code{delDCRightsHolder(rightsHolder)}}{
#'    Deletes rights Holder
#'  }
#'  \item{\code{addDCSource(source)}}{
#'    Adds source
#'  }
#'  \item{\code{delDCSource(source)}}{
#'    Deletes source
#'  }
#'  \item{\code{addDCSubject(subject)}}{
#'    Adds subject
#'  }
#'  \item{\code{delDCSubject(subject)}}{
#'    Deletes subject
#'  }
#'  \item{\code{addDCTableOfContents(tableOfContents)}}{
#'    Adds table of contents
#'  }
#'  \item{\code{delDCTableOfContents(tableOfContents)}}{
#'    Deletes table of contents
#'  }
#'  \item{\code{addDCTemporal(temporal)}}{
#'    Adds temporal
#'  }
#'  \item{\code{delDCTemporal(temporal)}}{
#'    Deletes temporal
#'  }
#'  \item{\code{addDCTitle(title)}}{
#'    Adds title
#'  }
#'  \item{\code{delDCTitle(title)}}{
#'    Deletes title
#'  }
#'  \item{\code{addDCType(type)}}{
#'    Adds type
#'  }
#'  \item{\code{delDCType(type)}}{
#'    Deletes type
#'  }
#' }
#'
#' @examples
#'  \donttest{
#'   #encoding
#'   dcentry <- DCEntry$new()
#'   dcentry$setId("my-dc-entry")
#'
#'   #fill dc entry
#'   dcentry$addDCDate(Sys.time())
#'   dcentry$addDCTitle("atom4R - Tools to read/write and publish metadata as Atom XML format")
#'   dcentry$addDCType("Software")
#'   creator <- DCCreator$new(value = "Blondel, Emmanuel")
#'   creator$attrs[["affiliation"]] <- "Independent"
#'   dcentry$addDCCreator(creator)
#'   dcentry$addDCSubject("R")
#'   dcentry$addDCSubject("FAIR")
#'   dcentry$addDCSubject("Interoperability")
#'   dcentry$addDCSubject("Open Science")
#'   dcentry$addDCDescription("Atom4R offers tools to read/write and publish metadata as Atom XML")
#'   dcentry$addDCPublisher("GitHub")

#'   funder <- DCContributor$new(value = "CNRS")
#'   funder$attrs[["type"]] <- "Funder"
#'   dcentry$addDCContributor(funder)
#'   dcentry$addDCRelation("Github repository: https://github.com/eblondel/atom4R")
#'   dcentry$addDCSource("Atom Syndication format - https://www.ietf.org/rfc/rfc4287")
#'   dcentry$addDCSource("AtomPub, The Atom publishing protocol - https://tools.ietf.org/html/rfc5023")
#'   dcentry$addDCSource("Sword API - http://swordapp.org/")
#'   dcentry$addDCSource("Dublin Core Metadata Initiative - https://www.dublincore.org/")
#'   dcentry$addDCSource("Guidelines for implementing Dublin Core in XML")
#'   dcentry$addDCLicense("NONE")
#'   dcentry$addDCRights("MIT License")
#'
#'   xml <- dcentry$encode()
#'
#'   #decoding
#'   dcentry2 <- DCEntry$new(xml = xml)
#'   xml2 <- dcentry2$encode()
#'  }
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
