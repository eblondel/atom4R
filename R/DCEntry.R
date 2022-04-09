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

     #'@description Initializes an object of class \link{DCEntry}
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },

     #'@description Adds a Dublin Core element
     #'@param term term
     #'@param value value
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCElement = function(term, value){
       elem <- NULL
       if(is(value,"DCElement")){
         elem <- value
       }else{
         clazz <- DCElement$getClassByElement(term)
         elem <- clazz$new(value = value)
       }
       if(is.null(self[[term]])) self[[term]] <- list()
       self[[term]][[length(self[[term]])+1]] <- elem
     },

     #'@description Deletes a Dublin Core element
     #'@param term term
     #'@param value value
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
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

     #'@description Adds DC abstract
     #'@param abstract object of class \link{DCAbstract}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCAbstract = function(abstract){
       self$addDCElement("abstract", abstract)
     },

     #'@description Deletes DC abstract
     #'@param abstract object of class \link{DCAbstract}
     delDCAbstract = function(abstract){
       self$delDCElement("abstract", abstract)
     },

     #'@description Adds DC access rights
     #'@param accessRights object of class \link{DCAccessRights}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCAccessRights = function(accessRights){
       self$addDCElement("accessRights", accessRights)
     },

     #'@description Deletes DC access rights
     #'@param accessRights object of class \link{DCAccessRights}
     delDCAccessRights = function(accessRights){
       self$delDCElement("accessRights", accessRights)
     },

     #'@description Adds DC accrual method
     #'@param accrualMethod object of class \link{DCAccrualMethod}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCAccrualMethod = function(accrualMethod){
       self$addDCElement("accrualMethod", accrualMethod)
     },

     #'@description Deletes DC accrual method
     #'@param accrualMethod object of class \link{DCAccrualMethod}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCAccrualMethod = function(accrualMethod){
       self$delDCElement("accrualMethod", accrualMethod)
     },

     #'@description Adds DC accrual periodicity
     #'@param accrualPeriodicity object of class \link{DCAccrualPeriodicity}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCAccrualPeriodicity = function(accrualPeriodicity){
       self$addDCElement("accrualPeriodicity", accrualPeriodicity)
     },

     #'@description Deletes DC accrual periodicity
     #'@param accrualPeriodicity object of class \link{DCAccrualPeriodicity}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCAccrualPeriodicity = function(accrualPeriodicity){
       self$delDCElement("accrualPeriodicity", accrualPeriodicity)
     },

     #'@description Adds DC accrual policy
     #'@param accrualPolicy object of class \link{DCAccrualPolicy}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCAccrualPolicy = function(accrualPolicy){
       self$addDCElement("accrualPolicy", accrualPolicy)
     },

     #'@description Deletes DC accrual policy
     #'@param accrualPolicy object of class \link{DCAccrualPolicy}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCAccrualPolicy = function(accrualPolicy){
       self$delDCElement("accrualPolicy", accrualPolicy)
     },

     #'@description Adds DC alternative
     #'@param alternative object of class \link{DCAlternative}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCAlternative = function(alternative){
       self$addDCElement("alternative", alternative)
     },

     #'@description Deletes DC alternative
     #'@param alternative object of class \link{DCAlternative}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCAlternative = function(alternative){
       self$delDCElement("alternative", alternative)
     },

     #'@description Adds DC audience
     #'@param audience object of class \link{DCAudience}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCAudience = function(audience){
       self$addDCElement("audience", audience)
     },

     #'@description Deletes DC audience
     #'@param audience object of class \link{DCAudience}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCAudience = function(audience){
       self$delDCElement("audience", audience)
     },

     #'@description Adds DC available
     #'@param available object of class \link{DCAvailable}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCAvailable = function(available){
       self$addDCElement("available", available)
     },

     #'@description Deletes DC available
     #'@param available object of class \link{DCAvailable}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCAvailable = function(available){
       self$delDCElement("available", available)
     },

     #'@description Adds DC bibliographic citation
     #'@param bibliographicCitation object of class \link{DCBibliographicCitation}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCBibliographicCitation = function(bibliographicCitation){
       self$addDCElement("bibliographicCitation", bibliographicCitation)
     },

     #'@description Deletes DC bibliographic citation
     #'@param bibliographicCitation object of class \link{DCBibliographicCitation}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCBibliographicCitation = function(bibliographicCitation){
       self$delDCElement("bibliographicCitation", bibliographicCitation)
     },

     #'@description Adds DC conforms to
     #'@param conformsTo object of class \link{DCConformsTo}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCConformsTo = function(conformsTo){
       self$addDCElement("conformsTo", conformsTo)
     },

     #'@description Deletes DC conforms to
     #'@param conformsTo object of class \link{DCConformsTo}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCConformsTo = function(conformsTo){
       self$delDCElement("conformsTo", conformsTo)
     },

     #'@description Adds DC contributor
     #'@param contributor object of class \link{DCContributor}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCContributor = function(contributor){
       self$addDCElement("contributor", contributor)
     },

     #'@description Deletes DC contributor
     #'@param contributor object of class \link{DCContributor}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCContributor = function(contributor){
       self$delDCElement("contributor", contributor)
     },

     #'@description Adds DC coverage
     #'@param coverage object of class \link{DCCoverage}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCCoverage = function(coverage){
       self$addDCElement("coverage", coverage)
     },

     #'@description Deletes DC coverage
     #'@param coverage object of class \link{DCCoverage}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCCoverage = function(coverage){
       self$delDCElement("coverage", coverage)
     },

     #'@description Adds DC created
     #'@param created object of class \link{DCCreated}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCCreated = function(created){
       self$addDCElement("created", created)
     },

     #'@description Deletes DC created
     #'@param created object of class \link{DCCreated}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCCreated = function(created){
       self$delDCElement("created", created)
     },

     #'@description Adds DC creator
     #'@param creator object of class \link{DCCreator}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCCreator = function(creator){
       self$addDCElement("creator", creator)
     },

     #'@description Deletes DC creator
     #'@param creator object of class \link{DCCreator}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCCreator = function(creator){
       self$delDCElement("creator", creator)
     },

     #'@description Adds DC date
     #'@param date object of class \link{DCDate}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCDate = function(date){
       self$addDCElement("date", date)
     },

     #'@description Deletes DC date
     #'@param date object of class \link{DCDate}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCDate = function(date){
       self$delDCElement("date", date)
     },

     #'@description Adds DC date accepted
     #'@param dateAccepted object of class \link{DCDateAccepted}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCDateAccepted = function(dateAccepted){
       self$addDCElement("dateAccepted",  dateAccepted)
     },

     #'@description Deletes DC date accepted
     #'@param dateAccepted object of class \link{DCDateAccepted}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCDateAccepted = function(dateAccepted){
       self$delDCElement("dateAccepted", dateAccepted)
     },

     #'@description Adds DC date copyrighted
     #'@param dateCopyrighted object of class \link{DCDateCopyrighted}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCDateCopyrighted = function(dateCopyrighted){
       self$addDCElement("dateCopyrighted", dateCopyrighted)
     },

     #'@description Deletes DC date copyrighted
     #'@param dateCopyrighted object of class \link{DCDateCopyrighted}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCDateCopyrighted = function(dateCopyrighted){
       self$delDCElement("dateCopyrighted", dateCopyrighted)
     },

     #'@description Adds DC date submitted
     #'@param dateSubmitted object of class \link{DCDateSubmitted}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCDateSubmitted = function(dateSubmitted){
       self$addDCElement("dateSubmitted", dateSubmitted)
     },

     #'@description Deletes DC date submitted
     #'@param dateSubmitted object of class \link{DCDateSubmitted}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCDateSubmitted = function(dateSubmitted){
       self$delDCElement("dateSubmitted", dateSubmitted)
     },

     #'@description Adds DC description
     #'@param description object of class \link{DCDescription}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCDescription = function(description){
       self$addDCElement("description", description)
     },

     #'@description Deletes DC description
     #'@param description object of class \link{DCDescription}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCDescription = function(description){
       self$delDCElement("description", description)
     },

     #'@description Adds DC educational level
     #'@param educationalLevel object of class \link{DCEducationalLevel}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCEducationalLevel = function(educationalLevel){
       self$addDCElement("educationalLevel", educationalLevel)
     },

     #'@description Deletes DC educational level
     #'@param educationalLevel object of class \link{DCEducationalLevel}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCEducationalLevel = function(educationalLevel){
       self$delDCElement("educationalLevel", educationalLevel)
     },

     #'@description Adds DC extent
     #'@param extent object of class \link{DCExtent}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCExtent = function(extent){
       self$addDCElement("extent", extent)
     },

     #'@description Deletes DC extent
     #'@param extent object of class \link{DCExtent}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCExtent = function(extent){
       self$delDCElement("extent", extent)
     },

     #'@description Adds DC format
     #'@param format object of class \link{DCFormat}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCFormat = function(format){
       self$addDCElement("format", format)
     },

     #'@description Deletes DC format
     #'@param format object of class \link{DCFormat}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCFormat = function(format){
       self$delDCElement("format", format)
     },

     #'@description Adds DC identifier
     #'@param identifier object of class \link{DCIdentifier}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCIdentifier = function(identifier){
       self$addDCElement("identifier", identifier)
     },

     #'@description Deletes DC identifier
     #'@param identifier object of class \link{DCIdentifier}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCIdentifier = function(identifier){
       self$delDCElement("identifier", identifier)
     },

     #'@description Adds DC instructionalMethod
     #'@param instructionalMethod object of class \link{DCInstructionalMethod}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCInstructionalMethod = function(instructionalMethod){
       self$addDCElement("instructionalMethod", instructionalMethod)
     },

     #'@description Deletes DC instructionalMethod
     #'@param instructionalMethod object of class \link{DCInstructionalMethod}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCInstructionalMethod = function(instructionalMethod){
       self$delDCElement("instructionalMethod", instructionalMethod)
     },

     #'@description Adds DC issued
     #'@param issued object of class \link{DCIssued}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCIssued = function(issued){
       self$addDCElement("issued", issued)
     },

     #'@description Deletes DC issued
     #'@param issued object of class \link{DCIssued}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCIssued = function(issued){
       self$delDCElement("issued", issued)
     },

     #'@description Adds DC language
     #'@param language object of class \link{DCLanguage}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCLanguage = function(language){
       self$addDCElement("language", language)
     },

     #'@description Deletes DC language
     #'@param language object of class \link{DCLanguage}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCLanguage = function(language){
       self$delDCElement("language", language)
     },

     #'@description Adds DC license
     #'@param license object of class \link{DCLicense}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCLicense = function(license){
       self$addDCElement("license", license)
     },

     #'@description Deletes DC license
     #'@param license object of class \link{DCLicense}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCLicense = function(license){
       self$delDCElement("license", license)
     },

     #'@description Adds DC mediator
     #'@param mediator object of class \link{DCMediator}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCMediator = function(mediator){
       self$addDCElement("mediator", mediator)
     },

     #'@description Deletes DC mediator
     #'@param mediator object of class \link{DCMediator}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCMediator = function(mediator){
       self$delDCElement("mediator", mediator)
     },

     #'@description Adds DC medium
     #'@param medium object of class \link{DCMedium}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCMedium = function(medium){
       self$addDCElement("medium", medium)
     },

     #'@description Deletes DC medium
     #'@param medium object of class \link{DCMedium}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCMedium = function(medium){
       self$delDCElement("medium", medium)
     },

     #'@description Adds DC modified
     #'@param modified object of class \link{DCModified}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCModified = function(modified){
       self$addDCElement("modified", modified)
     },

     #'@description Deletes DC modified
     #'@param modified object of class \link{DCModified}
     #'@return \code{TRUE} if deletes, \code{FALSE} otherwise
     delDCModified = function(modified){
       self$delDCElement("modified", modified)
     },

     #'@description Adds DC provenance
     #'@param provenance object of class \link{DCProvenance}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCProvenance = function(provenance){
       self$addDCElement("provenance", provenance)
     },

     #'@description Deletes DC provenance
     #'@param provenance object of class \link{DCProvenance}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCProvenance = function(provenance){
       self$delDCElement("provenance", provenance)
     },

     #'@description Adds DC publisher
     #'@param publisher object of class \link{DCPublisher}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCPublisher = function(publisher){
       self$addDCElement("publisher", publisher)
     },

     #'@description Deletes DC publisher
     #'@param publisher object of class \link{DCPublisher}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCPublisher = function(publisher){
       self$delDCElement("publisher", publisher)
     },

     #'@description Adds DC references
     #'@param references object of class \link{DCReferences}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCReferences = function(references){
       self$addDCElement("references", references)
     },

     #'@description Deletes DC references
     #'@param references object of class \link{DCReferences}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCReferences = function(references){
       self$delDCElement("references", references)
     },

     #'@description Adds DC relation
     #'@param relation object of class \link{DCRelation}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCRelation = function(relation){
       self$addDCElement("relation", relation)
     },

     #'@description Deletes DC relation
     #'@param relation object of class \link{DCRelation}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCRelation = function(relation){
       self$delDCElement("relation", relation)
     },

     #'@description Adds DC replaces
     #'@param replaces object of class \link{DCReplaces}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCReplaces = function(replaces){
       self$addDCElement("replaces", replaces)
     },

     #'@description Deletes DC replaces
     #'@param replaces object of class \link{DCReplaces}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCReplaces = function(replaces){
       self$delDCElement("replaces", replaces)
     },

     #'@description Adds DC requires
     #'@param requires object of class \link{DCRequires}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCRequires = function(requires){
       self$addDCElement("requires", requires)
     },

     #'@description Deletes DC requires
     #'@param requires object of class \link{DCRequires}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCRequires = function(requires){
       self$delDCElement("requires", requires)
     },

     #'@description Adds DC rights
     #'@param rights object of class \link{DCRights}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCRights = function(rights){
       self$addDCElement("rights", rights)
     },

     #'@description Deletes DC rights
     #'@param rights object of class \link{DCRights}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCRights = function(rights){
       self$delDCElement("rights", rights)
     },

     #'@description Adds DC rightsHolder
     #'@param rightsHolder object of class \link{DCRightsHolder}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCRightsHolder = function(rightsHolder){
       self$addDCElement("rightsHolder", rightsHolder)
     },

     #'@description Deletes DC rightsHolder
     #'@param rightsHolder object of class \link{DCRightsHolder}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCRightsHolder = function(rightsHolder){
       self$delDCElement("rightsHolder", rightsHolder)
     },

     #'@description Adds DC source
     #'@param source object of class \link{DCSource}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCSource = function(source){
       self$addDCElement("source", source)
     },

     #'@description Deletes DC source
     #'@param source object of class \link{DCSource}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCSource = function(source){
       self$delDCElement("source", source)
     },

     #'@description Adds DC subject
     #'@param subject object of class \link{DCSubject}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCSubject = function(subject){
       self$addDCElement("subject", subject)
     },

     #'@description Deletes DC subject
     #'@param subject object of class \link{DCSubject}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCSubject = function(subject){
       self$delDCElement("subject", subject)
     },

     #'@description Adds DC tableOfContents
     #'@param tableOfContents object of class \link{DCTableOfContents}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCTableOfContents = function(tableOfContents){
       self$addDCElement("tableOfContents", tableOfContents)
     },

     #'@description Deletes DC tableOfContents
     #'@param tableOfContents object of class \link{DCTableOfContents}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCTableOfContents = function(tableOfContents){
       self$delDCElement("tableOfContents", tableOfContents)
     },

     #'@description Adds DC temporal
     #'@param temporal object of class \link{DCTemporal}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCTemporal = function(temporal){
       self$addDCElement("temporal", temporal)
     },

     #'@description Deletes DC temporal
     #'@param temporal object of class \link{DCTemporal}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCTemporal = function(temporal){
       self$delDCElement("temporal", temporal)
     },

     #'@description Adds DC title
     #'@param title object of class \link{DCTitle}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCTitle = function(title){
       self$addDCElement("title", title)
     },

     #'@description Deletes DC title
     #'@param title object of class \link{DCTitle}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCTitle = function(title){
       self$delDCElement("title", title)
     },

     #'@description Adds DC type
     #'@param type object of class \link{DCType}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCType = function(type){
       self$addDCElement("type", type)
     },

     #'@description Deletes DC type
     #'@param type object of class \link{DCType}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCType = function(type){
       self$delDCElement("type", type)
     }

   )
)
