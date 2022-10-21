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
#'   dcentry$addDCHasPart("part1")
#'   dcentry$addDCHasPart("part2")
#'   dcentry$addDCHasVersion("0.2")
#'   dcentry$addDCIsPartOf("CRAN")
#'   dcentry$addDCIsPartOf("GitHub")
#'   dcentry$addDCIsReferencedBy("CRAN")
#'   dcentry$addDCIsReferencedBy("GitHub")
#'   dcentry$addDCIsRequiredBy("zen4R")
#'   dcentry$addDCIsRequiredBy("cloud4R")
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
     #'@param extended extended. Default is \code{FALSE}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCElement = function(term, value, extended = FALSE){
       elem <- NULL
       dc_classname <- paste("DC", toupper(substr(term, 1, 1)), substr(term, 2, nchar(term)), sep="")
       if(is(value,"DCElement")){
         if(!extended){
           if(is(value, dc_classname)){
             elem <- value
           }else{
             stop(sprintf("Value should be an object of class '%s'", dc_classname))
           }
         }else{
           elem <- value
         }
       }else{
         if(!extended){
           clazz <- eval(parse(text = paste0("atom4R::",dc_classname)))
           elem <- clazz$new(value = value)
         }else{
           clazz <- DCElement$getClassByElement(term)
           elem <- clazz$new(value = value)
         }
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

     #'@description Set a list of DC elements
     #'@param term term
     #'@param values vector of values
     setDCElements = function(term, values){
       dc_classname <- paste("DC", toupper(substr(term, 1, 1)), substr(term, 2, nchar(term)), sep="")
       clazz <- eval(parse(text = paste0("atom4R::",dc_classname)))
       self[[term]] <- lapply(values, function(x){ clazz$new(value = x) })
     },

     #'@description Get a list of DC elements
     #'@param term term
     #'@return a list of objects extending \link{DCElement}
     getDCElements = function(term){
        return(self[[term]])
     },

     #'@description Get a DC element by value
     #'@param term term
     #'@param value value
     getDCElementByValue = function(term, value){
        element <- NULL
        els <- self[[term]]
        els <- els[sapply(els, function(x){x$value == value})]
        if(length(els)>0) element <- els[[1]]
        return(element)
     },

     #'@description Adds DC abstract
     #'@param abstract object of class \link{DCAbstract} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCAbstract = function(abstract){
       self$addDCElement("abstract", abstract)
     },

     #'@description Deletes DC abstract
     #'@param abstract object of class \link{DCAbstract} or vector of class \link{character} and length 1
     delDCAbstract = function(abstract){
       self$delDCElement("abstract", abstract)
     },

     #'@description Set DC abstracts
     #'@param abstracts abstracts, vector of class \link{character}
     setDCAbstracts = function(abstracts){
        self$setDCElements("abstract", abstracts)
     },

     #'@description Get DC abstracts
     #'@return a list of objects of class \link{DCAbstract}
     getDCAbstracts = function(){
        self$getDCElements("abstract")
     },

     #'@description Adds DC access rights
     #'@param accessRights object of class \link{DCAccessRights} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCAccessRights = function(accessRights){
       self$addDCElement("accessRights", accessRights)
     },

     #'@description Deletes DC access rights
     #'@param accessRights object of class \link{DCAccessRights} or vector of class \link{character} and length 1
     delDCAccessRights = function(accessRights){
       self$delDCElement("accessRights", accessRights)
     },

     #'@description Set access rights
     #'@param accessRights vector of class \link{character}
     setDCAccessRights = function(accessRights){
       self$setDCElements("accessRights", accessRights)
     },

     #'@description Get DC access rights
     #'@return a list of objects of class \link{DCAccessRights}
     getDCAccessRights = function(){
       self$getDCElements("accessRights")
     },

     #'@description Adds DC accrual method
     #'@param accrualMethod object of class \link{DCAccrualMethod} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCAccrualMethod = function(accrualMethod){
       self$addDCElement("accrualMethod", accrualMethod)
     },

     #'@description Deletes DC accrual method
     #'@param accrualMethod object of class \link{DCAccrualMethod} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCAccrualMethod = function(accrualMethod){
       self$delDCElement("accrualMethod", accrualMethod)
     },

     #'@description Set DC accrual method
     #'@param accrualMethods vector of class \link{character}
     setDCAccrualMethods = function(accrualMethods){
       self$setDCElements("accrualMethod", accrualMethods)
     },

     #'@description Get DC accrual method
     #'@return a list of objects of class \link{DCAccrualMethod}
     getDCAccrualMethods = function(){
        self$getDCElements("accrualMethod")
     },

     #'@description Adds DC accrual periodicity
     #'@param accrualPeriodicity object of class \link{DCAccrualPeriodicity} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCAccrualPeriodicity = function(accrualPeriodicity){
       self$addDCElement("accrualPeriodicity", accrualPeriodicity)
     },

     #'@description Deletes DC accrual periodicity
     #'@param accrualPeriodicity object of class \link{DCAccrualPeriodicity} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCAccrualPeriodicity = function(accrualPeriodicity){
       self$delDCElement("accrualPeriodicity", accrualPeriodicity)
     },

     #'@description Set DC accrual periodicities
     #'@param accrualPeriodicities vector of class \link{character}
     setDCAccrualPeriodicities = function(accrualPeriodicities){
       self$setDCElements("accrualPeriodicity", accrualPeriodicities)
     },

     #'@description Get DC accrual periodicities
     #'@return a list of objects of class \link{DCAccrualPeriodicity}
     getDCAccrualPeriodicities = function(){
        self$getDCElements("accrualPeriodicity")
     },

     #'@description Adds DC accrual policy
     #'@param accrualPolicy object of class \link{DCAccrualPolicy} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCAccrualPolicy = function(accrualPolicy){
       self$addDCElement("accrualPolicy", accrualPolicy)
     },

     #'@description Deletes DC accrual policy
     #'@param accrualPolicy object of class \link{DCAccrualPolicy} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCAccrualPolicy = function(accrualPolicy){
       self$delDCElement("accrualPolicy", accrualPolicy)
     },

     #'@description Set DC accrual policies
     #'@param accrualPolicies vector of class \link{character}
     setDCAccrualPolicies = function(accrualPolicies){
       self$setDCElements("accrualPolicy", accrualPolicies)
     },

     #'@description Get DC accrual policies
     #'@return a list of objects of class \link{DCAccrualPolicy}
     getDCAccrualPolicies = function(){
       self$getDCElements("accrualPolicy")
     },

     #'@description Adds DC alternative
     #'@param alternative object of class \link{DCAlternative} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCAlternative = function(alternative){
       self$addDCElement("alternative", alternative)
     },

     #'@description Deletes DC alternative
     #'@param alternative object of class \link{DCAlternative} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCAlternative = function(alternative){
       self$delDCElement("alternative", alternative)
     },

     #'@description Set DC alternatives
     #'@param alternatives vector of class \link{character}
     setDCAlternatives = function(alternatives){
       self$setDCElements("alternative", alternatives)
     },

     #'@description Get DC alternatives
     #'@return a list of objects of class \link{DCAlternative}
     getDCAlternatives = function(){
       self$getDCElements("alternative")
     },

     #'@description Adds DC audience
     #'@param audience object of class \link{DCAudience} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCAudience = function(audience){
       self$addDCElement("audience", audience)
     },

     #'@description Deletes DC audience
     #'@param audience object of class \link{DCAudience} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCAudience = function(audience){
       self$delDCElement("audience", audience)
     },

     #'@description Set DC audiences
     #'@param audiences vector of class \link{character}
     setDCAudiences = function(audiences){
        self$setDCElements("audience", audiences)
     },

     #'@description Get DC audiences
     #'@return a list of objects of class \link{DCAudience}
     getDCAudiences = function(){
        self$getDCElements("audience")
     },

     #'@description Adds DC available
     #'@param available object of class \link{DCAvailable} or vector of class \link{Date},\link{POSIXt} or \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCAvailable = function(available){
       self$addDCElement("available", available)
     },

     #'@description Deletes DC available
     #'@param available object of class \link{DCAvailable} or vector of class \link{Date},\link{POSIXt} or \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCAvailable = function(available){
       self$delDCElement("available", available)
     },

     #'@description Set DC availables
     #'@param availables vector of class \link{character}
     setDCAvailables = function(availables){
       self$setDCElements("available", availables)
     },

     #'@description Get DC availables
     #'@return a list of objects of class \link{DCAvailable}
     getDCAvailables = function(){
       self$getDCElements("available")
     },

     #'@description Adds DC bibliographic citation
     #'@param bibliographicCitation object of class \link{DCBibliographicCitation} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCBibliographicCitation = function(bibliographicCitation){
       self$addDCElement("bibliographicCitation", bibliographicCitation)
     },

     #'@description Deletes DC bibliographic citation
     #'@param bibliographicCitation object of class \link{DCBibliographicCitation} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCBibliographicCitation = function(bibliographicCitation){
       self$delDCElement("bibliographicCitation", bibliographicCitation)
     },

     #'@description Set bibliographic citations
     #'@param bibliographicCitations vector of class \link{character}
     setDCBibliographicCitations = function(bibliographicCitations){
        self$setDCElements("bibliographicCitation", bibliographicCitations)
     },

     #'@description Get bibliographic citations
     #'@return the list of objects of class \link{DCBibliographicCitation}
     getDCBibliographicCitations = function(){
        self$getDCElements("bibliographicCitation")
     },

     #'@description Adds DC conforms to
     #'@param conformsTo object of class \link{DCConformsTo} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCConformsTo = function(conformsTo){
       self$addDCElement("conformsTo", conformsTo)
     },

     #'@description Deletes DC conforms to
     #'@param conformsTo object of class \link{DCConformsTo} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCConformsTo = function(conformsTo){
       self$delDCElement("conformsTo", conformsTo)
     },

     #'@description Set DC conforms to
     #'@param conformsTo vector of class \link{character}
     setDCConformsTo = function(conformsTo){
        self$setDCElements("conformsTo", conformsTo)
     },

     #'@description Get DC conforms to
     #'@return the list of objects of class \link{DCConformsTo}
     getDCConformsTo = function(){
        self$getDCElements("conformsTo")
     },

     #'@description Adds DC contributor
     #'@param contributor object of class \link{DCContributor} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCContributor = function(contributor){
       self$addDCElement("contributor", contributor)
     },

     #'@description Deletes DC contributor
     #'@param contributor object of class \link{DCContributor} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCContributor = function(contributor){
       self$delDCElement("contributor", contributor)
     },

     #'@description Set DC contributors
     #'@param contributors vector of class \link{character}
     setDCContributors = function(contributors){
        self$setDCElements("contributor", contributors)
     },

     #'@description Get DC contributors
     #'@return list of objects of class \link{DCContributor}
     getDCContributors = function(){
        self$getDCElements("contributor")
     },

     #'@description Adds DC coverage
     #'@param coverage object of class \link{DCCoverage} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCCoverage = function(coverage){
       self$addDCElement("coverage", coverage)
     },

     #'@description Deletes DC coverage
     #'@param coverage object of class \link{DCCoverage} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCCoverage = function(coverage){
       self$delDCElement("coverage", coverage)
     },

     #'@description Set DC coverages
     #'@param coverages coverages vector of class \link{character}
     setDCCoverages = function(coverages){
        self$setDCElements("coverage", coverages)
     },

     #'@description Get DC coverages
     #'@return a list of objects of class \link{DCCoverage}
     getDCCoverages = function(){
        self$getDCElements("coverage")
     },

     #'@description Adds DC created
     #'@param created object of class \link{DCCreated} or vector of class \link{Date},\link{POSIXt} or \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCCreated = function(created){
       self$addDCElement("created", created)
     },

     #'@description Deletes DC created
     #'@param created object of class \link{DCCreated} or vector of class \link{Date},\link{POSIXt} or \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCCreated = function(created){
       self$delDCElement("created", created)
     },

     #'@description Adds DC creator
     #'@param creator object of class \link{DCCreator} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCCreator = function(creator){
       self$addDCElement("creator", creator)
     },

     #'@description Deletes DC creator
     #'@param creator object of class \link{DCCreator} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCCreator = function(creator){
       self$delDCElement("creator", creator)
     },

     #'@description Set DC creators
     #'@param creators creators
     setDCCreators = function(creators){
        setDCElements("creator", creators)
     },

     #'@description Get DC creators
     #'@return a list of objects of class \link{DCCreator}
     getDCCreators = function(){
        self$getDCElements("creator")
     },

     #'@description Adds DC date
     #'@param date object of class \link{DCDate} or vector of class \link{Date},\link{POSIXt} or \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCDate = function(date){
       self$addDCElement("date", date)
     },

     #'@description Deletes DC date
     #'@param date object of class \link{DCDate} or vector of class \link{Date},\link{POSIXt} or \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCDate = function(date){
       self$delDCElement("date", date)
     },

     #'@description Set DC Creators
     #'@param dates dates vector of class \link{Date} or \link{POSIXt}
     setDCDates = function(dates){
       self$setDCElements("date", dates)
     },

     #'@description Get DC Dates
     #'@return a list of objects of class \link{DCDate}
     getDCDates = function(){
        self$getDCElements("date")
     },

     #'@description Adds DC date accepted
     #'@param dateAccepted object of class \link{DCDateAccepted} or vector of class \link{Date},\link{POSIXt} or \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCDateAccepted = function(dateAccepted){
       self$addDCElement("dateAccepted",  dateAccepted)
     },

     #'@description Deletes DC date accepted
     #'@param dateAccepted object of class \link{DCDateAccepted} or vector of class \link{Date},\link{POSIXt} or \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCDateAccepted = function(dateAccepted){
       self$delDCElement("dateAccepted", dateAccepted)
     },

     #'@description Adds DC date copyrighted
     #'@param dateCopyrighted object of class \link{DCDateCopyrighted} or vector of class \link{Date},\link{POSIXt} or \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCDateCopyrighted = function(dateCopyrighted){
       self$addDCElement("dateCopyrighted", dateCopyrighted)
     },

     #'@description Deletes DC date copyrighted
     #'@param dateCopyrighted object of class \link{DCDateCopyrighted} or vector of class \link{Date},\link{POSIXt} or \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCDateCopyrighted = function(dateCopyrighted){
       self$delDCElement("dateCopyrighted", dateCopyrighted)
     },

     #'@description Adds DC date submitted
     #'@param dateSubmitted object of class \link{DCDateSubmitted} or vector of class \link{Date},\link{POSIXt} or \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCDateSubmitted = function(dateSubmitted){
       self$addDCElement("dateSubmitted", dateSubmitted)
     },

     #'@description Deletes DC date submitted
     #'@param dateSubmitted object of class \link{DCDateSubmitted} or vector of class \link{Date},\link{POSIXt} or \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCDateSubmitted = function(dateSubmitted){
       self$delDCElement("dateSubmitted", dateSubmitted)
     },

     #'@description Adds DC description
     #'@param description object of class \link{DCDescription} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCDescription = function(description){
       self$addDCElement("description", description)
     },

     #'@description Deletes DC description
     #'@param description object of class \link{DCDescription} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCDescription = function(description){
       self$delDCElement("description", description)
     },

     #'@description Set DC descriptions
     #'@param descriptions vector of class \link{character}
     setDCDescriptions = function(descriptions){
       self$setDCElements("description", descriptions)
     },

     #'@description Get DC descriptions
     #'@return a list of objects of class \link{DCDescription}
     getDCDescriptions = function(){
       self$getDCElements("description")
     },

     #'@description Adds DC educational level
     #'@param educationalLevel object of class \link{DCEducationalLevel} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCEducationalLevel = function(educationalLevel){
       self$addDCElement("educationalLevel", educationalLevel)
     },

     #'@description Deletes DC educational level
     #'@param educationalLevel object of class \link{DCEducationalLevel} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCEducationalLevel = function(educationalLevel){
       self$delDCElement("educationalLevel", educationalLevel)
     },

     #'@description set DC education levels
     #'@param educationLevels vector of class \link{character}
     setDCEducationalLevels = function(educationLevels){
       self$setDCElements("educationLevel", educationLevels)
     },

     #'@description Get DC educational levels
     #'@return a list of objects of class \link{DCEducationalLevel}
     getDCEducationalLevels = function(){
        self$getDCElements("educationalLevel")
     },

     #'@description Adds DC extent
     #'@param extent object of class \link{DCExtent} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCExtent = function(extent){
       self$addDCElement("extent", extent)
     },

     #'@description Deletes DC extent
     #'@param extent object of class \link{DCExtent} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCExtent = function(extent){
       self$delDCElement("extent", extent)
     },

     #'@description Set DC extents
     #'@param extents vector of class \link{character}
     setDCExtents = function(extents){
        self$setDCElements("extent", extents)
     },

     #'@description Get DC extents
     #'@return a list of objects of class \link{DCExtent}
     getDCExtents = function(){
        self$getDCElements("extent")
     },

     #'@description Adds DC format
     #'@param format object of class \link{DCFormat} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCFormat = function(format){
       self$addDCElement("format", format)
     },

     #'@description Deletes DC format
     #'@param format object of class \link{DCFormat} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCFormat = function(format){
       self$delDCElement("format", format)
     },

     #'@description Set DC formats
     #'@param formats vector of class \link{character}
     setDCFormats = function(formats){
        self$setDCElements("format", formats)
     },

     #'@description Get DC formats
     #'@return a list of objects of class \link{DCFormat}
     getDCFormats = function(){
        self$getDCElements("format")
     },

     #'@description Adds DC hasPart
     #'@param hasPart object of class \link{DCHasPart} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCHasPart = function(hasPart){
        self$addDCElement("hasPart", hasPart)
     },

     #'@description Deletes DC hasPart
     #'@param hasPart object of class \link{DCHasPart} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCHasPart = function(hasPart){
        self$delDCElement("hasPart", hasPart)
     },

     #'@description Set DC hasParts
     #'@param hasParts vector of class \link{character}
     setDCHasParts = function(hasParts){
        self$setDCElements("hasPart", hasParts)
     },

     #'@description Get DC has part
     #'@return a list of objects of class \link{DCHasPart}
     getDCHasParts = function(){
        self$getDCElements("hasPart")
     },

     #'@description Adds DC hasVersion
     #'@param hasVersion object of class \link{DCHasVersion} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCHasVersion = function(hasVersion){
        self$addDCElement("hasVersion", hasVersion)
     },

     #'@description Deletes DC hasVersion
     #'@param hasVersion object of class \link{DCHasVersion} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCHasVersion = function(hasVersion){
        self$delDCElement("hasVersion", hasVersion)
     },

     #'@description Set DC hasVersions
     #'@param hasVersions vector of class \link{character}
     setDCHasVersions = function(hasVersions){
        self$setDCElements("hasVersion", hasVersions)
     },

     #'@description Get DC has versions
     #'@return a list of objects of class \link{DCHasVersion}
     getDCHasVersions = function(){
        self$getDCElements("hasVersion")
     },

     #'@description Adds DC identifier
     #'@param identifier object of class \link{DCIdentifier} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCIdentifier = function(identifier){
       self$addDCElement("identifier", identifier)
     },

     #'@description Deletes DC identifier
     #'@param identifier object of class \link{DCIdentifier} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCIdentifier = function(identifier){
       self$delDCElement("identifier", identifier)
     },

     #'@description Set DC identifiers
     #'@param identifiers vector of class \link{character}
     setDCIdentifiers = function(identifiers){
        self$setDCElements("identifier", identifiers)
     },

     #'@description Get DC identifiers
     #'@return a list of objects of class \link{DCIdentifier}
     getDCIdentifiers = function(){
       self$getDCElements("identifier")
     },

     #'@description Adds DC instructionalMethod
     #'@param instructionalMethod object of class \link{DCInstructionalMethod} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCInstructionalMethod = function(instructionalMethod){
       self$addDCElement("instructionalMethod", instructionalMethod)
     },

     #'@description Deletes DC instructionalMethod
     #'@param instructionalMethod object of class \link{DCInstructionalMethod} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCInstructionalMethod = function(instructionalMethod){
       self$delDCElement("instructionalMethod", instructionalMethod)
     },

     #'@description Set DC Instructional methods
     #'@param instructionalMethods vector of class \link{character}
     setDCInstructionalMethods = function(instructionalMethods){
        self$setDCElements("instructionalMethod", instructionalMethods)
     },

     #'@description Get DC instructional methods
     #'@return a list of objects of class \link{DCInstructionalMethod}
     getDCInstructionalMethods = function(){
       self$getDCElements("instructionalMethod")
     },

     #'@description Adds DC isPartOf
     #'@param isPartOf object of class \link{DCIsPartOf} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCIsPartOf = function(isPartOf){
        self$addDCElement("isPartOf", isPartOf)
     },

     #'@description Deletes DC isPartOf
     #'@param isPartOf object of class \link{DCIsPartOf} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCIsPartOf = function(isPartOf){
        self$delDCElement("isPartOf", isPartOf)
     },

     #'@description Set DC IsPartOf
     #'@param isPartOf vector of class \link{character}
     setDCIsPartOf = function(isPartOf){
        self$setDCElements("isPartOf", isPartOf)
     },

     #'@description Get DC Is Part of
     #'@return a list of objects of class \link{DCIsPartOf}
     getDCIsPartOfs = function(){
       self$getDCElements("isPartOf")
     },

     #'@description Adds DC isReferencedBy
     #'@param isReferencedBy object of class \link{DCIsReferencedBy} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCIsReferencedBy = function(isReferencedBy){
        self$addDCElement("isReferencedBy", isReferencedBy)
     },

     #'@description Deletes DC isReferencedBy
     #'@param isReferencedBy object of class \link{DCIsReferencedBy} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCIsReferencedBy = function(isReferencedBy){
        self$delDCElement("isReferencedBy", isReferencedBy)
     },

     #'@description Set DC isReferencedBys
     #'@param isReferencedBys vector of class \link{character}
     setDCIsReferencedBys = function(isReferencedBys){
        self$setDCElements("isReferencedBy", isReferencedBys)
     },

     #'@description Get DC Is Referenced by
     #'@return a list of objects of class \link{DCIsReferencedBy}
     getDCIsReferencedBys = function(){
        self$getDCElements("isReferencedBy")
     },

     #'@description Adds DC isReplacedBy
     #'@param isReplacedBy object of class \link{DCIsReplacedBy} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCIsReplacedBy = function(isReplacedBy){
        self$addDCElement("isReplacedBy", isReplacedBy)
     },

     #'@description Deletes DC isReferencedBy
     #'@param isReplacedBy object of class \link{DCIsReplacedBy} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCIsReplacedBy = function(isReplacedBy){
        self$delDCElement("isReplacedBy", isReplacedBy)
     },

     #'@description Set DC isReplacedBys
     #'@param isReplacedBys vector of class \link{character}
     setDCIsReplacedBys = function(isReplacedBys){
        self$setDCElements("isReplacedBy", isReplacedBys)
     },

     #'@description Get DC Is Replaced by
     #'@return a list of objects of class \link{DCIsReplacedBy}
     getDCIsReplacedBys = function(){
        self$getDCElements("isReplacedBy")
     },

     #'@description Adds DC isRequiredBy
     #'@param isRequiredBy object of class \link{DCIsRequiredBy} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCIsRequiredBy = function(isRequiredBy){
        self$addDCElement("isRequiredBy", isRequiredBy)
     },

     #'@description Deletes DC isRequiredBy
     #'@param isRequiredBy object of class \link{DCIsRequiredBy} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCIsRequiredBy = function(isRequiredBy){
        self$delDCElement("isRequiredBy", isRequiredBy)
     },

     #'@description Set DC isRequiredBys
     #'@param isRequiredBys vector of class \link{character}
     setDCIsRequiredBys = function(isRequiredBys){
       self$setDCElements("isRequiredBy", isRequiredBys)
     },

     #'@description Get DC Is Required by
     #'@return a list of objects of class \link{DCIsRequiredBy}
     getDCIsRequiredBys = function(){
        self$getDCElements("isRequiredBy")
     },

     #'@description Adds DC isVersionOf
     #'@param isVersionOf object of class \link{DCIsVersionOf} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCIsVersionOf = function(isVersionOf){
        self$addDCElement("isVersionOf", isVersionOf)
     },

     #'@description Deletes DC isVersionOf
     #'@param isVersionOf object of class \link{DCIsVersionOf} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCIsVersionOf = function(isVersionOf){
        self$delDCElement("isVersionOf", isVersionOf)
     },

     #'@description Set DC isVersionOfs
     #'@param isVersionOfs vector of class \link{character}
     setDCIsVersionOfs = function(isVersionOfs){
        self$setDCElements("isVersionOf", isVersionOfs)
     },

     #'@description Get DC Is Version Ofs
     #'@return a list of objects of class \link{DCIsVersionOf}
     getDCIsVersionOfs = function(){
        self$getDCElements("isVersionOf")
     },

     #'@description Adds DC issued
     #'@param issued object of class \link{DCIssued} or vector of class \link{Date},\link{POSIXt} or \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCIssued = function(issued){
       self$addDCElement("issued", issued)
     },

     #'@description Deletes DC issued
     #'@param issued object of class \link{DCIssued} or vector of class \link{Date},\link{POSIXt} or \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCIssued = function(issued){
       self$delDCElement("issued", issued)
     },

     #'@description Adds DC language
     #'@param language object of class \link{DCLanguage} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCLanguage = function(language){
       self$addDCElement("language", language)
     },

     #'@description Deletes DC language
     #'@param language object of class \link{DCLanguage} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCLanguage = function(language){
       self$delDCElement("language", language)
     },

     #'@description Set DC languages
     #'@param languages languages vector of class \link{character}
     setDCLanguages = function(languages){
       self$setDCElements("language", languages)
     },

     #'@description Get languages
     #'@return a list of objects of class \link{DCLanguage}
     getDCLanguages = function(){
       self$getDCElements("language")
     },

     #'@description Adds DC license
     #'@param license object of class \link{DCLicense} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCLicense = function(license){
       self$addDCElement("license", license)
     },

     #'@description Deletes DC license
     #'@param license object of class \link{DCLicense} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCLicense = function(license){
       self$delDCElement("license", license)
     },

     #'@description Set DC licences
     #'@param licenses vector of class \link{character}
     setDCLicenses = function(licenses){
       set$DCElements("license", licenses)
     },

     #'@description Get DC licenses
     #'@return a list of objects of class \link{DCLicense}
     getDCLicenses = function(){
        self$getDCElements("license")
     },

     #'@description Adds DC mediator
     #'@param mediator object of class \link{DCMediator} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCMediator = function(mediator){
       self$addDCElement("mediator", mediator)
     },

     #'@description Deletes DC mediator
     #'@param mediator object of class \link{DCMediator} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCMediator = function(mediator){
       self$delDCElement("mediator", mediator)
     },

     #'@description Set DC mediators
     #'@param mediators vector of class \link{character}
     setDCMediators = function(mediators){
       self$setDCElements("mediator", mediators)
     },

     #'@description Get DC mediators
     #'@return a list of objects of class \link{DCMediator}
     getDCMediators = function(){
        self$getDCElements("mediator")
     },

     #'@description Adds DC medium
     #'@param medium object of class \link{DCMedium} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCMedium = function(medium){
       self$addDCElement("medium", medium)
     },

     #'@description Deletes DC medium
     #'@param medium object of class \link{DCMedium} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCMedium = function(medium){
       self$delDCElement("medium", medium)
     },

     #'@description Set DC mediums
     #'@param mediums vector of class \link{character}
     setDCMediums = function(mediums){
       self$setDCElements("medium", mediums)
     },

     #'@description Get DC mediums
     #'@return a list of objects of class \link{DCMedium}
     getDCMediums = function(){
        self$getDCElements("medium")
     },

     #'@description Adds DC modified
     #'@param modified object of class \link{DCModified} or vector of class \link{Date},\link{POSIXt} or \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCModified = function(modified){
       self$addDCElement("modified", modified)
     },

     #'@description Deletes DC modified
     #'@param modified object of class \link{DCModified} or vector of class \link{Date},\link{POSIXt} or \link{character} and length 1
     #'@return \code{TRUE} if deletes, \code{FALSE} otherwise
     delDCModified = function(modified){
       self$delDCElement("modified", modified)
     },

     #'@description Adds DC provenance
     #'@param provenance object of class \link{DCProvenance} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCProvenance = function(provenance){
       self$addDCElement("provenance", provenance)
     },

     #'@description Deletes DC provenance
     #'@param provenance object of class \link{DCProvenance} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCProvenance = function(provenance){
       self$delDCElement("provenance", provenance)
     },

     #'@description Set DC provenances
     #'@param provenances vector of class \link{character}
     setDCProvenances = function(provenances){
        self$setDCElements("provenance", provenances)
     },

     #'@description Get DC provenances
     #'@return a list of objects of class \link{DCProvenance}
     getDCProvenances = function(){
        self$getDCElements("provenance")
     },

     #'@description Adds DC publisher
     #'@param publisher object of class \link{DCPublisher} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCPublisher = function(publisher){
       self$addDCElement("publisher", publisher)
     },

     #'@description Deletes DC publisher
     #'@param publisher object of class \link{DCPublisher} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCPublisher = function(publisher){
       self$delDCElement("publisher", publisher)
     },

     #'@description Set DC publishers
     #'@param publishers vector of class \link{character}
     setDCPublishers = function(publishers){
        self$setDCElements("publisher", publishers)
     },

     #'@description Get DC publishers
     #'@return a list of objects of class \link{DCPublisher}
     getDCPublishers = function(){
       self$getDCElements("publisher")
     },

     #'@description Adds DC references
     #'@param references object of class \link{DCReferences} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCReferences = function(references){
       self$addDCElement("references", references)
     },

     #'@description Deletes DC references
     #'@param references object of class \link{DCReferences} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCReferences = function(references){
       self$delDCElement("references", references)
     },

     #'@description Set DC references
     #'@param references vector of class \link{character}
     setDCReferences = function(references){
        self$setDCElements("references", references)
     },

     #'@description Get DC references
     #'@return a list of objects of class \link{DCReferences}
     getDCReferences = function(){
        self$getDCElements("references")
     },

     #'@description Adds DC relation
     #'@param relation object of class \link{DCRelation} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCRelation = function(relation){
       self$addDCElement("relation", relation)
     },

     #'@description Deletes DC relation
     #'@param relation object of class \link{DCRelation} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCRelation = function(relation){
       self$delDCElement("relation", relation)
     },

     #'@description Set DC relations
     #'@param relations vector of class \link{character}
     setDCRelations = function(relations){
        self$setDCElements("relation", relations)
     },

     #'@description Get DC relations
     #'@return a list of objects of class \link{DCRelation}
     getDCRelations = function(){
       self$getDCElements("relation")
     },

     #'@description Adds DC replaces
     #'@param replaces object of class \link{DCReplaces} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCReplaces = function(replaces){
       self$addDCElement("replaces", replaces)
     },

     #'@description Deletes DC replaces
     #'@param replaces object of class \link{DCReplaces} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCReplaces = function(replaces){
       self$delDCElement("replaces", replaces)
     },

     #'@description Set DC replaces
     #'@param replaces vector of class \link{character}
     setDCReplaces = function(replaces){
       self$setDCElements("replace", replaces)
     },

     #'@description Get DC replaces
     #'@return a list of objects of class \link{DCReplaces}
     getDCReplaces = function(){
        self$getDCElements("replaces")
     },

     #'@description Adds DC requires
     #'@param requires object of class \link{DCRequires} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCRequires = function(requires){
       self$addDCElement("requires", requires)
     },

     #'@description Deletes DC requires
     #'@param requires object of class \link{DCRequires} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCRequires = function(requires){
       self$delDCElement("requires", requires)
     },

     #'@description Set DC requires
     #'@param requires vector of class \link{character}
     setDCRequires = function(requires){
       self$setDCElements("requires", requires)
     },

     #'@description Get DC requires
     #'@return a list of objects of class \link{DCRequires}
     getDCRequires = function(){
        self$getDCElements("requires")
     },

     #'@description Adds DC rights
     #'@param rights object of class \link{DCRights} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCRights = function(rights){
       self$addDCElement("rights", rights)
     },

     #'@description Deletes DC rights
     #'@param rights object of class \link{DCRights} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCRights = function(rights){
       self$delDCElement("rights", rights)
     },

     #'@description Set DC rights
     #'@param rights vector of class \link{character}
     setDCRights = function(rights){
        self$setDCElements("rights", rights)
     },

     #'@description Get DC rights
     #'@return a list of objects of class \link{DCRights}
     getDCRights = function(){
        self$getDCElements("rights")
     },

     #'@description Adds DC rightsHolder
     #'@param rightsHolder object of class \link{DCRightsHolder} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCRightsHolder = function(rightsHolder){
       self$addDCElement("rightsHolder", rightsHolder)
     },

     #'@description Deletes DC rightsHolder
     #'@param rightsHolder object of class \link{DCRightsHolder} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCRightsHolder = function(rightsHolder){
       self$delDCElement("rightsHolder", rightsHolder)
     },

     #'@description Set DC rights holders
     #'@param rightsHolders vector of class \link{character}
     setDCRightsHolders = function(rightsHolders){
       self$setDCElements("rightsHolder", rightsHolders)
     },

     #'@description Get DC rights holders
     #'@return a list of objects of class \link{DCRightsHolder}
     getDCRightsHolders = function(){
       self$getDCElements("rightsHolder")
     },

     #'@description Adds DC source
     #'@param source object of class \link{DCSource} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCSource = function(source){
       self$addDCElement("source", source)
     },

     #'@description Deletes DC source
     #'@param source object of class \link{DCSource} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCSource = function(source){
       self$delDCElement("source", source)
     },

     #'@description Set DC sources
     #'@param sources vector of class \link{character}
     setDCSources = function(sources){
       self$setDCSources("source", sources)
     },

     #'@description Get DC sources
     #'@return a list of objects of class \link{DCSource}
     getDCSources = function(){
        self$getDCElements("source")
     },

     #'@description Adds DC subject
     #'@param subject object of class \link{DCSubject} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCSubject = function(subject){
       self$addDCElement("subject", subject)
     },

     #'@description Deletes DC subject
     #'@param subject object of class \link{DCSubject} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCSubject = function(subject){
       self$delDCElement("subject", subject)
     },

     #'@description Set DC subjects
     #'@param subjects vector of class \link{character}
     setDCSubjects = function(subjects){
       self$setDCElements("subject", subjects)
     },

     #'@description Get DC Subjects
     #'@return a list of objects of class \link{DCSubject}
     getDCSubjects = function(){
        self$getDCElements("subject")
     },

     #'@description Adds DC tableOfContents
     #'@param tableOfContents object of class \link{DCTableOfContents} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCTableOfContents = function(tableOfContents){
       self$addDCElement("tableOfContents", tableOfContents)
     },

     #'@description Deletes DC tableOfContents
     #'@param tableOfContents object of class \link{DCTableOfContents} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCTableOfContents = function(tableOfContents){
       self$delDCElement("tableOfContents", tableOfContents)
     },

     #'@description Set DC tables of contents
     #'@param tablesOfContents vector of class \link{character}
     setDCTablesOfContents = function(tablesOfContents){
       self$setDCElements("tableOfContents", tablesOfContents)
     },

     #'@description Get DC tables of contents
     #'@return a list of objects of class \link{DCTableOfContents}
     getDCTablesOfContent = function(){
        self$getDCElements("tableOfContents")
     },

     #'@description Adds DC temporal
     #'@param temporal object of class \link{DCTemporal} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCTemporal = function(temporal){
       self$addDCElement("temporal", temporal)
     },

     #'@description Deletes DC temporal
     #'@param temporal object of class \link{DCTemporal} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCTemporal = function(temporal){
       self$delDCElement("temporal", temporal)
     },

     #'@description Set DC temporals
     #'@param temporals vector of class \link{character}
     setDCTemporals = function(temporals){
       self$setDCElements("temporal", temporals)
     },

     #'@description Get DC temporals
     #'@return a list of objects of class \link{DCTemporal}
     getDCTemporals = function(){
        self$getDCElements("temporal")
     },

     #'@description Adds DC title
     #'@param title object of class \link{DCTitle} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCTitle = function(title){
       self$addDCElement("title", title)
     },

     #'@description Deletes DC title
     #'@param title object of class \link{DCTitle} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCTitle = function(title){
       self$delDCElement("title", title)
     },

     #'@description Set DC titles
     #'@param titles vector of class \link{character}
     setDCTitles = function(titles){
       self$setDCElements("title", titles)
     },

     #'@description Get DC titles
     #'@return a list of objects of class \link{DCTitle}
     getDCTitles = function(){
        self$getDCElements("title")
     },

     #'@description Adds DC type
     #'@param type object of class \link{DCType} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDCType = function(type){
       self$addDCElement("type", type)
     },

     #'@description Deletes DC type
     #'@param type object of class \link{DCType} or vector of class \link{character} and length 1
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDCType = function(type){
       self$delDCElement("type", type)
     },

     #'@description Set DC Types
     #'@param types vector of class \link{character}
     setDCTypes = function(types){
        self$setDCElements("type", types)
     },

     #'@description Get DC types
     #'@return a list of objects of class \link{DCType}
     getDCTypes = function(){
        self$getDCElements("type")
     },

     #'@description export to a \code{data.frame}
     #'@return an object of class \code{data.frame}
     asDataFrame = function(){
        xml = self$encode(validate = F)
        out <- data.frame(
          "term" = names(XML::xmlChildren(xml)),
          "value" = sapply(xmlChildren(xml), xmlValue)
        )
        out <- out[out$term != "comment",]
        return(out)
     }
   )
)
