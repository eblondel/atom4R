#' DCElement
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#'
#' @name DCElement
#' @title DublinCore element class
#' @description This class models an DublinCore element
#' @keywords Dublin Core element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core element
#' @format \code{\link{R6Class}} object.
#'
#' @note Class used internally by \pkg{atom4R}
#'
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
DCElement <- R6Class("DCElement",
   inherit = AtomAbstractObject,
   private = list(
     dcTerms = function() {
       dcTermsVocabId <- "http://purl.org/dc/terms/"
       terms <- getDCMIVocabulary(id = dcTermsVocabId)$data
       sapply(terms$s, function(x){unlist(strsplit(x, dcTermsVocabId))[2]})
     },
     xmlElement = "_abstract_",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(
     #'@field value value
     value = NULL,

     #'@description Initializes an abstract \link{DCElement}
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param term term
     #'@param value value
     #'@param vocabulary vocabulary
     #'@param extended extended
     initialize = function(xml = NULL, term = NULL, value = NULL, vocabulary = NULL, extended = FALSE){
       super$initialize(xml = xml, element = term, wrap = FALSE)
       if(is.null(xml)){
         if(!extended) if(!term %in% private$dcTerms()){
           stop(sprintf("'%s' is not a valid Dublin Core term", term))
         }
         if(!is.null(vocabulary)){
           vocab <- getDCMIVocabulary(id = vocabulary)
           if(is.null(vocab$data)){
             vocab$fetch()
           }
           if(is.null(vocab)){
             stop(sprintf("No controlled vocabulary for id '%s'", vocabulary))
           }
           if(!value %in% vocab$data$label){
             errMsg <- sprintf("Value '%s' not authorized by DCMI controlled vocabulary for term '%s'.\n", value, term)
             errMsg <- paste0(errMsg, sprintf("Controlled vocabulary can be browsed in R with the following code:\nvocab = getDCMIVocabulary(id = \"%s\");\nvocab$fetch();\nvocab$data", vocabulary))
             stop(errMsg)
           }
         }
         self$value = value
       }
     }
   )
)

DCElement$getClasses = function(extended = FALSE, pretty = FALSE){
  getClassesInheriting(classname = "DCElement", extended = extended, pretty = pretty)
}

DCElement$getClassByElement = function(element){
  dc_classes <- DCElement$getClasses(extended = TRUE, pretty = TRUE)
  dc_class <- dc_classes[dc_classes$element == element,]
  clazz <- try(eval(parse(text=dc_class$class)))
  if(is(clazz, "try-error")) clazz <- try(eval(parse(text=paste0("atom4R::", dc_class$class))))
  return(clazz)
}

#' @name DCAbstract
#' @title DCAbstract
#' @description This class models an DublinCore 'abstract' element
#' @keywords Dublin Core 'abstract' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'abstract' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#' Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/abstract
#'
DCAbstract <- R6Class("DCAbstract",
  inherit = DCDescription,
  private = list(
    xmlElement = "abstract",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCAbstract}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCAccessRights
#' @title DCAccessRights
#' @description This class models an DublinCore 'accessRights' element
#' @keywords Dublin Core 'accessRights' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'accessRights' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#' Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/accessRights
#'
DCAccessRights <- R6Class("DCAccessRights",
  inherit = DCRights,
  private = list(
    xmlElement = "accessRights",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCAccessRights}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCAccrualMethod
#' @title DCAccrualMethod
#' @description This class models an DublinCore 'accrualMethod' element
#' @keywords Dublin Core 'accrualMethod' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'accrualMethod' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#' Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/accrualMethod
#'
DCAccrualMethod <- R6Class("DCAccrualMethod",
  inherit = DCElement,
  private = list(
    xmlElement = "accrualMethod",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCAccrualMethod}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCAccrualPeriodicity
#' @title DCAccrualPeriodicity
#' @description This class models an DublinCore 'accrualPeriodicity' element
#' @keywords Dublin Core 'accrualPeriodicity' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'accrualPeriodicity' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#' Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/accrualPeriodicity
#'
DCAccrualPeriodicity <- R6Class("DCAccrualPeriodicity",
   inherit = DCElement,
   private = list(
     xmlElement = "accrualPeriodicity",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(

     #'@description Initializes an object of class \link{DCAccrualPeriodicity}
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param value value
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#' @name DCAccrualPolicy
#' @title DCAccrualPolicy
#' @description This class models an DublinCore 'accrualPolicy' element
#' @keywords Dublin Core 'accrualPolicy' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'accrualPolicy' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#' Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/accrualPolicy
#'
DCAccrualPolicy <- R6Class("DCAccrualPolicy",
  inherit = DCElement,
  private = list(
    xmlElement = "accrualPolicy",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCAccrualPolicy}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCAlternative
#' @title DCAlternative
#' @description This class models an DublinCore 'alternative' element
#' @keywords Dublin Core 'alternative' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'alternative' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#' Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/alternative
#'
DCAlternative <- R6Class("DCAlternative",
   inherit = DCTitle,
   private = list(
     xmlElement = "alternative",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(

     #'@description Initializes an object of class \link{DCAlternative}
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param value value
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#' @name DCAudience
#' @title DCAudience
#' @description This class models an DublinCore 'audience' element
#' @keywords Dublin Core 'audience' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'audience' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#' Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/audience
#'
DCAudience <- R6Class("DCAudience",
   inherit = DCElement,
   private = list(
     xmlElement = "audience",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(

     #'@description Initializes an object of class \link{DCAudience}
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param term term
     #'@param value value
     initialize = function(xml = NULL, term = NULL, value = NULL){
       if(is.null(term)) term <- private$xmlElement
       super$initialize(xml = xml, term = term, value = value)
     }
   )
)

#' @name DCAvailable
#' @title DCAvailable
#' @description This class models an DublinCore 'available' element
#' @keywords Dublin Core 'available' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'available' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#' Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/available
#'
DCAvailable <- R6Class("DCAvailable",
  inherit = DCDate,
  private = list(
    xmlElement = "available",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCAvailable}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCBibliographicCitation
#' @title DCBibliographicCitation
#' @description This class models an DublinCore 'bibliographicCitation' element
#' @keywords Dublin Core 'bibliographicCitation' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'bibliographicCitation' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/bibliographicCitation
#'
DCBibliographicCitation <- R6Class("DCBibliographicCitation",
 inherit = DCIdentifier,
 private = list(
   xmlElement = "bibliographicCitation",
   xmlNamespacePrefix = "DCTERMS",
   document = FALSE
 ),
 public = list(

   #'@description Initializes an object of class \link{DCBibliographicCitation}
   #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
   #'@param value value
   initialize = function(xml = NULL, value = NULL){
     super$initialize(xml = xml, term = private$xmlElement, value = value)
   }
 )
)

#' @name DCConformsTo
#' @title DCConformsTo
#' @description This class models an DublinCore 'conformsTo' element
#' @keywords Dublin Core 'conformsTo' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'conformsTo' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/conformsTo
#'
DCConformsTo <- R6Class("DCConformsTo",
  inherit = DCRelation,
  private = list(
    xmlElement = "conformsTo",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCConformsTo}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCContributor
#' @title DCContributor
#' @description This class models an DublinCore 'contributor' element
#' @keywords Dublin Core 'contributor' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'contributor' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/contributor
#'
DCContributor <- R6Class("DCContributor",
   inherit = DCElement,
   private = list(
     xmlElement = "contributor",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(

     #'@description This method is used to create an Dublin core 'contributor' element.
     #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param value value
     #'@param dc use DC namespace?
     initialize = function(xml = NULL, value = NULL, dc = FALSE){
       if(dc) private$xmlNamespacePrefix = "DC"
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#' @name DCCoverage
#' @title DCCoverage
#' @description This class models an DublinCore Terms 'coverage' element
#' @keywords Dublin Core 'coverage' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'coverage' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/coverage
#'
DCCoverage <- R6Class("DCCoverage",
   inherit = DCElement,
   private = list(
     xmlElement = "coverage",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(

     #'@description This method is used to create an Dublin core 'coverage' element.
     #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param term term
     #'@param value value
     #'@param dc use DC namespace?
     initialize = function(xml = NULL, term = NULL, value = NULL, dc = FALSE){
       if(is.null(term)) term <- private$xmlElement
       if(dc) private$xmlNamespacePrefix = "DC"
       super$initialize(xml = xml, term = term, value = value)
     }
   )
)

#' @name DCCreated
#' @title DCCreated
#' @description This class models an DublinCore Terms 'date' element
#' @keywords Dublin Core 'date' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'date' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/created
#'
DCCreated <- R6Class("DCCreated",
  inherit = DCDate,
  private = list(
    xmlElement = "created",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCCreated}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCCreator
#' @title DCCreator
#' @description This class models an DublinCore 'creator' element
#' @keywords Dublin Core 'creator' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'creator' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/creator
#'
DCCreator <- R6Class("DCCreator",
   inherit = DCElement,
   private = list(
     xmlElement = "creator",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(

     #'@description This method is used to create an Dublin core 'creator' element.
     #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param value value
     #'@param dc use DC namespace?
     initialize = function(xml = NULL, value = NULL, dc = FALSE){
       if(dc) private$xmlNamespacePrefix = "DC"
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#' @name DCDate
#' @title DCDate
#' @description This class models an DublinCore 'date' element
#' @keywords Dublin Core 'date' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'date' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/date
#'
DCDate <- R6Class("DCDate",
   inherit = DCElement,
   private = list(
     xmlElement = "date",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(

     #'@description This method is used to create an Dublin core 'date' element.
     #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param term term
     #'@param value value
     #'@param dc use DC namespace?
     initialize = function(xml = NULL, term = NULL, value = NULL, dc = FALSE){
       if(is.null(term)) term <- private$xmlElement
       if(dc) private$xmlNamespacePrefix = "DC"
       super$initialize(xml = xml, term = term, value = value)
     }
   )
)

#' @name DCDateAccepted
#' @title DCDateAccepted
#' @description This class models an DublinCore 'dateAccepted' element
#' @keywords Dublin Core 'dateAccepted' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'dateAccepted' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/dateAccepted
#'
DCDateAccepted <- R6Class("DCDateAccepted",
  inherit = DCDate,
  private = list(
    xmlElement = "dateAccepted",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCDateAccepted}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCDateCopyrighted
#' @title DCDateCopyrighted
#' @description This class models an DublinCore 'dateCopyrighted' element
#' @keywords Dublin Core 'dateCopyrighted' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'dateCopyrighted' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/dateCopyrighted
#'
DCDateCopyrighted <- R6Class("DCDateCopyrighted",
  inherit = DCDate,
  private = list(
    xmlElement = "dateCopyrighted",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCDateCopyrighted}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCDateSubmitted
#' @title DCDateSubmitted
#' @description This class models an DublinCore 'dateSubmitted' element
#' @keywords Dublin Core 'dateSubmitted' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'dateSubmitted' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/dateSubmitted
#'
DCDateSubmitted <- R6Class("DCDateSubmitted",
  inherit = DCDate,
  private = list(
    xmlElement = "dateSubmitted",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCDateSubmitted}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCDescription
#' @title DCDescription
#' @description This class models an DublinCore 'description' element
#' @keywords Dublin Core 'description' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'description' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/description
#'
DCDescription <- R6Class("DCDescription",
  inherit = DCElement,
  private = list(
    xmlElement = "description",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description This method is used to create an Dublin core 'description' element.
    #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param term term
    #'@param value value
    #'@param dc use DC namespace?
    initialize = function(xml = NULL, term = NULL, value = NULL, dc = FALSE){
      if(is.null(term)) term <- private$xmlElement
      if(dc) private$xmlNamespacePrefix = "DC"
      super$initialize(xml = xml, term = term, value = value)
    }
  )
)

#' @name DCEducationalLevel
#' @title DCEducationalLevel
#' @description This class models an DublinCore 'educationalLevel' element
#' @keywords Dublin Core 'educationalLevel' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'educationalLevel' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/educationalLevel
#'
DCEducationalLevel <- R6Class("DCEducationalLevel",
 inherit = DCAudience,
 private = list(
   xmlElement = "educationalLevel",
   xmlNamespacePrefix = "DCTERMS",
   document = FALSE
 ),
 public = list(

   #'@description Initializes an object of class \link{DCEducationalLevel}
   #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
   #'@param value value
   initialize = function(xml = NULL, value = NULL){
     super$initialize(xml = xml, term = private$xmlElement, value = value)
   }
 )
)

#' @name DCExtent
#' @title DCExtent
#' @description This class models an DublinCore 'extent' element
#' @keywords Dublin Core 'extent' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'extent' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/extent
#'
DCExtent <- R6Class("DCExtent",
  inherit = DCFormat,
  private = list(
    xmlElement = "extent",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCExtent}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCFormat
#' @title DCFormat
#' @description This class models an DublinCore 'format' element
#' @keywords Dublin Core 'format' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'format' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/format
#'
DCFormat <- R6Class("DCFormat",
  inherit = DCElement,
  private = list(
    xmlElement = "format",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCFormat}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param term term
    #'@param value value
    initialize = function(xml = NULL, term = NULL, value = NULL){
      if(is.null(term)) term <- private$xmlElement
      super$initialize(xml = xml, term = term, value = value)
    }
  )
)

#' @name DCHasPart
#' @title DCHasPart
#' @description This class models an DublinCore 'hasPart' element
#' @keywords Dublin Core 'hasPart' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'hasPart' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/hasPart
#'
DCHasPart <- R6Class("DCHasPart",
  inherit = DCElement,
  private = list(
    xmlElement = "hasPart",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description This method is used to create an Dublin core 'hasPart' element.
    #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    #'@param dc use DC namespace?
    initialize = function(xml = NULL, value = NULL, dc = FALSE){
      if(dc) private$xmlNamespacePrefix = "DC"
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCHasVersion
#' @title DCHasVersion
#' @description This class models an DublinCore 'hasVersion' element
#' @keywords Dublin Core 'hasVersion' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'hasPart' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/hasVersion
#'
DCHasVersion <- R6Class("DCHasVersion",
   inherit = DCElement,
   private = list(
     xmlElement = "hasVersion",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(

     #'@description This method is used to create an Dublin core 'hasVersion' element.
     #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param value value
     #'@param dc use DC namespace?
     initialize = function(xml = NULL, value = NULL, dc = FALSE){
       if(dc) private$xmlNamespacePrefix = "DC"
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#' @name DCIdentifier
#' @title DCIdentifier
#' @description This class models an DublinCore 'identifier' element
#' @keywords Dublin Core 'identifier' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'identifier' element
#' @format \code{\link{R6Class}} object.
#' @export

#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/identifier
#'
DCIdentifier <- R6Class("DCIdentifier",
  inherit = DCElement,
  private = list(
    xmlElement = "identifier",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description This method is used to create an Dublin core 'identifier' element.
    #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param term term
    #'@param value value
    #'@param dc use DC namespace?
    initialize = function(xml = NULL, term = NULL, value = NULL, dc = FALSE){
      if(is.null(term)) term <- private$xmlElement
      if(dc) private$xmlNamespacePrefix = "DC"
      super$initialize(xml = xml, term = term, value = value)
    }
  )
)

#' @name DCInstructionalMethod
#' @title DCInstructionalMethod
#' @description This class models an DublinCore 'instructionalMethod' element
#' @keywords Dublin Core 'instructionalMethod' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core 'instructionalMethod' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/instructionalMethod
#'
DCInstructionalMethod <- R6Class("DCInstructionalMethod",
  inherit = DCElement,
  private = list(
    xmlElement = "instructionalMethod",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCInstructionalMethod}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCIssued
#' @title DCIssued
#' @description This class models an DublinCore 'issued' element
#' @keywords Dublin Core 'issued' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'issued' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/issued
#'
DCIssued <- R6Class("DCIssued",
  inherit = DCDate,
  private = list(
    xmlElement = "issued",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCIssued}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCIsPartOf
#' @title DCIsPartOf
#' @description This class models an DublinCore 'isPartOf' element
#' @keywords Dublin Core 'isPartOf' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'isPartOf' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/isPartOf
#'
DCIsPartOf <- R6Class("DCIsPartOf",
  inherit = DCElement,
  private = list(
    xmlElement = "isPartOf",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description This method is used to create an Dublin core 'isPartOf' element.
    #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    #'@param dc use DC namespace?
    initialize = function(xml = NULL, value = NULL, dc = FALSE){
      if(dc) private$xmlNamespacePrefix = "DC"
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCIsReferencedBy
#' @title DCIsReferencedBy
#' @description This class models an DublinCore 'isReferencedBy' element
#' @keywords Dublin Core 'isReferencedBy' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'isReferencedBy' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/isReferencedBy
#'
DCIsReferencedBy <- R6Class("DCIsReferencedBy",
  inherit = DCElement,
  private = list(
    xmlElement = "isReferencedBy",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description This method is used to create an Dublin core 'isReferencedBy' element.
    #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    #'@param dc use DC namespace?
    initialize = function(xml = NULL, value = NULL, dc = FALSE){
      if(dc) private$xmlNamespacePrefix = "DC"
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCIsReplacedBy
#' @title DCIsReplacedBy
#' @description This class models an DublinCore 'isReplacedBy' element
#' @keywords Dublin Core 'isReplacedBy' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'isReplacedBy' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/isReplacedBy
#'
DCIsReplacedBy <- R6Class("DCIsReplacedBy",
  inherit = DCElement,
  private = list(
    xmlElement = "isReplacedBy",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description This method is used to create an Dublin core 'isReplacedBy' element.
    #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    #'@param dc use DC namespace?
    initialize = function(xml = NULL, value = NULL, dc = FALSE){
      if(dc) private$xmlNamespacePrefix = "DC"
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCIsRequiredBy
#' @title DCIsRequiredBy
#' @description This class models an DublinCore 'isRequiredBy' element
#' @keywords Dublin Core 'isRequiredBy' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'isRequiredBy' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/isRequiredBy
#'
DCIsRequiredBy <- R6Class("DCIsRequiredBy",
  inherit = DCElement,
  private = list(
    xmlElement = "isRequiredBy",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description This method is used to create an Dublin core 'isRequiredBy' element.
    #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    #'@param dc use DC namespace?
    initialize = function(xml = NULL, value = NULL, dc = FALSE){
      if(dc) private$xmlNamespacePrefix = "DC"
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCIsVersionOf
#' @title DCIsVersionOf
#' @description This class models an DublinCore 'isVersionOf' element
#' @keywords Dublin Core 'isVersionOf' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'isVersionOf' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/isVersionOf
#'
DCIsVersionOf <- R6Class("DCIsVersionOf",
  inherit = DCElement,
  private = list(
    xmlElement = "isVersionOf",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description This method is used to create an Dublin core 'isVersionOf' element.
    #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    #'@param dc use DC namespace?
    initialize = function(xml = NULL, value = NULL, dc = FALSE){
      if(dc) private$xmlNamespacePrefix = "DC"
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCLanguage
#' @title DCLanguage
#' @description This class models an DublinCore 'language' element
#' @keywords Dublin Core 'language' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'language' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/language
#'
DCLanguage <- R6Class("DCLanguage",
   inherit = DCElement,
   private = list(
     xmlElement = "language",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(

     #'@description This method is used to create an Dublin core 'language' element.
     #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param value value
     #'@param dc use DC namespace?
     initialize = function(xml = NULL, value = NULL, dc = FALSE){
       if(dc) private$xmlNamespacePrefix = "DC"
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#' @name DCLicense
#' @title DCLicense
#' @description This class models an DublinCore 'license' element
#' @keywords Dublin Core 'license' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'license' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/license
#'
DCLicense <- R6Class("DCLicense",
   inherit = DCRights,
   private = list(
     xmlElement = "license",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(

     #'@description Initializes an object of class \link{DCLicense}
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param value value
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#' @name DCMediator
#' @title DCMediator
#' @description This class models an DublinCore 'mediator' element
#' @keywords Dublin Core 'mediator' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'mediator' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/mediator
#'
DCMediator <- R6Class("DCMediator",
  inherit = DCAudience,
  private = list(
    xmlElement = "mediator",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCMediator}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCMedium
#' @title DCMedium
#' @description This class models an DublinCore 'medium' element
#' @keywords Dublin Core 'medium' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'medium' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/medium
#'
DCMedium <- R6Class("DCMedium",
  inherit = DCFormat,
  private = list(
    xmlElement = "medium",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCMedium}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCModified
#' @title DCModified
#' @description This class models an DublinCore 'modified' element
#' @keywords Dublin Core 'modified' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'modified' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/modified
#'
DCModified <- R6Class("DCModified",
   inherit = DCDate,
   private = list(
     xmlElement = "modified",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(

     #'@description Initializes an object of class \link{DCModified}
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param value value
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#' @name DCProvenance
#' @title DCProvenance
#' @description This class models an DublinCore 'provenance' element
#' @keywords Dublin Core 'provenance' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'provenance' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/provenance
#'
DCProvenance <- R6Class("DCProvenance",
  inherit = DCElement,
  private = list(
    xmlElement = "provenance",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCProvenance}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCPublisher
#' @title DCPublisher
#' @description This class models an DublinCore 'publisher' element
#' @keywords Dublin Core 'publisher' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'publisher' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/publisher
#'
DCPublisher <- R6Class("DCPublisher",
   inherit = DCElement,
   private = list(
     xmlElement = "publisher",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(

     #'@description This method is used to create an Dublin core 'publisher' element.
     #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param value value
     #'@param dc use DC namespace?
     initialize = function(xml = NULL, value = NULL, dc = FALSE){
       if(dc) private$xmlNamespacePrefix = "DC"
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#' @name DCReferences
#' @title DCReferences
#' @description This class models an DublinCore 'references' element
#' @keywords Dublin Core 'references' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'references' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/references
#'
DCReferences <- R6Class("DCReferences",
  inherit = DCRelation,
  private = list(
    xmlElement = "references",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCReferences}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCRelation
#' @title DCRelation
#' @description This class models an DublinCore 'relation' element
#' @keywords Dublin Core 'relation' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'relation' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/relation
#'
DCRelation <- R6Class("DCRelation",
  inherit = DCElement,
  private = list(
    xmlElement = "relation",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description This method is used to create an Dublin core 'relation' element.
    #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param term term
    #'@param value value
    #'@param dc use DC namespace?
    initialize = function(xml = NULL, term = NULL, value = NULL, dc = FALSE){
      if(is.null(term)) term <- private$xmlElement
      if(dc) private$xmlNamespacePrefix = "DC"
      super$initialize(xml = xml, term = term, value = value)
    }
  )
)

#' @name DCReplaces
#' @title DCReplaces
#' @description This class models an DublinCore 'replaces' element
#' @keywords Dublin Core 'replaces' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'replaces' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/replaces
#'
DCReplaces <- R6Class("DCReplaces",
  inherit = DCRelation,
  private = list(
    xmlElement = "replaces",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCReplaces}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCRequires
#' @title DCRequires
#' @description This class models an DublinCore 'requires' element
#' @keywords Dublin Core 'requires' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'requires' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/requires
#'
DCRequires <- R6Class("DCRequires",
  inherit = DCRelation,
  private = list(
    xmlElement = "requires",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCRequires}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCRights
#' @title DCRights
#' @description This class models an DublinCore 'rights' element
#' @keywords Dublin Core 'rights' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'rights' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/rights
#'
DCRights <- R6Class("DCRights",
   inherit = DCElement,
   private = list(
     xmlElement = "rights",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(

     #'@description This method is used to create an Dublin core 'rights' element.
     #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param term term
     #'@param value value
     #'@param dc use DC namespace?
     initialize = function(xml = NULL, term = NULL, value = NULL, dc = FALSE){
       if(is.null(term)) term <- private$xmlElement
       if(dc) private$xmlNamespacePrefix = "DC"
       super$initialize(xml = xml, term = term, value = value)
     }
   )
)

#' @name DCRightsHolder
#' @title DCRightsHolder
#' @description This class models an DublinCore 'rightsHolder' element
#' @keywords Dublin Core 'rightsHolder' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'rightsHolder' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/rightsHolder
#'
DCRightsHolder <- R6Class("DCRightsHolder",
  inherit = DCElement,
  private = list(
    xmlElement = "rightsHolder",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCRightsHolder}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#' @name DCSubject
#' @title DCSubject
#' @description This class models an DublinCore 'subject' element
#' @keywords Dublin Core 'subject' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'subject' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/subject
#'
DCSubject <- R6Class("DCSubject",
   inherit = DCElement,
   private = list(
     xmlElement = "subject",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(

     #'@description This method is used to create an Dublin core 'subject' element.
     #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param value value
     #'@param dc use DC namespace?
     initialize = function(xml = NULL, value = NULL, dc = FALSE){
       if(dc) private$xmlNamespacePrefix = "DC"
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#' @name DCSpatial
#' @title DCSpatial
#' @description This class models an DublinCore 'spatial' element
#' @keywords Dublin Core 'spatial' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'spatial' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/spatial
#'
DCSpatial <- R6Class("DCSpatial",
   inherit = DCCoverage,
   private = list(
     xmlElement = "spatial",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(

     #'@description Initializes an object of class \link{DCSpatial}
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param value value
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#' @name DCSource
#' @title DCSource
#' @description This class models an DublinCore 'source' element
#' @keywords Dublin Core 'source' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'source' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/source
#'
DCSource <- R6Class("DCSource",
   inherit = DCRelation,
   private = list(
     xmlElement = "source",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(

     #'@description This method is used to create an Dublin core 'source' element.
     #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param value value
     #'@param dc use DC namespace?
     initialize = function(xml = NULL, value = NULL, dc = FALSE){
       if(dc) private$xmlNamespacePrefix = "DC"
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#' @name DCTableOfContents
#' @title DCTableOfContents
#' @description This class models an DublinCore 'tableOfContents' element
#' @keywords Dublin Core 'tableOfContents' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'tableOfContents' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/tableOfContents
#'
DCTableOfContents <- R6Class("DCTableOfContents",
   inherit = DCDescription,
   private = list(
     xmlElement = "tableOfContents",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(

     #'@description Initializes an object of class \link{DCTableOfContents}
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param value value
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#' @name DCTemporal
#' @title DCTemporal
#' @description This class models an DublinCore 'temporal' element
#' @keywords Dublin Core 'temporal' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'temporal' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/temporal
#'
DCTemporal <- R6Class("DCTemporal",
   inherit = DCCoverage,
   private = list(
     xmlElement = "temporal",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(

     #'@description Initializes an object of class \link{DCTemporal}
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param value value
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)


#' @name DCTitle
#' @title DCTitle
#' @description This class models an DublinCore 'title' element
#' @keywords Dublin Core 'title' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'title' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/title
#'
DCTitle <- R6Class("DCTitle",
   inherit = DCElement,
   private = list(
     xmlElement = "title",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(

     #'@description This method is used to create an Dublin core 'title' element.
     #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param term term
     #'@param value value
     #'@param dc use DC namespace?
     initialize = function(xml = NULL, term = NULL, value = NULL, dc = FALSE){
       if(is.null(term)) term <- private$xmlElement
       if(dc) private$xmlNamespacePrefix = "DC"
       super$initialize(xml = xml, term = term, value = value)
     }
   )
)

#' @name DCType
#' @title DCType
#' @description This class models an DublinCore 'type' element
#' @keywords Dublin Core 'type' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'type' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/type
#'
DCType <- R6Class("DCType",
   inherit = DCElement,
   private = list(
     xmlElement = "type",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(


     #'@description This method is used to create an Dublin core 'type' element.
     #'    Use \code{dc} to \code{TRUE} to use Dublin core namespace instead of DC terms.
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param value value
     #'@param dc use DC namespace?
     initialize = function(xml = NULL, value = NULL, dc = FALSE){
       if(dc) private$xmlNamespacePrefix = "DC"
       super$initialize(xml = xml, term = private$xmlElement, value = value,
                        vocabulary = "http://purl.org/dc/dcmitype/")
     }
   )
)

#' @name DCValid
#' @title DCValid
#' @description This class models an DublinCore 'valid' element
#' @keywords Dublin Core 'valid' element
#' @return Object of \code{\link{R6Class}} for modelling an Dublin Core Terms 'valid' element
#' @format \code{\link{R6Class}} object.
#' @export
#'
#' @references
#'   Dublin Core Metadata Initiative. https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/valid
#'
DCValid <- R6Class("DCValid",
  inherit = DCDate,
  private = list(
    xmlElement = "valid",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(

    #'@description Initializes an object of class \link{DCValid}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)
