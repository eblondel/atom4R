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
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, term, value)}}{
#'    This method is used to create an Dublin core element
#'  }
#' }
#'
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
DCElement <- R6Class("DCElement",
   inherit = AtomAbstractObject,
   private = list(
     dcTerms = function() {
       dcTermsVocabId <- "http://purl.org/dc/terms/"
       terms <- getDCMIVocabulary(id = dcTermsVocabId)$get()
       sapply(terms$s, function(x){unlist(strsplit(x, dcTermsVocabId))[2]})
     },
     xmlElement = "_abstract_",
     xmlNamespacePrefix = "DC",
     document = FALSE
   ),
   public = list(
     value = NULL,
     initialize = function(xml = NULL, term = NULL, value = NULL, vocabulary = NULL){
       super$initialize(xml = xml, element = term, wrap = FALSE)
       if(is.null(xml)){
         if(!term %in% private$dcTerms()){
           stop(sprintf("'%s' is not a valid Dublin Core term", term))
         }
         if(!is.null(vocabulary)){
           vocab <- getDCMIVocabulary(id = vocabulary)
           if(is.null(vocab)){
             stop(sprintf("No controlled vocabulary for id '%s'", vocabulary))
           }
           if(!value %in% vocab$get()$label){
             errMsg <- sprintf("Value '%s' not authorized by DCMI controlled vocabulary for term '%s'.\n", value, term)
             errMsg <- paste0(errMsg, sprintf("Controlled vocabulary can be browsed in R with the following code:\ngetDCMIVocabulary(id = \"%s\")$get()", vocabulary))
             stop(errMsg)
           }
         }
         self$value = value
       }
     }
   )
)

DCElement$getDCClasses = function(extended = FALSE, pretty = FALSE){
  list_of_classes <- unlist(sapply(search(), ls))
  list_of_classes <- list_of_classes[sapply(list_of_classes, function(x){
    clazz <- invisible(try(eval(parse(text=x)),silent=TRUE))
    r6Predicate <- class(clazz)[1]=="R6ClassGenerator"
    envPredicate <- extended
    if(r6Predicate & !extended){
      if(is.environment(clazz$parent_env)){
        envPredicate <- environmentName(clazz$parent_env)=="atom4R"
      }
    }
    includePredicate <- TRUE
    if(r6Predicate){
      if(!is.null(clazz$classname)){
        includePredicate <- clazz$classname != "atom4RLogger" &&
          !startsWith(clazz$classname, "Atom") &&
          !startsWith(clazz$classname, "Sword") &&
          !startsWith(clazz$classname, "DCMI")
      }
    }
    return(r6Predicate & envPredicate & includePredicate)
  })]
  list_of_classes <- as.vector(list_of_classes)
  if(pretty){
    std_info <- do.call("rbind",lapply(list_of_classes, function(x){
      clazz <- invisible(try(eval(parse(text=x)),silent=TRUE))
      std_info <- data.frame(
        ns_prefix = clazz$private_fields$xmlNamespacePrefix,
        ns_uri = AtomNamespace[[clazz$private_fields$xmlNamespacePrefix]]$uri,
        element = clazz$private_fields$xmlElement,
        stringsAsFactors = FALSE
      )
      return(std_info)
    }))

    list_of_classes <- data.frame(
      dc_class = list_of_classes,
      std_info,
      stringsAsFactors = FALSE
    )
  }
  return(list_of_classes)
}

DCElement$getDCClassByElement = function(element){
  dc_classes <- DCElement$getDCClasses(pretty = T)
  dc_class <- dc_classes[dc_classes$element == element,]
  clazz <- try(eval(parse(text=dc_class$dc_class)))
  return(clazz)
}

#'@export
DCAbstract <- R6Class("DCAbstract",
  inherit = DCDescription,
  private = list(
    xmlElement = "abstract",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#'@export
DCAccessRights <- R6Class("DCAccessRights",
  inherit = DCRights,
  private = list(
    xmlElement = "accessRights",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#'@export
DCAccrualMethod <- R6Class("DCAccrualMethod",
  inherit = DCElement,
  private = list(
    xmlElement = "accrualMethod",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#'@export
DCAccrualPeriodicity <- R6Class("DCAccrualPeriodicity",
   inherit = DCElement,
   private = list(
     xmlElement = "accrualPeriodicity",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#'@export
DCAccrualPolicy <- R6Class("DCAccrualPolicy",
  inherit = DCElement,
  private = list(
    xmlElement = "accrualPolicy",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#'@export
DCAlternative <- R6Class("DCAlternative",
   inherit = DCTitle,
   private = list(
     xmlElement = "alternative",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#'@export
DCAudience <- R6Class("DCAudience",
   inherit = DCElement,
   private = list(
     xmlElement = "audience",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, term = NULL, value = NULL){
       if(is.null(term)) term <- private$xmlElement
       super$initialize(xml = xml, term = term, value = value)
     }
   )
)

#'@export
DCAvailable <- R6Class("DCAvailable",
  inherit = DCDate,
  private = list(
    xmlElement = "available",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#'@export
DCBibliographicCitation <- R6Class("DCBibliographicCitation",
 inherit = DCIdentifier,
 private = list(
   xmlElement = "bibliographicCitation",
   xmlNamespacePrefix = "DCTERMS",
   document = FALSE
 ),
 public = list(
   initialize = function(xml = NULL, value = NULL){
     super$initialize(xml = xml, term = private$xmlElement, value = value)
   }
 )
)

#'@export
DCConformsTo <- R6Class("DCConformsTo",
  inherit = DCRelation,
  private = list(
    xmlElement = "conformsTo",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#'@export
DCContributor <- R6Class("DCContributor",
   inherit = DCElement,
   private = list(
     xmlElement = "contributor",
     xmlNamespacePrefix = "DC",
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)
#'@export
DCTContributor <- R6Class("DCTContributor",
   inherit = DCContributor,
   private = list(
     xmlElement = "contributor",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, value = value)
     }
   )
)

#'@export
DCCoverage <- R6Class("DCCoverage",
   inherit = DCElement,
   private = list(
     xmlElement = "coverage",
     xmlNamespacePrefix = "DC",
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, term = NULL, value = NULL){
       if(is.null(term)) term <- private$xmlElement
       super$initialize(xml = xml, term = term, value = value)
     }
   )
)

#'@export
DCCreated <- R6Class("DCCreated",
  inherit = DCDate,
  private = list(
    xmlElement = "created",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#'@export
DCCreator <- R6Class("DCCreator",
   inherit = DCElement,
   private = list(
     xmlElement = "creator",
     xmlNamespacePrefix = "DC",
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)
#'@export
DCTCreator <- R6Class("DCTCreator",
   inherit = DCCreator,
   private = list(
     xmlElement = "creator",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, value = value)
     }
   )
)

#'@export
DCDate <- R6Class("DCDate",
   inherit = DCElement,
   private = list(
     xmlElement = "date",
     xmlNamespacePrefix = "DC",
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, term = NULL, value = NULL){
       if(is.null(term)) term <- private$xmlElement
       super$initialize(xml = xml, term = term, value = value)
     }
   )
)

#'@export
DCDateAccepted <- R6Class("DCDateAccepted",
  inherit = DCDate,
  private = list(
    xmlElement = "dateAccepted",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#'@export
DCDateCopyrighted <- R6Class("DCDateCopyrighted",
  inherit = DCDate,
  private = list(
    xmlElement = "dateCopyrighted",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#'@export
DCDateSubmitted <- R6Class("DCDateSubmitted",
  inherit = DCDate,
  private = list(
    xmlElement = "dateSubmitted",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#'@export
DCDescription <- R6Class("DCDescription",
  inherit = DCElement,
  private = list(
    xmlElement = "description",
    xmlNamespacePrefix = "DC",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, term = NULL, value = NULL){
      if(is.null(term)) term <- private$xmlElement
      super$initialize(xml = xml, term = term, value = value)
    }
  )
)

#'@export
DCEducationalLevel <- R6Class("DCEducationalLevel",
 inherit = DCAudience,
 private = list(
   xmlElement = "educationalLevel",
   xmlNamespacePrefix = "DCTERMS",
   document = FALSE
 ),
 public = list(
   initialize = function(xml = NULL, value = NULL){
     super$initialize(xml = xml, term = private$xmlElement, value = value)
   }
 )
)

#'@export
DCExtent <- R6Class("DCExtent",
  inherit = DCFormat,
  private = list(
    xmlElement = "extent",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#'@export
DCFormat <- R6Class("DCFormat",
  inherit = DCElement,
  private = list(
    xmlElement = "format",
    xmlNamespacePrefix = "DC",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, term = NULL, value = NULL){
      if(is.null(term)) term <- private$xmlElement
      super$initialize(xml = xml, term = term, value = value)
    }
  )
)

#'@export
DCIdentifier <- R6Class("DCIdentifier",
  inherit = DCElement,
  private = list(
    xmlElement = "identifier",
    xmlNamespacePrefix = "DC",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, term = NULL, value = NULL){
      if(is.null(term)) term <- private$xmlElement
      super$initialize(xml = xml, term = term, value = value)
    }
  )
)

#'@export
DCInstructionalMethod <- R6Class("DCInstructionalMethod",
  inherit = DCElement,
  private = list(
    xmlElement = "instructionalMethod",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#'@export
DCIssued <- R6Class("DCIssued",
  inherit = DCDate,
  private = list(
    xmlElement = "issued",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#'@export
DCLanguage <- R6Class("DCLanguage",
   inherit = DCElement,
   private = list(
     xmlElement = "language",
     xmlNamespacePrefix = "DC",
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#'@export
DCLicense <- R6Class("DCLicense",
   inherit = DCRights,
   private = list(
     xmlElement = "license",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#'@export
DCMediator <- R6Class("DCMediator",
  inherit = DCAudience,
  private = list(
    xmlElement = "mediator",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#'@export
DCMedium <- R6Class("DCMedium",
  inherit = DCFormat,
  private = list(
    xmlElement = "medium",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#'@export
DCModified <- R6Class("DCModified",
   inherit = DCDate,
   private = list(
     xmlElement = "modified",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#'@export
DCProvenance <- R6Class("DCProvenance",
  inherit = DCElement,
  private = list(
    xmlElement = "provenance",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#'@export
DCPublisher <- R6Class("DCPublisher",
   inherit = DCElement,
   private = list(
     xmlElement = "publisher",
     xmlNamespacePrefix = "DC",
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#'@export
DCReferences <- R6Class("DCReferences",
  inherit = DCRelation,
  private = list(
    xmlElement = "references",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#'@export
DCRelation <- R6Class("DCRelation",
  inherit = DCElement,
  private = list(
    xmlElement = "relation",
    xmlNamespacePrefix = "DC",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, term = NULL, value = NULL){
      if(is.null(term)) term <- private$xmlElement
      super$initialize(xml = xml, term = term, value = value)
    }
  )
)

#'@export
DCReplaces <- R6Class("DCReplaces",
  inherit = DCRelation,
  private = list(
    xmlElement = "replaces",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#'@export
DCRequires <- R6Class("DCRequires",
  inherit = DCRelation,
  private = list(
    xmlElement = "requires",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#'@export
DCRights <- R6Class("DCRights",
   inherit = DCElement,
   private = list(
     xmlElement = "rights",
     xmlNamespacePrefix = "DCTERMS", #should be DC, see why Dataverse doesn't get it
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, term = NULL, value = NULL){
       if(is.null(term)) term <- private$xmlElement
       super$initialize(xml = xml, term = term, value = value)
     }
   )
)

#'@export
DCRightsHolder <- R6Class("DCRightsHolder",
  inherit = DCElement,
  private = list(
    xmlElement = "rightsHolder",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)

#'@export
DCSubject <- R6Class("DCSubject",
   inherit = DCElement,
   private = list(
     xmlElement = "subject",
     xmlNamespacePrefix = "DC",
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#'@export
DCSpatial <- R6Class("DCSpatial",
   inherit = DCCOverage,
   private = list(
     xmlElement = "spatial",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#'@export
DCSource <- R6Class("DCSource",
   inherit = DCRelation,
   private = list(
     xmlElement = "source",
     xmlNamespacePrefix = "DC",
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#'@export
DCTableOfContents <- R6Class("DCTableOfContents",
   inherit = DCDescription,
   private = list(
     xmlElement = "tableOfContents",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)

#'@export
DCTemporal <- R6Class("DCTemporal",
   inherit = DCCoverage,
   private = list(
     xmlElement = "temporal",
     xmlNamespacePrefix = "DCTERMS",
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value)
     }
   )
)


#'@export
DCTitle <- R6Class("DCTitle",
   inherit = DCElement,
   private = list(
     xmlElement = "title",
     xmlNamespacePrefix = "DC",
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, term = NULL, value = NULL){
       if(is.null(term)) term <- private$xmlElement
       super$initialize(xml = xml, term = term, value = value)
     }
   )
)

#'@export
DCType <- R6Class("DCType",
   inherit = DCElement,
   private = list(
     xmlElement = "type",
     xmlNamespacePrefix = "DC",
     document = FALSE
   ),
   public = list(
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml, term = private$xmlElement, value = value,
                        vocabulary = "http://purl.org/dc/dcmitype/")
     }
   )
)

#'@export
DCValid <- R6Class("DCValid",
  inherit = DCDate,
  private = list(
    xmlElement = "valid",
    xmlNamespacePrefix = "DCTERMS",
    document = FALSE
  ),
  public = list(
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, term = private$xmlElement, value = value)
    }
  )
)
