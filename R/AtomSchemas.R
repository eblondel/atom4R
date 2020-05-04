#' @name registerAtomSchema
#' @aliases registerAtomSchema
#' @title registerAtomSchema
#' @export
#' @description \code{registerAtomSchema} allows to register a new schema
#' in \pkg{atom4R}
#'
#' @usage registerAtomSchema(xsdFile)
#'
#' @param xsdFile the schema XSD file
#'
#' @examples
#'   registerAtomSchema(xsdFile = "https://jvndb.jvn.jp/schema/atom.xsd")
#'
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
registerAtomSchema <- function(xsdFile){
  schemas <- tryCatch(
    XML::xmlParse(
      xsdFile, isSchema = TRUE, xinclude = TRUE,
      error = function (msg, code, domain, line, col, level, filename, class = "XMLError"){}
    )
  )
  .atom4R$schemas <- schemas
}

#'setAtomSchemas
#'@export
setAtomSchemas <- function(){
  packageStartupMessage("Loading Atom XML schemas...")
  schemaPath <- "extdata/schemas"
  namespace <- "atom"
  defaultXsdFile <- system.file(paste(schemaPath, namespace, sep="/"), paste0(namespace,".xsd"),
                                package = "atom4R", mustWork = TRUE)
  registerAtomSchema(defaultXsdFile)
}

#' @name getAtomSchemas
#' @aliases getAtomSchemas
#' @title getAtomSchemas
#' @export
#' @description \code{getAtomSchemas} gets the schemas registered in \pkg{atom4R}
#'
#' @usage getAtomSchemas()
#'
#' @examples
#'   getAtomSchemas()
#'
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
getAtomSchemas <- function(){
  return(.atom4R$schemas)
}
