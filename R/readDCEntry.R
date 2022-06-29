#' @name readDCEntry
#' @aliases readDCEntry
#' @title readDCEntry
#' @export
#' @description \code{readDCEntry} is a function to read a DC XML entry from a file
#' or url into an object in the \pkg{atom4R} model.
#'
#' @usage readDCEntry(file, url, raw)
#'
#' @param file a valid file path, as object of class \code{character}
#' @param url a valid URL, as object of class \code{character}
#' @param raw indicates if the function should return the raw XML. By
#' default this is set to \code{FALSE} and the function will try to map
#' the xml data to the \pkg{atom4R} data model.
#'
#' @return a \pkg{atom4R} object inheriting \code{DCEntry}
#'
#' @examples
#' \donttest{
#'   dcfile <- paste0(
#'     "https://raw.githubusercontent.com/eblondel/atom4R/master/",
#'     "inst/extdata/examples/zenodo_dc_export.xml"
#'   )
#'   dc <- readDCEntry(dcfile)
#' }
#'
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
readDCEntry <- function(file = NULL, url = NULL, raw = FALSE){

  if(is.null(file) & is.null(url)){
    stop("Please provide at least a metadata file or url")
  }
  encoding <- "UTF-8"
  raw_xml <- NULL
  if(!is.null(url)){
    req <- httr::GET(url)
    if(httr::status_code(req) != 200){
      stop("The URL resource is unavailable")
    }
    doc <- content(req, as = "text", encoding = encoding)
    raw_xml <- XML::xmlParse(doc, encoding = encoding, addFinalizer = FALSE)
  }else{
    raw_xml <- readr::read_lines(file)
    raw_xml <- paste0(raw_xml, collapse="")
    if(Encoding(raw_xml) != "UTF-8") Encoding(raw_xml) <- "UTF-8"
    if(Encoding(raw_xml) == "unknown"){
      raw_xml <- XML::xmlParse(raw_xml, error = function (msg, ...) {}, addFinalizer = FALSE)
    }else{
      raw_xml <- XML::xmlParse(raw_xml, encoding = Encoding(raw_xml),
                               error = function (msg, ...) {}, addFinalizer = FALSE)
    }
  }

  out <- NULL
  if(!is.null(raw_xml)){
    raw_xml <- as(raw_xml, "XMLInternalNode")
    if(raw){
      out <- raw_xml
    }else{
      out <- DCEntry$new(xml = raw_xml)
    }
  }
  return(out)

}
