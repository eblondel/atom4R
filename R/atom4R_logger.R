#' atom4RLogger
#'
#' @docType class
#' @export
#' @keywords logger
#' @return Object of \code{ \link[R6]{R6Class}} for modelling a simple logger
#' @format \code{ \link[R6]{R6Class}} object.
#'
#' @note Logger class used internally by atom4R
#'
atom4RLogger <- R6Class("atom4RLogger",
  portable = TRUE,
  public = list(
    #'@field verbose.info If package info log messages have to be printed out
    verbose.info = FALSE,
    #'@field verbose.debug If curl debug log messages have to be printed out
    verbose.debug = FALSE,
    #' @field loggerType the type of logger
    loggerType = NULL,
    #'@description Provides log messages
    #'@param type type of log ("INFO", "WARN", "ERROR")
    #'@param text the log message text
    logger = function(type, text){
      if(self$verbose.info){
        cat(sprintf("[atom4R][%s] %s \n", type, text))
      }
    },
    #'@description Provides INFO log messages
    #'@param text the log message text
    INFO = function(text){self$logger("INFO", text)},
    #'@description Provides WARN log messages
    #'@param text the log message text
    WARN = function(text){self$logger("WARN", text)},
    #'@description Provides ERROR log messages
    #'@param text the log message text
    ERROR = function(text){self$logger("ERROR", text)},

    #'@description Initalizes the logger
    #'@param logger logger type "INFO", "DEBUG" or \code{NULL}
    initialize = function(logger = NULL){
      if(!missing(logger)){
        if(!is.null(logger)){
          self$loggerType <- toupper(logger)
          if(!(self$loggerType %in% c("INFO","DEBUG"))){
            stop(sprintf("Unknown logger type '%s", logger))
          }
          if(self$loggerType == "INFO"){
            self$verbose.info = TRUE
          }else if(self$loggerType == "DEBUG"){
            self$verbose.info = TRUE
            self$verbose.debug = TRUE
          }
        }
      }
    },

    #'@description Get class name
    #'@return object of class \code{data.frame}
    getClassName = function(){
      return(class(self)[1])
    },

    #'@description Get class
    #'@return object of class  \link[R6]{R6Class}
    getClass = function(){
      class <- eval(parse(text=self$getClassName()))
      return(class)
    }

  )
)
