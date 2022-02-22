.onLoad <- function (libname, pkgname) { # nocov start
  #hidden objects
  assign(".atom4R", new.env(), envir= asNamespace(pkgname))

  #set Atom namespace
  setAtomNamespaces()

  #set Atom schemas
  setAtomSchemas()

  #set DCMI Vocabularies
  setDCMIVocabularies()

  old_col_types <- getOption ("readr.show_col_types")
  if (!is.null (old_col_types)) {
      options (atom4R.show_col_types = old_col_types)
  }
  old_progress <- getOption ("readr.show_progress")
  if (!is.null (old_progress)) {
      options (atom4R.show_progress = old_progress)
  }
  options (readr.show_col_types = FALSE)
  options (readr.show_progress = FALSE)
}

.onUnload <- function (libname, pkgname) {

    if (!is.null (options (atom4R.show_col_types))) {
        options (readr.show_col_types = options (atom4R.show_col_types))
    }
    if (!is.null (options (atom4R.show_progress))) {
        options (readr.show_progress = options (atom4R.show_progress))
    }
    options (atom4R.show_col_types = NULL)
    options (atom4R.show_progress = NULL)
}
# nocov end
