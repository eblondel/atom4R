.onLoad <- function (libname, pkgname) { # nocov start
  #hidden objects
  assign(".atom4R", new.env(), envir= asNamespace(pkgname))

  #set Atom namespace
  setAtomNamespaces()

  #set Atom schemas
  setAtomSchemas()

} # nocov end
