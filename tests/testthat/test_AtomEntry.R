# test_AtomFeed.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for AtomFeed.R
#=======================
require(atom4R, quietly = TRUE)
require(testthat)
require(XML)

context("AtomEntry")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()

  #encoding
  atom <- AtomEntry$new()
  atom$setId("my-atom-entry")
  atom$setTitle("My Atom feed entry")
  atom$setSummary("My Atom feed entry very comprehensive abstract")
  author1 <- AtomAuthor$new(
    name = "John Doe",
    uri = "http://www.atomxml.com/johndoe",
    email = "johndoe@atom4R.com"
  )
  atom$addAuthor(author1)
  author2 <- AtomAuthor$new(
    name = "John Doe's sister",
    uri = "http://www.atomxml.com/johndoesister",
    email = "johndoesister@atom4R.com"
  )
  atom$addAuthor(author2)
  contrib1 <- AtomContributor$new(
    name = "Contrib1",
    uri = "http://www.atomxml.com/contrib1",
    email = "contrib1@atom4R.com"
  )
  atom$addContributor(contrib1)
  contrib2 <- AtomContributor$new(
    name = "Contrib2",
    uri = "http://www.atomxml.com/contrib2",
    email = "contrib2@atom4R.com"
  )
  atom$addContributor(contrib2)
  atom$addCategory("dataset")
  atom$addCategory("spatial")
  atom$addCategory("fisheries")

  #dublin core terms
  atom$addDCElement("identifier", "my-dcmi-identifier")
  atom$addDCElement("title", "My DCMI title")
  atom$addDCElement("abstract", "My DCMI abstract")


  xml <- atom$encode()
  expect_is(atom, "AtomEntry")

  #decoding
  atom2 <- AtomEntry$new(xml = xml)
  xml2 <- atom2$encode()

  expect_true(AtomAbstractObject$compare(atom, atom2))

})

test_that("encoding - DC Elements",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()

  #encoding
  atom <- AtomEntry$new()
  atom$setId("my-dcmi-identifier")

  #dublin core terms
  atom$setDCAccrualMethod("accmeth:itemCreation")
  atom$setDCAccrualPeriodicity("freq:irregular")
  atom$setDCAccrualPolicy("accpol:partial")
  atom$setDCAlternative("My DCMI alternate title")
  atom$setDCAccessRights("Free access rights")
  atom$setDCBibliographicCitation("Blondel, 2020. My DCMI identifier. In: the scientific journal")
  atom$setDCIdentifier("my-dcmi-identifier")
  atom$setDCTitle("My DCMI title")
  atom$setDCAbstract("My DCMI abstract")
  atom$setDCDescription("My DCMI Description")
  atom$setDCSpatial("Some spatial description about the record")


  xml <- atom$encode()
  expect_is(atom, "AtomEntry")

  #decoding
  atom2 <- AtomEntry$new(xml = xml)
  xml2 <- atom2$encode()

  expect_true(AtomAbstractObject$compare(atom, atom2))

})
