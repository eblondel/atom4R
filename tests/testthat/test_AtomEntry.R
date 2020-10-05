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
  atom$addCategory("DRAFT", "dataset")
  atom$addCategory("World", "spatial")
  atom$addCategory("Tuna", "fishery")

  xml <- atom$encode()
  expect_is(atom, "AtomEntry")

  #decoding
  atom2 <- AtomEntry$new(xml = xml)
  xml2 <- atom2$encode()

  expect_true(AtomAbstractObject$compare(atom, atom2))

})

test_that("encoding - DC Elements",{
  testthat::skip_on_cran()

  #encoding
  atom <- DCEntry$new()
  atom$setId("my-dcmi-identifier")

  #dublin core terms
  atom$addDCAccrualMethod("accmeth:itemCreation")
  atom$addDCAccrualPeriodicity("freq:irregular")
  atom$addDCAccrualPolicy("accpol:partial")
  atom$addDCAlternative("My DCMI alternate title")
  atom$addDCAccessRights("Free access rights")
  atom$addDCBibliographicCitation("Blondel, 2020. My DCMI identifier. In: the scientific journal")
  atom$addDCIdentifier("my-dcmi-identifier")
  atom$addDCTitle("My DCMI title")
  atom$addDCAbstract("My DCMI abstract")
  atom$addDCDescription("My DCMI Description")

  xml <- atom$encode()
  expect_is(atom, "AtomEntry")

  #decoding
  atom2 <- DCEntry$new(xml = xml)
  xml2 <- atom2$encode()

  expect_true(AtomAbstractObject$compare(atom, atom2))

})
