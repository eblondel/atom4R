# test_DCEntry.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for DCEntry.R
#=======================
require(atom4R, quietly = TRUE)
require(testthat)
require(XML)

context("DCElement")

test_that("encoding/decoding DCEntry",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()

  #encoding
  dcentry <- DCEntry$new()
  dcentry$setId("my-dc-entry")

  #fill dc entry
  dcentry$addDCDate(Sys.time())
  dcentry$addDCTitle("atom4R - Tools to read/write and publish metadata as Atom XML format")
  dcentry$addDCType("Software")
  creator <- DCCreator$new(value = "Blondel, Emmanuel")
  creator$attrs[["affiliation"]] <- "Independent"
  dcentry$addDCCreator(creator)
  dcentry$addDCSubject("R")
  dcentry$addDCSubject("FAIR")
  dcentry$addDCSubject("Interoperability")
  dcentry$addDCSubject("Open Science")
  dcentry$addDCDescription("Atom4R offers tools to read/write and publish metadata as Atom XML syndication format, including Dublin Core entries. Publication can be done using the Sword API which implements AtomPub API specifications")
  dcentry$addDCPublisher("GitHub")

  funder <- DCContributor$new(value = "CNRS")
  funder$attrs[["type"]] <- "Funder"
  dcentry$addDCContributor(funder)
  dcentry$addDCRelation("Github repository: https://github.com/eblondel/atom4R")
  dcentry$addDCSource("Atom Syndication format - https://www.ietf.org/rfc/rfc4287")
  dcentry$addDCSource("AtomPub, The Atom publishing protocol - https://tools.ietf.org/html/rfc5023")
  dcentry$addDCSource("Sword API - http://swordapp.org/")
  dcentry$addDCSource("Dublin Core Metadata Initiative - https://www.dublincore.org/")
  dcentry$addDCSource("Guidelines for implementing Dublin Core in XML - https://www.dublincore.org/specifications/dublin-core/dc-xml-guidelines/")
  dcentry$addDCLicense("NONE")
  dcentry$addDCRights("MIT License")

  xml <- dcentry$encode()
  expect_is(dcentry, "DCEntry")

  #decoding
  dcentry2 <- DCEntry$new(xml = xml)
  xml2 <- dcentry2$encode()

  expect_true(AtomAbstractObject$compare(dcentry, dcentry2))

})
