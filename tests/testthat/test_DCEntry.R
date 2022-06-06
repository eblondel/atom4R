# test_DCEntry.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for DCEntry.R
#=======================
require(atom4R, quietly = TRUE)
require(testthat)
require(XML)

context("DCEntry")

test_that("encoding/decoding DCEntry",{
  testthat::skip_on_cran()

  #encoding
  dcentry <- DCEntry$new()
  dcentry$setId("my-dc-entry")

  #fill dc entry
  dcentry$addDCDate(Sys.time())
  dcentry$addDCTitle("atom4R - Tools to read/write and publish metadata as Atom XML format")
  dcentry$addDCType("Software")
  creator <- DCCreator$new(value = "Blondel, Emmanuel")
  dcentry$addDCCreator(creator)
  dcentry$addDCSubject("R")
  dcentry$addDCSubject("FAIR")
  dcentry$addDCSubject("Interoperability")
  dcentry$addDCSubject("Open Science")
  dcentry$addDCDescription("Atom4R offers tools to read/write and publish metadata as Atom XML syndication format, including Dublin Core entries. Publication can be done using the Sword API which implements AtomPub API specifications")
  dcentry$addDCPublisher("GitHub")

  funder <- DCContributor$new(value = "CNRS")
  dcentry$addDCContributor(funder)
  dcentry$addDCRelation("Github repository: https://github.com/eblondel/atom4R")
  dcentry$addDCSource("Atom Syndication format - https://www.ietf.org/rfc/rfc4287")
  dcentry$addDCSource("AtomPub, The Atom publishing protocol - https://tools.ietf.org/html/rfc5023")
  dcentry$addDCSource("Sword API - http://swordapp.org/")
  dcentry$addDCSource("Dublin Core Metadata Initiative - https://www.dublincore.org/")
  dcentry$addDCSource("Guidelines for implementing Dublin Core in XML - https://www.dublincore.org/specifications/dublin-core/dc-xml-guidelines/")
  dcentry$addDCLicense("NONE")
  dcentry$addDCRights("MIT License")
  dcentry$addDCHasPart("part1")
  dcentry$addDCHasPart("part2")
  dcentry$addDCHasVersion("0.2")
  dcentry$addDCIsPartOf("CRAN")
  dcentry$addDCIsPartOf("GitHub")
  dcentry$addDCIsReferencedBy("CRAN")
  dcentry$addDCIsReferencedBy("GitHub")
  dcentry$addDCIsRequiredBy("zen4R")
  dcentry$addDCIsRequiredBy("cloud4R")

  xml <- dcentry$encode()
  expect_is(dcentry, "DCEntry")

  #decoding
  dcentry2 <- DCEntry$new(xml = xml)
  xml2 <- dcentry2$encode()

  expect_true(AtomAbstractObject$compare(dcentry, dcentry2))

})

test_that("decoding Zenodo Dublin core",{

  dcxml <- xmlParse(httr::content(httr::GET("https://raw.githubusercontent.com/eblondel/atom4R/master/inst/extdata/examples/zenodo_dc_export.xml")))
  dcentry <- DCEntry$new(xml = dcxml)
  dcentry_xml <- dcentry$encode()

  dcxml_children <- XML::xmlChildren(XML::xmlChildren(dcxml)[[1]])
  dcentry_xml_children <- XML::xmlChildren(dcentry_xml)
  dcentry_xml_children <- dcentry_xml_children[!names(dcentry_xml_children) %in% c("updated", "comment")]
  expect_equal(
    length(dcxml_children),
    length(dcentry_xml_children)
  )
})

test_that("reading DC XML",{
  dcfile <- "https://raw.githubusercontent.com/eblondel/atom4R/master/inst/extdata/examples/zenodo_dc_export.xml"
  dc <- readDCEntry(dcfile)
  expect_is(dc, "DCEntry")
})
