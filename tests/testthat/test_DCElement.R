# test_DCElement.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for DCElement.R
#=======================
require(atom4R, quietly = TRUE)
require(testthat)
require(XML)

context("DCElement")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()

  #encoding
  dcelement <- DCAbstract$new(value= "some abstract")

  xml <- dcelement$encode()
  expect_is(dcelement, "DCAbstract")

  #decoding
  dcelement2 <- DCAbstract$new(xml = xml)
  xml2 <- dcelement2$encode()

  expect_true(AtomAbstractObject$compare(dcelement, dcelement2))

})
