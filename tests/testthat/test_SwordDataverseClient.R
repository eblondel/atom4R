# test_SwordDataverseClient.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for SwordDataverseClient.R
#=======================
require(atom4R, quietly = TRUE)
require(testthat)
require(XML)
require(httr)

context("SwordDataverseClient")

if(FALSE){

#sleep during Dataverse configuration
message("Dataverse server: sleeping during server configuration...")
Sys.sleep(time = 30)
ping <- try(status_code(GET('http://dataverse-dev.localhost:8085/')), silent = TRUE)
while(is(ping, "try-error") || ping == 500){
  message("Dataverse server doesn't seem ready, sleeping 30s more...")
  Sys.sleep(time = 30)
  ping <- try(status_code(GET('http://dataverse-dev.localhost:8085/')), silent = TRUE)
}
message("Dataverse server ready for testing...")

#config dataverse SWORD API
initAPI <- function(){
  return(
    try(SwordDataverseClient$new(
      hostname = "http://dataverse-dev.localhost:8085",
      token = "dbf293b4-d13e-45d4-99c6-f0cf18159f0d",
      logger = "DEBUG"
    ), silent = TRUE)
  )
}
message("Dataverse SWORD API: sleeping during API configuration...")
Sys.sleep(30)
API <- initAPI()
while(is(API, "try-error")){
  message("Dataverse SWORD API doesn't seem ready: sleeping 30s more...")
  Sys.sleep(30)
  API <- initAPI()
}
message("Dataverse SWORD API ready for testing...")

if(is(API, "SwordDataverseClient")){

  test_that("list Dataverses (Sword collections)",{
    testthat::skip_on_cran()
    cols <- API$listCollections() #or getDataverses()
    expect_is(cols, "list")
    expect_true(length(cols)>0)
  })

  test_that("list Dataverse members",{
    testthat::skip_on_cran()
    members <- API$getCollectionMembers("Root")
    expect_is(members, "AtomFeed")
  })

  test_that("create DC entry",{
    testthat::skip_on_cran()
    #encoding
    dcentry <- DCEntry$new()
    dcentry$setId("my-dc-entry")

    #fill dc entry
    dcentry$addDCDate(Sys.time())
    dcentry$addDCTitle("atom4R - Tools to read/write and publish metadata as Atom XML format")
    dcentry$addDCType("Software")
    creator <- DCCreator$new(value = "Blondel, Emmanuel")
    #creator$attrs[["affiliation"]] <- "Independent"
    dcentry$addDCCreator(creator)
    dcentry$addDCSubject("R")
    dcentry$addDCSubject("FAIR")
    dcentry$addDCSubject("Interoperability")
    dcentry$addDCSubject("Open Science")
    dcentry$addDCSubject("Dataverse")
    dcentry$addDCDescription("Atom4R offers tools to read/write and publish metadata as Atom XML syndication format, including Dublin Core entries. Publication can be done using the Sword API which implements AtomPub API specifications")
    dcentry$addDCPublisher("GitHub")

    funder <- DCContributor$new(value = "CNRS")
    funder$attrs[["Type"]] <- "Funder"
    dcentry$addDCContributor(funder)

    contact <- DCContributor$new(value = "wilfried.heintz@inrae.fr")
    contact$attrs[["Type"]] <- "Contact"
    dcentry$addDCContributor(contact)

    editor <- DCContributor$new(value = "geoflow@geoflow.io")
    editor$attrs[["Type"]] <- "Editor"
    dcentry$addDCContributor(editor)

    dcentry$addDCRelation("Github repository: https://github.com/eblondel/atom4R")
    relation = DCRelation$new()
    dcentry$addDCRelation("CRAN repository: Not yet available")
    dcentry$addDCSource("Atom Syndication format - https://www.ietf.org/rfc/rfc4287")
    dcentry$addDCSource("AtomPub, The Atom publishing protocol - https://tools.ietf.org/html/rfc5023")
    dcentry$addDCSource("Sword API - http://swordapp.org/")
    dcentry$addDCSource("Dublin Core Metadata Initiative - https://www.dublincore.org/")
    dcentry$addDCSource("Guidelines for implementing Dublin Core in XML - https://www.dublincore.org/specifications/dublin-core/dc-xml-guidelines/")
    dcentry$addDCRights("MIT License")

    out <- API$createDataverseRecord("dynids", dcentry)

    expect_is(out, "AtomEntry")
  })

}

}
