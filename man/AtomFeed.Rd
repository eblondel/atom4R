% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AtomFeed.R
\docType{class}
\name{AtomFeed}
\alias{AtomFeed}
\title{Atom feed class}
\format{
\code{\link{R6Class}} object.
}
\value{
Object of \code{\link{R6Class}} for modelling an Atom feed
}
\description{
This class models an atom feed
}
\details{
AtomFeed
}
\section{Fields}{

\describe{
\item{\code{id}}{Identifier}

\item{\code{updated}}{Update date}

\item{\code{title}}{Title}

\item{\code{subtitle}}{Subtitle}

\item{\code{rights}}{Rights (license, use, ...)}

\item{\code{author}}{Author person}

\item{\code{contributor}}{Contributor person}

\item{\code{generator}}{Generator}

\item{\code{icon}}{Icon}

\item{\code{logo}}{Logo}

\item{\code{category}}{Category}

\item{\code{entries}}{List of entries}
}}

\section{Methods}{

\describe{
 \item{\code{new(xml)}}{
   This method is used to create an Atom Feed
 }
 \item{\code{setId(id)}}{
   Set identifier
 }
 \item{\code{setUpdated(updated)}}{
   Set update date (object of class 'character' or 'POSIX')
 }
 \item{\code{addLink(link, rel, type)}}{
   Adds a link. Default \code{rel} value is set to "alternate". Default
   \code{type} value is set to "text/html"
 }
 \item{\code{delLink(link, rel, type)}}{
   Deletes a link
 }
 \item{\code{setSelfLink(link)}}{
   Sets a self-relation link
 }
 \item{\code{setAlternateLink(link, type)}}{
   Sets an alternate-relation link. Default type is "text/html"
 }
 \item{\code{setTitle(title)}}{
   Set title
 }
 \item{\code{setSubtitle(subtitle)}}{
   Set subtitle
 }
 \item{\code{addAuthor(author)}}{
   Adds an author, object of class \code{AtomAuthor}
 }
 \item{\code{delAuthor(author)}}{
   Deletes an author, object of class \code{AtomAuthor}
 }
 \item{\code{addContributor(contributor)}}{
   Adds a contributor, object of class \code{AtomContributor}
 }
 \item{\code{delContributor(contributor)}}{
   Deletes a contributor, object of class \code{AtomContributor}
 }
 \item{\code{setGenerator(generator, type)}}{
   Sets generator
 }
 \item{\code{setIcon(icon)}}{
   Sets icon
 }
 \item{\code{addCategory(term, scheme, label)}}{
   Adds a category
 }
 \item{\code{delCategory(term, scheme, label)}}{
   Deletes a category
 }
 \item{\code{addEntry(entry)}}{
   Adds an entry, object of class \code{AtomEntry}
 }
 \item{\code{delEntry(entry)}}{
   Deletes an entry, object of class \code{AtomEntry}
 }
}
}

\examples{
  #encoding
  atom <- AtomFeed$new()
  atom$setId("my-atom-feed")
  atom$setTitle("My Atom feed title")
  atom$setSubtitle("MyAtom feed subtitle")
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
  atom$setIcon("https://via.placeholder.com/300x150.png/03f/fff?text=atom4R")
  atom$setSelfLink("http://example.com/atom.feed")
  atom$setAlternateLink("http://example.com/my-atom-feed")
  atom$addCategory("dataset")
  atom$addCategory("spatial")
  atom$addCategory("fisheries")
  #add entry
  entry <- AtomEntry$new()
  entry$setId("my-atom-entry")
  entry$setTitle("My Atom feed entry")
  entry$setSummary("My Atom feed entry very comprehensive abstract")
  author1 <- AtomAuthor$new(
    name = "John Doe",
    uri = "http://www.atomxml.com/johndoe",
    email = "johndoe@atom4R.com"
  )
  entry$addAuthor(author1)
  author2 <- AtomAuthor$new(
    name = "John Doe's sister",
    uri = "http://www.atomxml.com/johndoesister",
    email = "johndoesister@atom4R.com"
  )
  entry$addAuthor(author2)
  contrib1 <- AtomContributor$new(
    name = "Contrib1",
    uri = "http://www.atomxml.com/contrib1",
    email = "contrib1@atom4R.com"
  )
  entry$addContributor(contrib1)
  contrib2 <- AtomContributor$new(
    name = "Contrib2",
    uri = "http://www.atomxml.com/contrib2",
    email = "contrib2@atom4R.com"
  )
  entry$addContributor(contrib2)
  entry$addCategory("dataset")
  entry$addCategory("spatial")
  entry$addCategory("fisheries")
  atom$addEntry(entry)
  xml <- atom$encode()

}
\author{
Emmanuel Blondel <emmanuel.blondel1@gmail.com>
}
\keyword{atom}
\keyword{feed}
