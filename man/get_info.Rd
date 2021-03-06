% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrapeCDC.R
\name{get_info}
\alias{get_info}
\title{Scrape all CDC results information}
\usage{
get_info()
}
\value{
A dataframe containing all extractable information from all text files in the working
directory.
}
\description{
A wrapper for the scraping functions to produce a dataframe of citations for
each page of CDC search results.
}
\examples{
info <- get_info();
}
