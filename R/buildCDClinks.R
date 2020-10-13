#' Build CDC link
#'
#' Function takes as input a partial Boolean search string and produces a functioning (set of)
#' URLs; one for each page og search results on the CDC website.
#' @description Constructs series of CDC website search page URLs
#' @param and_terms Vector of alphanumeric terms searched using the AND Boolean operator,
#' specified by the CDC as 'with all of the words'.
#' @param exact_phrase Vector of alphanumeric terms enclosed in inverted commas and searched
#' as phrases (e.g. "large cat"), specified by the CDC as 'with the exact phrase'.
#' @param or_terms Vector of alphanumeric terms searched using the OR Boolean operator,
#' specified by the CDC as 'with at least one of the words'.
#' @param not_terms Vector of alphanumeric terms searched using the NOT Boolean operator,
#' specified by the CDC as 'without the words'.
#' @param language Two-letter language code for search language. The default is 'en' (English).
#' @param date_from The date (e.g. 01/01/2000) from which searching will be performed
#' (inclusive). If no value provided, all dates are searched.
#' @param date_to The date (e.g. 01/10/2020) to which searching will be performed
#' (inclusive). If no value provided, all dates are searched.
#' @param start_page Integer specifying which page(s) of search results should be displayed.
#' If multiple pages are selected, multiple URLs are returned, one for each page of ten
#' search results.
#' @param pages Integer for the number of pages of search results to be returned (one link per
#' page). A maximum of 100 pages can be displayed in the CDC. The default value is 1.
#' @examples
#' and_terms <- c('disease', 'spread')
#' not_terms <- c('animal', 'fish')
#' exact_phrase <- c('corona virus')
#' or_terms <- c('pandemic', 'global')
#' date_from <- '01/01/1980'
#' date_to <- '10/11/2020'
#' link <- buildCDClinks(and_terms = and_terms,
#'     not_terms = not_terms,
#'     exact_phrase = exact_phrase,
#'     or_terms = or_terms,
#'     date_from = date_from,
#'     date_to = date_to,
#'     pages = 3)
#' link;
#' @return A link containing the specified search results.
#' @export

buildCDClinks <- function(and_terms = '',
                          not_terms = '',
                          exact_phrase = '',
                          or_terms = '',
                          date_from = '',
                          date_to = '',
                          pages = 1,
                          start_page = 1,
                          language = '') {
    #calculations
    and_terms <- if(any(and_terms == '') == TRUE) { #if and_terms is blank then leave it blank, otherwise combine terms with '+'
      and_terms <- ''
    } else {
      and_terms <- paste('all=',
                         gsub(' ',
                              '%20',
                              paste(and_terms,
                                    collapse = '%20')),
                         sep = '')
    }

    not_terms <- if(any(not_terms == '') == TRUE){ #if not_terms is blank then leave it blank, otherwise combine terms with '+OR+'
      not_terms <- ''
    } else {
      not_terms <- paste('&none=',
                         gsub(' ',
                              '%20',
                              paste(not_terms,
                                    collapse = '%20')),
                         sep = '')
    }

    exact_phrase <- if(any(exact_phrase == '') == TRUE){ #if exact_phrase is blank then leave it blank, otherwise combine terms with '+' and top/tail with '+%22'/'%22'
      exact_phrase <- ''
    } else {
      exact_phrase <- paste('&exact=',
                            gsub(' ',
                                 '%20',
                                 paste(exact_phrase,
                                       collapse = '%20')),
                            sep = '')
    }

    or_terms <- if(any(or_terms == '') == TRUE){ #if or_terms is blank then leave it blank, otherwise combine terms with '+OR+'
      or_terms <- ''
    } else {
      or_terms <- paste('&any=',
                        gsub(' ',
                             '%20',
                             paste(or_terms,
                                   collapse = '%20')),
                        sep = '')
    }

    if((language == '') == TRUE){ #specify the language
      language <- ''
    } else {
      language <- paste('&language=',
                      language,
                      sep = '')
    }

    if((date_from == '') == TRUE){ #specify the start date
      date_from <- ''
    } else {
      date_from <- paste('&date1=',
                         gsub('\\/',
                              '%2F',
                              date_from),
                         sep = '')
    }
    if((date_to == '') == TRUE){ #specify the stop date
      date_to <- ''
    } else {
      date_to <- paste('&date2=',
                       gsub('\\/',
                            '%2F',
                            date_to),
                       sep = '')
    }

    #build URL
    if (pages == 1){ #if pages = 1 then only a single link is generated
      page <- start_page
      link <- paste('https://search.cdc.gov/search/index.html?',
                    and_terms,
                    not_terms,
                    exact_phrase,
                    or_terms,
                    language,
                    date_from,
                    date_to,
                    page,
                    sep = '')
    } else { #otherwise, one link is generated for each page
      init <- seq(start_page, pages, 1)
      page <- paste('&dpage=',
                    init,
                    '#results',
                    sep = '')
      link <- paste('https://search.cdc.gov/search/index.html?',
                 and_terms,
                 not_terms,
                 exact_phrase,
                 or_terms,
                 language,
                 date_from,
                 date_to,
                 page,
                 sep = '')
    }
    return(link)
}
