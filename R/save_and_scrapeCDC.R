#' Global wrapper function for generating, saving and scraping info
#'
#' @description Function wraps the `buildCDClinks()`, `save_codes()`, and scrape functions.
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
#' @param browser The name of the browser used for scraping. Default is 'firefox'.
#' @return A dataframe containing all extractable information from all html files in the working
#' directory.
#' @importFrom magrittr "%>%"
#' @examples
#' and_terms <- c('disease', 'spread')
#' not_terms <- c('animal', 'fish')
#' exact_phrase <- c('corona virus')
#' or_terms <- c('pandemic', 'global')
#' date_from <- '01/01/1980'
#' date_to <- '10/11/2020'
#' info <- save_and_scrapeCDC(and_terms = and_terms,
#'     not_terms = not_terms,
#'     exact_phrase = exact_phrase,
#'     or_terms = or_terms,
#'     date_from = date_from,
#'     date_to = date_to,
#'     pages = 3)
#' head(info);
#'@export
save_and_scrapeCDC <- function(and_terms = '',
                               not_terms = '',
                               exact_phrase = '',
                               or_terms = '',
                               date_from = '',
                               date_to = '',
                               pages = 1,
                               start_page = 1,
                               language = '',
                               browser = 'firefox'){
  links <- buildCDClinks(and_terms = and_terms,
                         not_terms = not_terms,
                         exact_phrase = exact_phrase,
                         or_terms = or_terms,
                         date_from = date_from,
                         date_to = date_to,
                         pages = pages,
                         start_page = start_page,
                         language = language)
  save_codes(urls = links,
             browser = browser)
  info <- get_info()
  info <- tibble::as_tibble(info)
  report <- paste('Search parameters:',
                  paste('All these words: ',
                       paste(and_terms,
                             collapse = '; '),
                       sep = ''),
                 paste('None of these words: ',
                       paste(not_terms,
                             collapse = '; '),
                       sep = ''),
                 paste('This exact word or phrase: ',
                       paste('"',
                             exact_phrase,
                             '"',
                             collapse = '; '),
                       sep = ''),
                 paste('Any these words: ',
                       paste(or_terms,
                             collapse = '; '),
                       sep = ''),
                 paste('Between these dates:',
                       date_from,
                       'and',
                       date_to,
                       sep = ' '),
                 if(pages == ''){
                   'Number of pages exported: 1'
                   } else {
                     paste('Number of pages exported: ',
                           pages,
                           sep = '')
                   },
                 if(start_page == ''){
                   'Starting from page: 1'
                 } else {
                   paste('Starting from page: ',
                         start_page,
                         sep = '')
                 },
                 if(language == ''){
                   'Language: Any'
                 } else {
                   paste('Language: ',
                         language,
                         sep = '')
                 },
                 if(browser == ''){
                   'Browser used for searches: firefox'
                 } else {
                   paste('Browser used for searches: ',
                       browser,
                       sep = '')
                   },
                 paste('Search date, time, timezone: ',
                       Sys.time(),
                       sep = ''),
                 '\n',
                 'CDC search pages exported:',
                 paste(links,
                       collapse = '\n'),
                 '\n',
                 sep = '\n')
  cat(report, file = 'searchreport.txt')
  return(info)
}
