#' Download one results page as html file
#'
#' @description Downloads one page of CDC results from a URLs as html files with
#' a specific wait-time to avoid IP address blocking.
#' @param url One URLs corresponding to a page of CDC search results.
#' @param browser The name of the browser used for scraping. Default is 'firefox'.
#' @importFrom magrittr "%>%"
#' @examples
#' url <- 'https://search.cdc.gov/search/index.html?all=disease%20spread&none=animal%20fish&exact=corona%20virus&any=pandemic%20global&date1=01%2F01%2F1980&date2=10%2F11%2F2020&dpage=1'
#' code <- save_code(url);
#' @return An object containing the HTML code. A text file containing the html code
#' is saved to the working directory.
#' @export
save_code <- function(url, browser = 'firefox'){
  rD <- RSelenium::rsDriver(port = 1210L, browser = browser, check = FALSE)
  remDr <- rD$client
  remDr$navigate(url)
  Sys.sleep(1) #pause to allow page to load
  htmlcode <- remDr$getPageSource()[[1]] %>%
    xml2::read_html() %>%
    paste(collapse = '')
  write(htmlcode, 'text.txt')
  remDr$close()
  rm(rD)
  gc()

  return(htmlcode)
}


#' Download multiple results pages as html files
#'
#' @description Downloads one or more CDC URLs as html files with a specific wait-time
#' to avoid IP address blocking.
#' @param urls One or more URLs corresponding to pages of CDC search results.
#' @param browser The name of the browser used for scraping. Default is 'firefox'.
#' @importFrom magrittr "%>%"
#' @examples
#' urls <- c("https://search.cdc.gov/search/index.html?all=disease%20spread&none=animal%20fish&exact=corona%20virus&any=pandemic%20global&date1=01%2F01%2F1980&date2=10%2F11%2F2020&dpage=1#results",
#' "https://search.cdc.gov/search/index.html?all=disease%20spread&none=animal%20fish&exact=corona%20virus&any=pandemic%20global&date1=01%2F01%2F1980&date2=10%2F11%2F2020&dpage=2#results",
#' "https://search.cdc.gov/search/index.html?all=disease%20spread&none=animal%20fish&exact=corona%20virus&any=pandemic%20global&date1=01%2F01%2F1980&date2=10%2F11%2F2020&dpage=3#results")
#' codes <- save_codes(urls);
#' @return A list of objects containing the HTML codes for each file. Text files
#' containing the code are also saved to the working directory.
#' @export
save_codes <- function(urls, browser = 'firefox'){
  rD <- RSelenium::rsDriver(port = 1210L, browser = browser, check = FALSE)
  remDr <- rD$client
  x <- list()
  for (i in urls){
    remDr$navigate(i)
    Sys.sleep(1) #pause to allow page to load
    htmlcode <- remDr$getPageSource()[[1]] %>%
      xml2::read_html() %>%
      paste(collapse = '')
    write(htmlcode, paste('page',
                          gsub('#results',
                               '',
                               sub('.*page=',
                                   '',
                                   i)),
                          '.txt',
                          sep = ''))
    x <- append(x, htmlcode)
  }
  remDr$close()
  rm(rD)
  gc()

  return(x)
}
