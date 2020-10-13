#' Scrape all CDC results information
#'
#' @description A wrapper for the scraping functions to produce a dataframe of citations for
#' each page of CDC search results.
#' @return A dataframe containing all extractable information from all text files in the working
#' directory.
#' @importFrom magrittr "%>%"
#' @examples
#' info <- get_info();
#'@export
get_info <- function(){
  codes <- get_htmls_code()
  sourcefile <- rep(names(codes), each = 10)
  df <- data.frame()
  for (i in codes){
    code <- unlist(i)
    x <- split_by_div(code)
    title <- get_titles(x)
    link <- get_links(x)
    description <- get_descriptions(x)
    row <- cbind(title, link, description)
    df <- rbind(df, row)
  }
  df$sourcefile <- sourcefile
  return(df)
}


#' Scrape in code from local html file
#'
#'@description Scrape in code from a text file saved locally
#'@param filename Name of the file to be scraped. File should be saved in the working
#'directory.
#'@return A string of html code for a webpage
#'@examples
#'code <- get_html_code(file.choose());
#'@export
get_html_code <- function(filename){
  x <- xml2::read_html(filename)
  x <- paste(x, collapse = '')
  return(x)
}


#' Scrape in code from multiple local html files
#'
#'@description Scrape in code from html files saved locally. Files should be saved in the working
#'directory.
#'@return One or more strings of html code for a webpage, stored as a list
#'@examples
#'html_codes <- get_htmls_code()
#'file1 <- unlist(html_codes[1]);
#'@export
get_htmls_code <- function(){
  filenames <- list.files(getwd())
  filenames <- filenames[grep('.txt', filenames)] #select only the HTML files in the working directory
  x <- as.list(mapply(get_html_code, filenames))
  return(x)
}


#' Split file by '<div' code into different lines
#'
#' @description Split the imported html code into lines based on the '<div' field code.
#' @param html A string consisting of the html code for a webpage
#' @return A vector of strings, one for each separated line
#' @examples
#' lines <- split_by_div(code)
#' lines;
#' @export
split_by_div <- function(html) {
  x <- unlist(strsplit(html,
                       "\\<div",
                       useBytes = TRUE))
  return(x)
}


#' Extract titles from CDC results
#'
#' @description Extract the titles of search results from CDC search results, saved as html files.
#' @param html A vector of lines consisting of the html code for a webpage
#' @return A vector of result titles (10 per page)
#' @examples
#' titles <- get_titles(lines);
#' @export
get_titles <- function(html){
  y <- grep("data-index", html) #find location of lines containing results tag 'data-index'
  titles <- html[y+1] #extract lines
  titles <- (gsub("<.*?>", "", titles)) #remove code inside '<>'
  titles <- sub(" class=\"searchResultsTitle lead\">", "", titles) #remove field codes
  titles <- sub("\\|.*", "", titles)
  titles <- sub("</.*", "", titles) #remove field codes
  titles <- trimws(titles) #remove field codes
  return(titles)
}


#' Extract article descriptions from Google Scholar results
#'
#' @description Extract article descriptions (summaries of key sentences), from CDC
#' search results.
#' @param html A vector of lines consisting of the html code for a webpage
#' @return A vector of descriptions
#' @examples
#' descriptions <- get_descriptions(lines)
#' descriptions;
#' @export
get_descriptions <- function(html){
  y <- grep("data-index", html) #find location of lines containing results tag 'data-index'
  descriptions <- html[y+5] #extract lines
  descriptions <- sub(' class=\"searchResultsDescription\">', '', descriptions)
  descriptions <- gsub("<.*?>", "", descriptions)
  descriptions <- gsub("\\\n", " ", descriptions)
  descriptions <- gsub('</', '', descriptions)
  return(descriptions)
}


#' Extract article links from CDC results
#'
#' @description Extract links to websites holding article information from CDC
#' search results.
#' @param html A vector of lines consisting of the html code for a webpage
#' @return A vector of URLs
#' @examples
#' links <- get_links(lines)
#' links;
#' @export
get_links <- function(html){
  y <- grep("data-index", html) #find location of lines containing results tag 'data-index'
  links <- html[y+3] #extract lines
  links <- sub(' class=\"searchResultsUrl\">', '', links)
  links <- trimws(sub("</", "", links))
  return(links)
}
