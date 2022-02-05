#' Country finder
#' @author Thomas Brailey
#' @description Uses wikipedia entries to guess the country of origin of a 
#' particular phrase, such as a paramilitary group, a party slogan, and the 
#' like. 
#' @param name A string vector that you want to find the corresponding country to
#' @return country A country name from ACLED's country list

# Manually add countries if information is missing. Jot down sources to verify information
find_country <- function(name, root = TRUE) {
  
  # Display keywords you are searching for
  print(paste0("Searching for keyphrase: ", name))
  
  # Generate URL
  url  <- URLencode(paste0("https://www.google.com/search?q=", name, " wikipedia"))
  page <- xml2::read_html(url)
  
  # Extract all relevant links
  nodes <- rvest::html_nodes(page, "a")
  links <- rvest::html_attr(nodes,"href")
  
  # Iterate over possible links to find country
  for(x in 1:length(links)){
    
    # Extract first link of the search results
    link <- links[startsWith(links, "/url?q=")][x]
    
    # Clean link
    link <- sub("^/url\\?q\\=(.*?)\\&sa.*$","\\1", link)
    link <- sub("25","", link)
    link <- sub("%E2%2580%2593", "–", link)
    link <- sub("%25C3%25A9", "é", link)
    
    # Tell user which link you're trying 
    print(paste0("Trying link ", link))
    
    # Enter site
    site <- try(expr = link %>% httr::GET(., httr::timeout(10)) %>% rvest::read_html(.), silent = TRUE)
    
    # If list of links is exhausted, break and go to next keyphrase
    if(sum(class(site) == "try-error") == 1 & x == length(links)){
      country <- NA 
      break
    }
    
    # If link times out or is bad, go to next link
    if(sum(class(site) == "try-error") == 1){
      next
    }
    
    # Pull all words from first few paragraphs of site 
    text <- try(expr = site %>% 
                  html_nodes("p") %>% 
                  html_text() %>% 
                  .[nchar(.) > 150] %>%
                  strsplit(split = " ") %>% 
                  unlist(.) %>%
                  stringr::str_replace_all(., pattern = "of|the", "") %>%
                  stringr::str_replace_all(., pattern = "\\W+|\\d+", " ") %>%
                  stringr::str_c(., collapse = " ") %>%
                  stringr::str_squish(.),
                silent = TRUE)
    
    # If link error, go to next link
    if(sum(class(text) == "try-error") == 1){
      next
    }
    
    # If data has 0 observations, go to next link
    if(length(text) == 0){
      next
    }
    
    # Pull key country names from text
    matching_dat <- tibble::as_tibble(unlist(stringr::str_match_all(text, stringr::str_c(countries_acled, collapse="|")))) %>% 
      dplyr::filter(!is.na(value)) %>%
      dplyr::distinct(value)
    
    # Save matched cases as a vector
    partial_match <- matching_dat$value
    
    # Loop through strings to find country match
    i <- 1
    for(i in 1:length(partial_match)){
      
      pm <- paste(partial_match[1:i], collapse = " ")
      
      # If we get a match, break inner loop
      if(sum(stringr::str_detect(pattern = countries_acled, string = pm)) > 0){
        country <- countries_acled[stringr::str_detect(pattern = pm, string = countries_acled)]
        break
      }
    }
    
    # If we get a match, break outer loop 
    if(exists("country")){
      break
    }
  }
  
  # Print likely match and return value 
  print(paste0("Most likely country: ", country, "."))
  return(country)
}