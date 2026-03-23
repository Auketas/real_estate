library(httr)
library(rvest)
library(assertthat)

get_listing_info <- function(page_num) {
  url <- paste0(
    "https://www.imovirtual.com/pt/resultados/comprar/apartamento/porto/porto?page=",
    page_num
  )
  
  resp <- GET(
    url,
    add_headers("User-Agent" = "Mozilla/5.0")
  )
  
  page <- read_html(resp)
  
  links <- page %>%
    html_elements('a[data-cy="listing-item-link"]') %>%
    html_attr("href")
  
  descriptions <- page %>%
    html_elements('p[data-cy="listing-item-title"]') %>%
    html_text()
    
  prices <- page %>%
    html_nodes("span[data-sentry-element='MainPrice']") %>%
    html_text(trim = TRUE)
  prices <- prices[grepl("€\\s*$", prices)]
  
  
  if (length(links) == 0) {
    return(NULL)
  }
  assert_that(length(links)==length(descriptions),msg=paste0("links and descriptions on page ",page_num," don't have same length"))
  assert_that(length(links)==length(prices),msg=paste0("links and prices on page ",page_num," don't have same length"))
  
  return(data.frame(links=paste0("https://www.imovirtual.com", links),descriptions=descriptions,prices=prices))
}

all_links <- character()
all_descriptions <- character()
all_prices <- character()
page <- 1

repeat {
  cat("Scraping page", page, "\n")
  
  results <- get_listing_info(page)
  links <- results$links
  descriptions <- results$descriptions
  prices <- results$prices
  
  if (length(setdiff(links, all_links)) == 0) {
    cat("No new listings — stopping.\n")
    break
  }
  
  if (is.null(links)) {
    cat("No listings found — stopping.\n")
    break
  }
  
  all_links <- c(all_links, links)
  all_descriptions <- c(all_descriptions, descriptions)
  all_prices <- c(all_prices,prices)
  page <- page + 1
  
  Sys.sleep(1.5)  # be polite
}

all_links <- unique(all_links)
all_descriptions <- unique(all_descriptions)
all_prices <- unique(all_prices)
length(all_links)
length(all_descriptions)
length(all_prices)


