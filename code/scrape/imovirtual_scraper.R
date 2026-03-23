library(httr)
library(rvest)
library(assertthat)
library(jsonlite)
library(stringr)
library(dplyr)
library(tidygeocoder)
library(DBI)
library(RSQLite)
library(httr2)

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
  
  cards <- page %>%
    html_elements("section[data-sentry-component='BaseCard']")
  
  nads <- length(cards)
  
  results <- lapply(cards, function(card) {
    
    link <- card %>%
      html_element("a[data-cy='listing-item-link']") %>%
      html_attr("href")
    
    id <- str_extract(link, "ID[[:alnum:]]+")
    
    description <- card %>%
      html_element("p[data-cy='listing-item-title']") %>%
      html_text(trim = TRUE)
    
    price <- card %>%
      html_elements("span[data-sentry-element='MainPrice']") %>%
      html_text(trim = TRUE)
    
    # Handle multiple prices
    if (length(price) != 1) {
      return(NULL)  # skip messy listings
    }
    
    data.frame(
      id = id,
      link = link,
      description = description,
      price = price,
      stringsAsFactors = FALSE
    )
  })
  
  # Combine and remove NULLs
  results <- do.call(rbind, results)
  return(list(results=results,nads=nads))  
}

scrape_listings <- function(){
  all_links <- c()
  all_descriptions <- c()
  all_prices <- c()
  all_ids <- c()
  page <- 1
  nzeros <- 0
  zsame <- 0
  olddescriptions <- 0
  repeat {
    cat("Scraping page", page, "\n")
    
    pageresults <- get_listing_info(page)
    results <- pageresults$results
    nads <- pageresults$nads
    if(length(results$link)==0){
      nzeros <- nzeros+1
    }else{
      nzeros <- 0
    }
    links <- results$link
    descriptions <- results$description
    prices <- results$price
    ids <- results$id
    assert_that(length(links)==length(descriptions),msg=paste0("Different number of links and descriptions on page ",page))
    assert_that(length(links)==length(prices),msg=paste0("Different number of links and prices on page ",page))
    assert_that(length(links)==length(ids),msg=paste0("Different number of links and ids on page ",page))
    if(identical(descriptions,olddescriptions)){
      nsame <- nsame+1
    }else{
      nsame <-  0
    }
    if(nsame == 5){
      print("Same listings found on five subsequent pages - Stopping")
      break
    }
    if(nzeros==5){
      print("No listings found on five subsequent pages - Stopping")
      break
    }
    
    if (nads==0) {
      cat("No listings found — stopping.\n")
      break
    }
    
    print(paste0(length(links)," listings scraped"))
    
    all_links <- c(all_links, links)
    all_descriptions <- c(all_descriptions, descriptions)
    all_prices <- c(all_prices,prices)
    all_ids <- c(all_ids,ids)
    olddescriptions <- descriptions
    
    page <- page + 1
    
    Sys.sleep(1.5)  # be polite
  }
  return(data.frame(id=all_ids,link=all_links,description=all_descriptions,price=all_prices))
}

scrape_ad <- function(url){
  resp <- GET(
    url,
    add_headers("User-Agent" = "Mozilla/5.0")
  )
  
  page <- read_html(resp)
  features <- page %>%
    html_elements("div.css-1okys8k.e178zspo0") %>%
    html_text(trim = TRUE)
  
  area <- features[which(features=="Área:")+1]
  tipologia <- features[which(features=="Tipologia:")+1]
  andar <- features[which(features=="Andar:")+1]
  anunciante <- features[which(features=="Tipo de anunciante:")+1]
  tipo <- features[which(features=="Tipo de imóvel:")+1]
  novo  <- features[which(features=="Nova construção:")+1]
  jardim <- ifelse(any(grepl("jardim", features)), 1, 0)
  energia <- features[which(features=="Certificado energético:")+1]
  elevador <- features[which(features=="Elevador:")+1]
  garagem <- ifelse(any(grepl("garagem", features)), 1, 0)
  terraco <- ifelse(any(grepl("terraço", features)), 1, 0)
  varanda <- ifelse(any(grepl("varanda", features)), 1, 0)
  
  script <- page %>%
    html_element("script#__NEXT_DATA__") %>%
    html_text()
  
  data <- fromJSON(script)
  lat <- data$props$pageProps$ad$location$coordinates$latitude
  lon <- data$props$pageProps$ad$location$coordinates$longitude
  
  id <- data$props$pageProps$id
  
  neighbourhood <- convert_coordinates_to_neighbourhood(lon,lat)
  
  
  return(c(id,area,tipologia,andar,anunciante,tipo,novo,jardim,energia,elevador,garagem,terraco,varanda,lat,lon,neighbourhood))
}

update_database <- function(db){
  today <- Sys.Date()
  
  # 1. scrape listing pages
  current_ads <- scrape_listings()
  
  # 2. load db
  db_ads <- read_db(db)
  pricetable <- read_db(pricedb)
  
  # --- NEW ---
  new_ids <- setdiff(current_ads$id, db_ads$ad_id)
  
  # --- EXISTING ---
  existing_ids <- intersect(current_ads$id, db_ads$ad_id)
  
  # --- INACTIVE ---
  inactive_ids <- setdiff(
    db_ads$ad_id[db_ads$is_active == TRUE],
    current_ads$id
  )
  
  #Add new listings
  new_listings <- current_ads[current_ads$id %in%  new_ids,]
  new_data <- scrape_new_ads(new_listings,today)
  db_ads <- rbind(db_ads,new_data)
  
  #Update active existing listings
  db_ads$last_seen[db_ads$ad_id %in% existing_ids] <- today
  for(id in existing_ids){
    dbprice <- db_ads$price[db_ads$ad_id==id]
    currentprice <- current_ads$price[current_price$id==id]
    if(dbprice!=currentprice){
      pricetable <- update_price_table(id,dbprice,currentprice,today,pricetable)
      db_ads$price[db_ads$ad_id==id] <- currentprice
    }
  }
  
  #Update inactive existing listings
  db_ads$is_active[db_ads$ad_id %in% inactive_ids]  <- "No"
}

scrape_new_ads <- function(new_listings,date){
  newdata <- matrix(nrow=nrow(new_listings),ncol=16)
  for(i in 1:nrow(new_listings)){
    newdata[i,] <- scrape_ad(new_listings$link[i])
  }
  newdata <- data.frame(newdata)
  colnames(newdata) <- c("id","area","tipologia","andar","anunciante","tipo","novo","jardim","energia","elevador","garagem","terraco","varanda","lat","lon","neighbourhood")
  newdata$price <- new_listings$price
  newdata$is_active <- "Yes"
  newdata$first_seen <- date
  newdata$last_seen <- date
  return(newdata)
}

update_price_table <- function(id,dbprice,currentprice,today,pricetable){
  newdata <- c(id,dbprice,currentprice,today)
  newdata <- data.frame(t(newdata))
  colnames(newdata) <- c("id","old_price","new_price","date")
  pricetable <- rbind(pricetable,newdata)
  return(pricetable)
}

convert_coordinates_to_neighbourhood <- function(lon,lat){
  df <- data.frame(lon=lon,lat=lat)
  result <- reverse_geocode(
    df,
    lat = lat,
    long = lon,
    method = "osm",
    full_results = TRUE
  )
  
  # Extract best available "neighbourhood-like" field
  neighbourhood <- result %>%
    transmute(
      neighbourhood = coalesce(
        neighbourhood,
        suburb
      )
    ) %>%
    pull(neighbourhood)
  return(neighbourhood)
}