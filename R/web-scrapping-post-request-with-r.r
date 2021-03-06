################################################################################
#                  Web scrapping using search request with R
#                  Milos Popovic
#                  2022/03/06
################################################################################

windowsFonts(georg = windowsFont('Georgia'))

# libraries we need
libs <- c("rvest", "httr", "tidyverse", "sf", "giscoR", "cartogram")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# 1. REQUEST ARMS DATA
#---------

get_data <- function(url, res, r) {

  url <- "https://armstrade.sipri.org/armstrade/html/export_values.php"

  res <- POST(
    url = url,
    encode = "form",
    body = list(
      `low_year` = '2016',
      `high_year` = '2020',
      `import_or_export` = 'export',
      `country_code` = 'USA',
      `summarize` = 'country',
      `filetype` = 'html'
    )
  )

  r <- res %>% 
    read_html() %>%
    html_table()

  return(r)
}

# 2. CLEAN UP ARMS DATA
#---------

clean_data <- function(df, d) {

  df <- get_data() # call list
  d <- df[[2]] %>% #extract table from the list
      as_tibble() %>% # convert to data.frame
      t() #transpose
  d <- d[,-c(1:10, (ncol(d)-1):ncol(d))] #get rid of descriptive/empty columns
  colnames(d) <- as.character(d[1,]) # replace header with first row
  d <- d[-1,] %>%
    as.data.frame()
  names(d)[1] <- "year" #label year column
  d <- d[d$year == "Total", ]  #filter Total values for the period
  d <- d %>%
         filter_all(any_vars(!is.na(.))) %>% #only complete rows
         gather(country, value, -year) %>% #reshape
         select(country, value) %>% #select only country and value
         mutate(value = as.numeric(value)) # declare value to be numeric
  
  return(d)
}

# 3. FETCH EUROPEAN COUNTRIES
#---------

europeList <- function(urlfile, iso3) {
  urlfile <-'https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv'
  
  iso3 <- read.csv(urlfile) %>%
        filter(region=="Europe") %>%
        select("name", "alpha.3") %>%
        rename(iso3 = alpha.3,
               country = name) %>%
        mutate(country = replace(country, str_detect(country, "Bosnia and Herzegovina"), "Bosnia-Herzegovina")) %>% 
        mutate(country = replace(country, str_detect(country, "United Kingdom of Great Britain and Northern Ireland"), "United Kingdom")) 
  return(iso3)
}

countries <- europeList()

# load national map of Europe
europeMap <- function(europe, eur) {
  
  europe <- giscoR::gisco_get_countries(
  year = "2016",
  epsg = "4326",
  resolution = "10",
  region = "Europe"
) %>%
  rename(iso3 = ISO3_CODE) 

eur <- europe %>% dplyr::filter(iso3%in%countries$iso3)
return(eur)
}

# join arms and list of European countries
europeArmsData <- function(arms, l) {

l <- europeList()

arms <- clean_data() %>%
  right_join(l, "country") %>%
  filter_all(any_vars(!is.na(.)))

return(arms)
}

# 4. MERGE ARMS DATA AND EUROPE SF OBJECT
#---------

joinArmsData <- function(armsDF, eurSF, arms) {

armsDF <- europeArmsData()
eurSF <- europeMap()

arms <- eurSF %>%
  left_join(armsDF, "iso3") %>%
  select(country, value, geometry)

return(arms)
}

# 5. CARTOGRAM
#---------
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

getNonCartogram <- function(f, nc) {
  
  f <- joinArmsData() %>%
        st_transform(crs = crsLAEA)
  nc <- cartogram_ncont(f,
                  weight = "value",
                  inplace=F)
return(nc)
}


# 6. BOUNDING BOX
#----------
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

bbox <- function(bb, laeabb, b) {

bb <- st_sfc(
  st_polygon(list(cbind(
    c(-10.6600, 33.00, 33.00, -10.6600, -10.6600),
    c(32.5000, 32.5000, 71.0500, 71.0500, 32.5000) 
    ))),
  crs = crsLONGLAT)

laeabb <- st_transform(bb, crs = crsLAEA)
box <- st_bbox(laeabb)
return(box)
}


# 7. PLOT
#----------

u <- "https://raw.githubusercontent.com/milos-agathon/web-scrapping-sipri-arms-data-using-search-request-with-r/main/R/arms_map.r"
source(u) # load script
map <- armsMap()
ggsave(filename="sipri.png", width=7, height=8.5, dpi = 600, device='png', map)
