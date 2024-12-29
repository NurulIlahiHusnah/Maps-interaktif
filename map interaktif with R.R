library(sp)
library(leaflet)
library(htmltools)
library(geosphere)
library(rvest)
library(dplyr)
library(stringr)

toPct <- function(x) { round(as.numeric(gsub("%", "", x)) / 100, 4) }
formatKMB <- function(x) {
  tmp <- gsub('K', 'e3', x)
  tmp <- gsub('M', 'e6', tmp)
  tmp <- gsub('B', 'e9', tmp)
  as.numeric(tmp)
}

# Scrape FV Table
url <- "https://finviz.com/groups.ashx?g=country&v=140&o=name"
COUNTRY <- read_html(url) %>%
  html_nodes("table") %>%
  .[[8]] %>%
  html_table(header = TRUE, fill = TRUE)

# Manipulasi data
if (ncol(COUNTRY) > 1) {
  noms <- names(COUNTRY)
  noms <- gsub("/", "", noms)
  noms <- gsub(" ", "", noms)
  colnames(COUNTRY) <- noms
} else {
  stop("COUNTRY tidak memiliki kolom yang valid.")
}
# Tambahkan latitude/longitude
caps <- read.csv("https://gist.githubusercontent.com/ofou/df09a6834a8421b4f376c875194915c9/raw/355eb56e164ddc3cd1a9467c524422cb674e71a9/country-capital-lat-long-population.csv", quote="")
names(caps) <- tolower(names(caps))
caps$country <-str_replace_all(caps$country, c(
  "Republic of Korea" = "South Korea",
  "China, Hong Kong SAR" = "Hong Kong",
  "China, Taiwan Province of China" = "Taiwan",
  "United States of America" = "USA"
))

TBL <- data.frame(do.call(rbind, lapply(1:nrow(COUNTRY), function(i) {
  country <- COUNTRY[i,]
  SUB <- caps[which(caps$country == country$Name),]
  if (nrow(SUB) > 0) {
    country$longitude <- SUB$longitude
    country$latitude <- SUB$latitude
  } else {
    country$longitude <- NA
    country$latitude <- NA
  }
  country
})))

# Plot
DATA <- na.omit(TBL)
DATA <- DATA[!is.na(DATA$latitude) & !is.na(DATA$longitude),]
DATA$longitude <- as.numeric(DATA$longitude)
DATA$latitude <- as.numeric(DATA$latitude)
DATA$PerfYTD <- toPct(DATA$PerfYTD)  # Konversi ke numerik
coordinates(DATA) <- ~longitude + latitude
proj4string(DATA) <- CRS("+proj=longlat +datum=WGS84")

leaflet() %>%
  addMarkers(data = DATA, ~longitude, ~latitude, 
             label = ~htmlEscape(paste0(Name, " [", round(PerfYTD * 100, 2), "%]"))) %>%
  addTiles() %>%
  addCircleMarkers(data = DATA, ~longitude, ~latitude, 
                   radius = ~sqrt(abs(PerfYTD * 100)))

