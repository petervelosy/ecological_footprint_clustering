library(ggplot2)
library(readxl)
library(dplyr)
library(magrittr)
library(stringr)
library(factoextra)

raw.allCountries <- read_excel("biokapacitas_okologiai_labnyom.xlsx")
raw.2bExcluded <- read_excel("biokapacitas_okologiai_labnyom_2Bnelkul.xlsx")

alternativeNames <- read_excel("biokapacitas_okologiai_labnyom_2Bnelkul_neparannyal.xlsx", "Munka2", col_names = c("name", "alt_name"))

removeEmptyColumns <- function(df) {
  Filter(function(x) !all(is.na(x)), df)
}

convertInternalCellsToNumeric <- function(df) {
  convertCell <- function(x) round(as.numeric(x), 2)
  df[-length(df)][-1] <- lapply(df[-length(df)][-1], convertCell)
  df
}

raw.allCountries <- convertInternalCellsToNumeric(removeEmptyColumns(raw.allCountries))
raw.2bExcluded <- convertInternalCellsToNumeric(removeEmptyColumns(raw.2bExcluded))

cleanColumnName <- function(columnName) {
  str_replace_all(iconv(columnName, from="UTF-8", to="ASCII//TRANSLIT"), " ", "_")
}

colnames(raw.allCountries) <- lapply(colnames(raw.allCountries), cleanColumnName)
colnames(raw.2bExcluded) <- lapply(colnames(raw.2bExcluded), cleanColumnName)

data.biocapacity <- raw.2bExcluded %>%
  filter(grepl("BC", Orszag, fixed = TRUE)) %>%
  mutate(Orszag = str_replace(Orszag, " BC", ""))

data.footprint <- raw.2bExcluded %>%
  filter(grepl("EF", Orszag, fixed = TRUE)) %>%
  mutate(Orszag = str_replace(Orszag, " EF", ""))

raw.biocapacity.unified <- raw.2bExcluded %>%
  filter(grepl("BC", Orszag, fixed = TRUE)) %>%
  mutate(Orszag = str_replace(Orszag, " BC", ""))

raw.footprint.unified <- raw.2bExcluded %>%
  filter(grepl("EF", Orszag, fixed = TRUE)) %>%
  mutate(Orszag = str_replace(Orszag, " EF", ""))

colnames(raw.footprint.unified)[-1] <- lapply(colnames(raw.footprint.unified)[-1], paste, "_EF", sep="")

colnames(raw.biocapacity.unified)[-1] <- lapply(colnames(raw.biocapacity.unified)[-1], paste, "_BC", sep="")

data.unified <- cbind(raw.biocapacity.unified[1], raw.biocapacity.unified[-1], raw.footprint.unified[-1])

countriesToKeep <- c("Argentína", "Azerbajdzsán", "Bangladesh", "Botswana", "Burundi", "Csehország", "Dánia",   "Dél-Korea",   "Észak-Korea",   "Finnország",   "Görögország",   "Horvátország",   "Japán",   "Jemen",   "Kelet-Timor",   "Kína",   "Lengyelország",   "Lesotho",   "Líbia",   "Magyarország",   "Norvégia",   "Olaszország",   "Örményország",   "Portugália",   "Ruanda", "Szaúd-Arábia",   "Szíria", "Szlovénia",  "Szváziföld (Eswatini)", "Tunézia",   "Uruguay")

data.unified <- data.unified %>%
  filter(Orszag %in% countriesToKeep) %>%
  select(-Osszes_BC, -Osszes_a_tablazatban_BC, -Osszes_EF, -Osszes_a_tablazatban_EF, -Adatminoseg_BC, -Adatminoseg_EF)

dataToCluster <- data.unified[-1]
rownames(dataToCluster) <- data.unified$Orszag

write.csv(dataToCluster, file = "dataToCluster.csv", row.names = TRUE)