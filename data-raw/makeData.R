### ----- makeData -------------------------------------------------------------
### Create data/raw.RData
### ----------------------------------------------------------------------------
makeData <- function () {
  rawData <- read_excel(
    paste('data-raw/full-mfg.xlsx', sep=''),
    col_types = 'text'
  )[-1, ]

  ### Remove Washington D.C.
  censusData <- rawData %>% filter(`GEO.display-label` != 'District of Columbia')


  devtools::use_data(censusData, internal=TRUE, overwrite=TRUE)
}
