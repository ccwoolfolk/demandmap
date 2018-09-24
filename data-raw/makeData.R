### ----- makeData -------------------------------------------------------------
### Create data/raw.RData
### ----------------------------------------------------------------------------
makeData <- function () {
  censusData <- read_excel(
    paste('data-raw/full-mfg.xlsx', sep=''),
    col_types = 'text',
    skip=1
  )
  devtools::use_data(censusData, internal=TRUE, overwrite=TRUE)
}
