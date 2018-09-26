library(readxl)
library(dplyr)
library(testthat)

### ----- stripRevised ---------------------------------------------------------
### Remove the "(r)" from a vector of numeric data
### ----------------------------------------------------------------------------
stripRevised <- function (values) {
  gsub(
    pattern='([0-9]*)\\(r\\)',
    replacement='\\1',
    x=values,
    fixed=FALSE
  )
}

### ----- code2estimate --------------------------------------------------------
### Convert a vector of combined numeric and EMP codes into numerical estimates
### ----------------------------------------------------------------------------
code2estimate <- function (values) {
  if (is.factor(values)) stop ('code2estimate given factor values instead of character')
  valueMap <- data.frame(
    code=c('a', 'b', 'c', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm'),
    minValue=c(0, 20, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000),
    maxValue=c(19, 99, 249, 499, 999, 2499, 4999, 9999, 24999, 49999, 99999, 200000) # 200,000 purely speculative
  )


  idx <- match(values, valueMap$code)

  output <- c()
  for (i in 1:length(idx)) {
    if (is.na(idx[i])) toAdd <- as.numeric(values[i])
    else toAdd <- mean(as.numeric(valueMap[idx[i], c('minValue', 'maxValue')]))
    output <- c(output, toAdd)
  }
  output
}

### ----- estimateEMP ----------------------------------------------------------
### Convert an EMP code into a numerical estimate
### ----------------------------------------------------------------------------
estimateEMP <- function (inputData) {
  inputData %>% mutate(EMP = code2estimate(EMP))
}

### ----- makeData -------------------------------------------------------------
### Create data/raw.RData
### ----------------------------------------------------------------------------
makeData <- function () {
  rawData <- read_excel(
    paste('data-raw/full-mfg.xlsx', sep=''),
    col_types = 'text'
  )[-1, ]

  ### Strip revised label from numeric data

  ### Remove Washington D.C.
  censusData <- rawData %>% mutate_all(
    stripRevised
  ) %>% filter(
    `GEO.display-label` != 'District of Columbia'
  )

  ### Convert employee count ranges to point estimates
  censusData <- estimateEMP(censusData)

  devtools::use_data(censusData, internal=TRUE, overwrite=TRUE)
}

### ----- makeDataTests --------------------------------------------------------
### Run tests for manual inspection
### ----------------------------------------------------------------------------
makeDataTests <- function() {
  ### stripRevised
  expect_equal(stripRevised(c('1', '2', '3(r)', '4', '5(r)')), as.character(1:5))

  ### code2estimate
  expect_equal(code2estimate('f'), mean(c(500, 999)))
  expect_equal(code2estimate('10'), 10)
  expect_equal(code2estimate(c(10, 20, 30)), c(10,20,30))
  expect_equal(code2estimate(c('10', 'f', '5', 'a')), c(10, mean(c(500,999)), 5, mean(c(0,19))))

  ### estimateEMP
  empCol <- c('1', '2', 'a', 'b', 'c', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', '10', '20', '30')
  estabCol <- as.numeric(1:length(empCol))
  testData <- as_tibble(data.frame(
    ESTAB=estabCol,
    EMP=empCol,
    stringsAsFactors=FALSE
  ))

  expected <- as_tibble(data.frame(
    ESTAB=estabCol,
    EMP=code2estimate(empCol)
  ))

  expect_equal(estimateEMP(testData), expected)
}
