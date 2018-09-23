### ----- makeWeightsInput -----------------------------------------------------
### Create a data.frame with labels and weights columns
### labels = character[n].
### weights = numeric[n].
### ----------------------------------------------------------------------------
makeWeightsInput <- function(labels, weights) {
  stopifnot(length(labels) == length(weights))

  data.frame(
    stringsAsFactors=FALSE,
    labels=labels,
    weights=weights
  )
}

### ----- getData --------------------------------------------------------------
### Get the full state-level data set
### Returns a tibble
### ----------------------------------------------------------------------------
getData <- function () {
  raw <- read_excel(
    paste(dataPath, 'raw.xlsx', sep=''),
    col_types = 'text',
    skip=1
  )

  data <- raw[-1, ]
  colTypes <- strsplit(x='text	text	text	text	text	numeric	numeric	numeric	numeric	numeric	numeric	numeric	numeric	numeric	numeric	numeric	numeric	numeric', split='\t')[[1]]
  for (i in 1:length(colTypes)) {
    type <- colTypes[i]
    if (type == 'numeric') {
      values <- as.numeric(pull(data[ ,i]))
      values[is.na(values)] <- 0 # Note, this replaces any NAs with zero for now
      data[ ,i] <- values
    }
  }
  return(data)
}

### ----- getTotal -------------------------------------------------------------
### Calculate the total of a column, aggregated by NAICS.id
### naics = vector of NAICS.id values
### columnLabel = expr[1]. Column heading - ESTAB, EMP, etc.
### Returns vector of numeric values representing sum of column for NAICS.id's
### ----------------------------------------------------------------------------
getTotal <- function (data, naics, columnLabel) {
  totalQuo <- enquo(columnLabel)

  industryTotals <- group_by(data, NAICS.id) %>% summarize(
    total=sum(!! totalQuo)
  )

  industryTotals$total[match(naics, industryTotals$NAICS.id)]
}

### ----- getWeights -----------------------------------------------------------
### labels = vector of NAICS.id values, column names, etc. [n]
### wts = matrix[n, 2] with labels in col 1 and weights in col 2 (0.5, etc)
### Returns vector of numeric values repsenting weights for corres. labels
### ----------------------------------------------------------------------------
getWeights <- function (labels, wts) {
  wts[match(labels, wts[ ,1]), 2]
}

### ----- addPctColumn ---------------------------------------------------------
### Add a "pctLABEL" column that shows the % of total for that label (EMP) in that NAICS code
### data = tibble
### columnLabels = character[n].
### Returns tibble with "pctCOLUMNLABEL" column attached
### ----------------------------------------------------------------------------
addPctColumn <- function(data, columnLabels) {
  if (length(columnLabels) < 1) return(data)

  columnLabel <- columnLabels[1]
  currentData <- addPctColumn(data, columnLabels[-1])

  pctLabel <- paste('pct', columnLabel, sep='')
  totals <- getTotal(currentData, currentData$NAICS.id, (!! sym(columnLabel)))
  currentData %>% mutate(
    (!! pctLabel) :=  (!! sym(columnLabel)) / totals
  )
}

### ----- addScoreColumn ---------------------------------------------------------
### Add a "score" column that calcs score based on both industry and label weights
### data = tibble
### industryWeights = data.frame with 'labels' and 'weights' columns
### metricWeights = data.frame with 'labels' and 'weights' columns
### Returns tibble with "score" column attached
### ----------------------------------------------------------------------------
addScoreColumn <- function (data, industryWeights, metricWeights) {
  cols <- paste('pct', metricWeights[ ,1], sep='')
  rawScores <- data[ ,cols] * matrix(metricWeights$weights, nrow=nrow(data), ncol=nrow(metricWeights), byrow=TRUE)
  industryAppliedWts <- getWeights(data$NAICS.id, industryWeights)
  combScore <- rowSums(rawScores) * industryAppliedWts
  return(
    mutate(
      data,
      score=combScore
    )
  )
}

### ----- aggregateStateScores -------------------------------------------------
### Convert a tibble with "stateName" and "totalScore" column into a simple data.frame for plotting
### data = tibble
### Returns tibble with state, totalScore, long, lat, and printScore cols
### ----------------------------------------------------------------------------
aggregateStateScores <- function(data) {
  if (!is.tbl(data)) stop('aggregateStateScores requires a tibble')
  if (!('totalScore' %in% names(data))) stop('No totalScore column header')
  if (!('stateName' %in% names(data))) stop('No stateName column header')

  stateIdx <- match(data$stateName, datasets::state.name)
  if (any(is.na(stateIdx))) stop(paste('Invalid stateName "', data$stateName[is.na(stateIdx)][1], '"', sep=''))
  long <- datasets::state.center$x[stateIdx]
  lat <- datasets::state.center$y[stateIdx]
  printScore <- round(data$totalScore * 100, 0)

  df <- data.frame(
    state=data$stateName,
    totalScore=data$totalScore,
    long=long,
    lat=lat,
    printScore=printScore
  )

  return(as_tibble(df))
}

### ----- data2stateScores ------------------------------------------------------
###
### rawData = tibble representing Census Bureau data set
### metricWeights = data.frame with "labels" (char.) and "weights" (num.)
### industryWeights = data.frame with "labels" (char.) and "weights" (num.)
###
### Returns a tibble with state, totalScore, long, lat, and printScore cols
### ----------------------------------------------------------------------------
data2stateScores <- function(rawData, metricWeights, industryWeights) {
  dataWithMetricColumns <- addPctColumn(rawData, as.character(metricWeights$labels))
  dataWithScores <- addScoreColumn(dataWithMetricColumns, industryWeights, metricWeights)

  stateScoreInputs <- dataWithScores %>% select(
    `GEO.display-label`, score
  ) %>% group_by(`GEO.display-label`) %>% summarize(
    totalScore= sum(score)
  )

  names(stateScoreInputs) <- c('stateName', 'totalScore')
  stateScores <- aggregateStateScores(stateScoreInputs)
}

### ----- plotDemandMap ---------------------------------------------------------
### Create a ggplot object representing the demand map
### mapData = data.frame w/ long, lat, and region columns; region = state name
###           This provides the lat/lon for the state boundaries
### stateScoresData = tibble w/ state, totalScore, printScore, lat, and long
###                   This provides the lat/lon for the state midpoints
### Returns ggplot
### ----------------------------------------------------------------------------
plotDemandMap <- function(mapData, stateScoresData) {
  gg <- ggplot()
  gg <- gg + geom_map(data=mapData, map=mapData,
                      aes(map_id=tolower(region)),
                      fill="#ffffff", color="black", size=0.15)
  gg <- gg + geom_map(data=stateScoresData, map=mapData,
                      aes(fill=totalScore, map_id=tolower(state)),
                      color="#ffffff", size=0.15) + coord_quickmap()
  gg + geom_text(data=stateScoresData, aes(x=long, y=lat, label=printScore), color='white', size=3)
}
