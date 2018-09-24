#' Create paired weight and label input objects
#'
#' @param labels character[n].
#' @param weights numeric[n].
#' @return data.frame with labels and weights columns
makeWeightsInput <- function(labels, weights) {
  stopifnot(length(labels) == length(weights))

  data.frame(
    stringsAsFactors=FALSE,
    labels=labels,
    weights=weights
  )
}


#' Get the full state-level data set
#'
#' @return a tibble
getData <- function () {
  packageData <- censusData
  colTypes <- strsplit(x='text	text	text	text	text	numeric	numeric	numeric	numeric	numeric	numeric	numeric	numeric	numeric	numeric	numeric	numeric	numeric', split='\t')[[1]]
  for (i in 1:length(colTypes)) {
    type <- colTypes[i]
    if (type == 'numeric') {
      values <- as.numeric(pull(packageData[ ,i]))
      values[is.na(values)] <- 0 # Note, this replaces any NAs with zero for now
      packageData[ ,i] <- values
    }
  }
  return(packageData)
}

#' Calculate the total of a column, aggregated by NAICS.id
#'
#' @param data tibble
#' @param naics vector of NAICS.id values
#' @param columnLabel expr[1]. Column heading - ESTAB, EMP, etc.
#' @eturn vector of numeric values representing sum of column for NAICS.id's
getTotal <- function (data, naics, columnLabel) {
  totalQuo <- enquo(columnLabel)

  industryTotals <- group_by(data, NAICS.id) %>% summarize(
    total=sum(!! totalQuo)
  )

  industryTotals$total[match(naics, industryTotals$NAICS.id)]
}

#' Retrieve weights for a given set of labels from a source table
#'
#' @param labels vector of NAICS.id values, column names, etc. [n]
#' @param wts matrix[n, 2] with labels in col 1 and weights in col 2 (0.5, etc)
#' @return Vector of numeric values repsenting weights for corres. labels
getWeights <- function (labels, wts) {
  wts[match(labels, wts[ ,1]), 2]
}


#' Add a "pctLABEL" column that shows the percent of total for that label (EMP) in that NAICS code
#'
#' @param data tibble
#' @param columnLabels character[n]
#' @return Tibble with "pctCOLUMNLABEL" column attached
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

#' Add a "score" column that calcs score based on both industry and label weights
#'
#' @param data tibble
#' @param industryWeights data.frame with 'labels' and 'weights' columns
#' @param metricWeights data.frame with 'labels' and 'weights' columns
#' @return Tibble with "score" column attached
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

#' Convert a tibble with "stateName" and "totalScore" column into a simple data.frame for plotting
#'
#' @param data tibble
#' @return Tibble with state, totalScore, long, lat, and printScore cols
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

#' Convert weight inputs and raw data into aggregated scores by state
#'
#' @param rawData tibble representing Census Bureau data set
#' @param metricWeights data.frame with "labels" (char.) and "weights" (num.)
#' @param industryWeights data.frame with "labels" (char.) and "weights" (num.)
#' @return A tibble with state, totalScore, long, lat, and printScore cols
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

#' Create a ggplot object representing the demand map
#'
#' @param mapData data.frame w/ long, lat, and region columns; region = state name. This provides the lat/lon for the state boundaries.
#' @param stateScoresData tibble w/ state, totalScore, printScore, lat, and long. This provides the lat/lon for the state midpoints.
#' @return ggplot
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

#' Census Bureau data set
#'
#' Contains state and NAICS detail from the Census Bureau
"censusData"
