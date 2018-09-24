context('functions.r file')

test_that('getTotal()', {
  exampleData <- as_tibble(data.frame(
    NAICS.id=as.character(c(11, 12, 11)),
    EMP=c(1, 2, 3),
    ESTAB=c(10, 20, 30)
  ))
  naics <- c(11, 11, 12)

  expectedEmp <- c(4, 4, 2)
  expectedEstab <- c(40, 40, 20)

  expect_equal(getTotal(exampleData, naics, EMP), expectedEmp)
  expect_equal(getTotal(exampleData, naics, ESTAB), expectedEstab)
})

test_that('getWeights()', {
  wts <- matrix(
    c(11, 12, 0.9, 0.1),
    2, 2
  )
  naics <- c(12, 11, 11, 12)
  expect_equal(getWeights(naics, wts), c(0.1, 0.9, 0.9, 0.1))
})

test_that('addPctColumns()', {
  emp <- 1:6
  estab <- 6:1
  testData <- as_tibble(data.frame(
    NAICS.id=c(11, 12, 11, 12, 11, 12),
    EMP=emp,
    ESTAB=estab
  ))

  expectedEMP <- c(1 / 9, 2 / 12, 3 / 9, 4 / 12, 5 / 9, 6 / 12)
  expectedESTAB <- c(6 / 12, 5 / 9, 4 / 12, 3 / 9, 2 / 12, 1 / 9)

  resultEMP <- addPctColumn(testData, 'EMP')
  expect_equal(resultEMP$pctEMP, expectedEMP)

  resultESTAB <- addPctColumn(testData, 'ESTAB')
  expect_equal(resultESTAB$pctESTAB, expectedESTAB)

  resultCombined <- addPctColumn(testData, c('EMP', 'ESTAB'))
  expect_equal(resultCombined$pctEMP, expectedEMP)
  expect_equal(resultCombined$pctESTAB, expectedESTAB)

})

test_that('filterIndustries()', {
  testData <- as_tibble(data.frame(
    NAICS.id=1:6 * 10,
    EMP=6:1,
    ESTAB=6:1
  ))

  industryLabels <- c(20, 40, 50)

  result <- filterIndustries(testData, industryLabels)
  expect_equal(nrow(result), length(industryLabels))
  expect_equal(result$NAICS.id, industryLabels)
})

test_that('addScoreColumn()', {
  emp <- 1:6
  estab <- 6:1
  expectedEMP <- c(1 / 9, 2 / 12, 3 / 9, 4 / 12, 5 / 9, 6 / 12)
  expectedESTAB <- c(6 / 12, 5 / 9, 4 / 12, 3 / 9, 2 / 12, 1 / 9)
  testData <- as_tibble(data.frame(
    NAICS.id=c(11, 12, 11, 12, 11, 12),
    EMP=emp,
    ESTAB=estab,
    pctEMP=expectedEMP,
    pctESTAB=expectedESTAB
  ))

  industryWts <- data.frame(
    labels=c(11, 12),
    weights=c(0.2, 0.8)
  )

  metricWts <- data.frame(
    labels=c('EMP', 'ESTAB'),
    weights=c(0.7, 0.3)
  )

  result <- addScoreColumn(testData, industryWts, metricWts)

  industryAppliedWeights <- ifelse(testData$NAICS.id == 11, industryWts$weights[1], industryWts$weights[2])
  expectedScores <- (expectedEMP * 0.7 + expectedESTAB * 0.3) * industryAppliedWeights

  expect_equal(result$score, expectedScores)
  expect_equal(sum(result$score), 1)
})

test_that('aggregateStateScores()', {
  # Requires a tibble
  expect_error(aggregateStateScores(TRUE), 'aggregateStateScores requires a tibble')

  # Requires totalScore column
  x <- as_tibble(data.frame(x=1:10, stateName=1:10))
  expect_error(aggregateStateScores(x), 'No totalScore column header')

  # Requires stateName column
  x <- as_tibble(data.frame(totalScore=1:10, x=1:10))
  expect_error(aggregateStateScores(x), 'No stateName column header')

  # Returns tibble with state, totalScore, long, lat, and printScore cols
  x <- as_tibble(data.frame(stateName=c('Alabama', 'Kansas'), totalScore=c(1, 2)))
  result <- aggregateStateScores(x)
  expect_true(is.tbl(result))
  expect_equal(names(result), c('state', 'totalScore', 'long', 'lat', 'printScore'))

  # Invalid stateName throws error
  x <- as_tibble(data.frame(stateName=c('Alabamaaa', 'Kansas'), totalScore=c(1, 2)))
  expect_error(aggregateStateScores(x), 'Invalid stateName "Alabamaaa"')

  # Lat/lon coordinate and printScore spot check
  x <- as_tibble(data.frame(stateName=c('Alabama', 'Kansas'), totalScore=c(1, 2)))
  result <- aggregateStateScores(x)
  expect_equal(result$long, c(-86.7509, -98.1156))
  expect_equal(result$lat, c(32.5901, 38.4204))
  expect_equal(result$printScore, c(100, 200))
})
