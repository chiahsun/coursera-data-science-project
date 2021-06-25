trainValidationTestSplit <- function(popultation) {
  # https://stackoverflow.com/questions/34028371/randomly-sample-data-frame-into-3-groups-in-r
  ss <- sample(rep(1:3, diff(floor(10 * c(0, 0.8, 0.9, 1)))))
  return (list(train = popultation[ss == 1], 
               validation = popultation[ss == 2], 
               test = popultation[ss == 3]))
}