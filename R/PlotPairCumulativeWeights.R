PlotPairCumulativeWeights <- function (pairs, numOriginalRowNumbersToPlot = 20) {
  rowNumSample <- sample(unique(pairs$OriginalRowNumber))[1:numOriginalRowNumbersToPlot]
  pairsWithCumWeightSums <- pairs %>% 
    group_by(OriginalRowNumber) %>%
    arrange(OriginalRowNumber, -Weight) %>%
    mutate(CumulativeWeight = cumsum(Weight), Rank = dense_rank(-Weight))
  pairsSubset <- subset(pairsWithCumWeightSums, OriginalRowNumber %in% rowNumSample)
  ggplot() + 
    geom_line(aes(x = Rank, y = CumulativeWeight, color = factor(OriginalRowNumber)), data = pairsSubset, alpha = 0.2) + 
    geom_line(aes(x = Rank, y = CumulativeWeight), stat = "summary", fun.y = "median", data = pairsWithCumWeightSums)
}