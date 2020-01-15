#' Check that DQ functions are working
test_that("DQ bound lookup is working", {
  scoring_bounds <- setDQScoringBounds()
  
  dqi <- 'TemporalCorrelation'
  raw_score <- 10
  score <- lookupDQBoundScore(raw_score,dqi,scoring_bounds)
  show_failure(expect_equal(score, 3))
  
  raw_score <- 0
  score <- lookupDQBoundScore(raw_score,dqi,scoring_bounds)
  show_failure(expect_equal(score, 1))
  
  raw_score <- 3
  score <- lookupDQBoundScore(raw_score,dqi,scoring_bounds)
  show_failure(expect_equal(score, 1))
  
  raw_score <- 3.1
  score <- lookupDQBoundScore(raw_score,dqi,scoring_bounds)
  show_failure(expect_equal(score, 2))
  
  raw_score <- 1000
  score <- lookupDQBoundScore(raw_score,dqi,scoring_bounds)
  show_failure(expect_equal(score, 5))
  
  raw_score <- NA
  score <- lookupDQBoundScore(raw_score,dqi,scoring_bounds)
  show_failure(expect_equal(score, NA))
  
  dqi <- 'DataCollection'
  raw_score <- 1
  score <- lookupDQBoundScore(raw_score,dqi,scoring_bounds)
  show_failure(expect_equal(score, 1))
  
  raw_score <- 0.7
  score <- lookupDQBoundScore(raw_score,dqi,scoring_bounds)
  show_failure(expect_equal(score, 2))
  
  raw_score <- 0.45
  score <- lookupDQBoundScore(raw_score,dqi,scoring_bounds)
  show_failure(expect_equal(score, 3))
  
  raw_score <- 0
  score <- lookupDQBoundScore(raw_score,dqi,scoring_bounds)
  show_failure(expect_equal(score, 4))
  
  raw_score <- NA
  score <- lookupDQBoundScore(raw_score,dqi,scoring_bounds)
  show_failure(expect_equal(score, NA))
  
})