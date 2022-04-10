tscv <- function(tsbl,
                 models = list(),
                 cv_start_date = lubridate::dmy("15-12-2021"),
                 forecast_size = 192,
                 error_indices = 97:192,
                 total_folds = 50, # this is basically how many subsamples of cross-val I want -
                 # since moving ahead 1 step at a time would yield around ~~ 1700 of them, and that
                 # would take a while... so I sub-sample that randomly - which should give us roughly the
                 # same result, but faster :)
                 fold_starts = NULL, # if you want to use pre-generated folds
                 return_folds = FALSE, # if generating folds for reproducibility
                 ...) {
  time_index <- tsbl[[tsibble::index_var(tsbl)]]

  index_first_test <- which(time_index == cv_start_date) + 1
  # if necessary, create folds
  if (is.null(fold_starts)) {
    all_folds <- sum(time_index > cv_start_date) # all possible folds - if we moved 1 step ahead to 'retrain'
    # the model, this is what we would get.
    all_folds <- seq_len(all_folds - forecast_size - 1)
    # subsample all folds - the sorting is simply because I would have to have them ordered randomly
    # in case there is a trend in e.g. model error as we move either way.
    all_folds <- sort(sample(all_folds, total_folds))
    # this gives us the (starting) indices of all folds
    fold_start_indices <- index_first_test + all_folds
  } else {
    fold_start_indices <- fold_starts
  }
  if (return_folds) {
    return(fold_start_indices)
  }
  # create all individual folds - this includes training, testing, and validation
  # though name them train, fcst, accuracy (should become apparent why once you see what
  # functions I pass them to )
  folds <- purrr::imap(fold_start_indices, function(fold_start, fold_index) {
    train_indices <- seq_len(fold_start - 1)
    test_indices <- fold_start:(fold_start + forecast_size - 1)

    list(
      train = dplyr::bind_cols(
        fold = fold_index,
        tsbl[train_indices, ]
      ),
      fcst = dplyr::bind_cols(
        fold = fold_index,
        tsbl[test_indices, ]
      ),
      accuracy = dplyr::bind_cols(
        fold = fold_index,
        tsbl[test_indices, ][error_indices, ]
      )
    )
  })
  # sadly, some book-keeping is in order, but not too much
  train <- dplyr::bind_rows(purrr::map(folds, ~ .[["train"]]))
  train <- tsibble::tsibble(train,
    key = "fold",
    index = tsibble::index_var(tsbl)
  )
  fcst <- dplyr::bind_rows(purrr::map(folds, ~ .[["fcst"]]))
  fcst <- tsibble::tsibble(fcst,
    key = "fold",
    index = tsibble::index_var(tsbl)
  )

  acc <- dplyr::bind_rows(purrr::map(folds, ~ .[["accuracy"]]))
  acc <- tsibble::tsibble(acc,
    key = "fold",
    index = tsibble::index_var(tsbl)
  )

  # train models
  mdls <- train %>%
    fabletools::model(
      !!!models,
      .safely = TRUE
    )
  # I know this looks a bit silly - why are we passing the data in new_data?
  # this does not actually use our target, it simply passes the
  # a) size of the data (i.e. length of our forecast), and
  # b) any exogenous regressors which we must have available - in our case only
  # the weekend variable
  fcst <- fabletools::forecast(mdls, new_data = fcst, times = 0)

  return(fabletools::accuracy(fcst, acc))
}
