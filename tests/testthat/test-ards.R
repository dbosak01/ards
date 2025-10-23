base_path <- "c:/packages/ards/tests/testthat/"

base_path <- "."



dev <- FALSE



# df <- read.table(header = TRUE, text = '
#     var     label         trtA          trtB
#     "ampg"   "N"          "19"          "13"
#     "ampg"   "Mean (Std)" "18.8 (6.5)"  "22.0 (4.9)"
#     "ampg"   "Median"     "16.4"        "21.4"
#     "ampg"   "Q1 - Q3"    "15.1 - 21.2" "19.2 - 22.8"
#     "ampg"   "Range"      "10.4 - 33.9" "14.7 - 32.4"
#     "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)"
#     "cyl"    "6 Cylinder" "4 ( 21.1%)"  "3 ( 23.1%)"
#     "cyl"    "4 Cylinder" "5 ( 26.3%)"  "6 ( 46.2%)"')


test_that("ards1: ards_init() works as expected.", {


  df <- init_ards(studyid = "abc",
                  tableid = "01", adsns = c("adsl", "advs"),
                  population = "safety population",
                  time = "SCREENING", where = "saffl = TRUE")

  df


  res <- get_ards()

  expect_equal(is.null(res), FALSE)
  expect_equal(nrow(res), 0)
  expect_equal(ncol(res), 33)


})


test_that("ards2: add_ards() works as expected on one column.", {


  init_ards(studyid = "abc",
            tableid = "01", adsns = c("adsl", "advs"),
            population = "safety population",
            time = "SCREENING", where = "saffl = TRUE")


  df1 <- data.frame("ACNT" = c(1, 2, 3))


  ards1 <- add_ards(df1, statvars = "ACNT", byvars = "label",
                    anal_var = "cyl")

  df2 <- data.frame("ACNT" = c(4, 5, 6))

  ards2 <- add_ards(df2, statvars = "ACNT", byvars = "label",
                    anal_var = "cyl")

  res <- get_ards()


  res

  expect_equal(is.null(res), FALSE)
  expect_equal(nrow(res), 6)
  expect_equal(ncol(res), 33)
  expect_equal(res[[1, 1]], 'abc')
  expect_equal(res[ , "statval"], c(1, 2, 3, 4, 5, 6))


})


test_that("ards3: add_ards() works as expected on multiple columns.", {


  df <- read.table(header = TRUE, text = '
    var     val  label         ACNT ADENOM APCT                BCNT BDENOM BPCT
    "cyl"   8    "8 Cylinder" 10   19     0.5263157894736842  4    13     0.3076923076923077
    "cyl"   6    "6 Cylinder" 4    19     0.2105263157894737  3    13     0.2307692307692308
    "cyl"   6    "4 Cylinder" 5    19     0.2631578947368421  6    13     0.4615384615384615')


  init_ards(studyid = "abc",
            tableid = "01", adsns = c("adsl", "advs"),
            population = "safety population",
            time = "SCREENING", where = "saffl = TRUE")



  add_ards(df, statvars = c("ACNT", "ADENOM", "APCT", "BCNT", "BDENOM", "BPCT"), statdesc = "label",
           anal_var = "cyl", anal_val = "val")


  res <- get_ards()

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(nrow(res), 18)
  expect_equal(ncol(res), 33)

})



test_that("ards4: add_ards() works as expected on multiple columns with treatment variable.", {


  df <- read.table(header = TRUE, text = '
    var    val label         CNT DENOM   PCT                TRT
    "cyl"  8   "8 Cylinder" 10   19     0.5263157894736842 A
    "cyl"  6   "6 Cylinder" 4    19     0.2105263157894737 A
    "cyl"  4   "4 Cylinder" 5    19     0.2631578947368421 A
    "cyl"  8   "8 Cylinder" 4    13     0.3076923076923077 B
    "cyl"  6   "6 Cylinder" 3    13     0.2307692307692308 B
    "cyl"  4   "4 Cylinder" 6    13     0.4615384615384615 B')


  init_ards(studyid = "abc",
            tableid = "01", adsns = c("adsl", "advs"),
            population = "safety population",
            time = "SCREENING", where = "saffl = TRUE")


  add_ards(df, statvars = c('CNT', 'DENOM', 'PCT'), statdesc = "label",
           anal_var = "cyl", anal_val = "val", trtvar = "TRT")

  res <- get_ards()

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(nrow(res), 18)
  expect_equal(ncol(res), 33)
  #expect_equal(

})



test_that("ards5: add_ards() works as expected in dplyr pipeline.", {

  library(dplyr)
  library(tibble)
  library(tidyr)

  init_ards(studyid = "abc",
            tableid = "01", adsns = c("adsl", "advs"),
            population = "safety population",
            time = "SCREENING", where = "saffl = TRUE")

  dat <- mtcars
  dat$trt <- c(rep("A", 16), rep("B", 16))

  trt_pop <- count(dat, trt) |> deframe()

  cyldf <- dat |>
    mutate(denom = trt_pop[paste0(dat$trt)]) |>
    group_by(cyl, trt, denom) |>
    summarize(cnt = n()) |>
    mutate(analvar = "cyl", label = paste(cyl, "Cylinder"),  pct = denom / cnt) |>
    ungroup() |>
    add_ards(statvars = c('cnt', 'denom', 'pct'), statdesc = "label",
             anal_var = "cyl", trtvar = "trt") |>
    pivot_wider(names_from = trt,
                values_from = c(cnt, pct)) |>
    transmute(analvar, label,
              trtA = sprintf("%1d (%5.2f%%)", cnt_A, pct_A),
              trtB = sprintf("%1d (%5.2f%%)", cnt_B, pct_B),)

  cyldf

  expect_equal(is.null(cyldf), FALSE)
  expect_equal(nrow(cyldf), 3)
  expect_equal(ncol(cyldf), 4)

  res <- get_ards()
  res

  expect_equal(is.null(res), FALSE)
  expect_equal(nrow(res), 18)
  expect_equal(ncol(res), 33)


})



test_that("ards6: add_ards() works as expected for continuous variable in pipeline.", {

  library(dplyr)
  library(tibble)
  library(tidyr)

  init_ards(studyid = "abc",
            tableid = "01", adsns = c("adsl", "advs"),
            population = "safety population",
            time = "SCREENING", where = "saffl = TRUE")

  dat <- mtcars
  dat$trt <- c(rep("A", 16), rep("B", 16))
  dat <- dat[ , c("mpg", "trt")]

  mpgdf <- dat |>
    group_by(trt) |>
    summarize(n = n(),
              mean = mean(mpg),
              std = sd(mpg),
              median = median(mpg),
              min = min(mpg),
              max = max(mpg)) |>
    mutate(analvar = "mpg") |>
    ungroup() |>
    add_ards(statvars = c("n", "mean", "std", "median", "min", "max"),
             statdesc = c("N", "Mean", "Std", "Median", "Min", "Max"),
             anal_var = "mpg", trtvar = "trt") |>
    pivot_longer(c(n, mean, std, median, min, max),
                 names_to = "label", values_to = "stats") |>
    pivot_wider(names_from = trt,
                values_from = c(stats)) |>
    transmute(analvar, label,
              trtA = sprintf("%.2f", A),
              trtB = sprintf("%.2f", B))

  mpgdf

  expect_equal(is.null(mpgdf), FALSE)
  expect_equal(nrow(mpgdf), 6)
  expect_equal(ncol(mpgdf), 4)

  res <- get_ards()
  res

  expect_equal(is.null(res), FALSE)
  expect_equal(nrow(res), 12)
  expect_equal(ncol(res), 33)


})



test_that("ards7: add_ards() works as expected for multiple analysis variables.", {

  library(dplyr)
  library(tibble)
  library(tidyr)

  init_ards(studyid = "abc",
            tableid = "01", adsns = c("adsl", "advs"),
            population = "safety population",
            time = "SCREENING", where = "saffl = TRUE")

  dat <- mtcars
  dat$trt <- c(rep("A", 16), rep("B", 16))


  mpgdf <- dat |>
    select(mpg, trt) |>
    group_by(trt) |>
    summarize(n = n(),
              mean = mean(mpg),
              std = sd(mpg),
              median = median(mpg),
              min = min(mpg),
              max = max(mpg)) |>
    mutate(analvar = "mpg") |>
    ungroup() |>
    add_ards(statvars = c("n", "mean", "std", "median", "min", "max"),
             statdesc = c("N", "Mean", "Std", "Median", "Min", "Max"),
             anal_var = "mpg", trtvar = "trt") |>
    transmute(analvar, trt,
              n = sprintf("%d", n),
              mean_sd = sprintf("%.1f (%.2f)", mean, std),
              median = sprintf("%.1f", median),
              min_max = sprintf("%.1f-%.1f", min, max)) |>
    pivot_longer(c(n, mean_sd, median, min_max),
                 names_to = "label", values_to = "stats") |>
    pivot_wider(names_from = trt,
                values_from = c(stats)) |>
    transmute(analvar, label = c("N", "Mean (Std)", "Median", "Min-Max"),
             trtA = A, trtB = B)

  mpgdf

  expect_equal(is.null(mpgdf), FALSE)
  expect_equal(nrow(mpgdf), 4)
  expect_equal(ncol(mpgdf), 4)


  trt_pop <- count(dat, trt) |> deframe()

  cyldf <- dat |>
    mutate(denom = trt_pop[paste0(dat$trt)]) |>
    group_by(cyl, trt, denom) |>
    summarize(cnt = n()) |>
    mutate(analvar = "cyl", label = paste(cyl, "Cylinder"),  pct = denom / cnt) |>
    ungroup() |>
    add_ards(statvars = c("cnt", "denom", "pct"), statdesc = "label",
             anal_var = "cyl", trtvar = "trt") |>
    pivot_wider(names_from = trt,
                values_from = c(cnt, pct)) |>
    transmute(analvar, label,
              trtA = sprintf("%1d (%5.2f%%)", cnt_A, pct_A),
              trtB = sprintf("%1d (%5.2f%%)", cnt_B, pct_B),)

  cyldf

  expect_equal(is.null(cyldf), FALSE)
  expect_equal(nrow(cyldf), 3)
  expect_equal(ncol(cyldf), 4)

  final <- bind_rows(mpgdf, cyldf)

  final

  expect_equal(is.null(final), FALSE)
  expect_equal(nrow(final), 7)
  expect_equal(ncol(final), 4)


  res <- get_ards()
  res

  expect_equal(is.null(res), FALSE)
  expect_equal(nrow(res), 30)
  expect_equal(ncol(res), 33)


})




test_that("ards8: init_ards() works as expected when no parameters are passed.", {
  
  
  df <- read.table(header = TRUE, text = '
    var    val label         CNT DENOM   PCT                TRT
    "cyl"  8   "8 Cylinder" 10   19     0.5263157894736842 A
    "cyl"  6   "6 Cylinder" 4    19     0.2105263157894737 A
    "cyl"  4   "4 Cylinder" 5    19     0.2631578947368421 A
    "cyl"  8   "8 Cylinder" 4    13     0.3076923076923077 B
    "cyl"  6   "6 Cylinder" 3    13     0.2307692307692308 B
    "cyl"  4   "4 Cylinder" 6    13     0.4615384615384615 B')
  
  
  init_ards()
  
  
  add_ards(df, statvars = c('CNT', 'DENOM', 'PCT'), statdesc = "label",
           anal_var = "cyl", anal_val = "val", trtvar = "TRT")
  
  res <- get_ards()
  
  res
  
  expect_equal(is.null(res), FALSE)
  expect_equal(nrow(res), 18)
  expect_equal(ncol(res), 33)
  
})

test_that("ards9: add_ards() works as expected on one column with multiple byvars.", {
  
  
  init_ards(studyid = "abc",
            tableid = "01", adsns = c("adsl", "advs"),
            population = "safety population",
            time = "SCREENING", where = "saffl = TRUE")
  
  
  df1 <- data.frame("ACNT" = c(1, 2, 3))
  
  
  ards1 <- add_ards(df1, statvars = "ACNT", byvars = c("label", "group"),
                    anal_var = "cyl")
  
  df2 <- data.frame("ACNT" = c(4, 5, 6))
  
  ards2 <- add_ards(df2, statvars = "ACNT", byvars = c("label", "group"),
                    anal_var = "cyl")
  
  res <- get_ards()
  
  
  res
  
  expect_equal(is.null(res), FALSE)
  expect_equal(nrow(res), 6)
  expect_equal(ncol(res), 33)
  expect_equal(res[[1, 1]], 'abc')
  expect_equal(res[ , "statval"], c(1, 2, 3, 4, 5, 6))
  expect_equal(res$byvar1[1], "label")
  expect_equal(res$byvar2[1], "group")
  
  
})


test_that("ards10: restore_ards() basic functionality.", {
  
  library(dplyr)
  library(tibble)
  library(tidyr)
  
  init_ards(studyid = "abc",
            tableid = "01", adsns = c("adsl", "advs"),
            population = "safety population",
            time = "SCREENING", where = "saffl = TRUE")
  
  dat <- mtcars
  dat$trt <- c(rep("A", 16), rep("B", 16))
  
  
  mpgdf <- dat |>
    select(mpg, trt) |>
    group_by(trt) |>
    summarize(n = n(),
              mean = mean(mpg),
              std = sd(mpg),
              median = median(mpg),
              min = min(mpg),
              max = max(mpg)) |>
    mutate(analvar = "mpg") |>
    ungroup() |>
    add_ards(statvars = c("n", "mean", "std", "median", "min", "max"),
             statdesc = c("N", "Mean", "Std", "Median", "Min", "Max"),
             anal_var = "mpg", trtvar = "trt") |>
    transmute(analvar, trt,
              n = sprintf("%d", n),
              mean_sd = sprintf("%.1f (%.2f)", mean, std),
              median = sprintf("%.1f", median),
              min_max = sprintf("%.1f-%.1f", min, max)) |>
    pivot_longer(c(n, mean_sd, median, min_max),
                 names_to = "label", values_to = "stats") |>
    pivot_wider(names_from = trt,
                values_from = c(stats)) |>
    transmute(analvar, label = c("N", "Mean (Std)", "Median", "Min-Max"),
              trtA = A, trtB = B)
  
  mpgdf
  
  expect_equal(is.null(mpgdf), FALSE)
  expect_equal(nrow(mpgdf), 4)
  expect_equal(ncol(mpgdf), 4)
  
  
  trt_pop <- count(dat, trt) |> deframe()
  
  cyldf <- dat |>
    mutate(denom = trt_pop[paste0(dat$trt)]) |>
    group_by(cyl, trt, denom) |>
    summarize(cnt = n()) |>
    mutate(analvar = "cyl", label = paste(cyl, "Cylinder"),  pct = denom / cnt) |>
    ungroup() |>
    add_ards(statvars = c("cnt", "denom", "pct"), statdesc = "label",
             anal_var = "cyl", trtvar = "trt") |>
    pivot_wider(names_from = trt,
                values_from = c(cnt, pct)) |>
    transmute(analvar, label,
              trtA = sprintf("%1d (%5.2f%%)", cnt_A, pct_A),
              trtB = sprintf("%1d (%5.2f%%)", cnt_B, pct_B),)
  
  cyldf
  
  expect_equal(is.null(cyldf), FALSE)
  expect_equal(nrow(cyldf), 3)
  expect_equal(ncol(cyldf), 4)
  
  final <- bind_rows(mpgdf, cyldf)
  
  final
  
  expect_equal(is.null(final), FALSE)
  expect_equal(nrow(final), 7)
  expect_equal(ncol(final), 4)
  
  
  tmp <- get_ards()
  tmp
  
  expect_equal(is.null(tmp), FALSE)
  expect_equal(nrow(tmp), 30)
  expect_equal(ncol(tmp), 33)
  
  # Init_vars TRUE
  res <- restore_ards(tmp, init_vars = TRUE)
  
  expect_equal(length(res), 2)
  expect_equal(names(res), c("mpg", "cyl"))
  expect_equal(ncol(res$mpg), 14) 
  expect_equal(nrow(res$mpg), 2)
  expect_equal(ncol(res$cyl), 12)
  expect_equal(nrow(res$cyl), 6)
  
  # Init_vars FALSE
  res <- restore_ards(tmp)
  
  expect_equal(length(res), 2)
  expect_equal(names(res), c("mpg", "cyl"))
  expect_equal(ncol(res$mpg), 8) 
  expect_equal(nrow(res$mpg), 2)
  expect_equal(ncol(res$cyl), 6)
  expect_equal(nrow(res$cyl), 6)
  
  # Test removing some variables
  tmp$byvar9 <- NULL
  tmp$byval9 <- NULL
  tmp$statdesc <- NULL
  tmp$resultid <- NULL
  tmp$studyid <- NULL
  tmp$tableid <- NULL
  tmp$population <- NA
  
  # Should still work
  res <- restore_ards(tmp)
  
  # Missing variables not output
  expect_equal(length(res), 2)
  expect_equal(names(res), c("mpg", "cyl"))
  expect_equal(ncol(res$mpg), 8) 
  expect_equal(nrow(res$mpg), 2)
  expect_equal(ncol(res$cyl), 6)
  expect_equal(nrow(res$cyl), 6)
  
  # Test removing required variable
  tmp$anal_var <- NULL
  expect_error(restore_ards(tmp))
  
})

test_that("ards11: restore_ards() works with by variable.", {
  
  
  df <- read.table(header = TRUE, text = '
    var    val label         CNT DENOM   PCT                TRT
    "cyl"  8   "8 Cylinder" 10   19     0.5263157894736842 A
    "cyl"  6   "6 Cylinder" 4    19     0.2105263157894737 A
    "cyl"  4   "4 Cylinder" 5    19     0.2631578947368421 A
    "cyl"  8   "8 Cylinder" 4    13     0.3076923076923077 B
    "cyl"  6   "6 Cylinder" 3    13     0.2307692307692308 B
    "cyl"  4   "4 Cylinder" 6    13     0.4615384615384615 B')
  
  
  init_ards(studyid = "abc",
            tableid = "01", adsns = c("adsl", "advs"),
            population = "safety population",
            time = "SCREENING", where = "saffl = TRUE")
  
  # No trtvar but there is a byvar
  add_ards(df, statvars = c('CNT', 'DENOM', 'PCT'), statdesc = "label",
           anal_var = "cyl", anal_val = "val", byvars = "TRT")
  
  tmp <- get_ards()
  
  tmp
  
  expect_equal(is.null(tmp), FALSE)
  expect_equal(nrow(tmp), 18)
  expect_equal(ncol(tmp), 33)
  
  # Restore
  res <- restore_ards(tmp)
  
  # Still 6 columns
  expect_equal(is.null(res), FALSE)
  expect_equal(nrow(res$cyl), 6)
  expect_equal(ncol(res$cyl), 6)
  
})


test_that("ards12: restore_ards() anal_var parameter works as expected.", {
  
  
  init_ards(studyid = "abc",
            tableid = "01", adsns = c("adsl", "advs"),
            population = "safety population",
            time = "SCREENING", where = "saffl = TRUE")
  
  
  df1 <- data.frame("ACNT" = c(1, 2, 3))
  
  
  ards1 <- add_ards(df1, statvars = "ACNT", byvars = "label",
                    anal_var = "cyl")
  
  df2 <- data.frame("ACNT" = c(4, 5, 6))
  
  ards2 <- add_ards(df2, statvars = "ACNT", byvars = "label",
                    anal_var = "cyl")
  
  ardsf <- get_ards()
  
  # Default restore
  res <- restore_ards(ardsf)
  
  r1 <- res$cyl
  
  expect_equal(is.null(r1), FALSE)
  expect_equal(nrow(r1), 6)
  expect_equal(ncol(r1), 3)
  expect_equal("anal_var" %in% names(r1), TRUE)

  # Rename anal_var
  res <- restore_ards(ardsf, anal_var = "analblock")
  
  r1 <- res$cyl
  
  expect_equal(is.null(r1), FALSE)
  expect_equal(nrow(r1), 6)
  expect_equal(ncol(r1), 3)
  expect_equal("analblock" %in% names(r1), TRUE)
  
  
  # Remove anal_var
  res <- restore_ards(ardsf, anal_var = NULL)
  
  r1 <- res$cyl
  
  expect_equal(is.null(r1), FALSE)
  expect_equal(nrow(r1), 6)
  expect_equal(ncol(r1), 2)
  expect_equal("anal_var" %in% names(r1), FALSE)
  
})
