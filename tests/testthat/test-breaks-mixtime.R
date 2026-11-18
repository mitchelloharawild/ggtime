test_that("breaks_mixtime() follows calendar nesting for month durations", {
  x <- mixtime::duration(1:36, chronon = mixtime::cal_gregorian$month(1L))
  b <- vctrs::vec_data(vecvec::unvecvec(breaks_mixtime()(x)))
  expect_true(all(diff(b) %in% c(6, 12)))
  expect_true(12 %in% b)
  expect_true(24 %in% b)
})

test_that("breaks_mixtime() follows calendar nesting for quarter durations", {
  x <- mixtime::duration(0:20, chronon = mixtime::cal_gregorian$quarter(1L))
  b <- vctrs::vec_data(vecvec::unvecvec(breaks_mixtime()(x)))
  expect_true(all(diff(b) %in% c(2, 4)))
  expect_true(4 %in% b)
})

test_that("breaks_mixtime() uses round numbers for year durations", {
  x <- mixtime::duration(0:30, chronon = mixtime::cal_gregorian$year(1L))
  b <- vctrs::vec_data(vecvec::unvecvec(breaks_mixtime()(x)))
  expect_equal(b, seq(0, 30, by = 5))
})
