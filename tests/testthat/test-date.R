test_that("flavour works", {
  expect_equal(flavour(), "LP")
})


test_that("jahr works", {
  expect_equal(jahr("01.01.2000"), 2000)
})

test_that("monat works", {
  expect_equal(monat("01.01.2000"), 1)
  expect_equal(monat("01.12.1999"), 12)
  expect_equal(monat("01.11.2111"), 11)
  expect_equal(monat("01.10.1844"), 10)
  expect_equal(monat("14.06.1789"), 6)
  expect_equal(monat("01.08.1001"), 8)
  expect_equal(monat("01.07.2024"), 7)
  expect_equal(monat("01.06.2033"), 6)
  expect_equal(monat("07.05.2099"), 5)
  expect_equal(monat("01.04.1111"), 4)
  expect_equal(monat("01.03.3000"), 3)
  expect_equal(monat("12.02.2000"), 2)
})

test_that("tag works", {
  expect_equal(tag("01.01.2000"), 1)
})

test_that("dateAddMonths works", {
  expect_equal(dateAddMonths("01.01.2000", -1), "01.12.1999")
  expect_equal(dateAddMonths("01.01.2000",  1), "01.02.2000")
  expect_equal(dateAddMonths("01.01.2000", 13), "01.02.2001")
  expect_equal(dateAddMonths("01.01.2000",-13), "01.12.1998")
  expect_equal(dateAddMonths("01.01.2000",-12), "01.01.1999")
  expect_equal(dateAddMonths("01.01.2000", 12), "01.01.2001")
  expect_equal(dateAddMonths("01.01.2000", -1201), "01.12.1899")
  expect_equal(dateAddMonths("01.01.2000",  1201), "01.02.2100")
})

test_that("dateDiff works", {
  expect_equal(dateDiff("01.01.2000", "01.12.1999")$diffJ, 0)
  expect_equal(dateDiff("01.01.2000", "01.12.1999")$diffM, -1)
  expect_equal(dateDiff("01.01.2000", "01.12.1999")$diffDauer, -1/12)
  expect_equal(dateDiff("01.01.2000", "01.12.1999")$diffDauerM, -1)
  expect_equal(dateDiff("01.12.1999", "01.01.2000")$diffJ, 0)
  expect_equal(dateDiff("01.12.1999", "01.01.2000")$diffM, 1)
  expect_equal(dateDiff("01.12.1999", "01.01.2000")$diffDauer, 1/12)
  expect_equal(dateDiff("01.12.1999", "01.01.2000")$diffDauerM, 1)
})
